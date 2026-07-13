"""Persuasion-explanation coder: multi-provider, multi-label, one response per call.

Each participant open-ended response (Persuasive_oe / Notpersuasive_oe) is coded for
response_quality + 15 binary themes + primary_theme + evidence_quote + confidence.
Structure mirrors stance_v2/score_post_stance_v2.py: 5-model panel via litellm ->
OpenRouter, temperature 0 where supported (auto-dropped otherwise), json_schema
structured output with instructed-JSON fallback, per-model append-only JSONL cache
(sha256 key), corrective retry, resumable.

Usage:
    python3 score_explanation.py --probe                       # 3-item JSON probe per panel model
    python3 score_explanation.py --limit 5                     # debug: first 5 items
    python3 score_explanation.py --items pilot_item_ids.txt    # pilot / restricted subset
    python3 score_explanation.py --items analysis_item_ids.txt # full analysis-sample run
    python3 score_explanation.py --replicate 1 --items retest_item_ids.txt  # test-retest
"""

from __future__ import annotations

import argparse
import asyncio
import hashlib
import json
import os
import re
import sys
import time
from pathlib import Path

def _repo_root(start: Path) -> Path:
    """Walk up to the repo root: the dir containing BOTH data/ and code/."""
    p = start.resolve()
    for cand in [p, *p.parents]:
        if (cand / "data").is_dir() and (cand / "code").is_dir():
            return cand
    raise RuntimeError("repo root (dir containing data/ and code/) not found")


HERE = Path(__file__).resolve().parent
REPO_ROOT = _repo_root(HERE)
sys.path.insert(0, str(HERE))

from taxonomy import (  # noqa: E402
    SYSTEM_PROMPT, json_schema, THEME_KEYS, RESPONSE_QUALITY, PRIMARY_THEME,
)

import litellm  # noqa: E402

litellm.suppress_debug_info = True

DATA_DIR = REPO_ROOT / "data/api_cached/explanation_coding"
INPUTS_PATH = DATA_DIR / "explanation_inputs.jsonl"
JSON_SCHEMA = json_schema()

PROMPT_VERSION = "v1.0"

DEFAULT_PANEL = [
    "openrouter/anthropic/claude-sonnet-4.6",
    "openrouter/openai/gpt-5.2",
    "openrouter/google/gemini-3.1-pro-preview",
    "openrouter/x-ai/grok-4.3",
    "openrouter/deepseek/deepseek-v3.2",
]


def load_env() -> None:
    if os.environ.get("OPENROUTER_API_KEY"):
        return
    # Preferred: OPENROUTER_API_KEY in the environment (see .env.example at the repo root).
    # The repo does NOT ship a real .env or the external portable_claim_factcheck_toolkit/;
    # those candidates are kept only as upstream/local fallbacks (not-shipped) so the panel
    # can be re-run from the author's machine.
    candidates = [
        REPO_ROOT / ".env",
        REPO_ROOT / "code" / ".env",
        # NOT SHIPPED — external author-side locations, retained for local re-runs only.
        REPO_ROOT.parent / "portable_claim_factcheck_toolkit" / ".env",
        REPO_ROOT / "code" / "python_api" / "claim_factcheck" / ".env",
    ]
    for path in candidates:
        if path.exists():
            for line in path.read_text().splitlines():
                m = re.match(r"^\s*([A-Z_]+)\s*=\s*(.+?)\s*$", line)
                if m and m.group(1) not in os.environ:
                    os.environ[m.group(1)] = m.group(2).strip("'\"")
            if os.environ.get("OPENROUTER_API_KEY"):
                return
    raise SystemExit("OPENROUTER_API_KEY not found in environment or known .env locations")


def parse_json_like(text: str):
    """Robust JSON extraction: strip code fences, then try direct parse, then the
    first balanced {...} object."""
    if not isinstance(text, str):
        return None
    clean = text.strip()
    if clean.startswith("```"):
        clean = "\n".join(l for l in clean.splitlines() if not l.strip().startswith("```")).strip()
    try:
        return json.loads(clean)
    except Exception:  # noqa: BLE001
        pass
    start = clean.find("{")
    if start == -1:
        return None
    depth = 0
    for i in range(start, len(clean)):
        if clean[i] == "{":
            depth += 1
        elif clean[i] == "}":
            depth -= 1
            if depth == 0:
                try:
                    return json.loads(clean[start:i + 1])
                except Exception:  # noqa: BLE001
                    return None
    return None


def cache_key(model: str, item: dict, replicate: int) -> str:
    payload = {
        "model": model,
        "prompt_version": PROMPT_VERSION,
        "replicate": replicate,
        "item_id": item["item_id"],
        "response": item["response_text"],
        "question": item["question_prompt"],
        "topic": item.get("topic_hint", ""),
    }
    return hashlib.sha256(json.dumps(payload, sort_keys=True).encode()).hexdigest()


def user_message(item: dict) -> str:
    which = ("what they found PERSUASIVE about the AI's comments" if item["field"] == "persuasive"
             else "what they did NOT find persuasive about the AI's comments")
    return json.dumps({
        "which_question": which,
        "question_prompt_shown_to_participant": item["question_prompt"],
        "participant_topic_for_reference_resolution_only": item.get("topic_hint", ""),
        "response_text": item["response_text"],
    }, ensure_ascii=False)


def validate_result(parsed: dict) -> tuple[dict | None, str | None]:
    if not isinstance(parsed, dict):
        return None, "not a JSON object"
    rq = parsed.get("response_quality")
    if rq not in RESPONSE_QUALITY:
        return None, f"bad response_quality {rq!r}"
    raw_themes = parsed.get("themes")
    if not isinstance(raw_themes, dict):
        return None, "themes missing/not an object"
    themes = {}
    for k in THEME_KEYS:
        if k not in raw_themes:
            return None, f"missing theme {k!r}"
        themes[k] = bool(raw_themes[k])
    pt = parsed.get("primary_theme")
    if pt not in PRIMARY_THEME:
        return None, f"bad primary_theme {pt!r}"
    try:
        conf = float(parsed.get("confidence"))
    except (TypeError, ValueError):
        return None, "bad confidence"
    coerced = False
    # invariant: non-substantive responses carry no themes
    if rq != "substantive":
        if any(themes.values()) or pt != "none":
            coerced = True
        themes = {k: False for k in THEME_KEYS}
        pt = "none"
    out = {
        "response_quality": rq,
        "themes": themes,
        "primary_theme": pt,
        "evidence_quote": str(parsed.get("evidence_quote", ""))[:300],
        "rationale": str(parsed.get("rationale", ""))[:500],
        "confidence": min(1.0, max(0.0, conf)),
        "coerced_nonsubstantive": coerced,
    }
    return out, None


# per-model capability flags learned during the run
_caps: dict[str, dict] = {}


def _param_error(exc: Exception) -> str | None:
    msg = str(exc).lower()
    if "temperature" in msg:
        return "temperature"
    if any(s in msg for s in ("response_format", "json_schema", "structured",
                              "output_config", "format.schema", "schema:")):
        return "response_format"
    return None


async def score_one(model: str, item: dict, replicate: int, sem: asyncio.Semaphore,
                    timeout: int, max_tries: int) -> dict:
    caps = _caps.setdefault(model, {"temperature": True, "response_format": True})
    record = {
        "cache_key": cache_key(model, item, replicate),
        "item_id": item["item_id"],
        "ResponseId": item["ResponseId"],
        "study": item["study"],
        "field": item["field"],
        "model": model,
        "prompt_version": PROMPT_VERSION,
        "replicate": replicate,
        "request_ok": False,
        "error": None,
        "effective_params": None,
        "raw_text": None,
    }
    messages = [
        {"role": "system", "content": SYSTEM_PROMPT},
        {"role": "user", "content": user_message(item)},
    ]
    corrective = False
    async with sem:
        for attempt in range(1, max_tries + 1):
            kwargs: dict = {"model": model, "messages": messages, "timeout": timeout}
            if caps["temperature"]:
                kwargs["temperature"] = 0
            if caps["response_format"]:
                kwargs["response_format"] = {"type": "json_schema", "json_schema": JSON_SCHEMA}
            try:
                resp = await litellm.acompletion(**kwargs)
            except Exception as exc:  # noqa: BLE001
                knob = _param_error(exc)
                if knob and caps[knob]:
                    caps[knob] = False  # drop unsupported param and retry immediately
                    continue
                record["error"] = f"attempt {attempt}: {type(exc).__name__}: {exc}"[:500]
                if attempt < max_tries:
                    await asyncio.sleep(min(20.0, 0.8 * (2 ** (attempt - 1))))
                    continue
                return record
            text = resp.choices[0].message.content or ""
            record["raw_text"] = text[:2000]
            parsed = parse_json_like(text)
            if isinstance(parsed, dict):
                out, err = validate_result(parsed)
                if out is not None:
                    record.update(out)
                    record["request_ok"] = True
                    record["error"] = None
                    record["raw_text"] = None
                    record["effective_params"] = {
                        "temperature": 0 if caps["temperature"] else "provider_default",
                        "structured_output": caps["response_format"],
                    }
                    usage = getattr(resp, "usage", None)
                    if usage is not None:
                        record["tokens"] = {
                            "prompt": getattr(usage, "prompt_tokens", None),
                            "completion": getattr(usage, "completion_tokens", None),
                        }
                    return record
                record["error"] = f"validation: {err}"
            else:
                record["error"] = "unparseable JSON"
            if not corrective:
                corrective = True
                messages = messages[:2] + [
                    {"role": "assistant", "content": text[:1500]},
                    {"role": "user", "content":
                        f"Your previous response was invalid ({record['error']}). Return ONLY a "
                        "corrected JSON object matching the schema: response_quality, a themes object "
                        "with all 15 boolean keys, primary_theme, evidence_quote, rationale, confidence."},
                ]
            if attempt < max_tries:
                await asyncio.sleep(min(20.0, 0.8 * (2 ** (attempt - 1))))
    return record


def model_slug(model: str) -> str:
    return re.sub(r"[^A-Za-z0-9._-]+", "_", model.removeprefix("openrouter/"))


def load_done(path: Path) -> set[str]:
    done = set()
    if path.exists():
        with open(path) as f:
            for line in f:
                try:
                    rec = json.loads(line)
                except json.JSONDecodeError:
                    continue
                if rec.get("request_ok"):
                    done.add(rec["cache_key"])
    return done


async def run_model(model: str, items: list[dict], replicate: int, concurrency: int,
                    timeout: int, max_tries: int) -> None:
    out_path = DATA_DIR / f"scores_{model_slug(model)}.jsonl"
    done = load_done(out_path)
    todo = [it for it in items if cache_key(model, it, replicate) not in done]
    print(f"[{model}] {len(todo)} to score ({len(items) - len(todo)} cached) -> {out_path.name}")
    if not todo:
        return
    sem = asyncio.Semaphore(concurrency)
    lock = asyncio.Lock()
    n_ok = n_err = 0
    t0 = time.time()

    async def worker(it: dict) -> None:
        nonlocal n_ok, n_err
        rec = await score_one(model, it, replicate, sem, timeout, max_tries)
        async with lock:
            with open(out_path, "a") as f:
                f.write(json.dumps(rec, ensure_ascii=False) + "\n")
            if rec["request_ok"]:
                n_ok += 1
            else:
                n_err += 1
            total = n_ok + n_err
            if total % 100 == 0:
                rate = total / max(1e-9, time.time() - t0)
                print(f"[{model}] {total}/{len(todo)} ok={n_ok} err={n_err} ({rate:.1f}/s)")

    await asyncio.gather(*(worker(it) for it in todo))
    print(f"[{model}] DONE ok={n_ok} err={n_err} elapsed={time.time() - t0:.0f}s")


def main() -> None:
    ap = argparse.ArgumentParser()
    ap.add_argument("--models", default=",".join(DEFAULT_PANEL),
                    help="comma-separated litellm model ids")
    ap.add_argument("--inputs", default=str(INPUTS_PATH))
    ap.add_argument("--items", default=None,
                    help="path to file of item_ids (one per line) to restrict to")
    ap.add_argument("--limit", type=int, default=None, help="cap number of items (debug)")
    ap.add_argument("--replicate", type=int, default=0,
                    help="replicate index (0 = main pass; 1 = test-retest)")
    ap.add_argument("--concurrency", type=int, default=8, help="concurrent requests per model")
    ap.add_argument("--timeout", type=int, default=180)
    ap.add_argument("--max-tries", type=int, default=5)
    ap.add_argument("--probe", action="store_true",
                    help="3-item JSON-compliance probe on each panel model")
    args = ap.parse_args()

    load_env()
    items = [json.loads(line) for line in open(args.inputs)]
    print(f"loaded {len(items)} items from {Path(args.inputs).name} (prompt {PROMPT_VERSION})")

    if args.items:
        items_path = Path(args.items)
        if not items_path.exists():
            items_path = DATA_DIR / args.items   # item-id lists live alongside cached data
        keep = {line.strip() for line in open(items_path) if line.strip()}
        items = [it for it in items if it["item_id"] in keep]
        print(f"restricted to {len(items)} items via {items_path}")
    if args.limit:
        items = items[: args.limit]

    models = [m.strip() for m in args.models.split(",") if m.strip()]
    if args.probe:
        items = items[:3]
        print("PROBE MODE: 3 items per panel model")

    async def run_all() -> None:
        for model in models:  # sequential across models, concurrent within
            await run_model(model, items, args.replicate, args.concurrency,
                            args.timeout, args.max_tries)

    asyncio.run(run_all())


if __name__ == "__main__":
    main()
