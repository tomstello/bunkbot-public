"""Stance classifier v2: multi-provider, richer schema, one post per call.

Scores each social-media post for stance toward the participant's focal
conspiracy claim, returning a structured judgment (relevance, response type,
stance category + 0-100 score, evidence quote, rationale, confidence).

Decoding: temperature 0 where the provider supports it; automatically dropped
for raters that reject it (reasoning-class models). Structured outputs
(json_schema) attempted first, with fallback to instructed-JSON parsing.
Effective request parameters are recorded per response.

Caching: per-model JSONL, append-only, resumable; cache key =
sha256(model, prompt_version, replicate, item payload).

Usage:
    python3 score_post_stance_v2.py --models anthropic/claude-sonnet-4.6 --limit 5
    python3 score_post_stance_v2.py --probe            # 3-item compliance probe per candidate
    python3 score_post_stance_v2.py --items pilot_item_ids.txt   # pilot subset
    python3 score_post_stance_v2.py                    # full run, default panel
    python3 score_post_stance_v2.py --replicate 1 --items retest_item_ids.txt  # test-retest
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

HERE = Path(__file__).resolve().parent


def _repo_root(start: Path) -> Path:
    p = start.resolve()
    for cand in [p, *p.parents]:
        if (cand / "data").is_dir() and (cand / "code").is_dir():
            return cand
    raise RuntimeError("repo root (dir containing data/ and code/) not found")


REPO_ROOT = _repo_root(Path(__file__))
# parse_json_like ships in the claim_factcheck toolkit's utils.py.
TOOLKIT = REPO_ROOT / "code" / "provenance" / "python_api" / "claim_factcheck" / "scripts"
sys.path.insert(0, str(TOOLKIT))

from utils import parse_json_like  # noqa: E402  (toolkit helper, robust JSON extraction)

import litellm  # noqa: E402

litellm.suppress_debug_info = True

# Transient pipeline intermediates: the scoring inputs and per-model
# scores_*.jsonl caches (not shipped).
WORK_DIR = REPO_ROOT / "output" / "provenance_work" / "stance_v2"
V2_DIR = WORK_DIR
INPUTS_PATH = V2_DIR / "stance_v2_inputs.jsonl"

PROMPT_VERSION = "v2.2"

DEFAULT_PANEL = [
    "openrouter/anthropic/claude-sonnet-4.6",
    "openrouter/openai/gpt-5.2",
    "openrouter/google/gemini-3.1-pro-preview",  # gemini-3-pro-preview retired upstream
    "openrouter/x-ai/grok-4.3",                  # grok-4 deprecated upstream
    "openrouter/deepseek/deepseek-v3.2",         # open-weights rater (replaced llama-4-maverick
                                                 # after v2.0 pilot: weakest pairwise r, author call)
]

PROBE_CANDIDATES = [
    "openrouter/meta-llama/llama-4-maverick",
    "openrouter/deepseek/deepseek-v3.2",
    "openrouter/qwen/qwen3.6-27b",
]

FOCAL_RELEVANCE = ["focal_claim", "related_conspiracy", "other_topic", "no_propositional_content"]
RESPONSE_TYPE = ["assertion", "question_raising", "mixed_assertion_question",
                 "uncertainty_statement", "declines_to_post", "meta_task", "unclassifiable"]
STANCE_CATEGORY = ["argues_against", "leans_against", "neutral_uncommitted",
                   "mixed_both_sides", "leans_for", "argues_for", "not_applicable"]

SYSTEM_PROMPT = """You are an expert content analyst rating social media posts written by research participants about conspiracy theories. Each participant chose a conspiracy theory they were genuinely uncertain about and wrote a public social media post about it. Your job is to characterize what each post communicates to a public audience about the participant's specific focal conspiracy claim. Do not judge whether the claim is actually true.

You will receive: the writing task the participant saw (genre context: an imagined public social-media post; hedged or uncertain posts were explicitly invited); the focal conspiracy claim (a single declarative sentence phrased so that the claim asserts the conspiracy is TRUE); background on how the participant originally described the topic and their reasons for partial belief and skepticism; and the post text. Use the background ONLY to resolve references in the post (names, events, shorthand). Do not let the participant's earlier beliefs color your rating of the post itself. Rate only the post.

Output fields:

focal_relevance:
- "focal_claim": the post is about the focal conspiracy claim, even obliquely via clear references
- "related_conspiracy": about a different but related conspiracy or claim, not the focal one
- "other_topic": about something else entirely
- "no_propositional_content": no topic-relevant content (e.g., pure greetings, comments about the survey or the AI experience, empty filler)

response_type (the post's dominant communicative mode):
- "assertion": makes or endorses claims (including reporting others' claims with apparent endorsement)
- "question_raising": primarily poses questions -- "just asking questions", inviting an audience to wonder
- "mixed_assertion_question": substantively both asserting and questioning
- "uncertainty_statement": primarily expresses the author's own uncertainty or indecision
- "declines_to_post": the participant declines or refuses to write or share a post about the topic (e.g., "I wouldn't post about this", "I don't share this kind of thing online", "no thanks")
- "meta_task": about the survey, the AI conversation, or the task itself rather than the topic
- "unclassifiable": gibberish or too fragmentary to interpret

sarcasm_or_irony: true if the stance is conveyed through sarcasm, irony, or mockery.

stance_category with stance_score (0-100):
- "argues_for" (~80-100): asserts or strongly implies the focal claim is true
- "leans_for" (~60-79): suggestive support; premise-accepting questions; "where there's smoke there's fire"
- "neutral_uncommitted" (~45-55): mentions the topic without direction; pure information-seeking questions
- "mixed_both_sides": presents both directions; set the score by the post's net impression (balanced = ~50; "evidence both ways but I lean X" = score in the lean's range)
- "leans_against" (~21-40): suggestive doubt; mundane explanations offered tentatively
- "argues_against" (~0-20): asserts or strongly implies the claim is false
- "not_applicable" (stance_score = null): ONLY when focal_relevance is "other_topic" or "no_propositional_content", OR when response_type is "declines_to_post". Posts about the focal claim otherwise ALWAYS receive a score, including question-only posts.

Critical rules:
1. Rhetorical and premise-accepting questions: score the implicature, and note that rhetorical questions cut BOTH ways. "Did the government facilitate 9/11?" posted to a public feed invites doubt of the official account -- leans_for with response_type "question_raising". But a question can equally mock or dismiss the conspiracy ("Sure, and I suppose the moon is made of cheese too, right?") -- that leans_against or argues_against. Use the focal claim and the participant background to judge which side the question pressures. A genuinely neutral information request ("Does anyone know whether this was investigated?") is neutral_uncommitted at ~50.
1b. Partial endorsement counts. A post that endorses key supporting evidence, precursors, or a documented weaker version of the claim, presented as supporting the focal claim, leans for -- it need not assert the entire focal claim outright (e.g., for an extreme MKUltra-style claim, "MKUltra was real and the documents are out there" leans for). Conversely, conceding documented elements while rejecting the focal extension ("MKUltra happened, but the mind-control stuff is nonsense") leans against. Score the net support the post communicates for the focal claim.
1c. Declining to post: if the participant declines to write or share a post, use response_type "declines_to_post" with stance_category "not_applicable" (null score), whatever the focal_relevance.
2. Reported speech ("Senator X says..."): score the post's communicative endorsement. Enthusiastic amplification without distancing endorses the claim; explicit distancing ("...but I doubt it") leans against.
3. Sarcasm or mockery: score the implied stance and set sarcasm_or_irony true.
4. Posts relaying what the AI told them ("According to the AI, there is no evidence that...") with apparent acceptance: score the accepted content as the post's stance (assertion via attribution).
5. Never use 50 as a dumping ground. 50 means genuinely balanced or uncommitted content about the focal claim. Off-topic or contentless posts are "not_applicable" with a null score instead.
6. evidence_quote: copy the most stance-diagnostic words verbatim from the post (25 words max). rationale: at most 40 words.
7. confidence (0-1): the probability that a careful human coder would land within 10 points of your score (or in your category for null-score items). Use the full range; 0.5 means genuinely ambiguous.

Return strict JSON matching the schema. No prose outside the JSON."""

JSON_SCHEMA = {
    "name": "post_stance_v2",
    "strict": True,
    "schema": {
        "type": "object",
        "properties": {
            "focal_relevance": {"type": "string", "enum": FOCAL_RELEVANCE},
            "response_type": {"type": "string", "enum": RESPONSE_TYPE},
            "sarcasm_or_irony": {"type": "boolean"},
            "stance_category": {"type": "string", "enum": STANCE_CATEGORY},
            # range constraints enforced in validate_result(); some providers
            # (Anthropic structured outputs) reject minimum/maximum on integers
            "stance_score": {"type": ["integer", "null"],
                             "description": "0-100, or null iff stance_category is not_applicable"},
            "evidence_quote": {"type": "string"},
            "rationale": {"type": "string"},
            "confidence": {"type": "number", "description": "0 to 1"},
        },
        "required": ["focal_relevance", "response_type", "sarcasm_or_irony", "stance_category",
                     "stance_score", "evidence_quote", "rationale", "confidence"],
        "additionalProperties": False,
    },
}


def load_env() -> None:
    if os.environ.get("OPENROUTER_API_KEY"):
        return
    # Preferred: OPENROUTER_API_KEY in the environment (see .env.example at the
    # repo root). The external portable_claim_factcheck_toolkit/.env is NOT
    # shipped; kept only for the author's original tree.
    candidates = [
        REPO_ROOT / ".env",
        REPO_ROOT / "code" / ".env",
        REPO_ROOT.parent / "portable_claim_factcheck_toolkit" / ".env",
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


def claim_text(item: dict) -> str:
    # v2.2 inputs carry a canonical affirms-phrased claim; fall back for old inputs
    return item.get("focal_claim") or item["focal_claim_restatement"]


def cache_key(model: str, item: dict, replicate: int) -> str:
    payload = {
        "model": model,
        "prompt_version": PROMPT_VERSION,
        "replicate": replicate,
        "item_id": item["item_id"],
        "post": item["post_text"],
        "claim": claim_text(item),
    }
    return hashlib.sha256(json.dumps(payload, sort_keys=True).encode()).hexdigest()


def user_message(item: dict) -> str:
    return json.dumps({
        "writing_task_shown_to_participant": item["task_wording"],
        "timepoint": "before the AI conversation" if item["timepoint"] == "pre" else "after the AI conversation",
        "focal_claim": claim_text(item),
        "participant_background_for_reference_resolution_only": {
            "their_topic_description": item["participant_topic_description"],
            "their_stated_reasons_for_and_against": item["participant_reasons"],
        },
        "post_text": item["post_text"],
    }, ensure_ascii=False)


def validate_result(parsed: dict) -> tuple[dict | None, str | None]:
    try:
        out = {
            "focal_relevance": parsed["focal_relevance"],
            "response_type": parsed["response_type"],
            "sarcasm_or_irony": bool(parsed["sarcasm_or_irony"]),
            "stance_category": parsed["stance_category"],
            "stance_score": parsed["stance_score"],
            "evidence_quote": str(parsed.get("evidence_quote", ""))[:300],
            "rationale": str(parsed.get("rationale", ""))[:500],
            "confidence": float(parsed["confidence"]),
        }
    except (KeyError, TypeError, ValueError) as exc:
        return None, f"missing/invalid field: {exc}"
    if out["focal_relevance"] not in FOCAL_RELEVANCE:
        return None, f"bad focal_relevance {out['focal_relevance']!r}"
    if out["response_type"] not in RESPONSE_TYPE:
        return None, f"bad response_type {out['response_type']!r}"
    if out["stance_category"] not in STANCE_CATEGORY:
        return None, f"bad stance_category {out['stance_category']!r}"
    score = out["stance_score"]
    if out["stance_category"] == "not_applicable":
        if score == -1:  # null-sentinel some providers emit under strict schemas
            out["stance_score"] = score = None
        if score is not None:
            return None, "not_applicable must have null stance_score"
        if (out["focal_relevance"] not in ("other_topic", "no_propositional_content")
                and out["response_type"] != "declines_to_post"):
            return None, ("not_applicable only allowed for other_topic/"
                          "no_propositional_content or declines_to_post")
    elif out["response_type"] == "declines_to_post":
        return None, "declines_to_post requires stance_category not_applicable"
    else:
        if score is None:
            return None, "null stance_score outside not_applicable"
        if not isinstance(score, (int, float)) or not (0 <= float(score) <= 100):
            return None, f"score out of range: {score!r}"
        out["stance_score"] = int(round(float(score)))
    out["confidence"] = min(1.0, max(0.0, out["confidence"]))
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
        "timepoint": item["timepoint"],
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
            try:
                parsed = parse_json_like(text)
            except Exception:  # noqa: BLE001
                parsed = None
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
                        "corrected JSON object matching the schema. Remember: stance_score must be "
                        "null if and only if stance_category is \"not_applicable\"."},
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
    out_path = V2_DIR / f"scores_{model_slug(model)}.jsonl"
    out_path.parent.mkdir(parents=True, exist_ok=True)
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
    ap.add_argument("--inputs", default=str(V2_DIR / "stance_v22_inputs.jsonl"),
                    help="items JSONL (default: v2.2 canonical-claim inputs)")
    ap.add_argument("--items", default=None,
                    help="path to file of item_ids (one per line) to restrict to")
    ap.add_argument("--limit", type=int, default=None, help="cap number of items (debug)")
    ap.add_argument("--replicate", type=int, default=0,
                    help="replicate index (0 = main pass; 1 = test-retest)")
    ap.add_argument("--concurrency", type=int, default=8, help="concurrent requests per model")
    ap.add_argument("--timeout", type=int, default=180)
    ap.add_argument("--max-tries", type=int, default=5)
    ap.add_argument("--probe", action="store_true",
                    help="run 3-item JSON-compliance probe on open-weights candidates")
    args = ap.parse_args()

    load_env()
    inputs_path = Path(args.inputs)
    items = [json.loads(line) for line in open(inputs_path)]
    print(f"loaded {len(items)} items from {inputs_path.name} (prompt {PROMPT_VERSION})")

    if args.items:
        keep = {line.strip() for line in open(args.items) if line.strip()}
        items = [it for it in items if it["item_id"] in keep]
        print(f"restricted to {len(items)} items via {args.items}")
    if args.limit:
        items = items[: args.limit]

    models = PROBE_CANDIDATES if args.probe else [m.strip() for m in args.models.split(",") if m.strip()]
    if args.probe:
        items = items[:3]
        print("PROBE MODE: 3 items per candidate")

    async def run_all() -> None:
        for model in models:  # sequential across models, concurrent within
            await run_model(model, items, args.replicate, args.concurrency,
                            args.timeout, args.max_tries)

    asyncio.run(run_all())


if __name__ == "__main__":
    main()
