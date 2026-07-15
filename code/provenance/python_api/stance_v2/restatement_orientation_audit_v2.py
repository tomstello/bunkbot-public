"""Restatement orientation audit v2 — the belief-DV-critical classification.

Sharper operationalization than v1 (restatement_direction_audit.py): instead of
asking whether a statement "asserts the conspiracy", asks directly what a 100
rating on the belief item MEANS, judged against the participant's own raw text:

    If a participant rates this statement 100 (definitely true), are they
    ENDORSING the conspiracy they described (affirms), REJECTING it /
    endorsing the official account (denies), or is it unclear?

Motivating false negative from v1: "The United States successfully landed on
the moon first." (participant's conspiracy: moon landing faked) was labeled
affirms/unclear by v1; it is unambiguously denies under this framing.

Each call also generates `affirmative_restatement` — a one-sentence
pro-conspiracy phrasing of the participant's conspiracy — used downstream as
the canonical rater-facing focal claim (provenance: this script).

3 audit models, majority vote. Writes the shipped
study4_restatement_orientation.jsonl/.csv (data/api_cached/sharing_and_stance/).
"""

from __future__ import annotations

import argparse
import asyncio
import hashlib
import json
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

from utils import parse_json_like  # noqa: E402

import litellm  # noqa: E402

litellm.suppress_debug_info = True

# Transient pipeline intermediate (built by build_stance_inputs.py).
WORK_DIR = REPO_ROOT / "output" / "provenance_work" / "stance_v2"
V2_DIR = WORK_DIR
# Shipped restatement-orientation audit outputs (.csv shipped; .jsonl paired
# intermediate kept alongside).
SHARING_DIR = REPO_ROOT / "data" / "api_cached" / "sharing_and_stance"
OUT_PATH = SHARING_DIR / "study4_restatement_orientation.jsonl"
CSV_PATH = SHARING_DIR / "study4_restatement_orientation.csv"

AUDIT_VERSION = "a2.0"
MODELS = [
    "openrouter/openai/gpt-5.2",
    "openrouter/anthropic/claude-sonnet-4.6",
    "openrouter/google/gemini-3.1-pro-preview",
]
CATS = ["affirms", "denies", "unclear"]

SYSTEM_PROMPT = """You are auditing a measurement instrument from a research study on conspiracy beliefs.

Each participant described a conspiracy theory in their own words (their topic description and their reasons for partial belief and skepticism). An AI then generated a one-sentence RESTATEMENT, and participants rated: "your level of confidence that this statement is true" on a 0-100 scale (0 = Definitely false, 100 = Definitely true).

The restatement was SUPPOSED to be phrased so that rating it 100 means fully believing the conspiracy. Sometimes the generator failed and produced the opposite phrasing (asserting the official, non-conspiracy account).

Your question: if a participant rates the RESTATEMENT as 100 (definitely true), are they:
- "affirms": endorsing the conspiracy they described (100 = believes the conspiracy). Example: conspiracy = moon landing faked; restatement = "The Apollo moon landings were staged."
- "denies": rejecting the conspiracy / endorsing the official account (100 = disbelieves the conspiracy). Example: conspiracy = moon landing faked; restatement = "The United States successfully landed on the moon."
- "unclear": the restatement has no clear direction relative to the conspiracy (a topic label, a question, both-directions, or unrelated to what the participant described).

Judge against the participant's own description of their conspiracy. Phrasing style does not matter; only what a 100 rating commits the rater to.

Also produce "affirmative_restatement": one declarative sentence stating the participant's conspiracy as TRUE (so that rating IT 100 would mean fully believing the conspiracy). If the original restatement already does this, you may reuse it verbatim.

Return strict JSON: {"orientation": "affirms|denies|unclear", "affirmative_restatement": "...", "rationale": "<= 20 words"}."""

JSON_SCHEMA = {
    "name": "orientation_audit",
    "strict": True,
    "schema": {
        "type": "object",
        "properties": {
            "orientation": {"type": "string", "enum": CATS},
            "affirmative_restatement": {"type": "string"},
            "rationale": {"type": "string"},
        },
        "required": ["orientation", "affirmative_restatement", "rationale"],
        "additionalProperties": False,
    },
}


def load_env() -> None:
    import os
    if os.environ.get("OPENROUTER_API_KEY"):
        return
    # Preferred: OPENROUTER_API_KEY in the environment (see .env.example at the
    # repo root). The repo does NOT ship a real .env or the external
    # portable_claim_factcheck_toolkit/; the last candidate is kept only for the
    # author's original tree.
    for path in (REPO_ROOT / ".env", REPO_ROOT / "code/.env",
                 REPO_ROOT.parent / "portable_claim_factcheck_toolkit/.env"):
        if path.exists():
            for line in path.read_text().splitlines():
                m = re.match(r"^\s*([A-Z_]+)\s*=\s*(.+?)\s*$", line)
                if m and m.group(1) not in os.environ:
                    os.environ[m.group(1)] = m.group(2).strip("'\"")
            if os.environ.get("OPENROUTER_API_KEY"):
                return
    raise SystemExit("OPENROUTER_API_KEY not found")


def cache_key(model: str, rid: str, restatement: str) -> str:
    return hashlib.sha256(json.dumps(
        {"m": model, "v": AUDIT_VERSION, "rid": rid, "r": restatement},
        sort_keys=True).encode()).hexdigest()


async def audit_one(model: str, part: dict, sem: asyncio.Semaphore) -> dict:
    rec = {
        "cache_key": cache_key(model, part["ResponseId"], part["focal_claim_restatement"]),
        "ResponseId": part["ResponseId"],
        "model": model,
        "audit_version": AUDIT_VERSION,
        "request_ok": False,
        "error": None,
    }
    messages = [
        {"role": "system", "content": SYSTEM_PROMPT},
        {"role": "user", "content": json.dumps({
            "participant_conspiracy_description": part["participant_topic_description"],
            "participant_reasons_for_and_against": part["participant_reasons"],
            "RESTATEMENT_shown_on_the_rating_scale": part["focal_claim_restatement"],
        }, ensure_ascii=False)},
    ]
    async with sem:
        for attempt in range(1, 5):
            try:
                resp = await litellm.acompletion(
                    model=model, messages=messages, temperature=0, timeout=120,
                    response_format={"type": "json_schema", "json_schema": JSON_SCHEMA})
            except Exception as exc:  # noqa: BLE001
                rec["error"] = f"{type(exc).__name__}: {exc}"[:300]
                await asyncio.sleep(min(15.0, 0.8 * 2 ** attempt))
                continue
            try:
                parsed = parse_json_like(resp.choices[0].message.content or "")
                if isinstance(parsed, dict) and parsed.get("orientation") in CATS:
                    rec.update(orientation=parsed["orientation"],
                               affirmative_restatement=str(parsed.get("affirmative_restatement", ""))[:400],
                               rationale=str(parsed.get("rationale", ""))[:150],
                               request_ok=True, error=None)
                    return rec
                rec["error"] = "invalid fields"
            except Exception:  # noqa: BLE001
                rec["error"] = "unparseable"
            await asyncio.sleep(min(15.0, 0.8 * 2 ** attempt))
    return rec


def main() -> None:
    ap = argparse.ArgumentParser()
    ap.add_argument("--limit", type=int, default=None)
    ap.add_argument("--concurrency", type=int, default=10)
    ap.add_argument("--inputs", default=str(V2_DIR / "stance_v2_inputs.jsonl"))
    ap.add_argument("--jsonl-out", default=str(OUT_PATH))
    ap.add_argument("--csv-out", default=str(CSV_PATH))
    args = ap.parse_args()
    load_env()

    inputs_path = Path(args.inputs)
    out_path = Path(args.jsonl_out)
    csv_path = Path(args.csv_out)
    out_path.parent.mkdir(parents=True, exist_ok=True)
    csv_path.parent.mkdir(parents=True, exist_ok=True)

    parts: dict[str, dict] = {}
    for line in open(inputs_path):
        it = json.loads(line)
        parts.setdefault(it["ResponseId"], it)
    participants = sorted(parts.values(), key=lambda p: p["ResponseId"])
    if args.limit:
        participants = participants[: args.limit]
    print(f"{len(participants)} participants x {len(MODELS)} audit models (v{AUDIT_VERSION})")

    done: set[str] = set()
    if out_path.exists():
        for line in open(out_path):
            try:
                rec = json.loads(line)
                if rec.get("request_ok"):
                    done.add(rec["cache_key"])
            except json.JSONDecodeError:
                pass

    async def run() -> None:
        sem = asyncio.Semaphore(args.concurrency)
        lock = asyncio.Lock()
        for model in MODELS:
            todo = [p for p in participants
                    if cache_key(model, p["ResponseId"], p["focal_claim_restatement"]) not in done]
            print(f"[{model}] {len(todo)} to audit")
            t0 = time.time()
            n = {"ok": 0, "err": 0}

            async def worker(p: dict) -> None:
                rec = await audit_one(model, p, sem)
                async with lock:
                    with open(out_path, "a") as f:
                        f.write(json.dumps(rec, ensure_ascii=False) + "\n")
                    n["ok" if rec["request_ok"] else "err"] += 1

            await asyncio.gather(*(worker(p) for p in todo))
            print(f"[{model}] DONE ok={n['ok']} err={n['err']} in {time.time() - t0:.0f}s")

    asyncio.run(run())

    # consolidate: majority orientation; canonical claim from highest-priority
    # affirms-judging model (gpt > claude > gemini) or original restatement
    import csv as _csv
    from collections import Counter

    def tag(model: str) -> str:
        if "gpt" in model:
            return "gpt"
        return "gemini" if "gemini" in model else "claude"

    TAGS = ["gpt", "claude", "gemini"]
    by_rid: dict[str, dict] = {}
    for line in open(out_path):
        rec = json.loads(line)
        if not rec.get("request_ok"):
            continue
        row = by_rid.setdefault(rec["ResponseId"], {"ResponseId": rec["ResponseId"]})
        t = tag(rec["model"])
        row[f"{t}_orientation"] = rec["orientation"]
        row[f"{t}_affirmative"] = rec["affirmative_restatement"]
        row[f"{t}_rationale"] = rec["rationale"]

    for rid, row in by_rid.items():
        votes = [row.get(f"{t}_orientation") for t in TAGS if row.get(f"{t}_orientation")]
        top, n = Counter(votes).most_common(1)[0] if votes else ("", 0)
        row["orientation_consensus"] = top if n > len(votes) / 2 else "disagree"
        row["n_votes"] = len(votes)
        # canonical affirmative claim: take it from a rater that voted WITH the
        # consensus (a dissenting rater's "affirmative" rewrite inherits its
        # misreading — e.g., reusing a denies-phrased original verbatim)
        with_consensus = [t for t in TAGS
                          if row.get(f"{t}_orientation") == row["orientation_consensus"]]
        pool = with_consensus or TAGS
        row["canonical_claim"] = next(
            (row[f"{t}_affirmative"] for t in pool if row.get(f"{t}_affirmative")), "")

    cols = (["ResponseId"] + [f"{t}_orientation" for t in TAGS]
            + ["orientation_consensus", "n_votes", "canonical_claim"]
            + [f"{t}_rationale" for t in TAGS])
    with open(csv_path, "w", newline="") as f:
        w = _csv.DictWriter(f, fieldnames=cols)
        w.writeheader()
        for rid in sorted(by_rid):
            w.writerow({c: by_rid[rid].get(c, "") for c in cols})
    print(f"consolidated -> {csv_path} ({len(by_rid)} participants)")
    counts = Counter(r["orientation_consensus"] for r in by_rid.values())
    print("consensus orientation:", dict(counts.most_common()))


if __name__ == "__main__":
    main()
