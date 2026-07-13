"""Audit the phrasing direction of S4 conRestatement / conSummary texts.

Studies 1-3 classified each AI restatement as affirms / denies / unclear and
reverse-coded belief (100 - x) for "denies" (see Bunkbot_Polished_Analysis.Rmd
L141-143). Study 4's pipeline has no such step. This script applies the same
scheme to all S4 participants, double-rated by two models.

For each participant: classify conRestatement (the belief-DV statement) and
conSummary (the persuader-prompt statement) as:
  - affirms: asserts the conspiracy is true/real
  - denies:  asserts the official/non-conspiracy account (conspiracy is false)
  - unclear: ambiguous, double-barreled, or not a directional claim

Usage:
    python3 restatement_direction_audit.py            # both audit models
    python3 restatement_direction_audit.py --limit 5  # smoke test
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

# Transient pipeline intermediates (the v1 direction audit; the v2 audit's
# study4_restatement_orientation.{csv,jsonl} is the shipped successor).
WORK_DIR = REPO_ROOT / "output" / "provenance_work" / "stance_v2"
V2_DIR = WORK_DIR
OUT_PATH = V2_DIR / "restatement_direction_audit.jsonl"
CSV_PATH = V2_DIR / "restatement_direction_audit.csv"

AUDIT_VERSION = "a1.0"
MODELS = [
    "openrouter/openai/gpt-5.2",
    "openrouter/anthropic/claude-sonnet-4.6",
    "openrouter/google/gemini-3.1-pro-preview",  # adjudicator (majority of 3)
]

CATS = ["affirms", "denies", "unclear"]

SYSTEM_PROMPT = """You are auditing AI-generated statements from a research study on conspiracy beliefs. For each participant, an AI produced (A) a one-sentence restatement of their chosen conspiracy theory, and (B) a longer summary. Both were SUPPOSED to be phrased so that the statement asserts the conspiracy is TRUE (participants then rated their confidence that the statement is true on 0-100).

Classify the phrasing direction of statement A and statement B separately:
- "affirms": the statement asserts the conspiracy is true / the secret plot is real. Agreeing with the statement = believing the conspiracy. (e.g., "The CIA assassinated JFK using Oswald as a cover.")
- "denies": the statement asserts the official or non-conspiracy account; agreeing with it = REJECTING the conspiracy. (e.g., "Oswald acted alone in killing John F. Kennedy.")
- "unclear": no clear direction (a question, a topic label like "the JFK assassination", both directions, or too vague to tell).

Use the participant's own topic description to understand what the conspiracy is. Judge only the direction of each statement's phrasing, not its plausibility.

Return strict JSON: {"restatement_category": "...", "summary_category": "...", "rationale": "<= 25 words"}."""

JSON_SCHEMA = {
    "name": "direction_audit",
    "strict": True,
    "schema": {
        "type": "object",
        "properties": {
            "restatement_category": {"type": "string", "enum": CATS},
            "summary_category": {"type": "string", "enum": CATS},
            "rationale": {"type": "string"},
        },
        "required": ["restatement_category", "summary_category", "rationale"],
        "additionalProperties": False,
    },
}


def load_env() -> None:
    import os
    if os.environ.get("OPENROUTER_API_KEY"):
        return
    # Preferred: OPENROUTER_API_KEY in the environment (see .env.example at the
    # repo root). The external portable_claim_factcheck_toolkit/.env is NOT
    # shipped; kept only for the author's original tree.
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


def cache_key(model: str, rid: str, restatement: str, summary: str) -> str:
    return hashlib.sha256(json.dumps(
        {"m": model, "v": AUDIT_VERSION, "rid": rid, "r": restatement, "s": summary},
        sort_keys=True).encode()).hexdigest()


async def audit_one(model: str, part: dict, sem: asyncio.Semaphore) -> dict:
    rec = {
        "cache_key": cache_key(model, part["ResponseId"], part["focal_claim_restatement"],
                               part["focal_claim_summary"]),
        "ResponseId": part["ResponseId"],
        "model": model,
        "audit_version": AUDIT_VERSION,
        "request_ok": False,
        "error": None,
    }
    messages = [
        {"role": "system", "content": SYSTEM_PROMPT},
        {"role": "user", "content": json.dumps({
            "participant_topic_description": part["participant_topic_description"],
            "statement_A_restatement": part["focal_claim_restatement"],
            "statement_B_summary": part["focal_claim_summary"],
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
                if (isinstance(parsed, dict)
                        and parsed.get("restatement_category") in CATS
                        and parsed.get("summary_category") in CATS):
                    rec.update(restatement_category=parsed["restatement_category"],
                               summary_category=parsed["summary_category"],
                               rationale=str(parsed.get("rationale", ""))[:200],
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
    ap.add_argument("--concurrency", type=int, default=8)
    args = ap.parse_args()
    load_env()
    V2_DIR.mkdir(parents=True, exist_ok=True)

    parts: dict[str, dict] = {}
    for line in open(V2_DIR / "stance_v2_inputs.jsonl"):
        it = json.loads(line)
        parts.setdefault(it["ResponseId"], it)
    participants = sorted(parts.values(), key=lambda p: p["ResponseId"])
    if args.limit:
        participants = participants[: args.limit]
    print(f"{len(participants)} participants x {len(MODELS)} audit models")

    done: set[str] = set()
    if OUT_PATH.exists():
        for line in open(OUT_PATH):
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
                    if cache_key(model, p["ResponseId"], p["focal_claim_restatement"],
                                 p["focal_claim_summary"]) not in done]
            print(f"[{model}] {len(todo)} to audit")
            t0 = time.time()
            n = {"ok": 0, "err": 0}

            async def worker(p: dict) -> None:
                rec = await audit_one(model, p, sem)
                async with lock:
                    with open(OUT_PATH, "a") as f:
                        f.write(json.dumps(rec, ensure_ascii=False) + "\n")
                    n["ok" if rec["request_ok"] else "err"] += 1
                    if (n["ok"] + n["err"]) % 200 == 0:
                        print(f"[{model}] {n['ok'] + n['err']}/{len(todo)}")

            await asyncio.gather(*(worker(p) for p in todo))
            print(f"[{model}] DONE ok={n['ok']} err={n['err']} in {time.time() - t0:.0f}s")

    asyncio.run(run())

    # consolidate to CSV: one row per participant with both models' calls
    import csv as _csv
    from collections import Counter

    def tag(model: str) -> str:
        if "gpt" in model:
            return "gpt"
        return "gemini" if "gemini" in model else "claude"

    TAGS = ["gpt", "claude", "gemini"]
    by_rid: dict[str, dict] = {}
    for line in open(OUT_PATH):
        rec = json.loads(line)
        if not rec.get("request_ok"):
            continue
        row = by_rid.setdefault(rec["ResponseId"], {"ResponseId": rec["ResponseId"]})
        t = tag(rec["model"])
        row[f"{t}_restatement"] = rec["restatement_category"]
        row[f"{t}_summary"] = rec["summary_category"]
        row[f"{t}_rationale"] = rec["rationale"]

    def majority(row: dict, field: str) -> str:
        votes = [row.get(f"{t}_{field}") for t in TAGS if row.get(f"{t}_{field}")]
        if not votes:
            return ""
        top, n = Counter(votes).most_common(1)[0]
        return top if n > len(votes) / 2 else "disagree"

    for rid, row in by_rid.items():
        row["restatement_consensus"] = majority(row, "restatement")
        row["summary_consensus"] = majority(row, "summary")
    cols = (["ResponseId"]
            + [f"{t}_restatement" for t in TAGS] + ["restatement_consensus"]
            + [f"{t}_summary" for t in TAGS] + ["summary_consensus"]
            + [f"{t}_rationale" for t in TAGS])
    with open(CSV_PATH, "w", newline="") as f:
        w = _csv.DictWriter(f, fieldnames=cols)
        w.writeheader()
        for rid in sorted(by_rid):
            w.writerow({c: by_rid[rid].get(c, "") for c in cols})
    print(f"consolidated -> {CSV_PATH} ({len(by_rid)} participants)")


if __name__ == "__main__":
    main()
