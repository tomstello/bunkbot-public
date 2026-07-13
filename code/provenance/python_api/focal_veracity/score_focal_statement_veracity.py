#!/usr/bin/env python3
"""Focal-statement veracity scorer.

Scores the AI restatement of each analytic participant's focal conspiracy
(`conRestatement` -- the exact declarative sentence that anchored the belief
ratings) on a single 0-100 veracity scale across all four studies, with an
explicit statement-type classification so denial-phrased and no-claim
restatements are identified rather than silently scored as conspiracy claims
(the flaw in the superseded earlier runs; see README.md).

Input : output/provenance_work/focal_veracity/focal_veracity_inputs.csv
        (built API-free by extract_inputs.R from the engine's analytic frames)
Output: output/provenance_work/focal_veracity/focal_veracity_scores.jsonl  (resumable)
        data/api_cached/focal_veracity/study{N}_{regime}_focal_statement_veracity.csv
        data/api_cached/focal_veracity/focal_statement_veracity_all_studies.csv

Judge : openrouter/perplexity/sonar-pro (live-web; the same fact-check layer as
        the paper's claim-level pipeline), temperature 0, strict JSON, 3 retries.

Usage:
  python3 score_focal_statement_veracity.py --dry-run          # print 3 prompts, no API calls
  python3 score_focal_statement_veracity.py --limit 20         # pilot batch
  python3 score_focal_statement_veracity.py                    # full run (resumes)
Requires OPENROUTER_API_KEY in this folder's .env, ../claim_factcheck/.env, or
the repo's prompts/.env.
"""
from __future__ import annotations

import argparse
import asyncio
import csv
import hashlib
import json
import os
import sys
import time
from pathlib import Path


def _repo_root(start: Path) -> Path:
    p = start.resolve()
    for cand in [p, *p.parents]:
        if (cand / "data").is_dir() and (cand / "code").is_dir():
            return cand
    raise RuntimeError("repo root (dir containing data/ and code/) not found")


ROOT_DIR = Path(__file__).resolve().parent
REPO_ROOT = _repo_root(Path(__file__))
WORK_DIR = REPO_ROOT / "output" / "provenance_work" / "focal_veracity"
SHIP_DIR = REPO_ROOT / "data" / "api_cached" / "focal_veracity"
INPUT_CSV = WORK_DIR / "focal_veracity_inputs.csv"
SCORES_JSONL = WORK_DIR / "focal_veracity_scores.jsonl"

try:
    from dotenv import load_dotenv
    for env in (ROOT_DIR / ".env",
                ROOT_DIR.parent / "claim_factcheck" / ".env",
                REPO_ROOT / "prompts" / ".env"):
        if env.exists():
            load_dotenv(env)
            break
except ImportError:
    pass

MODEL = os.environ.get("FOCAL_VERACITY_MODEL", "openrouter/perplexity/sonar-pro")
CONCURRENCY = int(os.environ.get("FOCAL_VERACITY_CONCURRENCY", "8"))
MAX_RETRIES = 3

SYSTEM_PROMPT = """\
You are a careful fact-checking judge assisting a research study on conspiracy beliefs.

You will be given:
- STATEMENT: one declarative sentence expressing a claim that a survey participant \
partially believes. It is an AI-generated restatement of the participant's own words, \
and this exact sentence anchored the participant's belief ratings.
- PARTICIPANT_DESCRIPTION: the participant's original free-text description \
(context only; often hedged, fragmentary, or equivocal).

Your job has two parts.

PART 1 - CLASSIFY the STATEMENT as exactly one of:
- "conspiracy_claim": it asserts that actors coordinated in secret to achieve an \
outcome, or asserts a covert plot, cover-up, staged event, or suppressed truth.
- "official_account": it asserts the mainstream/official account of events or denies \
that a conspiracy exists.
- "no_claim": it does not assert any checkable claim (e.g., it reports that the \
participant has no conspiracy theory in mind, is purely a question, or is too vague \
to carry factual content).

PART 2 - if statement_type is "conspiracy_claim" or "official_account", FACT-CHECK the \
claim AS STATED, using the best publicly available evidence:
- veracity_score (integer 0-100): 0 = clearly false as stated; 25 = mostly false or \
largely unsupported; 50 = genuinely uncertain, mixed, or inconclusive; 75 = mostly \
true; 100 = clearly true.
- Evaluate the claim at the scope stated. Do not narrow a broad claim to a defensible \
core, and do not broaden a narrow claim. If the statement bundles several assertions, \
score the central allegation about secret coordination, not peripheral details.
- checkability (integer 0-100): how amenable the claim is to empirical evaluation with \
public evidence (0 = unfalsifiable; 100 = directly checkable, well-documented).
- label: one of "false", "mostly_false", "mixed_or_unclear", "mostly_true", "true", \
or "not_checkable" (use "not_checkable" when checkability is very low).
- rationale: 1-2 sentences citing the decisive evidence, or why it is inconclusive.

Calibration notes: absence of evidence for a covert plot, in places where evidence \
would be expected to surface, counts against the claim. Do not penalize a claim merely \
for being socially disfavored; some conspiracy claims are true. Judge the world, not \
the participant.

If statement_type is "no_claim", set veracity_score, checkability to null and label to \
"not_checkable".

Return STRICT JSON only, no prose:
{"statement_type": "...", "veracity_score": <int|null>, "checkability": <int|null>, \
"label": "...", "rationale": "..."}"""

PROMPT_SHA = hashlib.sha256(SYSTEM_PROMPT.encode()).hexdigest()[:12]

VALID_TYPES = {"conspiracy_claim", "official_account", "no_claim"}
VALID_LABELS = {"false", "mostly_false", "mixed_or_unclear", "mostly_true", "true", "not_checkable"}


def user_prompt(row: dict) -> str:
    return (f"STATEMENT: {row['statement'].strip()}\n\n"
            f"PARTICIPANT_DESCRIPTION: {(row.get('participant_description') or '').strip() or '(none provided)'}")


def parse_response(text: str) -> dict:
    t = text.strip()
    if t.startswith("```"):
        t = t.strip("`")
        t = t[t.find("{"):]
    start, end = t.find("{"), t.rfind("}")
    if start < 0 or end <= start:
        raise ValueError(f"no JSON object in response: {text[:200]!r}")
    obj = json.loads(t[start:end + 1])
    st = obj.get("statement_type")
    if st not in VALID_TYPES:
        raise ValueError(f"bad statement_type: {st!r}")
    lab = obj.get("label")
    if lab not in VALID_LABELS:
        raise ValueError(f"bad label: {lab!r}")
    for k in ("veracity_score", "checkability"):
        v = obj.get(k)
        if v is not None:
            v = int(round(float(v)))
            if not (0 <= v <= 100):
                raise ValueError(f"{k} out of range: {v}")
            obj[k] = v
    if st == "no_claim":
        obj["veracity_score"] = None
        obj["checkability"] = None
        obj["label"] = "not_checkable"
    elif obj.get("veracity_score") is None:
        raise ValueError("veracity_score null for a checkable statement_type")
    return obj


async def score_one(sem: asyncio.Semaphore, row: dict) -> dict:
    from litellm import acompletion
    out = {"response_id": row["response_id"], "study": row["study"], "regime": row["regime"],
           "direction": row["direction"], "category": row["category"],
           "statement": row["statement"],
           "veracity_model": MODEL, "prompt_sha": PROMPT_SHA,
           "scored_at": time.strftime("%Y-%m-%dT%H:%M:%S")}
    last_err = None
    for attempt in range(1, MAX_RETRIES + 1):
        try:
            async with sem:
                resp = await acompletion(
                    model=MODEL,
                    messages=[{"role": "system", "content": SYSTEM_PROMPT},
                              {"role": "user", "content": user_prompt(row)}],
                    temperature=0,
                    max_tokens=500,
                )
            parsed = parse_response(resp.choices[0].message.content or "")
            out.update(parsed)
            out["status"] = "ok"
            return out
        except Exception as e:  # noqa: BLE001 - record and retry
            last_err = f"{type(e).__name__}: {e}"
            await asyncio.sleep(min(2 ** attempt, 15))
    out.update({"status": "error", "error": last_err, "statement_type": None,
                "veracity_score": None, "checkability": None, "label": None, "rationale": None})
    return out


def load_done() -> set[str]:
    done = set()
    if SCORES_JSONL.exists():
        with open(SCORES_JSONL) as f:
            for line in f:
                try:
                    d = json.loads(line)
                except json.JSONDecodeError:
                    continue
                if d.get("status") == "ok":
                    done.add(d["response_id"])
    return done


async def run(rows: list[dict], writer) -> int:
    sem = asyncio.Semaphore(CONCURRENCY)
    n_done = 0
    tasks = [score_one(sem, r) for r in rows]
    for fut in asyncio.as_completed(tasks):
        res = await fut
        writer.write(json.dumps(res, ensure_ascii=False) + "\n")
        writer.flush()
        n_done += 1
        if n_done % 50 == 0:
            print(f"  scored {n_done}/{len(rows)}", flush=True)
    return n_done


def materialize() -> None:
    """JSONL -> shipped per-study CSVs (latest 'ok' row per response_id wins)."""
    import collections
    best: dict[str, dict] = {}
    with open(SCORES_JSONL) as f:
        for line in f:
            try:
                d = json.loads(line)
            except json.JSONDecodeError:
                continue
            if d.get("status") == "ok":
                best[d["response_id"]] = d
    SHIP_DIR.mkdir(parents=True, exist_ok=True)
    regime_file = {
        "study1": "study1_jailbroken_focal_statement_veracity.csv",
        "study2": "study2_standard_focal_statement_veracity.csv",
        "study3": "study3_truth_constrained_focal_statement_veracity.csv",
        "study4": "study4_social_sharing_focal_statement_veracity.csv",
    }
    cols = ["response_id", "study", "regime", "direction", "category", "statement",
            "statement_type", "veracity_score", "checkability", "label", "rationale",
            "veracity_model", "prompt_sha", "scored_at"]
    by_study = collections.defaultdict(list)
    for d in best.values():
        by_study[d["study"]].append(d)
    all_rows = []
    for study, fname in regime_file.items():
        rows = sorted(by_study.get(study, []), key=lambda r: r["response_id"])
        if not rows:
            continue
        with open(SHIP_DIR / fname, "w", newline="") as f:
            w = csv.DictWriter(f, fieldnames=cols, extrasaction="ignore")
            w.writeheader()
            w.writerows(rows)
        all_rows.extend(rows)
        print(f"  shipped {fname}: {len(rows)} rows")
    with open(SHIP_DIR / "focal_statement_veracity_all_studies.csv", "w", newline="") as f:
        w = csv.DictWriter(f, fieldnames=cols, extrasaction="ignore")
        w.writeheader()
        w.writerows(sorted(all_rows, key=lambda r: (r["study"], r["response_id"])))
    print(f"  shipped focal_statement_veracity_all_studies.csv: {len(all_rows)} rows")


def main() -> None:
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("--limit", type=int, default=None, help="score at most N pending rows")
    ap.add_argument("--dry-run", action="store_true", help="print 3 prompts and exit (no API)")
    ap.add_argument("--materialize-only", action="store_true", help="rebuild shipped CSVs from the JSONL")
    args = ap.parse_args()

    if args.materialize_only:
        materialize()
        return

    if not INPUT_CSV.exists():
        sys.exit(f"input not found: {INPUT_CSV}\nRun extract_inputs.R first.")
    with open(INPUT_CSV, newline="") as f:
        rows = [r for r in csv.DictReader(f)]
    print(f"input rows: {len(rows)} | model: {MODEL} | prompt sha: {PROMPT_SHA}")

    if args.dry_run:
        for r in rows[:3]:
            print("=" * 60)
            print(user_prompt(r))
        print("=" * 60)
        print("(dry run: no API calls; system prompt sha", PROMPT_SHA + ")")
        return

    if not os.environ.get("OPENROUTER_API_KEY", "").strip():
        sys.exit("OPENROUTER_API_KEY not set (checked focal_veracity/.env, "
                 "claim_factcheck/.env, prompts/.env)")

    done = load_done()
    pending = [r for r in rows if r["response_id"] not in done]
    if args.limit:
        pending = pending[: args.limit]
    print(f"already scored: {len(done)} | scoring now: {len(pending)}")
    if pending:
        WORK_DIR.mkdir(parents=True, exist_ok=True)
        with open(SCORES_JSONL, "a") as writer:
            asyncio.run(run(pending, writer))
    materialize()


if __name__ == "__main__":
    main()
