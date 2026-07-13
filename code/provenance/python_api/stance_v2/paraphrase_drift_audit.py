"""Audit veracity-drift in the AI restatement of each participant's conspiracy.

The focal-claim veracity that conditions the accuracy/calibration analyses is
scored on an AI-generated restatement (canonical_claim), not the participant's
own words. When that restatement generalizes or softens a specific (often
false) belief into a defensible true-ish statement, the focal veracity is
inflated. This quantifies how often that happens.

For each participant, three models judge — against the participant's RAW
description — whether the restatement is faithful or drifts, and estimate the
veracity a fact-checker would assign to the participant's ACTUAL claim
(comparable to the existing focal_claim_veracity scored on the restatement).

3 models, majority vote on the drift label. Writes paraphrase_drift_audit.jsonl/.csv.
"""

from __future__ import annotations

import argparse
import asyncio
import csv
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
csv.field_size_limit(2**30)

# Transient pipeline intermediates (not shipped).
WORK_DIR = REPO_ROOT / "output" / "provenance_work" / "stance_v2"
V2_DIR = WORK_DIR
# NOT shipped: april04_focal_claim_veracity.csv (the per-conversation focal-claim
# veracity table) was removed from the public repo. Produce it upstream from the
# claim_factcheck focal-claim fact-check pipeline before running this audit.
FOCAL = WORK_DIR / "april04_focal_claim_veracity.csv"
OUT_JSONL = WORK_DIR / "paraphrase_drift_audit.jsonl"
OUT_CSV = WORK_DIR / "paraphrase_drift_audit.csv"

AUDIT_VERSION = "drift1.0"
MODELS = ["openrouter/openai/gpt-5.2",
          "openrouter/anthropic/claude-sonnet-4.6",
          "openrouter/google/gemini-3.1-pro-preview"]
LABELS = ["faithful", "inflated_truer", "deflated_falser", "altered_topic", "unclear"]

SYSTEM = """You audit whether an AI restatement faithfully captures a research participant's conspiracy belief, or distorts its truth value.

A participant described, in their own words, a conspiracy theory they were uncertain about. An AI then produced a one-sentence RESTATEMENT, which was later fact-checked for veracity. The risk: the restatement sometimes generalizes or softens a specific (often false) belief into a broad, defensible, true-ish statement — which makes the fact-check rate it as far truer than what the participant actually believed (e.g., a "the moon landing was faked" belief restated as "some historical events were planned by secret organizations," or "the Mandela Effect means reality has literally changed" restated as "groups of people share inaccurate memories").

Compare the RESTATEMENT to the participant's OWN description and judge:
- "faithful": the restatement captures the participant's specific claim with the same truth value.
- "inflated_truer": the restatement is broader, softer, or more defensible than what the participant described, so a fact-checker would rate the restatement as meaningfully TRUER than the participant's actual belief. THIS IS THE HARMFUL DRIFT.
- "deflated_falser": the restatement is meaningfully FALSER than the participant's actual claim.
- "altered_topic": the restatement is about a materially different claim.
- "unclear": the participant's description is too vague to judge.

Also estimate, on a 0–100 scale, the veracity a careful fact-checker would assign to the PARTICIPANT'S ACTUAL claim as they described it (0 = clearly false, 100 = clearly true), independent of how the restatement was phrased.

Return strict JSON: {"drift": "...", "participant_claim_veracity": <0-100 int>, "rationale": "<= 25 words"}."""

SCHEMA = {"name": "drift_audit", "strict": True, "schema": {"type": "object", "properties": {
    "drift": {"type": "string", "enum": LABELS},
    "participant_claim_veracity": {"type": "integer"},
    "rationale": {"type": "string"}},
    "required": ["drift", "participant_claim_veracity", "rationale"], "additionalProperties": False}}


def load_env() -> None:
    import os
    if os.environ.get("OPENROUTER_API_KEY"):
        return
    # Preferred: OPENROUTER_API_KEY in the environment (see .env.example at the
    # repo root). The external portable_claim_factcheck_toolkit/.env is NOT
    # shipped; kept only for the author's original tree.
    for p in (REPO_ROOT / ".env", REPO_ROOT / "code/.env", REPO_ROOT.parent / "portable_claim_factcheck_toolkit/.env"):
        if p.exists():
            for line in p.read_text().splitlines():
                m = re.match(r"^\s*([A-Z_]+)\s*=\s*(.+?)\s*$", line)
                if m and m.group(1) not in os.environ:
                    os.environ[m.group(1)] = m.group(2).strip("'\"")
            if os.environ.get("OPENROUTER_API_KEY"):
                return
    raise SystemExit("OPENROUTER_API_KEY not found")


def ck(model, cid, restate):
    return hashlib.sha256(json.dumps({"m": model, "v": AUDIT_VERSION, "c": cid, "r": restate}, sort_keys=True).encode()).hexdigest()


async def one(model, item, sem):
    rec = {"cache_key": ck(model, item["cid"], item["restatement"]), "conversation_id": item["cid"],
           "model": model, "request_ok": False, "error": None}
    msgs = [{"role": "system", "content": SYSTEM},
            {"role": "user", "content": json.dumps({
                "participant_description": item["topic"],
                "participant_reasons": item["reasons"],
                "AI_restatement_that_was_fact_checked": item["restatement"]}, ensure_ascii=False)}]
    async with sem:
        for attempt in range(1, 5):
            try:
                resp = await litellm.acompletion(model=model, messages=msgs, temperature=0, timeout=120,
                                                 response_format={"type": "json_schema", "json_schema": SCHEMA})
            except Exception as exc:  # noqa: BLE001
                rec["error"] = str(exc)[:200]
                await asyncio.sleep(min(15, 0.8 * 2 ** attempt)); continue
            try:
                p = parse_json_like(resp.choices[0].message.content or "")
                if isinstance(p, dict) and p.get("drift") in LABELS:
                    v = p.get("participant_claim_veracity")
                    rec.update(drift=p["drift"],
                               participant_claim_veracity=max(0, min(100, int(v))) if isinstance(v, (int, float)) else None,
                               rationale=str(p.get("rationale", ""))[:200], request_ok=True, error=None)
                    return rec
            except Exception:  # noqa: BLE001
                pass
            await asyncio.sleep(min(15, 0.8 * 2 ** attempt))
    return rec


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--limit", type=int, default=None)
    ap.add_argument("--concurrency", type=int, default=10)
    args = ap.parse_args()
    load_env()
    WORK_DIR.mkdir(parents=True, exist_ok=True)

    raw = {}
    for line in open(V2_DIR / "stance_v2_inputs.jsonl"):
        it = json.loads(line)
        raw.setdefault(it["ResponseId"], {"topic": it["participant_topic_description"],
                                          "reasons": it["participant_reasons"]})
    items = []
    for f in csv.DictReader(open(FOCAL)):
        cid = f["conversation_id"]
        if cid not in raw or not (f["canonical_claim"] or "").strip():
            continue
        items.append({"cid": cid, "topic": raw[cid]["topic"], "reasons": raw[cid]["reasons"],
                      "restatement": f["canonical_claim"],
                      "focal_vera": f["focal_claim_veracity"], "focal_label": f["focal_label"]})
    if args.limit:
        items = items[: args.limit]
    print(f"{len(items)} conversations x {len(MODELS)} models")

    done = set()
    if OUT_JSONL.exists():
        for line in open(OUT_JSONL):
            try:
                r = json.loads(line)
                if r.get("request_ok"):
                    done.add(r["cache_key"])
            except json.JSONDecodeError:
                pass

    async def run():
        sem = asyncio.Semaphore(args.concurrency); lock = asyncio.Lock()
        for model in MODELS:
            todo = [it for it in items if ck(model, it["cid"], it["restatement"]) not in done]
            print(f"[{model}] {len(todo)} to audit"); t0 = time.time(); nok = nerr = 0
            async def worker(it):
                nonlocal nok, nerr
                rec = await one(model, it, sem)
                async with lock:
                    with open(OUT_JSONL, "a") as fh:
                        fh.write(json.dumps(rec, ensure_ascii=False) + "\n")
                    nok += rec["request_ok"]; nerr += (not rec["request_ok"])
            await asyncio.gather(*(worker(it) for it in todo))
            print(f"[{model}] ok={nok} err={nerr} {time.time()-t0:.0f}s")
    asyncio.run(run())

    # consolidate: majority drift label, median participant-claim veracity
    from collections import Counter
    import statistics
    by = {}
    for line in open(OUT_JSONL):
        r = json.loads(line)
        if not r.get("request_ok"):
            continue
        d = by.setdefault(r["conversation_id"], {"drift": [], "vera": []})
        d["drift"].append(r["drift"])
        if r.get("participant_claim_veracity") is not None:
            d["vera"].append(r["participant_claim_veracity"])
    foc = {f["conversation_id"]: f for f in csv.DictReader(open(FOCAL))}
    with open(OUT_CSV, "w", newline="") as fh:
        w = csv.writer(fh)
        w.writerow(["conversation_id", "drift_consensus", "n_votes", "participant_claim_veracity_med",
                    "focal_claim_veracity_restatement", "focal_label", "direction"])
        for cid, d in sorted(by.items()):
            top, n = Counter(d["drift"]).most_common(1)[0]
            cons = top if n > len(d["drift"]) / 2 else "disagree"
            f = foc.get(cid, {})
            w.writerow([cid, cons, len(d["drift"]),
                        round(statistics.median(d["vera"]), 1) if d["vera"] else "",
                        f.get("focal_claim_veracity", ""), f.get("focal_label", ""), f.get("direction", "")])
    print(f"consolidated -> {OUT_CSV} ({len(by)} conversations)")
    print("drift consensus:", dict(Counter(
        (Counter(d["drift"]).most_common(1)[0][0] if Counter(d["drift"]).most_common(1)[0][1] > len(d["drift"]) / 2 else "disagree")
        for d in by.values()).most_common()))


if __name__ == "__main__":
    main()
