"""Build the claim-role classifier input for Studies 1 and 3 (legacy GPT-4o),
mirroring the columns the S2/S4 pipeline used so the aligned-direct veracity
metric is computed identically across all four studies.

Per-claim veracity comes from the published S1/S3 fact-check JSONLs; focal
claim (conRestatement/conSummary), condition, and category come from the
cleaned analytic files. We mirror the S2 treatment exactly: focal = conRestatement
as-is (no denies special-casing), direction from condition, all analytic-sample
conversations with a numeric-veracity claim.

Writes s1s3_claim_role_input.csv (combined; study_source distinguishes) to the
provenance working dir (transient intermediate; not shipped).
"""

from __future__ import annotations

import csv
import gzip
import json
import sys
from pathlib import Path

csv.field_size_limit(2**30)


def _repo_root(start: Path) -> Path:
    p = start.resolve()
    for cand in [p, *p.parents]:
        if (cand / "data").is_dir() and (cand / "code").is_dir():
            return cand
    raise RuntimeError("repo root (dir containing data/ and code/) not found")


REPO_ROOT = _repo_root(Path(__file__))
# Shipped inputs: per-claim veracity JSONLs and the cleaned analytic files.
VERACITY_DIR = REPO_ROOT / "data" / "api_cached" / "claim_veracity"
CLEAN_DIR = REPO_ROOT / "data" / "processed_s1s3"
# Transient combined claim-role classifier input (not shipped).
WORK_DIR = REPO_ROOT / "output" / "provenance_work" / "stance_v2"
OUT = WORK_DIR / "s1s3_claim_role_input.csv"

# (veracity JSONL path, cleaned analytic CSV path) per study.
STUDIES = {
    "Study1": (VERACITY_DIR / "study1_jailbroken_claim_extraction_veracity.jsonl.gz",
               CLEAN_DIR / "study1_jailbroken_clean.csv.gz"),
    "Study3": (VERACITY_DIR / "study3_truth_constrained_claim_extraction_veracity.jsonl.gz",
               CLEAN_DIR / "study3_truth_constrained_clean.csv.gz"),
}


def num(x):
    try:
        return float(x)
    except (TypeError, ValueError):
        return None


def load_clean(path: Path) -> dict:
    info = {}
    for r in csv.DictReader(gzip.open(path, "rt", encoding="utf-8", newline="")):
        rid = r.get("ResponseId", "")
        cond = r.get("condition", "")
        if "debunk" in cond:
            direction = "debunk"
        elif "bunk" in cond:
            direction = "bunk"
        else:
            continue
        # analytic-sample inclusion: equivocal + 25-75 window on reverse-coded
        # baseline (rc = 100 - x for denies-phrased restatements; computed here
        # since the clean file stores only the raw belief_rating_*_4)
        if str(r.get("isEquivocal", "")).strip().upper() != "TRUE":
            continue
        cat = (r.get("category", "") or "").strip()
        pre = num(r.get("belief_rating_pre_4"))
        post = num(r.get("belief_rating_post_4"))
        if pre is None or post is None:
            continue
        pre_rc = 100 - pre if cat == "denies" else pre
        if not (25 < pre_rc < 75):
            continue
        info[rid] = {
            "direction": direction,
            "conRestatement": (r.get("conRestatement", "") or "").strip(),
            "conSummary": (r.get("conSummary", "") or "").strip(),
            "category": (r.get("category", "") or "").strip(),
        }
    return info


def main() -> None:
    rows_out = []
    cols = ["study_source", "conversation_id", "message_id", "row_key", "turn_order",
            "claim_index_num", "claim_text", "content", "direction", "model_name",
            "model_pooled", "veracity_score", "conRestatement", "conSummary"]
    for study, (jsonl, clean) in STUDIES.items():
        info = load_clean(clean)
        kept = 0
        per_conv_idx = {}
        for line in gzip.open(jsonl, "rt", encoding="utf-8"):
            try:
                o = json.loads(line)
            except json.JSONDecodeError:
                continue
            rid = o.get("ResponseId", "")
            if rid not in info:
                continue
            ver = num(o.get("veracity_score"))
            claim = (o.get("individual_claim", "") or "").strip()
            if ver is None or not claim:
                continue
            ci = info[rid]
            idx = per_conv_idx.get(rid, 0)
            per_conv_idx[rid] = idx + 1
            rows_out.append({
                "study_source": study,
                "conversation_id": rid,
                # message_id MUST be conversation-specific: the role classifier
                # batches and judges directness per message_id, so a bare turn
                # number (shared across all conversations) collapses every
                # conversation's same-numbered turn into one giant batch and
                # destroys the per-turn context (fixed 2026-06-16).
                "message_id": f"{rid}::{o.get('id','')}",
                "row_key": f"{rid}::{o.get('id','')}::{idx}",
                "turn_order": o.get("id", ""),
                "claim_index_num": idx,
                "claim_text": claim,
                "content": (o.get("content", "") or "").strip()[:1500],
                "direction": ci["direction"],
                "model_name": "openai/gpt-4o",
                "model_pooled": "GPT-4o",
                "veracity_score": ver,
                "conRestatement": ci["conRestatement"],
                "conSummary": ci["conSummary"],
            })
            kept += 1
        print(f"{study}: {kept} claims across {len(set(r['conversation_id'] for r in rows_out if r['study_source']==study))} "
              f"analytic conversations")
    OUT.parent.mkdir(parents=True, exist_ok=True)
    with open(OUT, "w", newline="") as f:
        w = csv.DictWriter(f, fieldnames=cols)
        w.writeheader()
        w.writerows(rows_out)
    print(f"wrote {len(rows_out)} claim rows -> {OUT}")


if __name__ == "__main__":
    main()
