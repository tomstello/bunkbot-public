"""Aggregate S1/S3 aligned-direct claim veracity, mirroring the S2/S4 builder
(build_claim_role_portfolio_dataset.py) exactly so all four studies are on the
same metric.

aligned_direct = directness_to_focal == "direct" AND
    (stance_to_focal == "supports" if direction == "bunk" else "opposes").
Per conversation: mean veracity_score over its aligned-direct claims.
Per study x direction: mean of that conversation-level value over conversations
with >= 1 aligned-direct claim.

Input is the SUBSTANTIVE-ONLY role labels (specific_empirical pre-filtered, then
role-tagged in their own context), matching the S2/S4 pipeline. The analytic
sample (equivocality + 25-75 baseline window + non-missing post) is already
applied upstream in build_s1s3_claim_role_input.py.

Usage: python3 aggregate_s1s3_aligned_direct.py
"""

from __future__ import annotations

import csv
import json
import statistics
from collections import defaultdict
from pathlib import Path


def _repo_root(start: Path) -> Path:
    p = start.resolve()
    for cand in [p, *p.parents]:
        if (cand / "data").is_dir() and (cand / "code").is_dir():
            return cand
    raise RuntimeError("repo root (dir containing data/ and code/) not found")


REPO_ROOT = _repo_root(Path(__file__))
# Transient intermediates for this pipeline (not shipped). LABELS_JSONL is the
# per-claim role-classifier JSONL emitted upstream (build_s1s3_claim_role_input.py
# -> the claim_factcheck role classifier); only the harmonized/materialized
# data/api_cached/claim_labels/claim_role_labels_s1s3.csv.gz is shipped, not this
# raw JSONL. The output is likewise a transient summary CSV.
WORK_DIR = REPO_ROOT / "output" / "provenance_work" / "stance_v2"
LABELS_JSONL = WORK_DIR / "s1s3_claim_role_labels_substantive.jsonl"


def num(x):
    try:
        return float(x)
    except (TypeError, ValueError):
        return None


def main() -> None:
    # conversation_id -> {study, direction, [veracity of aligned-direct claims]}
    conv = {}
    seen = {}  # row_key -> record (dedup, keep last success)
    with LABELS_JSONL.open() as f:
        for line in f:
            try:
                o = json.loads(line)
            except json.JSONDecodeError:
                continue
            if o.get("request_status") != "success":
                continue
            seen[o.get("row_key")] = o

    for o in seen.values():
        cid = o.get("conversation_id")
        direction = (o.get("direction") or "").strip()
        study = o.get("study_source")
        if cid not in conv:
            conv[cid] = {"study": study, "direction": direction, "vera": []}
        direct = (o.get("directness_to_focal") == "direct")
        stance = o.get("stance_to_focal")
        aligned = (stance == "supports") if direction == "bunk" else (stance == "opposes")
        v = num(o.get("veracity_score"))
        if direct and aligned and v is not None:
            conv[cid]["vera"].append(v)

    # per-conversation aligned-direct mean veracity, then study x direction mean
    cells = defaultdict(list)  # (study, direction) -> [conversation means]
    for cid, d in conv.items():
        if d["vera"]:
            cells[(d["study"], d["direction"])].append(statistics.mean(d["vera"]))

    print(f"conversations with >=1 substantive claim: {len(conv):,}")
    print(f"{'study':<8}{'direction':<10}{'n_conv_aligned_direct':>22}{'mean_aligned_direct_veracity':>30}")
    rows = []
    for study in ["Study1", "Study3"]:
        for direction in ["bunk", "debunk"]:
            vals = cells.get((study, direction), [])
            m = statistics.mean(vals) if vals else float("nan")
            print(f"{study:<8}{direction:<10}{len(vals):>22}{m:>30.1f}")
            rows.append((study, direction, len(vals), m))

    out = WORK_DIR / "s1s3_aligned_direct_veracity.csv"
    out.parent.mkdir(parents=True, exist_ok=True)
    with out.open("w", newline="") as g:
        w = csv.writer(g)
        w.writerow(["study", "direction", "n_conversations", "aligned_direct_mean_veracity"])
        for study, direction, n, m in rows:
            w.writerow([study, direction, n, round(m, 2)])
    print(f"\nwrote {out}")


if __name__ == "__main__":
    main()
