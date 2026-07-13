#!/usr/bin/env python3
"""Build a stratified human-audit template from the fresh focal-statement scores:
50 rows stratified by study x label band, columns for a blind human coder
(human_statement_type, human_veracity_0_100, human_label, notes) with the model's
verdicts hidden in a separate answer key.

Usage: python3 make_human_audit_template.py [--n 50] [--seed 20260704]
"""
from __future__ import annotations

import argparse
import csv
import random
from pathlib import Path


def _repo_root(start: Path) -> Path:
    p = start.resolve()
    for cand in [p, *p.parents]:
        if (cand / "data").is_dir() and (cand / "code").is_dir():
            return cand
    raise RuntimeError("repo root not found")


REPO_ROOT = _repo_root(Path(__file__))
WORK_DIR = REPO_ROOT / "output" / "provenance_work" / "focal_veracity"
NEW_CSV = REPO_ROOT / "data" / "api_cached" / "focal_veracity" / "focal_statement_veracity_all_studies.csv"


def main() -> None:
    ap = argparse.ArgumentParser()
    ap.add_argument("--n", type=int, default=50)
    ap.add_argument("--seed", type=int, default=20260704)
    args = ap.parse_args()

    rows = list(csv.DictReader(open(NEW_CSV, newline="")))
    rng = random.Random(args.seed)

    def band(r):
        if r["statement_type"] != "conspiracy_claim":
            return r["statement_type"]
        v = float(r["veracity_score"]) if r["veracity_score"] else None
        if v is None:
            return "unscored"
        return "false" if v < 40 else ("uncertain" if v < 60 else "true")

    strata: dict[tuple, list] = {}
    for r in rows:
        strata.setdefault((r["study"], band(r)), []).append(r)
    # proportional allocation with at least 1 per non-empty stratum
    total = sum(len(v) for v in strata.values())
    picked = []
    for key, members in sorted(strata.items()):
        k = max(1, round(args.n * len(members) / total))
        picked += rng.sample(members, min(k, len(members)))
    rng.shuffle(picked)
    picked = picked[: args.n]

    WORK_DIR.mkdir(parents=True, exist_ok=True)
    blind_cols = ["response_id", "study", "statement"]
    with open(WORK_DIR / "human_audit_template.csv", "w", newline="") as f:
        w = csv.writer(f)
        w.writerow(blind_cols + ["human_statement_type", "human_veracity_0_100", "human_label", "notes"])
        for r in picked:
            w.writerow([r[c] for c in blind_cols] + ["", "", "", ""])
    with open(WORK_DIR / "human_audit_answer_key.csv", "w", newline="") as f:
        w = csv.DictWriter(f, fieldnames=["response_id", "study", "statement_type",
                                          "veracity_score", "label"], extrasaction="ignore")
        w.writeheader()
        w.writerows(picked)
    print(f"wrote {len(picked)}-row template + answer key to {WORK_DIR}")


if __name__ == "__main__":
    main()
