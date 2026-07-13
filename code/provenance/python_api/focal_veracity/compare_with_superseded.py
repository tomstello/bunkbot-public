#!/usr/bin/env python3
"""Robustness: agreement between the fresh focal-statement veracity scores and the
two SUPERSEDED artifacts (which also scored conRestatement but without
statement-type handling, on split scales/coverage).

The superseded files live OUTSIDE this repo (broader working tree); this script
fails soft if they are absent. Output: a small markdown report in the working dir.

Usage: python3 compare_with_superseded.py [--old5 PATH] [--old100-s4 PATH] [--old100-s2 PATH]
"""
from __future__ import annotations

import argparse
import csv
import json
import math
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

# default external locations (broader tree, one level above the repo)
BROADER = REPO_ROOT.parent
DEFAULT_OLD5 = BROADER / "Final Analyses" / "data" / "conspiracy_veracity_5.jsonl"
DEFAULT_OLD100_S4 = BROADER / "portable_claim_factcheck_toolkit" / "results" / "april04_focal_claim_veracity_item.csv"
DEFAULT_OLD100_S2 = BROADER / "portable_claim_factcheck_toolkit" / "results" / "study2_standard_focal_claim_veracity_item.csv"


def pearson(x, y):
    n = len(x)
    if n < 3:
        return float("nan")
    mx, my = sum(x) / n, sum(y) / n
    sx = math.sqrt(sum((a - mx) ** 2 for a in x))
    sy = math.sqrt(sum((a - my) ** 2 for a in y))
    if sx == 0 or sy == 0:
        return float("nan")
    return sum((a - mx) * (b - my) for a, b in zip(x, y)) / (sx * sy)


def spearman(x, y):
    def ranks(v):
        order = sorted(range(len(v)), key=lambda i: v[i])
        r = [0.0] * len(v)
        i = 0
        while i < len(order):
            j = i
            while j + 1 < len(order) and v[order[j + 1]] == v[order[i]]:
                j += 1
            avg = (i + j) / 2 + 1
            for k in range(i, j + 1):
                r[order[k]] = avg
            i = j + 1
        return r
    return pearson(ranks(x), ranks(y))


def main() -> None:
    ap = argparse.ArgumentParser()
    ap.add_argument("--old5", default=str(DEFAULT_OLD5))
    ap.add_argument("--old100-s4", default=str(DEFAULT_OLD100_S4))
    ap.add_argument("--old100-s2", default=str(DEFAULT_OLD100_S2))
    args = ap.parse_args()

    if not NEW_CSV.exists():
        raise SystemExit(f"new scores not found: {NEW_CSV} (run the scorer first)")
    new = {}
    with open(NEW_CSV, newline="") as f:
        for r in csv.DictReader(f):
            if r.get("veracity_score"):
                new[r["response_id"]] = (float(r["veracity_score"]), r["study"], r["statement_type"])

    lines = ["# Agreement: fresh focal-statement scores vs superseded artifacts", ""]
    lines.append(f"New scores: {len(new)} scored rows ({NEW_CSV.name})")

    old5 = Path(args.old5)
    if old5.exists():
        pairs = []
        with open(old5) as f:
            for line in f:
                try:
                    d = json.loads(line)
                except json.JSONDecodeError:
                    continue
                rid = d.get("ResponseId")
                v = d.get("veracity_score")
                if rid in new and v is not None:
                    pairs.append((float(v), new[rid][0]))
        if pairs:
            xs, ys = zip(*pairs)
            lines += ["", f"## vs conspiracy_veracity_5 (0-5 scale, S1-3; n overlap = {len(pairs)})",
                      f"- Spearman rho = {spearman(list(xs), list(ys)):.3f}",
                      f"- Pearson r    = {pearson(list(xs), list(ys)):.3f}"]
    else:
        lines += ["", f"(old 0-5 file not found at {old5} — skipped)"]

    for label, path in (("april04 S4 0-100", Path(args.old100_s4)),
                        ("study2 0-100", Path(args.old100_s2))):
        if not path.exists():
            lines += ["", f"(old file not found at {path} — skipped)"]
            continue
        pairs = []
        with open(path, newline="") as f:
            for r in csv.DictReader(f):
                rid = r.get("ResponseId") or r.get("conversation_id") or r.get("response_id")
                v = r.get("veracity_score")
                try:
                    v = float(v)
                except (TypeError, ValueError):
                    continue
                if rid in new:
                    pairs.append((v, new[rid][0]))
        if pairs:
            xs, ys = zip(*pairs)
            mad = sum(abs(a - b) for a, b in pairs) / len(pairs)
            lines += ["", f"## vs {label} (n overlap = {len(pairs)})",
                      f"- Pearson r  = {pearson(list(xs), list(ys)):.3f}",
                      f"- Spearman   = {spearman(list(xs), list(ys)):.3f}",
                      f"- mean |Δ|   = {mad:.1f} points (0-100)"]

    WORK_DIR.mkdir(parents=True, exist_ok=True)
    out = WORK_DIR / "agreement_with_superseded.md"
    out.write_text("\n".join(lines) + "\n")
    print("\n".join(lines))
    print(f"\nwrote {out}")


if __name__ == "__main__":
    main()
