"""Quantify the impact of restatement/summary direction errors on Study 4.

Joins the direction audit (restatement_direction_audit.csv) to the strict
analytic sample and reports:
  1. Direction rates overall and in the strict sample (vs S1-3 precedent)
  2. Restatement x summary cross-tab (incl. the JFK-style mismatch cell)
  3. Model-pair disagreement rate (audit reliability)
  4. Contamination signature: raw aligned belief change by restatement category
     (denies cases should look inverted if the flip is real)
  5. Corrected vs uncorrected pooled aligned-belief means by direction
     (rc correction: flip change scores for denies-phrased restatements)

Usage: python3 analyze_direction_audit.py
"""

from __future__ import annotations

import csv
import statistics
from collections import Counter, defaultdict
from pathlib import Path

HERE = Path(__file__).resolve().parent


def _repo_root(start: Path) -> Path:
    p = start.resolve()
    for cand in [p, *p.parents]:
        if (cand / "data").is_dir() and (cand / "code").is_dir():
            return cand
    raise RuntimeError("repo root (dir containing data/ and code/) not found")


REPO_ROOT = _repo_root(Path(__file__))
# Transient intermediates for this pipeline (not shipped).
WORK_DIR = REPO_ROOT / "output" / "provenance_work" / "stance_v2"
AUDIT = WORK_DIR / "restatement_direction_audit.csv"
# NOT shipped: the S4 analytic-strict set is built at RUNTIME by build_s4_data()
# in code/bunkbot_helpers.R; there is no static file in the repo. Materialize it
# here before running this script standalone.
STRICT = WORK_DIR / "s4_analytic_strict.csv"


def ci95(xs: list[float]) -> str:
    if len(xs) < 3:
        return "n/a"
    m = statistics.mean(xs)
    se = statistics.stdev(xs) / len(xs) ** 0.5
    return f"{m:+.2f} [{m - 1.96 * se:+.2f}, {m + 1.96 * se:+.2f}] (n={len(xs)})"


def main() -> None:
    audit = {r["ResponseId"]: r for r in csv.DictReader(open(AUDIT))}
    strict = list(csv.DictReader(open(STRICT)))
    print(f"audit rows: {len(audit)}; strict sample: {len(strict)}")

    # 1. rates
    for scope, rids in (("ALL audited", list(audit)),
                        ("STRICT sample", [r["ResponseId"] for r in strict])):
        rows = [audit[rid] for rid in rids if rid in audit]
        rc = Counter(r["restatement_consensus"] for r in rows)
        sc = Counter(r["summary_consensus"] for r in rows)
        n = len(rows)
        print(f"\n[{scope}] n={n} (missing from audit: {len(rids) - n})")
        print("  restatement:", {k: f"{v} ({100*v/n:.1f}%)" for k, v in rc.most_common()})
        print("  summary:    ", {k: f"{v} ({100*v/n:.1f}%)" for k, v in sc.most_common()})

    # 2. cross-tab in strict
    xt = Counter()
    for r in strict:
        a = audit.get(r["ResponseId"])
        if a:
            xt[(a["restatement_consensus"], a["summary_consensus"])] += 1
    print("\n[STRICT] restatement x summary cross-tab:")
    for (rcat, scat), n in sorted(xt.items(), key=lambda kv: -kv[1]):
        flag = "  <-- MISMATCH" if {rcat, scat} == {"affirms", "denies"} else ""
        print(f"  restatement={rcat:9s} summary={scat:9s}: {n}{flag}")

    # 3. audit-model agreement
    both = [r for r in audit.values() if r["gpt_restatement"] and r["claude_restatement"]]
    agree_r = sum(1 for r in both if r["gpt_restatement"] == r["claude_restatement"])
    agree_s = sum(1 for r in both if r["gpt_summary"] == r["claude_summary"])
    print(f"\naudit-model agreement: restatement {100*agree_r/len(both):.1f}%, "
          f"summary {100*agree_s/len(both):.1f}% (n={len(both)})")

    # 4. contamination signature on raw aligned belief change
    print("\n[STRICT] raw aligned belief change by restatement category x direction:")
    groups: dict[tuple, list[float]] = defaultdict(list)
    for r in strict:
        a = audit.get(r["ResponseId"])
        if not a or not r.get("aligned_belief_change"):
            continue
        groups[(a["restatement_consensus"], r["direction"])].append(float(r["aligned_belief_change"]))
    for (cat, direction), xs in sorted(groups.items()):
        print(f"  {cat:9s} {direction:7s}: {ci95(xs)}")
    print("  (if flips are real, 'denies' rows should look inverted vs 'affirms')")

    # 5. corrected vs uncorrected pooled aligned means
    print("\n[STRICT] pooled aligned belief change, uncorrected vs rc-corrected:")
    for direction in ("bunk", "debunk"):
        raw, fixed = [], []
        for r in strict:
            if r["direction"] != direction or not r.get("aligned_belief_change"):
                continue
            v = float(r["aligned_belief_change"])
            raw.append(v)
            a = audit.get(r["ResponseId"])
            fixed.append(-v if (a and a["restatement_consensus"] == "denies") else v)
        print(f"  {direction:7s} uncorrected: {ci95(raw)}")
        print(f"  {direction:7s} corrected:   {ci95(fixed)}")

    # 6. dump flagged participants for manual review / claim normalization
    flagged = []
    for r in strict:
        a = audit.get(r["ResponseId"])
        if a and (a["restatement_consensus"] != "affirms" or a["summary_consensus"] != "affirms"):
            flagged.append({"ResponseId": r["ResponseId"],
                            "restatement": a["restatement_consensus"],
                            "summary": a["summary_consensus"],
                            "direction": r["direction"], "model": r.get("model_pooled", "")})
    out = WORK_DIR / "direction_flagged_strict.csv"
    out.parent.mkdir(parents=True, exist_ok=True)
    with open(out, "w", newline="") as f:
        w = csv.DictWriter(f, fieldnames=["ResponseId", "restatement", "summary", "direction", "model"])
        w.writeheader()
        w.writerows(flagged)
    print(f"\nflagged strict participants (any non-affirms): {len(flagged)} -> {out.name}")


if __name__ == "__main__":
    main()
