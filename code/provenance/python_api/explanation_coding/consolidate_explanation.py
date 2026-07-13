"""Consolidate per-rater explanation-coding JSONLs into a wide CSV with consensus.

Consensus rules:
- per theme: majority vote across raters who returned a label (true if true_votes*2
  > n_raters; ties resolve to 0 -> conservative, avoids false positives)
- response_quality / primary_theme: plurality, ties broken by total rater confidence
- low_agreement_flag: any theme with a genuine split (>=2 raters each way)

Outputs data/api_cached/explanation_coding/explanation_consolidated.csv:
  item_id, ResponseId, study, field, n_raters_ok, mean_confidence,
  consensus_response_quality, consensus_primary_theme,
  <15 consensus theme 0/1 columns>, n_contested_themes, low_agreement_flag

Usage:
  python3 consolidate_explanation.py [--items pilot_item_ids.txt] [--out NAME.csv] [--replicate 0]
"""

from __future__ import annotations

import argparse
import csv
import json
import re
import statistics
from collections import Counter, defaultdict
from pathlib import Path

def _repo_root(start: Path) -> Path:
    """Walk up to the repo root: the dir containing BOTH data/ and code/."""
    p = start.resolve()
    for cand in [p, *p.parents]:
        if (cand / "data").is_dir() and (cand / "code").is_dir():
            return cand
    raise RuntimeError("repo root (dir containing data/ and code/) not found")


HERE = Path(__file__).resolve().parent
REPO_ROOT = _repo_root(HERE)
DIR = REPO_ROOT / "data/api_cached/explanation_coding"

from taxonomy import THEME_KEYS  # noqa: E402

PANEL = [
    "openrouter/anthropic/claude-sonnet-4.6",
    "openrouter/openai/gpt-5.2",
    "openrouter/google/gemini-3.1-pro-preview",
    "openrouter/x-ai/grok-4.3",
    "openrouter/deepseek/deepseek-v3.2",
]


def model_slug(model: str) -> str:
    return re.sub(r"[^A-Za-z0-9._-]+", "_", model.removeprefix("openrouter/"))


def plurality(values: list, confidences: list[float]):
    if not values:
        return None
    weight: dict = defaultdict(float)
    count: Counter = Counter()
    for v, c in zip(values, confidences):
        count[v] += 1
        weight[v] += c
    top = max(count.values())
    tied = [v for v, n in count.items() if n == top]
    return max(tied, key=lambda v: weight[v]) if len(tied) > 1 else tied[0]


def main() -> None:
    ap = argparse.ArgumentParser()
    ap.add_argument("--items", default=None, help="restrict to item_ids in this file")
    ap.add_argument("--out", default="explanation_consolidated.csv")
    ap.add_argument("--replicate", type=int, default=0)
    ap.add_argument("--prompt-version", default="v1.0")
    args = ap.parse_args()

    keep = None
    if args.items:
        items_path = Path(args.items)
        if not items_path.exists():
            items_path = DIR / args.items
        keep = {l.strip() for l in open(items_path) if l.strip()}

    # latest ok record per (item, model)
    by_item: dict[str, dict[str, dict]] = defaultdict(dict)
    meta: dict[str, dict] = {}
    for model in PANEL:
        path = DIR / f"scores_{model_slug(model)}.jsonl"
        if not path.exists():
            print(f"WARNING: missing {path.name}")
            continue
        for line in open(path):
            try:
                rec = json.loads(line)
            except json.JSONDecodeError:
                continue
            if not rec.get("request_ok") or rec.get("replicate", 0) != args.replicate:
                continue
            if rec.get("prompt_version") != args.prompt_version:
                continue
            if keep and rec["item_id"] not in keep:
                continue
            by_item[rec["item_id"]][model] = rec  # later lines overwrite
            meta.setdefault(rec["item_id"], {"ResponseId": rec["ResponseId"],
                                             "study": rec["study"], "field": rec["field"]})

    rows = []
    for item_id, per_model in sorted(by_item.items()):
        recs = list(per_model.values())
        n = len(recs)
        confs = [r.get("confidence") or 0.0 for r in recs]
        row = {
            "item_id": item_id,
            "ResponseId": meta[item_id]["ResponseId"],
            "study": meta[item_id]["study"],
            "field": meta[item_id]["field"],
            "n_raters_ok": n,
            "mean_confidence": round(statistics.mean(confs), 3) if confs else None,
            "consensus_response_quality": plurality([r["response_quality"] for r in recs], confs),
            "consensus_primary_theme": plurality([r["primary_theme"] for r in recs], confs),
        }
        contested = 0
        for theme in THEME_KEYS:
            votes_true = sum(1 for r in recs if r["themes"].get(theme))
            row[theme] = int(votes_true * 2 > n)               # strict majority
            if min(votes_true, n - votes_true) >= 2:           # genuine split
                contested += 1
        row["n_contested_themes"] = contested
        row["low_agreement_flag"] = int(contested > 0)
        rows.append(row)

    out_path = DIR / args.out
    fieldnames = (["item_id", "ResponseId", "study", "field", "n_raters_ok", "mean_confidence",
                   "consensus_response_quality", "consensus_primary_theme"] + THEME_KEYS +
                  ["n_contested_themes", "low_agreement_flag"])
    with open(out_path, "w", newline="") as f:
        w = csv.DictWriter(f, fieldnames=fieldnames)
        w.writeheader()
        w.writerows(rows)
    print(f"wrote {len(rows)} items -> {out_path}")

    full = sum(1 for r in rows if r["n_raters_ok"] == len(PANEL))
    print(f"items with all {len(PANEL)} raters: {full}")
    rq = Counter(r["consensus_response_quality"] for r in rows)
    print("consensus response_quality:", dict(rq))
    contested = sum(1 for r in rows if r["low_agreement_flag"])
    print(f"items with >=1 contested theme: {contested} ({100*contested/max(1,len(rows)):.1f}%)")
    # theme prevalence among substantive items
    sub = [r for r in rows if r["consensus_response_quality"] == "substantive"]
    print(f"\nconsensus theme prevalence (substantive n={len(sub)}):")
    for theme in THEME_KEYS:
        p = 100 * sum(r[theme] for r in sub) / max(1, len(sub))
        print(f"  {theme:10s} {p:5.1f}%")


if __name__ == "__main__":
    main()
