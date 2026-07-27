"""Consolidate per-rater stance-v2 JSONLs into a wide CSV with consensus columns.

Consensus rules (per plan):
- applicability: majority vote on whether the post is scoreable (non-null score)
- consensus_score: median of non-null stance scores
- categorical fields: plurality, ties broken by total rater confidence
- dispersion flag: cross-rater score SD > 15 -> human review queue

Usage:
    python3 consolidate_stance_v2.py [--items pilot_item_ids.txt] [--out NAME.csv]
"""

from __future__ import annotations

import argparse
import csv
import json
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
# Transient intermediates for this pipeline (stance_v2_inputs.jsonl, per-model
# scores_*.jsonl) live here; the consolidated panel is a SHIPPED output.
WORK_DIR = REPO_ROOT / "output" / "provenance_work" / "stance_v2"
V2_DIR = WORK_DIR
# Shipped consolidated stance classifications (study4_stance_classifications.csv).
SHARING_DIR = REPO_ROOT / "data" / "api_cached" / "sharing_and_stance"

PANEL = [
    "openrouter/anthropic/claude-sonnet-4.6",
    "openrouter/openai/gpt-5.2",
    "openrouter/google/gemini-3.1-pro-preview",
    "openrouter/x-ai/grok-4.3",
    "openrouter/deepseek/deepseek-v3.2",
]

SHORT = {
    "openrouter/anthropic/claude-sonnet-4.6": "claude",
    "openrouter/openai/gpt-5.2": "gpt",
    "openrouter/google/gemini-3.1-pro-preview": "gemini",
    "openrouter/x-ai/grok-4.3": "grok",
    "openrouter/deepseek/deepseek-v3.2": "deepseek",
}

FIELDS = ["stance_score", "stance_category", "response_type", "focal_relevance",
          "sarcasm_or_irony", "confidence", "evidence_quote", "rationale"]


def model_slug(model: str) -> str:
    import re
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
    ap.add_argument("--out", default="study4_stance_classifications.csv")
    ap.add_argument("--inputs", default=str(V2_DIR / "stance_v2_inputs.jsonl"))
    ap.add_argument("--replicate", type=int, default=0)
    ap.add_argument("--prompt-version", default="v2.2")
    args = ap.parse_args()

    keep = None
    if args.items:
        keep = {line.strip() for line in open(args.items) if line.strip()}

    inputs = {json.loads(l)["item_id"]: json.loads(l) for l in open(args.inputs)}
    # Scoring caches are shared and append-only; default consolidation is
    # restricted to the supplied input universe so separate gap jobs cannot
    # contaminate the canonical panel (or vice versa).
    keep = set(inputs) if keep is None else keep & set(inputs)

    # latest ok record per (item, model)
    by_item: dict[str, dict[str, dict]] = defaultdict(dict)
    for model in PANEL:
        path = V2_DIR / f"scores_{model_slug(model)}.jsonl"
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

    rows = []
    for item_id, per_model in sorted(by_item.items()):
        inp = inputs[item_id]
        row = {
            "item_id": item_id,
            "ResponseId": inp["ResponseId"],
            "timepoint": inp["timepoint"],
            "post_text": inp["post_text"],
            "v1_score": inp["v1_score"],
            "n_raters_ok": len(per_model),
        }
        scores, score_conf, cats, cat_conf = [], [], [], []
        rtypes, frels, all_conf = [], [], []
        for model in PANEL:
            tag = SHORT[model]
            rec = per_model.get(model)
            for f in FIELDS:
                row[f"{tag}_{f}"] = rec.get(f) if rec else None
            if rec:
                conf = rec.get("confidence") or 0.0
                all_conf.append(conf)
                cats.append(rec["stance_category"]); cat_conf.append(conf)
                rtypes.append(rec["response_type"]); frels.append(rec["focal_relevance"])
                if rec["stance_score"] is not None:
                    scores.append(rec["stance_score"]); score_conf.append(conf)
        n_null = sum(1 for c in cats if c == "not_applicable")
        applicable = len(scores) > n_null  # majority of raters scored it
        row["consensus_applicable"] = applicable
        row["consensus_score"] = statistics.median(scores) if (applicable and scores) else None
        row["consensus_category"] = plurality(cats, cat_conf)
        row["consensus_response_type"] = plurality(rtypes, all_conf)
        row["consensus_focal_relevance"] = plurality(frels, all_conf)
        row["score_sd"] = round(statistics.stdev(scores), 2) if len(scores) >= 2 else None
        row["dispersion_flag"] = (row["score_sd"] or 0) > 15
        row["n_scored"] = len(scores)
        row["n_not_applicable"] = n_null
        rows.append(row)

    # The canonical full-panel consolidation is a SHIPPED output and goes to
    # data/api_cached/sharing_and_stance/; any other --out (e.g. a pilot
    # consolidation) is a transient intermediate and stays in the working dir.
    requested_out = Path(args.out)
    if requested_out.is_absolute():
        out_dir = requested_out.parent
        out_name = requested_out.name
    elif args.out == "study4_stance_classifications.csv":
        out_dir = SHARING_DIR
        out_name = args.out
    else:
        out_dir = WORK_DIR
        out_name = args.out
    out_dir.mkdir(parents=True, exist_ok=True)
    out_path = out_dir / out_name
    with open(out_path, "w", newline="") as f:
        writer = csv.DictWriter(f, fieldnames=list(rows[0].keys()))
        writer.writeheader()
        writer.writerows(rows)
    print(f"wrote {len(rows)} items -> {out_path}")

    # quick summary
    full = [r for r in rows if r["n_raters_ok"] == len(PANEL)]
    print(f"items with all {len(PANEL)} raters: {len(full)}")
    nap = sum(1 for r in rows if not r["consensus_applicable"])
    disp = sum(1 for r in rows if r["dispersion_flag"])
    print(f"consensus not_applicable: {nap} ({100*nap/len(rows):.1f}%)")
    print(f"dispersion-flagged (SD>15): {disp} ({100*disp/len(rows):.1f}%)")
    paired = [(r["consensus_score"], float(r["v1_score"])) for r in rows
              if r["consensus_score"] is not None and r["v1_score"] not in (None, "")]
    if len(paired) > 2:
        cons, v1 = map(list, zip(*paired))
        mean = statistics.mean
        cov = mean([(a - mean(cons)) * (b - mean(v1)) for a, b in zip(cons, v1)])
        r_ = cov / (statistics.pstdev(cons) * statistics.pstdev(v1) + 1e-12)
        print(f"consensus vs v1: r = {r_:.3f} (n={len(cons)})")
    cons_all = [r["consensus_score"] for r in rows if r["consensus_score"] is not None]
    if cons_all:
        exact50 = sum(1 for s in cons_all if s == 50)
        print(f"consensus exactly 50: {100*exact50/len(cons_all):.1f}% (v1 was 26.4% overall)")


if __name__ == "__main__":
    main()
