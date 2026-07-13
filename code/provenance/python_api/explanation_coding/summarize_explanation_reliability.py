"""Inter-model reliability + validation report for the persuasion-explanation coder.

Reads the per-rater JSONLs directly (per-item theme vectors) and computes:
  - per theme (binary): prevalence, Krippendorff's alpha (nominal), Fleiss' kappa,
    mean pairwise percent agreement; flags themes with alpha < threshold
  - pooled: mean alpha / kappa across the 15 themes
  - response_quality and primary_theme: Krippendorff's alpha (nominal),
    unanimous and >=4/5 plurality rates
  - multilabel agreement: mean pairwise Hamming agreement over the 15 themes and
    exact 15-bit set-match rate (overall and substantive-only)
  - within-rater test-retest (replicate 1, if present): per-model theme agreement
  - leave-one-rater-out: how often the per-theme majority consensus changes when
    each rater is dropped (lower = more stable / no single model drives consensus)

Writes output/provenance_work/explanation_coding/explanation_reliability_report.md and
explanation_reliability_metrics.csv.

Usage: python3 summarize_explanation_reliability.py [--items pilot_item_ids.txt]
"""

from __future__ import annotations

import argparse
import itertools
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
# Reliability report + metrics CSV are derived analysis products (not shipped under data/);
# route them to the provenance working dir.
RESULTS = REPO_ROOT / "output/provenance_work/explanation_coding"

from taxonomy import THEME_KEYS  # noqa: E402

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
ALPHA_FLAG = 0.5

metrics: list[dict] = []


def add(section, metric, value, n=None):
    metrics.append({"section": section, "metric": metric, "value": value, "n": n})


def model_slug(model: str) -> str:
    return re.sub(r"[^A-Za-z0-9._-]+", "_", model.removeprefix("openrouter/"))


def load_records(prompt_version: str, keep: set | None):
    """-> data[replicate][item_id][short_model] = rec (latest ok)."""
    data: dict = defaultdict(lambda: defaultdict(dict))
    for model in PANEL:
        path = DIR / f"scores_{model_slug(model)}.jsonl"
        if not path.exists():
            continue
        for line in open(path):
            try:
                rec = json.loads(line)
            except json.JSONDecodeError:
                continue
            if not rec.get("request_ok") or rec.get("prompt_version") != prompt_version:
                continue
            if keep and rec["item_id"] not in keep:
                continue
            data[rec.get("replicate", 0)][rec["item_id"]][SHORT[model]] = rec
    return data


def kripp_nominal(units: list[list]) -> float:
    """Krippendorff's alpha, nominal metric, O(N). units = per-item label lists."""
    units = [u for u in units if len(u) >= 2]
    if not units:
        return float("nan")
    N = sum(len(u) for u in units)
    do_num = 0.0
    glob: Counter = Counter()
    for u in units:
        m = len(u)
        c = Counter(u)
        mism = (m * m - sum(v * v for v in c.values())) / 2.0  # mismatched unordered pairs
        do_num += mism / (m - 1)
        glob.update(c)
    Do = do_num / N
    De = (N * N - sum(v * v for v in glob.values())) / (N * (N - 1))
    return 1 - Do / De if De > 0 else float("nan")


def fleiss_kappa(rows: list[list[int]]) -> float:
    """Binary Fleiss kappa over items rated by the SAME number of raters."""
    rows = [r for r in rows if len(r) >= 2]
    if not rows:
        return float("nan")
    nset = {len(r) for r in rows}
    if len(nset) != 1:                      # restrict to the modal rater count
        modal = Counter(len(r) for r in rows).most_common(1)[0][0]
        rows = [r for r in rows if len(r) == modal]
    n = len(rows[0]); N = len(rows)
    n1 = [sum(r) for r in rows]             # # of "true" votes per item
    p1 = sum(n1) / (N * n)
    p0 = 1 - p1
    Pe = p1 * p1 + p0 * p0
    Pbar = sum((a * a + (n - a) * (n - a) - n) / (n * (n - 1)) for a in n1) / N
    return (Pbar - Pe) / (1 - Pe) if (1 - Pe) > 1e-12 else float("nan")


def pairwise_pct_agreement(label_lists: list[dict]) -> float:
    """Mean over rater pairs of fraction of items where both gave the same bit."""
    agree = total = 0
    for per_item in label_lists:
        raters = list(per_item)
        for a, b in itertools.combinations(raters, 2):
            total += 1
            agree += int(per_item[a] == per_item[b])
    return agree / total if total else float("nan")


def main() -> None:
    ap = argparse.ArgumentParser()
    ap.add_argument("--items", default=None)
    ap.add_argument("--prompt-version", default="v1.0")
    ap.add_argument("--out-prefix", default="explanation")
    args = ap.parse_args()
    if args.items:
        items_path = Path(args.items)
        if not items_path.exists():
            items_path = DIR / args.items
        keep = {l.strip() for l in open(items_path) if l.strip()}
    else:
        keep = None

    RESULTS.mkdir(exist_ok=True)
    data = load_records(args.prompt_version, keep)
    main_pass = data.get(0, {})
    items = sorted(main_pass)
    R = list(SHORT.values())

    lines = ["# Persuasion-explanation coder — reliability report", "",
             f"{len(items)} items x up to {len(R)} raters "
             f"(claude-sonnet-4.6, gpt-5.2, gemini-3.1-pro, grok-4.3, deepseek-v3.2), "
             f"prompt {args.prompt_version}.", ""]

    # ---- per-theme reliability ----
    lines += ["## Per-theme reliability (binary labels)", "",
              "| theme | prevalence | Kripp. alpha | Fleiss kappa | pairwise % agree |",
              "|---|---|---|---|---|"]
    alphas, kappas = [], []
    for theme in THEME_KEYS:
        units, rows_fixed, per_item_bits = [], [], []
        n_true = n_tot = 0
        for it in items:
            recs = main_pass[it]
            bits = {m: int(bool(recs[m]["themes"].get(theme))) for m in recs}
            if len(bits) >= 2:
                units.append(list(bits.values()))
                rows_fixed.append(list(bits.values()))
                per_item_bits.append(bits)
                n_true += sum(bits.values()); n_tot += len(bits)
        a = kripp_nominal(units)
        k = fleiss_kappa(rows_fixed)
        pa = pairwise_pct_agreement(per_item_bits)
        prev = n_true / n_tot if n_tot else float("nan")
        if a == a:
            alphas.append(a)
        if k == k:
            kappas.append(k)
        add("theme_alpha", theme, round(a, 3) if a == a else None, n_tot)
        add("theme_kappa", theme, round(k, 3) if k == k else None, n_tot)
        add("theme_prevalence", theme, round(prev, 4))
        flag = " ⚠" if (a == a and a < ALPHA_FLAG) else ""
        lines.append(f"| {theme}{flag} | {prev:.3f} | "
                     f"{a:.3f} | {k:.3f} | {pa:.3f} |" if a == a and k == k
                     else f"| {theme}{flag} | {prev:.3f} | {a:.3f} | n/a | {pa:.3f} |")
    lines += ["",
              f"- mean Krippendorff alpha across themes: **{statistics.mean(alphas):.3f}**" if alphas else "",
              f"- mean Fleiss kappa across themes: **{statistics.mean(kappas):.3f}**" if kappas else "",
              f"- themes below alpha {ALPHA_FLAG}: "
              f"{sum(1 for a in alphas if a < ALPHA_FLAG)}", ""]
    add("pooled", "mean_theme_alpha", round(statistics.mean(alphas), 3) if alphas else None)
    add("pooled", "mean_theme_kappa", round(statistics.mean(kappas), 3) if kappas else None)

    # ---- categorical fields ----
    lines += ["## Categorical fields", ""]
    for field in ("response_quality", "primary_theme"):
        units = []
        unam = plur4 = denom = 0
        for it in items:
            recs = main_pass[it]
            vals = [recs[m][field] for m in recs]
            if len(vals) >= 2:
                units.append(vals); denom += 1
                unam += int(len(set(vals)) == 1)
                plur4 += int(Counter(vals).most_common(1)[0][1] >= 4)
        a = kripp_nominal(units)
        add("categorical", f"{field}_alpha_nominal", round(a, 3) if a == a else None)
        add("categorical", f"{field}_unanimous", round(unam / denom, 3) if denom else None)
        add("categorical", f"{field}_ge4of5", round(plur4 / denom, 3) if denom else None)
        lines.append(f"- {field}: alpha (nominal) **{a:.3f}**; "
                     f"unanimous {unam/denom:.0%}; >=4/5 {plur4/denom:.0%}")
    lines.append("")

    # ---- multilabel agreement ----
    def multilabel(subset_items):
        ham_a = ham_t = exact_a = exact_t = 0
        for it in subset_items:
            recs = main_pass[it]
            rs = list(recs)
            vecs = {m: tuple(int(bool(recs[m]["themes"].get(t))) for t in THEME_KEYS) for m in rs}
            for a, b in itertools.combinations(rs, 2):
                ham_t += len(THEME_KEYS)
                ham_a += sum(x == y for x, y in zip(vecs[a], vecs[b]))
            if len(rs) >= 2:
                exact_t += 1
                exact_a += int(len(set(vecs.values())) == 1)
        return (ham_a / ham_t if ham_t else float("nan"),
                exact_a / exact_t if exact_t else float("nan"))

    sub_items = [it for it in items
                 if Counter(main_pass[it][m]["response_quality"] for m in main_pass[it]).most_common(1)[0][0]
                 == "substantive"]
    ham_all, exact_all = multilabel(items)
    ham_sub, exact_sub = multilabel(sub_items)
    add("multilabel", "hamming_agreement_all", round(ham_all, 3), len(items))
    add("multilabel", "exact_set_match_all", round(exact_all, 3), len(items))
    add("multilabel", "hamming_agreement_substantive", round(ham_sub, 3), len(sub_items))
    add("multilabel", "exact_set_match_substantive", round(exact_sub, 3), len(sub_items))
    lines += ["## Multilabel agreement (15-theme vectors)", "",
              f"- mean pairwise per-theme agreement (all items): **{ham_all:.3f}**",
              f"- exact 15-theme set match across raters (all): **{exact_all:.3f}**",
              f"- mean pairwise per-theme agreement (substantive, n={len(sub_items)}): **{ham_sub:.3f}**",
              f"- exact set match (substantive): **{exact_sub:.3f}**", ""]

    # ---- test-retest ----
    retest = data.get(1, {})
    lines += ["## Within-rater test-retest (replicate 1)", ""]
    if retest:
        for m in R:
            paired = [(main_pass[i][m], retest[i][m]) for i in retest
                      if i in main_pass and m in main_pass[i] and m in retest[i]]
            if not paired:
                continue
            theme_agree = statistics.mean(
                sum(int(bool(a["themes"].get(t)) == bool(b["themes"].get(t))) for t in THEME_KEYS) / len(THEME_KEYS)
                for a, b in paired)
            rq_agree = statistics.mean(a["response_quality"] == b["response_quality"] for a, b in paired)
            add("test_retest", f"{m}_theme_agreement", round(theme_agree, 3), len(paired))
            add("test_retest", f"{m}_response_quality_agreement", round(rq_agree, 3), len(paired))
            lines.append(f"- {m}: per-theme agreement {theme_agree:.3f}, "
                         f"response_quality agreement {rq_agree:.0%} (n={len(paired)})")
    else:
        lines.append("_No replicate-1 records found. Run "
                     "`score_explanation.py --replicate 1 --items retest_item_ids.txt`._")
    lines.append("")

    # ---- leave-one-rater-out consensus stability ----
    lines += ["## Leave-one-rater-out consensus stability", "",
              "Fraction of (item x theme) majority-consensus labels that change when a rater is "
              "dropped (lower = more stable):", ""]
    full_items = [it for it in items if len(main_pass[it]) == len(R)]
    for left in R:
        changed = total = 0
        for it in full_items:
            recs = main_pass[it]
            for t in THEME_KEYS:
                votes = [int(bool(recs[m]["themes"].get(t))) for m in recs]
                full_c = int(sum(votes) * 2 > len(votes))
                loo = [int(bool(recs[m]["themes"].get(t))) for m in recs if m != left]
                loo_c = int(sum(loo) * 2 > len(loo))
                total += 1; changed += int(full_c != loo_c)
        frac = changed / total if total else float("nan")
        add("leave_one_out", f"without_{left}", round(frac, 4), len(full_items))
        lines.append(f"- without {left}: {frac:.4f} of labels change")
    lines.append("")

    report = RESULTS / f"{args.out_prefix}_reliability_report.md"
    report.write_text("\n".join(l for l in lines if l is not None))
    import csv as _csv
    with open(RESULTS / f"{args.out_prefix}_reliability_metrics.csv", "w", newline="") as f:
        w = _csv.DictWriter(f, fieldnames=["section", "metric", "value", "n"])
        w.writeheader(); w.writerows(metrics)
    print(f"report  -> {report}")
    print(f"metrics -> {RESULTS / (args.out_prefix + '_reliability_metrics.csv')}")
    print("\n".join(lines[:30]))


if __name__ == "__main__":
    main()
