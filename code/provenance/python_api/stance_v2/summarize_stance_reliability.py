"""Stance v2.2 reliability + validation report.

Computes, over the full consolidated panel (3,690 items x 5 raters):
  - Krippendorff's alpha (interval) and ICC(2,k) on stance scores
  - pairwise Pearson r per rater pair
  - nominal agreement on stance_category / response_type / focal_relevance
    (Krippendorff's alpha, nominal) + >=4/5 plurality rates
  - agreement broken down by consensus response_type
  - within-rater test-retest (replicate 1, 200 items): r and category agreement
  - leave-one-rater-out consensus stability
  - v1 vs v2.2 comparison
  - response-type / relevance distribution (overall and by timepoint)
  - gold validation (when coder CSVs are present in
    data/validation/gold_coding/returns/)

Writes data/validation/stance_v2_reliability_report.md and a tidy CSV of all
metrics (data/validation/stance_v2_reliability_metrics.csv).
"""

from __future__ import annotations

import csv
import glob
import itertools
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
# Transient per-rater scores_*.jsonl live in the working dir.
WORK_DIR = REPO_ROOT / "output" / "provenance_work" / "stance_v2"
V2_DIR = WORK_DIR
# Shipped consolidated stance classifications (input).
CONSOL = REPO_ROOT / "data/api_cached/sharing_and_stance/study4_stance_classifications.csv"
# Shipped reliability/validation outputs.
VALIDATION_DIR = REPO_ROOT / "data" / "validation"
REPORT = VALIDATION_DIR / "stance_v2_reliability_report.md"
METRICS_CSV = VALIDATION_DIR / "stance_v2_reliability_metrics.csv"
# Human coder export CSVs (if present) are dropped here for gold validation.
GOLD_RETURNS = VALIDATION_DIR / "gold_coding" / "returns"

RATERS = ["claude", "gpt", "gemini", "grok", "deepseek"]
SLUGS = {
    "claude": "anthropic_claude-sonnet-4.6",
    "gpt": "openai_gpt-5.2",
    "gemini": "google_gemini-3.1-pro-preview",
    "grok": "x-ai_grok-4.3",
    "deepseek": "deepseek_deepseek-v3.2",
}

metrics: list[dict] = []


def add_metric(section: str, name: str, value, n=None):
    metrics.append({"section": section, "metric": name, "value": value, "n": n})


def corr(xs, ys):
    mx, my = statistics.mean(xs), statistics.mean(ys)
    cov = sum((a - mx) * (b - my) for a, b in zip(xs, ys))
    sx = (sum((a - mx) ** 2 for a in xs)) ** 0.5
    sy = (sum((b - my) ** 2 for b in ys)) ** 0.5
    return cov / (sx * sy + 1e-12)


def kripp_interval(units: list[list[float]]) -> float:
    units = [u for u in units if len(u) >= 2]
    npair = sum(len(u) for u in units)
    Do = sum(sum((a - b) ** 2 for i, a in enumerate(u) for b in u[i + 1:]) / (len(u) - 1)
             for u in units) / npair
    allv = [v for u in units for v in u]
    De = sum((a - b) ** 2 for i, a in enumerate(allv) for b in allv[i + 1:]) * 2 / (len(allv) * (len(allv) - 1))
    return 1 - 2 * Do / De


def kripp_nominal(units: list[list[str]]) -> float:
    units = [u for u in units if len(u) >= 2]
    npair = sum(len(u) for u in units)
    Do = sum(sum((a != b) for i, a in enumerate(u) for b in u[i + 1:]) / (len(u) - 1)
             for u in units) / npair
    allv = [v for u in units for v in u]
    De = sum((a != b) for i, a in enumerate(allv) for b in allv[i + 1:]) * 2 / (len(allv) * (len(allv) - 1))
    return 1 - Do / De


def icc2k(rows: list[dict]) -> float:
    """ICC(2,k) on stance scores over items with all raters scoring."""
    mat = []
    for r in rows:
        vals = [sc(r, m) for m in RATERS]
        if all(v is not None for v in vals):
            mat.append(vals)
    n, k = len(mat), len(RATERS)
    grand = statistics.mean(v for row in mat for v in row)
    row_means = [statistics.mean(row) for row in mat]
    col_means = [statistics.mean(row[j] for row in mat) for j in range(k)]
    ms_r = k * sum((m - grand) ** 2 for m in row_means) / (n - 1)
    ms_c = n * sum((m - grand) ** 2 for m in col_means) / (k - 1)
    ss_tot = sum((v - grand) ** 2 for row in mat for v in row)
    ss_err = ss_tot - (k - 1) * ms_c / 1 - (n - 1) * ms_r / 1
    # standard two-way decomposition
    ss_err = ss_tot - sum((m - grand) ** 2 for m in row_means) * k - sum((m - grand) ** 2 for m in col_means) * n
    ms_e = ss_err / ((n - 1) * (k - 1))
    return (ms_r - ms_e) / (ms_r + (ms_c - ms_e) / n)


def sc(r: dict, m: str):
    v = r.get(f"{m}_stance_score")
    return float(v) if v not in ("", "None", None) else None


def cat(r: dict, m: str, field: str):
    v = r.get(f"{m}_{field}")
    return v if v not in ("", "None", None) else None


def main() -> None:
    rows = list(csv.DictReader(open(CONSOL)))
    lines = ["# Stance v2.2 reliability report", "",
             f"{len(rows)} items x {len(RATERS)} raters "
             f"(claude-sonnet-4.6, gpt-5.2, gemini-3.1-pro, grok-4.3, deepseek-v3.2), "
             f"prompt v2.2, canonical affirms-phrased focal claims.", ""]

    # ---- score reliability ----
    units = [[v for m in RATERS if (v := sc(r, m)) is not None] for r in rows]
    alpha = kripp_interval(units)
    icc = icc2k(rows)
    add_metric("scores", "krippendorff_alpha_interval", round(alpha, 3))
    add_metric("scores", "icc_2k", round(icc, 3))
    lines += ["## Stance scores (0-100)", "",
              f"- Krippendorff's alpha (interval): **{alpha:.3f}**",
              f"- ICC(2,k): **{icc:.3f}**"]
    prs = []
    for a, b in itertools.combinations(RATERS, 2):
        pts = [(sc(r, a), sc(r, b)) for r in rows]
        pts = [(x, y) for x, y in pts if x is not None and y is not None]
        rr = corr([p[0] for p in pts], [p[1] for p in pts])
        prs.append(rr)
        add_metric("pairwise_r", f"{a}x{b}", round(rr, 3), len(pts))
    lines += [f"- pairwise r: mean **{statistics.mean(prs):.3f}**, "
              f"range {min(prs):.3f}-{max(prs):.3f}", ""]

    # ---- categorical reliability ----
    lines += ["## Categorical fields", ""]
    for field in ("stance_category", "response_type", "focal_relevance"):
        cunits = [[v for m in RATERS if (v := cat(r, m, field)) is not None] for r in rows]
        ka = kripp_nominal(cunits)
        plur = sum(1 for u in cunits if u and Counter(u).most_common(1)[0][1] >= 4) / len(cunits)
        unam = sum(1 for u in cunits if u and len(set(u)) == 1) / len(cunits)
        add_metric("categorical", f"{field}_alpha_nominal", round(ka, 3))
        add_metric("categorical", f"{field}_ge4of5", round(plur, 3))
        lines.append(f"- {field}: alpha (nominal) **{ka:.3f}**; unanimous {unam:.0%}; >=4/5 {plur:.0%}")
    lines.append("")

    # ---- agreement by response type ----
    lines += ["## Score dispersion by consensus response type", ""]
    by_rt: dict[str, list[float]] = defaultdict(list)
    for r in rows:
        if r["score_sd"] not in ("", "None"):
            by_rt[r["consensus_response_type"]].append(float(r["score_sd"]))
    for rt, sds in sorted(by_rt.items(), key=lambda kv: -len(kv[1])):
        add_metric("dispersion_by_type", rt, round(statistics.mean(sds), 2), len(sds))
        lines.append(f"- {rt}: mean cross-rater SD {statistics.mean(sds):.1f} (n={len(sds)})")
    lines.append("")

    # ---- test-retest ----
    lines += ["## Within-rater test-retest (200 duplicate items)", ""]
    for m in RATERS:
        path = V2_DIR / f"scores_{SLUGS[m]}.jsonl"
        main_rec, retest_rec = {}, {}
        for line in open(path):
            rec = json.loads(line)
            if rec.get("prompt_version") != "v2.2" or not rec.get("request_ok"):
                continue
            (retest_rec if rec.get("replicate") else main_rec)[rec["item_id"]] = rec
        pairs = [(main_rec[i], retest_rec[i]) for i in retest_rec if i in main_rec]
        spairs = [(a["stance_score"], b["stance_score"]) for a, b in pairs
                  if a["stance_score"] is not None and b["stance_score"] is not None]
        cat_agree = statistics.mean(a["stance_category"] == b["stance_category"] for a, b in pairs)
        rt_r = corr([p[0] for p in spairs], [p[1] for p in spairs]) if len(spairs) > 2 else float("nan")
        exact = statistics.mean(abs(x - y) <= 5 for x, y in spairs)
        add_metric("test_retest", f"{m}_score_r", round(rt_r, 3), len(spairs))
        add_metric("test_retest", f"{m}_category_agree", round(cat_agree, 3), len(pairs))
        lines.append(f"- {m}: score r = **{rt_r:.3f}**, category agreement {cat_agree:.0%}, "
                     f"|diff|<=5 in {exact:.0%} (n={len(pairs)})")
    lines.append("")

    # ---- leave-one-rater-out consensus stability ----
    lines += ["## Leave-one-rater-out consensus stability", ""]
    full_cons = {}
    for r in rows:
        vals = [v for m in RATERS if (v := sc(r, m)) is not None]
        if len(vals) >= 3:
            full_cons[r["item_id"]] = statistics.median(vals)
    for left_out in RATERS:
        loo = {}
        for r in rows:
            vals = [v for m in RATERS if m != left_out and (v := sc(r, m)) is not None]
            if len(vals) >= 2 and r["item_id"] in full_cons:
                loo[r["item_id"]] = statistics.median(vals)
        pts = [(full_cons[i], loo[i]) for i in loo]
        rr = corr([p[0] for p in pts], [p[1] for p in pts])
        add_metric("leave_one_out", f"without_{left_out}", round(rr, 3), len(pts))
        lines.append(f"- without {left_out}: r(consensus, LOO-consensus) = {rr:.3f}")
    lines.append("")

    # ---- v1 vs v2.2 ----
    pts = [(float(r["v1_score"]), float(r["consensus_score"])) for r in rows
           if r["consensus_score"] not in ("", "None") and r["v1_score"] not in ("", "None")]
    rr = corr([p[0] for p in pts], [p[1] for p in pts])
    cons_scores = [p[1] for p in pts]
    add_metric("v1_v2", "pearson_r", round(rr, 3), len(pts))
    nap = sum(1 for r in rows if r["consensus_applicable"] == "False")
    lines += ["## v1 (Gemini Flash, single) vs v2.2 consensus", "",
              f"- r = **{rr:.3f}** over {len(pts)} jointly scored items",
              f"- exactly-50 share: v1 26.4% -> v2.2 {statistics.mean(s == 50 for s in cons_scores):.1%}",
              f"- v2.2 not_applicable: {nap} items ({nap/len(rows):.1%}) — meta/off-topic posts "
              f"that v1 scored as fake-neutral 50s", ""]

    # ---- response-type distribution ----
    lines += ["## Consensus response-type distribution", ""]
    for tp in ("pre", "post"):
        cnt = Counter(r["consensus_response_type"] for r in rows if r["timepoint"] == tp)
        tot = sum(cnt.values())
        lines.append(f"- {tp}: " + ", ".join(f"{k} {v} ({v/tot:.0%})" for k, v in cnt.most_common()))
        for k, v in cnt.items():
            add_metric(f"response_type_{tp}", k, v)
    lines.append("")

    # ---- gold validation (if coder returns present) ----
    gold_files = sorted(glob.glob(str(GOLD_RETURNS / "*.csv")))
    if gold_files:
        lines += ["## Human gold validation", ""]
        cons = {r["item_id"]: r for r in rows}
        for gf in gold_files:
            grows = [g for g in csv.DictReader(open(gf)) if g.get("stance_score") or g.get("stance_category")]
            spts, cat_hits, n_cat = [], 0, 0
            for g in grows:
                c = cons.get(g["item_id"])
                if not c:
                    continue
                if g.get("stance_score") and c["consensus_score"] not in ("", "None"):
                    spts.append((float(g["stance_score"]), float(c["consensus_score"])))
                if g.get("stance_category"):
                    n_cat += 1
                    cat_hits += g["stance_category"] == c["consensus_category"]
            name = Path(gf).stem
            if len(spts) > 2:
                gr = corr([p[0] for p in spts], [p[1] for p in spts])
                mae = statistics.mean(abs(a - b) for a, b in spts)
                add_metric("gold", f"{name}_score_r", round(gr, 3), len(spts))
                lines.append(f"- {name}: consensus vs human r = **{gr:.3f}**, MAE {mae:.1f} "
                             f"(n={len(spts)}); category agreement {cat_hits}/{n_cat}")
        lines.append("")
    else:
        lines += ["## Human gold validation", "",
                  "_Pending: drop coder export CSVs into "
                  "data/validation/gold_coding/returns/ and re-run._", ""]

    VALIDATION_DIR.mkdir(parents=True, exist_ok=True)
    REPORT.write_text("\n".join(lines))
    with open(METRICS_CSV, "w", newline="") as f:
        w = csv.DictWriter(f, fieldnames=["section", "metric", "value", "n"])
        w.writeheader()
        w.writerows(metrics)
    print(f"report -> {REPORT}")
    print(f"metrics -> {METRICS_CSV}")
    print("\n".join(lines[:40]))


if __name__ == "__main__":
    main()
