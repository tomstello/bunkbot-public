"""Compare a human gold-coding sheet against the stance_v2 model ratings.

Joins a filled coder CSV (item_id, stance_score, stance_category, focal_relevance,
response_type) to stance_v2_consolidated.csv and reports, for the consensus and each
panel model: score correlation (Pearson/Spearman), MAE, and category agreement
(exact / 3-way / binary direction). Also applicability confusion and the biggest
score disagreements.

Usage: python3 compare_gold_human.py /path/to/coder.csv
"""
from __future__ import annotations
import csv, statistics, sys
from collections import Counter
from pathlib import Path

csv.field_size_limit(10**8)
HERE = Path(__file__).resolve().parent
def _repo_root(start):
    for c in [start, *start.parents]:
        if (c / "data").is_dir() and (c / "code").is_dir():
            return c
    raise RuntimeError("repo root (dir with data/ and code/) not found")
REPO_ROOT = _repo_root(HERE)
# 5-rater consolidated stance ratings (renamed from stance_v2_consolidated.csv)
CONS = REPO_ROOT / "data/api_cached/sharing_and_stance/study4_stance_classifications.csv"
MODELS = ["consensus", "claude", "gpt", "gemini", "grok", "deepseek"]
COLLAPSE = {"argues_against": "against", "leans_against": "against",
            "neutral_uncommitted": "neutral", "mixed_both_sides": "neutral",
            "leans_for": "for", "argues_for": "for", "not_applicable": "n/a"}


def num(x):
    x = (x or "").strip()
    try:
        return float(x)
    except ValueError:
        return None


def pearson(xs, ys):
    n = len(xs); mx, my = statistics.mean(xs), statistics.mean(ys)
    cov = sum((a-mx)*(b-my) for a, b in zip(xs, ys))
    sx = (sum((a-mx)**2 for a in xs))**.5; sy = (sum((b-my)**2 for b in ys))**.5
    return cov/(sx*sy) if sx*sy else float("nan")


def spearman(xs, ys):
    def rank(v):
        order = sorted(range(len(v)), key=lambda i: v[i])
        r = [0.0]*len(v); i = 0
        while i < len(v):
            j = i
            while j+1 < len(v) and v[order[j+1]] == v[order[i]]:
                j += 1
            avg = (i+j)/2 + 1
            for k in range(i, j+1):
                r[order[k]] = avg
            i = j+1
        return r
    return pearson(rank(xs), rank(ys))


def main():
    gold_path = sys.argv[1] if len(sys.argv) > 1 else str(REPO_ROOT / "data/validation/gold_coding/stance_gold_coderA.csv")
    gold = {r["item_id"]: r for r in csv.DictReader(open(gold_path, encoding="utf-8", errors="replace"))}
    cons = {r["item_id"]: r for r in csv.DictReader(open(CONS, encoding="utf-8", errors="replace"))}
    items = [i for i in gold if i in cons]
    print(f"human items: {len(gold)} | matched to model: {len(items)}\n")

    def model_score(rec, m):
        return num(rec["consensus_score"]) if m == "consensus" else num(rec.get(f"{m}_stance_score"))
    def model_cat(rec, m):
        return rec["consensus_category"] if m == "consensus" else rec.get(f"{m}_stance_category")

    # ---- score agreement (items where human AND model give a numeric score) ----
    print("=== STANCE SCORE (0-100) vs human ===")
    print(f"{'model':10s} {'n':>4s} {'pearson':>8s} {'spearman':>9s} {'MAE':>6s} {'RMSE':>6s} {'bias':>6s}")
    for m in MODELS:
        hs, ms = [], []
        for i in items:
            h = num(gold[i].get("stance_score")); v = model_score(cons[i], m)
            if h is not None and v is not None:
                hs.append(h); ms.append(v)
        if len(hs) < 3:
            continue
        mae = statistics.mean(abs(a-b) for a, b in zip(hs, ms))
        rmse = (statistics.mean((a-b)**2 for a, b in zip(hs, ms)))**.5
        bias = statistics.mean(b-a for a, b in zip(hs, ms))   # model - human
        print(f"{m:10s} {len(hs):4d} {pearson(hs,ms):8.3f} {spearman(hs,ms):9.3f} "
              f"{mae:6.1f} {rmse:6.1f} {bias:+6.1f}")

    # ---- category agreement ----
    print("\n=== STANCE CATEGORY agreement vs human (all matched items) ===")
    print(f"{'model':10s} {'exact':>7s} {'3-way':>7s} {'dir':>7s}  (dir = for/against among items both call directional)")
    for m in MODELS:
        exact = three = dn = dk = 0; tot = 0
        for i in items:
            hc = gold[i].get("stance_category"); mc = model_cat(cons[i], m)
            if not hc or not mc:
                continue
            tot += 1
            exact += (hc == mc)
            three += (COLLAPSE.get(hc) == COLLAPSE.get(mc))
            h3, m3 = COLLAPSE.get(hc), COLLAPSE.get(mc)
            if h3 in ("for", "against") and m3 in ("for", "against"):
                dk += 1; dn += (h3 == m3)
        print(f"{m:10s} {exact/tot:7.2f} {three/tot:7.2f} {dn/dk:7.2f}  (n={tot}, dir n={dk})")

    # ---- applicability confusion (consensus) ----
    print("\n=== APPLICABILITY (human not_applicable vs model consensus) ===")
    tp = fp = fn = tn = 0
    for i in items:
        h_na = (gold[i].get("stance_category") == "not_applicable") or (num(gold[i].get("stance_score")) is None)
        m_na = (cons[i]["consensus_category"] == "not_applicable") or (num(cons[i]["consensus_score"]) is None)
        if h_na and m_na: tn += 1
        elif h_na and not m_na: fn += 1
        elif not h_na and m_na: fp += 1
        else: tp += 1
    print(f"  both scoreable {tp}, both N/A {tn}, human N/A only {fn}, model N/A only {fp}")

    # ---- focal_relevance / response_type (consensus) ----
    for field in ("focal_relevance", "response_type"):
        ck = "consensus_" + field
        agree = tot = 0
        for i in items:
            h = gold[i].get(field); m = cons[i].get(ck)
            if h and m:
                tot += 1; agree += (h == m)
        print(f"{field} (consensus) exact agreement: {agree/tot:.2f} (n={tot})")

    # ---- biggest score disagreements ----
    print("\n=== Biggest consensus-vs-human score gaps ===")
    diffs = []
    for i in items:
        h = num(gold[i].get("stance_score")); v = num(cons[i]["consensus_score"])
        if h is not None and v is not None:
            diffs.append((abs(h-v), i, h, gold[i].get("stance_category"), v,
                          cons[i]["consensus_category"], (cons[i]["post_text"] or "")[:90]))
    for d, i, h, hc, v, mc, txt in sorted(diffs, reverse=True)[:10]:
        print(f"  |{d:4.0f}|  human {h:3.0f}/{hc:16s} model {v:3.0f}/{mc:16s} | {txt!r}")


if __name__ == "__main__":
    main()
