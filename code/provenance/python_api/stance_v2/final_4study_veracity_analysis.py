"""Final four-study aligned-direct veracity + veracity-belief analysis on the
HARMONIZED labels (all four studies: gpt-5.4-mini substantiveness + role; S1/S3
with the message-id fix). Produces: per study x model x direction aligned-direct
veracity + counts; claim descriptives; and the pooled veracity-belief relation
(all / bunk / debunk; full + compliant-restricted; conversation-level slope and
cell-level correlation).

Run after the S2/S4 harmonized role job finishes.
"""
from __future__ import annotations
import csv, gzip, json, math, statistics
csv.field_size_limit(2**30)
from collections import defaultdict
from pathlib import Path
import numpy as np


def _repo_root(start: Path) -> Path:
    p = start.resolve()
    for cand in [p, *p.parents]:
        if (cand / "data").is_dir() and (cand / "code").is_dir():
            return cand
    raise RuntimeError("repo root (dir containing data/ and code/) not found")


REPO_ROOT = _repo_root(Path(__file__))
# Shipped inputs.
LABELS_DIR = REPO_ROOT / "data" / "api_cached" / "claim_labels"
COMPLIANCE_DIR = REPO_ROOT / "data" / "api_cached" / "compliance_ape"
CLEAN_DIR = REPO_ROOT / "data" / "processed_s1s3"
# NOT shipped: the S4 analytic-strict set is built at RUNTIME by build_s4_data()
# in code/bunkbot_helpers.R; there is no static file in the repo. To run this
# script standalone, materialize that frame here first (one row per S4
# ResponseId with aligned_belief_change / direction / model_pooled /
# strict_compliant columns).
S4_STRICT_NOT_SHIPPED = REPO_ROOT / "output" / "provenance_work" / "stance_v2" / "s4_analytic_strict.csv"
TOTAL_FC = {"Study1": 35034, "Study2": 30036, "Study3": 30636, "Study4": 31769}

def num(x):
    try: return float(x)
    except (TypeError, ValueError): return None

def load_all_labels():
    recs = []
    # S1/S3 (v2, message-fixed) — shipped materialized CSV
    seen = {}
    for o in csv.DictReader(gzip.open(LABELS_DIR / "claim_role_labels_s1s3.csv.gz", "rt", encoding="utf-8")):
        if o.get("request_status") == "success": seen[o["row_key"]] = o
    for o in seen.values():
        recs.append({"study": o["study_source"], "model": "GPT-4o", "cid": o["conversation_id"],
                     "dir": (o.get("direction") or ""), "vera": num(o.get("veracity_score")),
                     "stance": o.get("stance_to_focal"), "direct": o.get("directness_to_focal")})
    # S2/S4 (harmonized) — shipped materialized CSV
    for r in csv.DictReader(gzip.open(LABELS_DIR / "claim_role_labels_s2s4.csv.gz", "rt", encoding="utf-8")):
        if r.get("request_status") and r["request_status"] != "success": continue
        recs.append({"study": r["study_source"], "model": r.get("model_pooled", "GPT-4o"),
                     "cid": r["conversation_id"], "dir": (r.get("direction") or ""),
                     "vera": num(r.get("veracity_score")), "stance": r.get("stance_to_focal"),
                     "direct": r.get("directness_to_focal")})
    return recs

def is_aligned_direct(r):
    aligned = (r["stance"] == "supports") if r["dir"] == "bunk" else (r["stance"] == "opposes")
    return r["direct"] == "direct" and aligned

def per_conv_veracity(recs):
    conv = defaultdict(lambda: {"study": None, "model": None, "dir": None, "vera": []})
    adc = defaultdict(int)
    for r in recs:
        c = conv[r["cid"]]; c["study"] = r["study"]; c["model"] = r["model"]; c["dir"] = r["dir"]
        if is_aligned_direct(r):
            adc[(r["study"], r["model"], r["dir"])] += 1
            if r["vera"] is not None: c["vera"].append(r["vera"])
    return conv, adc

def load_belief():
    """conversation_id -> {aligned_belief, study, model, dir, compliant}."""
    out = {}
    # S1/S2/S3 from cleaned analytic files; compliance from the APE summaries
    eval = {}
    for st, fn in [("Study1", "study1_jailbroken"), ("Study2", "study2_standard"),
                   ("Study3", "study3_truth_constrained")]:
        for r in csv.DictReader(open(COMPLIANCE_DIR / f"{fn}_compliance_ape.csv")):
            eval[r["ResponseId"]] = num(r.get("evaluator_label"))
    for study, fn in [("Study1", "study1_jailbroken_clean.csv.gz"),
                      ("Study2", "study2_standard_clean.csv.gz"),
                      ("Study3", "study3_truth_constrained_clean.csv.gz")]:
        for r in csv.DictReader(gzip.open(CLEAN_DIR / fn, "rt", encoding="utf-8")):
            if str(r.get("isEquivocal", "")).strip().upper() != "TRUE": continue
            cat = (r.get("category", "") or "").strip(); cond = (r.get("condition", "") or "")
            direction = "debunk" if "debunk" in cond else ("bunk" if "bunk" in cond else None)
            if not direction: continue
            pre = num(r.get("belief_rating_pre_4")); post = num(r.get("belief_rating_post_4"))
            if pre is None or post is None: continue
            pre_rc = 100 - pre if cat == "denies" else pre
            post_rc = 100 - post if cat == "denies" else post
            if not (25 < pre_rc < 75): continue
            aligned = (post_rc - pre_rc) if direction == "bunk" else (pre_rc - post_rc)
            out[r["ResponseId"]] = {"aligned": aligned, "study": study, "model": "GPT-4o",
                                    "dir": direction, "compliant": eval.get(r["ResponseId"]) == 1}
    # S4 from the analytic-strict frame. NOT shipped as a static file: it is
    # built at runtime by build_s4_data() in code/bunkbot_helpers.R. Materialize
    # it to S4_STRICT_NOT_SHIPPED before running this script standalone.
    for r in csv.DictReader(open(S4_STRICT_NOT_SHIPPED)):
        ab = num(r.get("aligned_belief_change"))
        if ab is None: continue
        out[r["ResponseId"]] = {"aligned": ab, "study": "Study4", "model": r.get("model_pooled"),
                                "dir": r.get("direction"),
                                "compliant": str(r.get("strict_compliant")).strip().lower() in ("true", "1")}
    return out

def ols_slope(rws):
    if len(rws) < 5: return None
    v = np.array([x["v"] / 10 for x in rws]); y = np.array([x["b"] for x in rws])
    X = np.column_stack([np.ones(len(v)), v]); beta, *_ = np.linalg.lstsq(X, y, rcond=None)
    resid = y - X @ beta; dof = len(y) - 2; sig = (resid @ resid) / dof
    se = math.sqrt(sig * np.linalg.inv(X.T @ X)[1, 1]); t = beta[1] / se
    p = 2 * (1 - 0.5 * (1 + math.erf(abs(t) / math.sqrt(2))))
    r = float(np.corrcoef(v, y)[0, 1])
    return {"slope": beta[1], "se": se, "p": p, "r": r, "n": len(rws)}

def main():
    recs = load_all_labels()
    conv, adc = per_conv_veracity(recs)
    belief = load_belief()

    print("=== Aligned-direct veracity (harmonized), bunk / debunk ===")
    cells = defaultdict(list)
    for cid, c in conv.items():
        if c["vera"]: cells[(c["study"], c["model"], c["dir"])].append(statistics.mean(c["vera"]))
    order = [("Study1", "GPT-4o"), ("Study2", "GPT-4o"), ("Study3", "GPT-4o"),
             ("Study4", "Claude"), ("Study4", "Gemini"), ("Study4", "GPT-5.2"), ("Study4", "Grok")]
    for st, m in order:
        b = cells.get((st, m, "bunk"), []); d = cells.get((st, m, "debunk"), [])
        mb = statistics.mean(b) if b else float("nan"); md = statistics.mean(d) if d else float("nan")
        print(f"  {st:<7} {m:<8} bunk {mb:5.1f} (n={len(b)}, {adc[(st,m,'bunk')]} claims)  "
              f"debunk {md:5.1f} (n={len(d)}, {adc[(st,m,'debunk')]} claims)")

    print("\n=== Descriptives ===")
    convs_per_study = defaultdict(set); ad_per_study = defaultdict(int)
    for cid, c in conv.items(): convs_per_study[c["study"]].add(cid)
    for (st, m, dr), n in adc.items(): ad_per_study[st] += n
    for st in ["Study1", "Study2", "Study3", "Study4"]:
        ncv = len(convs_per_study[st]); fc = TOTAL_FC[st]; ad = ad_per_study[st]
        print(f"  {st}: conversations={ncv}  fact-checked={fc:,}  claims/conv={fc/ncv:.1f}  "
              f"aligned-direct={ad:,} ({ad/fc:.1%} of claims, {ad/ncv:.2f}/conv)")

    # veracity-belief
    rows = []
    for cid, c in conv.items():
        if not c["vera"] or cid not in belief: continue
        bl = belief[cid]
        rows.append({"study": c["study"], "model": c["model"], "dir": c["dir"],
                     "v": statistics.mean(c["vera"]), "b": bl["aligned"], "compliant": bl["compliant"]})
    print(f"\n=== Veracity-belief: {len(rows):,} conversations ===")
    def report(sub, label):
        for dirn, name in [(None, "all"), ("bunk", "bunk"), ("debunk", "debunk")]:
            s = [x for x in sub if dirn is None or x["dir"] == dirn]
            r = ols_slope(s)
            if r: print(f"  [{label}] {name:<7}: n={r['n']:<5} slope/10={r['slope']:+.2f} (p={r['p']:.3f}) r={r['r']:+.3f}")
    report(rows, "all conversations")
    report([x for x in rows if x["compliant"]], "compliant only")
    # cell-level
    cl = defaultdict(lambda: {"v": [], "b": []})
    for x in rows: cl[(x["study"], x["model"], x["dir"])]["v"].append(x["v"]); cl[(x["study"], x["model"], x["dir"])]["b"].append(x["b"])
    cv = {k: (statistics.mean(c["v"]), statistics.mean(c["b"]), len(c["v"])) for k, c in cl.items()}
    print("\n  cell means (study, model, dir): veracity, belief, n")
    for k in sorted(cv): print(f"    {str(k):<34} {cv[k][0]:6.1f}  {cv[k][1]:6.1f}  n={cv[k][2]}")
    for dirn in [None, "bunk", "debunk"]:
        pts = [(v[0], v[1]) for k, v in cv.items() if dirn is None or k[2] == dirn]
        if len(pts) >= 3:
            r = float(np.corrcoef([a for a, _ in pts], [b for _, b in pts])[0, 1])
            print(f"  cell-level r ({dirn or 'all'}, {len(pts)} cells): {r:+.3f}")

if __name__ == "__main__":
    main()
