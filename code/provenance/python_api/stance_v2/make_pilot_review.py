"""Generate the human-review file for the stance-v2 pilot gate.

Selects ~27 pilot items across review-relevant categories and renders them
with per-rater judgments, consensus, and rationales for manual inspection.

Writes output/provenance_work/stance_v2/stance_v2_pilot_review.md
"""

from __future__ import annotations

import csv
import random
import statistics
from pathlib import Path

HERE = Path(__file__).resolve().parent


def _repo_root(start: Path) -> Path:
    p = start.resolve()
    for cand in [p, *p.parents]:
        if (cand / "data").is_dir() and (cand / "code").is_dir():
            return cand
    raise RuntimeError("repo root (dir containing data/ and code/) not found")


REPO_ROOT = _repo_root(Path(__file__))
# Transient pilot artifacts (not shipped).
WORK_DIR = REPO_ROOT / "output" / "provenance_work" / "stance_v2"
IN = WORK_DIR / "stance_v2_pilot_consolidated.csv"
OUT = WORK_DIR / "stance_v2_pilot_review.md"

RATERS = ["claude", "gpt", "gemini", "grok", "llama"]


def fmt_item(r: dict, idx: int) -> str:
    lines = [f"### {idx}. `{r['item_id']}` ({r['timepoint']})", ""]
    lines.append(f"> {r['post_text'].strip()}")
    lines.append("")
    lines.append(f"**v1 score: {float(r['v1_score']):.0f}** | consensus: "
                 f"score **{r['consensus_score'] or 'null'}**, {r['consensus_category']}, "
                 f"{r['consensus_response_type']}, {r['consensus_focal_relevance']}"
                 f"{' | DISPERSION FLAG (SD=' + r['score_sd'] + ')' if r['dispersion_flag'] == 'True' else ''}")
    lines.append("")
    lines.append("| rater | score | category | type | confidence |")
    lines.append("|---|---|---|---|---|")
    for m in RATERS:
        sc = r[f"{m}_stance_score"] or "null"
        lines.append(f"| {m} | {sc} | {r[f'{m}_stance_category']} | "
                     f"{r[f'{m}_response_type']} | {r[f'{m}_confidence']} |")
    quotes = [(m, r[f"{m}_rationale"]) for m in ("claude", "gemini") if r.get(f"{m}_rationale")]
    for m, q in quotes:
        lines.append(f"- *{m}*: {q}")
    lines.append("")
    return "\n".join(lines)


def main() -> None:
    rng = random.Random(20260611)
    rows = list(csv.DictReader(open(IN)))
    used: set[str] = set()

    def take(pool: list[dict], n: int) -> list[dict]:
        pool = [r for r in pool if r["item_id"] not in used]
        sel = rng.sample(pool, min(n, len(pool)))
        used.update(r["item_id"] for r in sel)
        return sel

    sections: list[tuple[str, str, list[dict]]] = []
    flagged = [r for r in rows if r["dispersion_flag"] == "True"]
    sections.append(("A. Cross-rater disagreements (all dispersion-flagged items)",
                     "Raters spread > 15 SD points. Check: is the disagreement reasonable ambiguity, "
                     "or a rubric gap?", take(flagged, 10)))
    napp = [r for r in rows if float(r["v1_score"]) == 50 and r["consensus_applicable"] == "False"]
    sections.append(("B. v1 = 50 items reclassified as not-applicable",
                     "Previously fake-neutral 50s; v2 says no stance-rateable content. Check: agree these "
                     "should be excluded/flagged rather than scored 50?", take(napp, 6)))
    moved = [r for r in rows if float(r["v1_score"]) == 50 and r["consensus_score"] not in ("", "None")
             and abs(float(r["consensus_score"]) - 50) >= 15]
    sections.append(("C. v1 = 50 items v2 scores as directional",
                     "Content v1 flattened to 50 that v2 reads as taking a side. Check: do you agree "
                     "with the direction?", take(moved, 6)))
    jaq = [r for r in rows if r["consensus_response_type"] == "question_raising"
           and r["consensus_score"] not in ("", "None")]
    sections.append(("D. Question-raising posts (scored on implicature)",
                     "JAQ-style posts now categorized and scored. Check: implicature scores sensible?",
                     take(jaq, 4)))
    concordant = [r for r in rows if r["dispersion_flag"] == "False"
                  and r["consensus_applicable"] == "True" and (r["score_sd"] or "") not in ("", "None")
                  and float(r["score_sd"]) <= 8]
    sections.append(("E. Random concordant items (sanity check)",
                     "High-agreement cases for calibration.", take(concordant, 4)))

    napp_rate_random = None
    # estimate full-run not_applicable rate from the no-?-no-meta majority of items
    plain = [r for r in rows if float(r["v1_score"]) != 50]
    if plain:
        napp_rate_random = 100 * sum(1 for r in plain if r["consensus_applicable"] == "False") / len(plain)

    cons = [float(r["consensus_score"]) for r in rows if r["consensus_score"] not in ("", "None")]
    parts = [
        "# Stance v2 pilot — author review (gate before full run)",
        "",
        f"148 pilot items x 5 raters (Claude Sonnet 4.6, GPT-5.2, Gemini 3.1 Pro, Grok 4.3, "
        f"Llama 4 Maverick), prompt v2.0, zero request errors.",
        "",
        "**Reliability:** Krippendorff alpha (interval) = .89; mean pairwise score r = .90 "
        "(weakest: grok x llama .81); stance_category >= 4/5 agreement on 76% of items; "
        "response_type >= 4/5 on 77%.",
        "",
        "**v1 exact-50 stratum decomposition (n = 73):** 28 not_applicable (meta/off-topic), "
        "32 genuinely neutral, 11 directional content flattened by v1, 2 mixed. "
        "Response types: 24 question_raising, 16 meta_task, 12 uncertainty_statement.",
        "",
        f"**Consensus vs v1:** r = .95 among jointly scoreable items. "
        f"Consensus exactly-50 rate in scored items: "
        f"{100 * sum(1 for s in cons if s == 50) / len(cons):.0f}% (pilot oversamples v1-50s).",
        "",
        f"**Estimated not-applicable rate outside the v1-50 stratum:** {napp_rate_random:.1f}%.",
        "",
        "**What to check below:** (1) do flagged disagreements reflect genuine ambiguity, "
        "(2) should section-B items really be excluded from stance scoring, (3) are section-C "
        "directional readings right, (4) are JAQ implicature scores sensible. If a pattern is "
        "off, the prompt gets one revision and the pilot reruns; otherwise prompt v2.0 freezes "
        "and the full 3,690 x 5 run starts.",
        "",
    ]
    idx = 1
    for title, blurb, items in sections:
        parts.append(f"## {title}")
        parts.append("")
        parts.append(f"*{blurb}*")
        parts.append("")
        for r in items:
            parts.append(fmt_item(r, idx))
            idx += 1
    OUT.parent.mkdir(parents=True, exist_ok=True)
    OUT.write_text("\n".join(parts))
    print(f"wrote {idx - 1} examples -> {OUT}")


if __name__ == "__main__":
    main()
