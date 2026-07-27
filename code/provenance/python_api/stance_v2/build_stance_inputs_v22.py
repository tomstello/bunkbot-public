"""Build v2.2 stance-scoring inputs: canonical focal claim, no summary.

Design decisions (2026-06-11): raters see ONE canonical, affirms-phrased
focal claim plus the participant's raw text. The summary is dropped (it never
reached the persuader prompts and its descriptive phrasing confused raters).

Canonical claim rule:
  - orientation consensus 'affirms'  -> original conRestatement verbatim
  - 'denies' / 'unclear' / 'disagree' -> audit-generated affirmative_restatement
    (provenance: restatement_orientation_audit_v2.py), flagged for review

Writes stance_v22_inputs.jsonl + canonical_claim_review.csv (non-affirms cases
for author eyeball).
"""

from __future__ import annotations

import csv
import argparse
import json
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
IN_PATH = WORK_DIR / "stance_v2_inputs.jsonl"
# Shipped restatement-orientation audit table (study4_restatement_orientation.csv).
ORIENT = REPO_ROOT / "data/api_cached/sharing_and_stance/study4_restatement_orientation.csv"
# Transient intermediates: v2.2 scoring inputs + the non-affirms review sheet.
OUT_PATH = WORK_DIR / "stance_v22_inputs.jsonl"
REVIEW = WORK_DIR / "canonical_claim_review.csv"


def main() -> None:
    ap = argparse.ArgumentParser()
    ap.add_argument("--inputs", default=str(IN_PATH))
    ap.add_argument("--orientation", default=str(ORIENT))
    ap.add_argument("--out", default=str(OUT_PATH))
    ap.add_argument("--review", default=str(REVIEW))
    args = ap.parse_args()
    in_path, orient_path = Path(args.inputs), Path(args.orientation)
    out_path, review_path = Path(args.out), Path(args.review)
    out_path.parent.mkdir(parents=True, exist_ok=True)
    review_path.parent.mkdir(parents=True, exist_ok=True)

    orient = {r["ResponseId"]: r for r in csv.DictReader(open(orient_path))}
    n = {"affirms": 0, "replaced": 0, "missing": 0}
    review_rows = []
    with open(out_path, "w") as out:
        for line in open(in_path):
            it = json.loads(line)
            o = orient.get(it["ResponseId"])
            if o is None:
                n["missing"] += 1
                cat, claim = "missing", it["focal_claim_restatement"]
            elif o["orientation_consensus"] == "affirms":
                n["affirms"] += 1
                cat, claim = "affirms", it["focal_claim_restatement"]
            else:
                n["replaced"] += 1
                cat = o["orientation_consensus"]
                claim = o["canonical_claim"] or it["focal_claim_restatement"]
                if it["timepoint"] == "pre":  # one review row per participant
                    review_rows.append({
                        "ResponseId": it["ResponseId"],
                        "orientation_consensus": cat,
                        "original_restatement": it["focal_claim_restatement"],
                        "canonical_claim_used": claim,
                        "participant_topic": it["participant_topic_description"][:200],
                    })
            rec = {
                "item_id": it["item_id"],
                "ResponseId": it["ResponseId"],
                "timepoint": it["timepoint"],
                "task_wording": it["task_wording"],
                "focal_claim": claim,
                "claim_source": "original_restatement" if cat in ("affirms", "missing")
                                else "audit_affirmative",
                "orientation_consensus": cat,
                "participant_topic_description": it["participant_topic_description"],
                "participant_reasons": it["participant_reasons"],
                "post_text": it["post_text"],
                "v1_score": it["v1_score"],
            }
            out.write(json.dumps(rec, ensure_ascii=False) + "\n")
    with open(review_path, "w", newline="") as f:
        w = csv.DictWriter(f, fieldnames=list(review_rows[0].keys()) if review_rows else
                           ["ResponseId"])
        w.writeheader()
        w.writerows(review_rows)
    print(f"items written: {n['affirms'] + n['replaced'] + n['missing']} "
          f"(affirms {n['affirms']}, replaced {n['replaced']}, missing-orientation {n['missing']})")
    print(f"review file ({len(review_rows)} participants) -> {review_path}")


if __name__ == "__main__":
    main()
