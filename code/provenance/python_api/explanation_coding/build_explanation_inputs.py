"""Build inputs for the persuasion-explanation coder, across Studies 1-4.

Emits one JSONL record per (participant x open-ended field) for every SUBSTANTIVE
response in a treatment (conversation) arm. Trivial/blank responses are NOT scored by
the panel (no API spend) but are still recorded in a roster CSV so the R analysis can
compute correct denominators.

Population: rows whose ResponseId starts with "R_" (drops Qualtrics metadata header
rows in the S4 export) and whose condition is a treatment arm (treatment_mid_bunk /
treatment_mid_debunk -- the only arms whose participants reacted to AI comments).

Outputs (data/api_cached/explanation_coding/):
  explanation_inputs.jsonl   scored items: item_id, ResponseId, study, study_factor,
                             condition, direction, field, question_prompt, topic_hint,
                             response_text
  explanation_roster.csv     every treatment participant x field row, with has_response

Usage:
  python3 build_explanation_inputs.py
  python3 build_explanation_inputs.py --studies S2,S3   # subset
"""

from __future__ import annotations

import argparse
import csv
import gzip
import json
from pathlib import Path

from taxonomy import QUESTION_TEXT

csv.field_size_limit(10**8)  # S4 export carries very large transcript fields


def _repo_root(start: Path) -> Path:
    """Walk up to the repo root: the dir containing BOTH data/ and code/."""
    p = start.resolve()
    for cand in [p, *p.parents]:
        if (cand / "data").is_dir() and (cand / "code").is_dir():
            return cand
    raise RuntimeError("repo root (dir containing data/ and code/) not found")


HERE = Path(__file__).resolve().parent
REPO_ROOT = _repo_root(HERE)
OUT_DIR = REPO_ROOT / "data/api_cached/explanation_coding"

# Inputs read from the shipped repo layout:
#   S1-3 -> data/processed_s1s3/study{N}_{regime}_clean.csv
#   S4   -> data/raw_qualtrics/study4_social_sharing_raw.csv.gz
STUDIES = {
    "S1": {"factor": "Jailbroken",
           "path": REPO_ROOT / "data/processed_s1s3/study1_jailbroken_clean.csv.gz"},
    "S2": {"factor": "Standard",
           "path": REPO_ROOT / "data/processed_s1s3/study2_standard_clean.csv.gz"},
    "S3": {"factor": "Truth-Constrained",
           "path": REPO_ROOT / "data/processed_s1s3/study3_truth_constrained_clean.csv.gz"},
    "S4": {"factor": "Sharing",
           "path": REPO_ROOT / "data/raw_qualtrics/study4_social_sharing_raw.csv.gz"},
}

TREATMENT_CONDITIONS = {"treatment_mid_bunk", "treatment_mid_debunk"}

FIELD_COLS = {"persuasive": "Persuasive_oe", "not_persuasive": "Notpersuasive_oe"}
FIELD_SHORT = {"persuasive": "pers", "not_persuasive": "notpers"}

# responses treated as non-answers (handled locally, never sent to the panel)
STOPWORDS = {"", "na", "n/a", "n.a.", "none", "no", "nope", "nothing", "n a", "none.",
             "no.", "nan", "null", "n/a.", "idk", "i dont know", "i don't know", "-"}


def is_trivial(text: str) -> bool:
    t = (text or "").strip()
    low = t.lower()
    if low in STOPWORDS:
        return True
    # strip trailing punctuation/spaces and re-test very short tokens
    stripped = low.strip(".!? \t\n")
    if stripped in STOPWORDS:
        return True
    return len(stripped) <= 2


def direction_of(condition: str) -> str:
    if condition.endswith("debunk"):
        return "debunk"
    if condition.endswith("bunk"):
        return "bunk"
    return "other"


def main() -> None:
    ap = argparse.ArgumentParser()
    ap.add_argument("--studies", default="S1,S2,S3,S4",
                    help="comma-separated subset of S1,S2,S3,S4")
    args = ap.parse_args()
    wanted = [s.strip() for s in args.studies.split(",") if s.strip()]

    OUT_DIR.mkdir(parents=True, exist_ok=True)
    inputs_path = OUT_DIR / "explanation_inputs.jsonl"
    roster_path = OUT_DIR / "explanation_roster.csv"

    items: list[dict] = []
    roster: list[dict] = []
    per_study_counts: dict[str, dict] = {}

    for study in wanted:
        cfg = STUDIES[study]
        path = cfg["path"]
        if not path.exists():
            raise SystemExit(f"missing data file for {study}: {path}")
        with gzip.open(path, "rt", encoding="utf-8", errors="replace", newline="") as fh:
            rows = list(csv.DictReader(fh))
        n_trt = n_sub = 0
        for r in rows:
            rid = (r.get("ResponseId") or "").strip()
            cond = (r.get("condition") or "").strip()
            if not rid.startswith("R_") or cond not in TREATMENT_CONDITIONS:
                continue
            n_trt += 1
            topic = (r.get("mid_topic") or "").strip()
            finished = (r.get("Finished") or "").strip()
            for field, col in FIELD_COLS.items():
                text = (r.get(col) or "").strip()
                trivial = is_trivial(text)
                item_id = f"{study}_{rid}_{FIELD_SHORT[field]}"
                roster.append({
                    "item_id": item_id, "study": study, "study_factor": cfg["factor"],
                    "ResponseId": rid, "condition": cond, "direction": direction_of(cond),
                    "field": field, "finished": finished,
                    "has_response": int(not trivial), "n_chars": len(text),
                    "topic_hint": topic, "response_text": text,
                })
                if trivial:
                    continue
                n_sub += 1
                items.append({
                    "item_id": item_id, "ResponseId": rid,
                    "study": study, "study_factor": cfg["factor"],
                    "condition": cond, "direction": direction_of(cond),
                    "field": field, "question_prompt": QUESTION_TEXT[field],
                    "topic_hint": topic, "response_text": text,
                })
        per_study_counts[study] = {"treatment_rows": n_trt, "scored_items": n_sub}

    with open(inputs_path, "w", encoding="utf-8") as f:
        for it in items:
            f.write(json.dumps(it, ensure_ascii=False) + "\n")

    with open(roster_path, "w", newline="", encoding="utf-8") as f:
        w = csv.DictWriter(f, fieldnames=list(roster[0].keys()))
        w.writeheader()
        w.writerows(roster)

    print(f"wrote {len(items)} scored items -> {inputs_path}")
    print(f"wrote {len(roster)} roster rows  -> {roster_path}")
    for s, c in per_study_counts.items():
        print(f"  {s}: treatment participants={c['treatment_rows']}, "
              f"scored items={c['scored_items']}")


if __name__ == "__main__":
    main()
