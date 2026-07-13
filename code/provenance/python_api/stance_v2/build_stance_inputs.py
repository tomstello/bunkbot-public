"""Build the stance-v2 scoring input file.

Joins participant context from the raw Qualtrics export onto the canonical
3,870 (ResponseId x timepoint) post items defined by the v1 score cache, and
emits one JSONL record per item with everything the v2 classifier needs.

The v1 cache defines the item universe so that v2 coverage can be asserted
against it 1:1. Post text is taken from the raw export and cross-checked
against the cache copy (whitespace-normalized) to catch join errors.

Usage:
    python3 build_stance_inputs.py
"""

from __future__ import annotations

import csv
import gzip
import json
import re
import sys
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
# Shipped inputs: v1 post-stance score cache and the raw Study 4 Qualtrics export.
V1_CACHE = REPO_ROOT / "data/api_cached/sharing_and_stance/study4_sharing_post_scores.csv"
RAW_QUALTRICS = REPO_ROOT / "data/raw_qualtrics/study4_social_sharing_raw.csv.gz"
# stance_v2_inputs.jsonl is a transient pipeline intermediate (consumed by the
# stance scorer/consolidator and the orientation/drift audits), not shipped.
OUT_DIR = WORK_DIR
OUT_PATH = OUT_DIR / "stance_v2_inputs.jsonl"

# Verbatim task wordings from the fielded QSF (QID200 / QID205), shown to the
# classifier so it understands the genre and that hedged posts were invited.
TASK_WORDING = {
    "pre": (
        "Write a social media post -- Imagine you are making a public post on a "
        "social media platform about the topic below. Write the post as you "
        "naturally would online. It's okay to be uncertain or mixed."
    ),
    "post": (
        "Write a final post -- Now that you've talked with the AI, write a new "
        "social media post that reflects your current view on the topic. This can "
        "be the same as before or different. Write it as you naturally would online."
    ),
}

RAW_FIELDS = [
    "ResponseId",
    "mid_topic",
    "mid_evidence",
    "conSummary",
    "conRestatement",
    "social_post",
    "final_post",
]


def norm(text: str) -> str:
    return re.sub(r"\s+", " ", (text or "")).strip()


def main() -> None:
    csv.field_size_limit(sys.maxsize)

    cache_items: dict[tuple[str, str], dict] = {}
    with open(V1_CACHE, newline="") as f:
        for row in csv.DictReader(f):
            key = (row["ResponseId"], row["timepoint"])
            # keep first occurrence per (ResponseId, timepoint), matching v1 merge
            cache_items.setdefault(key, row)
    rids = {rid for rid, _ in cache_items}
    print(f"v1 cache items: {len(cache_items)} across {len(rids)} ResponseIds")

    raw: dict[str, dict] = {}
    with gzip.open(RAW_QUALTRICS, "rt", encoding="utf-8", newline="") as f:
        reader = csv.DictReader(f)
        for i, row in enumerate(reader):
            if i < 2:  # Qualtrics metadata rows (question text, ImportId)
                continue
            rid = row.get("ResponseId", "")
            if rid in rids and rid not in raw:
                raw[rid] = {k: row.get(k, "") for k in RAW_FIELDS}
    print(f"raw rows matched: {len(raw)} / {len(rids)}")

    missing_raw = rids - set(raw)
    if missing_raw:
        raise SystemExit(f"FATAL: {len(missing_raw)} ResponseIds missing from raw export: "
                         f"{sorted(missing_raw)[:5]}")

    OUT_DIR.mkdir(parents=True, exist_ok=True)
    n_written = 0
    n_text_mismatch = 0
    with open(OUT_PATH, "w") as out:
        for (rid, timepoint), crow in sorted(cache_items.items()):
            rrow = raw[rid]
            post_text_raw = rrow["social_post"] if timepoint == "pre" else rrow["final_post"]
            if norm(post_text_raw) != norm(crow["post_text"]):
                n_text_mismatch += 1
                if n_text_mismatch <= 5:
                    print(f"text mismatch {rid}::{timepoint}: "
                          f"raw={norm(post_text_raw)[:60]!r} cache={norm(crow['post_text'])[:60]!r}")
            record = {
                "item_id": f"{rid}::{timepoint}",
                "ResponseId": rid,
                "timepoint": timepoint,
                "task_wording": TASK_WORDING[timepoint],
                "focal_claim_restatement": norm(rrow["conRestatement"]) or norm(crow["focal_claim_restatement"]),
                "focal_claim_summary": norm(rrow["conSummary"]) or norm(crow["focal_claim_summary"]),
                "participant_topic_description": norm(rrow["mid_topic"]),
                "participant_reasons": norm(rrow["mid_evidence"]),
                # post text from cache (the exact string v1 scored) for strict comparability
                "post_text": crow["post_text"],
                # auxiliary metadata, NOT sent to the model
                "v1_score": crow["score"],
                "v1_confidence": crow["confidence"],
            }
            out.write(json.dumps(record, ensure_ascii=False) + "\n")
            n_written += 1

    print(f"wrote {n_written} records -> {OUT_PATH}")
    print(f"post-text mismatches vs cache (informational): {n_text_mismatch}")
    if n_written != len(cache_items):
        raise SystemExit("FATAL: written count != cache item count")
    empty_ctx = sum(1 for (rid, tp) in cache_items
                    if not norm(raw[rid]["mid_topic"]) and not norm(raw[rid]["mid_evidence"]))
    print(f"items with empty participant context (topic+reasons both blank): {empty_ctx}")


if __name__ == "__main__":
    main()
