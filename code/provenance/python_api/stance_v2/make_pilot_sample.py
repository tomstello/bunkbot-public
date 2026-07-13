"""Draw the 150-item stratified pilot sample for stance-v2 prompt evaluation.

Strata target the failure modes of the v1 scorer: exact-50 lumps, 0/100
saturation, question posts, and meta/survey posts. Seeded for reproducibility.

Writes pilot_item_ids.txt (one item_id per line).
"""

from __future__ import annotations

import json
import random
import re
from pathlib import Path

HERE = Path(__file__).resolve().parent


def _repo_root(start: Path) -> Path:
    p = start.resolve()
    for cand in [p, *p.parents]:
        if (cand / "data").is_dir() and (cand / "code").is_dir():
            return cand
    raise RuntimeError("repo root (dir containing data/ and code/) not found")


REPO_ROOT = _repo_root(Path(__file__))
# Transient stance inputs live in the working dir.
WORK_DIR = REPO_ROOT / "output" / "provenance_work" / "stance_v2"
V2_DIR = WORK_DIR
# Kept script-dir-relative so the scorer/consolidator (--items) and this writer
# agree on its location.
OUT = HERE / "pilot_item_ids.txt"

META_RX = re.compile(r"\b(this survey|this study|the survey|this ai|the ai|chatbot|experiment)\b",
                     re.IGNORECASE)

STRATA = [
    ("v1_50", 40, lambda it: float(it["v1_score"]) == 50),
    ("v1_extreme", 20, lambda it: float(it["v1_score"]) in (0.0, 100.0)),
    ("question", 30, lambda it: "?" in it["post_text"]),
    ("meta", 15, lambda it: META_RX.search(it["post_text"]) is not None),
    ("random", 45, lambda it: True),
]


def main() -> None:
    rng = random.Random(20260611)
    items = [json.loads(line) for line in open(V2_DIR / "stance_v2_inputs.jsonl")]
    chosen: list[str] = []
    chosen_set: set[str] = set()
    for name, n, pred in STRATA:
        pool = [it for it in items if pred(it) and it["item_id"] not in chosen_set]
        # balance pre/post within each stratum
        pre = [it for it in pool if it["timepoint"] == "pre"]
        post = [it for it in pool if it["timepoint"] == "post"]
        take = rng.sample(pre, min(n // 2, len(pre))) + rng.sample(post, min(n - n // 2, len(post)))
        for it in take:
            chosen.append(it["item_id"])
            chosen_set.add(it["item_id"])
        print(f"{name}: +{len(take)} (pool {len(pool)})")
    OUT.write_text("\n".join(chosen) + "\n")
    print(f"total {len(chosen)} -> {OUT}")


if __name__ == "__main__":
    main()
