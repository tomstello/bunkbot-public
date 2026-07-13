"""Write analysis_item_ids.txt: substantive items whose participant is in the
screened analysis frame (Studies 1-4). The full scoring run is restricted to these
so we only pay for participants who actually appear in reported analyses.

Prereq: run export_analysis_frame.R (writes analysis_frame.csv) and
build_explanation_inputs.py (writes explanation_inputs.jsonl) first.

Usage: python3 make_analysis_allowlist.py
"""

from __future__ import annotations

import csv
import json
from collections import Counter
from pathlib import Path

csv.field_size_limit(10**8)

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


def main() -> None:
    frame_ids = set()
    with open(DIR / "analysis_frame.csv", encoding="utf-8") as f:
        for r in csv.DictReader(f):
            frame_ids.add(r["ResponseId"])

    keep, by_study = [], Counter()
    with open(DIR / "explanation_inputs.jsonl", encoding="utf-8") as f:
        for line in f:
            it = json.loads(line)
            if it["ResponseId"] in frame_ids:
                keep.append(it["item_id"])
                by_study[it["study"]] += 1

    (DIR / "analysis_item_ids.txt").write_text("\n".join(keep) + "\n")
    print(f"analysis-frame participants: {len(frame_ids)}")
    print(f"scored items in analysis sample: {len(keep)}  -> analysis_item_ids.txt")
    print(f"by study: {dict(by_study)}")
    print(f"estimated panel calls (x5 raters): {len(keep) * 5}")


if __name__ == "__main__":
    main()
