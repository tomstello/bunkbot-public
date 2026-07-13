"""Write pilot_item_ids.txt: a stratified pilot for prompt/taxonomy validation.

Stratifies by study x field x response-length tertile so the pilot exercises short,
medium, and long responses in every study and both question boxes. Deterministic
(fixed seed). Draws from the analysis-sample allowlist when present, else all inputs.

Usage: python3 make_pilot_sample.py [--n 120]
"""

from __future__ import annotations

import argparse
import json
import random
from collections import defaultdict
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


def main() -> None:
    ap = argparse.ArgumentParser()
    ap.add_argument("--n", type=int, default=120, help="approx total pilot size")
    ap.add_argument("--seed", type=int, default=20260622)
    args = ap.parse_args()

    items = [json.loads(l) for l in open(DIR / "explanation_inputs.jsonl")]
    allow = DIR / "analysis_item_ids.txt"
    if allow.exists():
        keep = {l.strip() for l in open(allow) if l.strip()}
        items = [it for it in items if it["item_id"] in keep]

    # length tertile cutpoints
    lens = sorted(len(it["response_text"]) for it in items)
    t1, t2 = lens[len(lens) // 3], lens[2 * len(lens) // 3]

    def bucket(it):
        n = len(it["response_text"])
        lb = "short" if n <= t1 else ("med" if n <= t2 else "long")
        return (it["study"], it["field"], lb)

    strata = defaultdict(list)
    for it in items:
        strata[bucket(it)].append(it["item_id"])

    rng = random.Random(args.seed)
    per = max(1, args.n // len(strata))
    picked = []
    for key in sorted(strata):
        ids = strata[key]
        rng.shuffle(ids)
        picked.extend(ids[:per])
    picked = sorted(set(picked))

    (DIR / "pilot_item_ids.txt").write_text("\n".join(picked) + "\n")
    print(f"{len(strata)} strata (study x field x length), ~{per}/stratum")
    print(f"wrote {len(picked)} pilot item ids -> pilot_item_ids.txt")


if __name__ == "__main__":
    main()
