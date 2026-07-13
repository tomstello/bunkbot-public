from __future__ import annotations

import argparse
from pathlib import Path

import polars as pl

from classify_claims_v2 import enrich, load_codebook, normalize_subtype
from utils import load_jsonl


def _repo_root(start: Path) -> Path:
    p = start.resolve()
    for cand in [p, *p.parents]:
        if (cand / "data").is_dir() and (cand / "code").is_dir():
            return cand
    raise RuntimeError("repo root (dir containing data/ and code/) not found")


REPO_ROOT = _repo_root(Path(__file__))
# Transient intermediates for this pipeline (not shipped). Keep old basenames here.
WORK_DIR = REPO_ROOT / "output" / "provenance_work" / "claim_factcheck"
# The codebook ships with the pipeline (script-dir-relative).
SCRIPT_DIR = Path(__file__).resolve().parent
PIPELINE_DIR = SCRIPT_DIR.parent


def main():
    parser = argparse.ArgumentParser(
        description="Materialize a merged claim-classification CSV from one or more JSONL checkpoints."
    )
    # TRANSIENT INTERMEDIATE (not shipped): extracted claim rows in the working dir.
    parser.add_argument("--input", default=str(WORK_DIR / "april04_extracted_claim_rows.csv"))
    # Shipped pipeline codebook (script-dir-relative).
    parser.add_argument("--codebook", default=str(PIPELINE_DIR / "codebooks" / "claim_category_v2_codebook.csv"))
    parser.add_argument(
        "--jsonl",
        nargs="+",
        required=True,
        help="One or more claim-classification JSONL files to merge.",
    )
    # TRANSIENT INTERMEDIATE (not shipped): merged classifications in the working dir.
    parser.add_argument("--output", default=str(WORK_DIR / "april04_claim_classifications_v2.csv"))
    args = parser.parse_args()

    input_df = pl.read_csv(args.input, infer_schema_length=10000)
    codebook, _ = load_codebook(Path(args.codebook))

    by_claim: dict[str, dict] = {}
    for jsonl_path in args.jsonl:
        for rec in load_jsonl(Path(jsonl_path)):
            if rec.get("request_status") != "success":
                continue
            claim_text = str(rec.get("claim_text", "")).strip()
            if not claim_text:
                continue
            subtype = normalize_subtype(rec.get("granular_subtype", ""), codebook)
            if not subtype:
                continue
            if claim_text in by_claim:
                continue
            by_claim[claim_text] = {
                "claim_text": claim_text,
                **enrich(
                    subtype,
                    rec.get("confidence", -1),
                    rec.get("classification_model", ""),
                    codebook,
                ),
            }

    if by_claim:
        class_df = pl.DataFrame(list(by_claim.values()), infer_schema_length=10000)
    else:
        class_df = pl.DataFrame(
            {
                "claim_text": pl.Series([], dtype=pl.Utf8),
                "granular_subtype": pl.Series([], dtype=pl.Utf8),
                "claim_category_v2": pl.Series([], dtype=pl.Utf8),
                "subtype_label": pl.Series([], dtype=pl.Utf8),
                "legacy_subtype_v1": pl.Series([], dtype=pl.Utf8),
                "legacy_claim_category": pl.Series([], dtype=pl.Utf8),
                "specificity_flag": pl.Series([], dtype=pl.Utf8),
                "claim_category_v2_confidence": pl.Series([], dtype=pl.Float64),
                "claim_category_v2_model": pl.Series([], dtype=pl.Utf8),
            }
        )

    merged = input_df.join(class_df, on="claim_text", how="left")
    output_path = Path(args.output)
    output_path.parent.mkdir(parents=True, exist_ok=True)
    merged.write_csv(output_path)
    print(f"Saved: {output_path}")
    print(f"Unique classified claims: {len(by_claim):,}")


if __name__ == "__main__":
    main()
