from __future__ import annotations

import argparse
from pathlib import Path

import polars as pl

from classify_factcheck_eligibility import row_key
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


def main():
    parser = argparse.ArgumentParser(
        description="Materialize the eligibility CSV from the checkpoint JSONL."
    )
    # TRANSIENT INTERMEDIATE (not shipped): extracted claim rows in the working dir.
    parser.add_argument("--input", default=str(WORK_DIR / "april04_extracted_claim_rows.csv"))
    parser.add_argument(
        "--jsonl",
        nargs="+",
        required=True,
        help="One or more eligibility JSONL checkpoints to merge.",
    )
    # TRANSIENT INTERMEDIATE (not shipped): merged eligibility labels in the working dir.
    parser.add_argument("--output", default=str(WORK_DIR / "factcheck_eligibility.csv"))
    args = parser.parse_args()

    input_path = Path(args.input)
    output_path = Path(args.output)
    output_path.parent.mkdir(parents=True, exist_ok=True)

    input_df = pl.read_csv(input_path, infer_schema_length=10000)

    labeled_rows = []
    for jsonl_arg in args.jsonl:
        jsonl_path = Path(jsonl_arg)
        for rec in load_jsonl(jsonl_path):
            if rec.get("request_status") != "success":
                continue
            labeled_rows.append(rec)

    if labeled_rows:
        label_df = pl.DataFrame(labeled_rows, infer_schema_length=10000)
    else:
        label_df = pl.DataFrame(
            {
                "row_key": pl.Series([], dtype=pl.Utf8),
                "checkability_label": pl.Series([], dtype=pl.Utf8),
                "materiality_label": pl.Series([], dtype=pl.Utf8),
                "factcheck_priority": pl.Series([], dtype=pl.Utf8),
                "confidence": pl.Series([], dtype=pl.Float64),
                "eligibility_model": pl.Series([], dtype=pl.Utf8),
            }
        )

    merged = input_df.with_columns(
        pl.struct(input_df.columns).map_elements(row_key, return_dtype=pl.String).alias("row_key")
    ).join(
        label_df.select(
            [
                "row_key",
                "checkability_label",
                "materiality_label",
                "factcheck_priority",
                "confidence",
                "eligibility_model",
            ]
        ),
        on="row_key",
        how="left",
    )

    merged = merged.with_columns(
        pl.when(
            (pl.col("factcheck_priority") == "include")
            & (pl.col("materiality_label") == "core")
        )
        .then(True)
        .otherwise(False)
        .alias("factcheck_queue_default")
    )
    merged.write_csv(output_path)
    print(f"Saved: {output_path}")
    print(f"Labeled rows: {label_df.height:,}")


if __name__ == "__main__":
    main()
