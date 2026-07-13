from __future__ import annotations

import argparse
from pathlib import Path

import polars as pl

from utils import load_jsonl


def main():
    parser = argparse.ArgumentParser(
        description="Materialize claim-role labels from one or more checkpoint JSONLs."
    )
    parser.add_argument("--input", required=True)
    parser.add_argument(
        "--jsonl",
        nargs="+",
        required=True,
        help="One or more claim-role JSONL checkpoints to merge.",
    )
    parser.add_argument("--output", required=True)
    args = parser.parse_args()

    input_df = pl.read_csv(args.input, infer_schema_length=10000)

    labeled_rows = []
    for jsonl_arg in args.jsonl:
        for rec in load_jsonl(Path(jsonl_arg)):
            if rec.get("request_status") != "success":
                continue
            labeled_rows.append(rec)

    if labeled_rows:
        label_df = pl.DataFrame(labeled_rows, infer_schema_length=10000)
    else:
        label_df = pl.DataFrame(
            {
                "row_key": pl.Series([], dtype=pl.Utf8),
                "stance_to_focal": pl.Series([], dtype=pl.Utf8),
                "directness_to_focal": pl.Series([], dtype=pl.Utf8),
                "confidence": pl.Series([], dtype=pl.Float64),
                "claim_role_model": pl.Series([], dtype=pl.Utf8),
            }
        )

    merged = input_df.join(
        label_df.select(
            [
                "row_key",
                "stance_to_focal",
                "directness_to_focal",
                "confidence",
                "claim_role_model",
            ]
        ),
        on="row_key",
        how="left",
    )

    output_path = Path(args.output)
    output_path.parent.mkdir(parents=True, exist_ok=True)
    merged.write_csv(output_path)
    print(f"Saved: {output_path}")
    print(f"Labeled rows: {label_df.height:,}")


if __name__ == "__main__":
    main()
