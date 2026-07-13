from __future__ import annotations

import argparse
from pathlib import Path

import polars as pl


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
        description="Build a filtered fact-check queue from eligibility-labeled claims."
    )
    parser.add_argument(
        "--input",
        # TRANSIENT INTERMEDIATE (not shipped): eligibility-labeled claims in the working dir.
        default=str(WORK_DIR / "factcheck_eligibility.csv"),
    )
    parser.add_argument(
        "--output",
        # TRANSIENT INTERMEDIATE (not shipped): filtered fact-check queue in the working dir.
        default=str(WORK_DIR / "factcheck_queue.csv"),
    )
    parser.add_argument(
        "--queue-policy",
        choices=["conservative", "include_all", "include_review"],
        default="conservative",
        help=(
            "conservative = only `include` claims labeled `core`; "
            "include_all = all `include` claims; "
            "include_review = `include` plus `review` claims."
        ),
    )
    parser.add_argument(
        "--min-confidence",
        type=float,
        default=0.0,
        help="Keep only rows with confidence at or above this value.",
    )
    parser.add_argument(
        "--max-claims-per-message",
        type=int,
        default=None,
        help="Optional cap on queued claims per message_id, ranked by confidence.",
    )
    parser.add_argument(
        "--exclude-categories",
        nargs="*",
        default=None,
        help="Optional claim_category_v2 values to exclude from the fact-check queue.",
    )
    args = parser.parse_args()

    input_path = Path(args.input)
    output_path = Path(args.output)
    output_path.parent.mkdir(parents=True, exist_ok=True)

    df = pl.read_csv(input_path, infer_schema_length=10000)
    if "factcheck_priority" not in df.columns:
        raise SystemExit("Input must contain `factcheck_priority`.")
    if "materiality_label" not in df.columns:
        raise SystemExit("Input must contain `materiality_label`.")

    if args.queue_policy == "conservative":
        queue = df.filter(
            (pl.col("factcheck_priority") == "include")
            & (pl.col("materiality_label") == "core")
        )
    elif args.queue_policy == "include_review":
        queue = df.filter(pl.col("factcheck_priority").is_in(["include", "review"]))
    else:
        queue = df.filter(pl.col("factcheck_priority") == "include")

    if args.min_confidence > 0:
        queue = queue.filter(pl.col("confidence").fill_null(-1) >= args.min_confidence)

    if args.max_claims_per_message is not None:
        if "message_id" not in queue.columns:
            raise SystemExit("Input must contain `message_id` for --max-claims-per-message.")
        queue = (
            queue.sort(["message_id", "confidence"], descending=[False, True])
            .with_columns(
                pl.col("message_id").cum_count().over("message_id").alias("_message_rank")
            )
            .filter(pl.col("_message_rank") <= args.max_claims_per_message)
            .drop("_message_rank")
        )

    if args.exclude_categories:
        if "claim_category_v2" not in queue.columns:
            raise SystemExit("Input must contain `claim_category_v2` for --exclude-categories.")
        queue = queue.filter(~pl.col("claim_category_v2").is_in(args.exclude_categories))

    queue.write_csv(output_path)

    counts = (
        df.with_columns(
            pl.concat_str(
                [
                    pl.when(pl.col("factcheck_priority").is_null())
                    .then(pl.lit("unlabeled"))
                    .otherwise(pl.col("factcheck_priority")),
                    pl.lit(" / "),
                    pl.when(pl.col("materiality_label").is_null())
                    .then(pl.lit("unlabeled"))
                    .otherwise(pl.col("materiality_label")),
                ]
            ).alias("queue_label")
        )
        .group_by("queue_label")
        .agg(pl.len().alias("n"))
        .sort("queue_label")
        .to_dicts()
    )

    print(f"Saved: {output_path}")
    print(f"Queue policy: {args.queue_policy}")
    print(f"Minimum confidence: {args.min_confidence}")
    if args.max_claims_per_message is not None:
        print(f"Max claims per message: {args.max_claims_per_message}")
    if args.exclude_categories:
        print("Excluded categories:", ", ".join(args.exclude_categories))
    print(f"Rows queued: {queue.height:,}")
    for row in counts:
        print(f"{row['queue_label']}: {row['n']:,}")


if __name__ == "__main__":
    main()
