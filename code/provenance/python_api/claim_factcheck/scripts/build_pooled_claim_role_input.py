#!/usr/bin/env python3
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


def truth_bin_expr(col: str) -> pl.Expr:
    num = pl.col(col).cast(pl.Float64, strict=False)
    return (
        pl.when(num < 33)
        .then(pl.lit("False"))
        .when(num < 67)
        .then(pl.lit("Mostly False"))
        .when(num.is_not_null())
        .then(pl.lit("True"))
        .otherwise(pl.lit(None))
    )


def pool_model_name(name: str | None) -> str | None:
    if name is None:
        return None
    if name in {"google/gemini-3-pro-preview", "google/gemini-3.1-pro-preview"}:
        return "Gemini"
    if name in {"anthropic/claude-opus-4-6", "claude-4-opus"}:
        return "Claude"
    if name in {"x-ai/grok-4", "grok-4"}:
        return "Grok"
    if name in {"openai/gpt-5.2", "gpt-5.2"}:
        return "GPT-5.2"
    if name in {"openai/gpt-4o", "gpt-4o-2024-08-06", "gpt-4o"}:
        return "GPT-4o"
    return name


def load_master(path: str) -> pl.DataFrame:
    master = pl.read_csv(path, infer_schema_length=10000)
    required = [
        "conversation_id",
        "study_source",
        "direction",
        "model_pooled",
        "aligned_belief_change",
        "belief_rating_pre_4",
        "conRestatement",
        "conSummary",
        "focal_claim_veracity",
        "focal_claim_checkability",
        "ready_for_accuracy_analysis",
    ]
    missing = [col for col in required if col not in master.columns]
    if missing:
        raise SystemExit(f"Master dataset missing columns: {', '.join(missing)}")

    return (
        master.with_columns(
            pl.coalesce(
                [
                    pl.col("ready_for_accuracy_analysis").cast(pl.Boolean, strict=False),
                    pl.col("ready_for_accuracy_analysis")
                    .cast(pl.Utf8)
                    .str.to_lowercase()
                    .is_in(["true", "t", "1", "yes"]),
                ]
            ).alias("ready_for_accuracy_analysis"),
            pl.col("aligned_belief_change").cast(pl.Float64, strict=False).alias("aligned_belief_change"),
            pl.col("belief_rating_pre_4").cast(pl.Float64, strict=False).alias("belief_rating_pre_4"),
            pl.col("focal_claim_veracity").cast(pl.Float64, strict=False).alias("focal_claim_veracity"),
            pl.col("focal_claim_checkability").cast(pl.Float64, strict=False).alias("focal_claim_checkability"),
            pl.col("model_pooled")
            .map_elements(pool_model_name, return_dtype=pl.String)
            .alias("model_pooled"),
            truth_bin_expr("focal_claim_veracity").alias("truth_bin"),
        )
        .filter(pl.col("ready_for_accuracy_analysis"))
        .select(
            [
                "conversation_id",
                "study_source",
                "direction",
                "model_pooled",
                "aligned_belief_change",
                "belief_rating_pre_4",
                "conRestatement",
                "conSummary",
                "focal_claim_veracity",
                "focal_claim_checkability",
                "truth_bin",
            ]
        )
        .unique(subset=["conversation_id"], keep="last")
    )


def load_april04(april_path: str, master: pl.DataFrame) -> pl.DataFrame:
    df = pl.read_csv(april_path, infer_schema_length=10000)
    required = [
        "conversation_id",
        "row_key",
        "message_id",
        "claim_index",
        "claim_text",
        "content",
        "turn_index",
        "message_timestamp",
        "direction",
        "model_name",
        "veracity_score",
    ]
    missing = [col for col in required if col not in df.columns]
    if missing:
        raise SystemExit(f"April04 claim file missing columns: {', '.join(missing)}")

    return (
        df.with_columns(
            pl.lit("Study4").alias("study_source"),
            pl.col("model_name")
            .map_elements(pool_model_name, return_dtype=pl.String)
            .alias("model_pooled"),
            pl.col("claim_index").cast(pl.Int64, strict=False).alias("claim_index_num"),
            pl.col("turn_index").cast(pl.Int64, strict=False).alias("turn_order"),
            pl.col("message_timestamp").cast(pl.Float64, strict=False).alias("message_time"),
            pl.col("veracity_score").cast(pl.Float64, strict=False).alias("veracity_score"),
        )
        .join(master, on=["conversation_id", "study_source"], how="inner")
        .select(
            [
                "study_source",
                "conversation_id",
                "message_id",
                "row_key",
                "turn_order",
                "message_time",
                "claim_index_num",
                "claim_text",
                "content",
                "direction",
                "model_name",
                "model_pooled",
                "claim_category_v2",
                "materiality_label",
                "factcheck_priority",
                "veracity_score",
                "aligned_belief_change",
                "belief_rating_pre_4",
                "conRestatement",
                "conSummary",
                "focal_claim_veracity",
                "focal_claim_checkability",
                "truth_bin",
            ]
        )
    )


def load_study2(study2_path: str, master: pl.DataFrame) -> pl.DataFrame:
    df = pl.read_csv(study2_path, infer_schema_length=10000)
    required = [
        "conversation_id",
        "row_key",
        "message_id",
        "claim_index",
        "claim_text",
        "content",
        "message_turn_id",
        "created_at",
        "direction",
        "model_name",
        "veracity_score",
        "veracity_request_status",
    ]
    missing = [col for col in required if col not in df.columns]
    if missing:
        raise SystemExit(f"Study2 claim file missing columns: {', '.join(missing)}")

    return (
        df.filter(
            pl.col("veracity_request_status") == "success",
            pl.col("veracity_score").is_not_null(),
        )
        .with_columns(
            pl.lit("Study2").alias("study_source"),
            pl.col("model_name")
            .map_elements(pool_model_name, return_dtype=pl.String)
            .alias("model_pooled"),
            pl.col("claim_index").cast(pl.Int64, strict=False).alias("claim_index_num"),
            pl.col("message_turn_id").cast(pl.Int64, strict=False).alias("turn_order"),
            pl.col("created_at").cast(pl.Float64, strict=False).alias("message_time"),
            pl.col("veracity_score").cast(pl.Float64, strict=False).alias("veracity_score"),
        )
        .join(master, on=["conversation_id", "study_source"], how="inner")
        .select(
            [
                "study_source",
                "conversation_id",
                "message_id",
                "row_key",
                "turn_order",
                "message_time",
                "claim_index_num",
                "claim_text",
                "content",
                "direction",
                "model_name",
                "model_pooled",
                "claim_category_v2",
                "materiality_label",
                "factcheck_priority",
                "veracity_score",
                "aligned_belief_change",
                "belief_rating_pre_4",
                "conRestatement",
                "conSummary",
                "focal_claim_veracity",
                "focal_claim_checkability",
                "truth_bin",
            ]
        )
    )


def main() -> None:
    parser = argparse.ArgumentParser(
        description="Build the pooled eligible-and-factchecked claim-level input for focal-role classification."
    )
    parser.add_argument(
        "--master-input",
        # TRANSIENT INTERMEDIATE (not shipped): pooled S2/S4 master with ready_for_accuracy_analysis
        # flags, produced upstream by the factcheck pipeline (paid API). Kept in the working dir.
        # NOTE: this is the "veryfalse40" master intermediate, distinct from the SHIPPED
        # claim_accuracy_pooled_s2_s4.csv final.
        default=str(WORK_DIR / "pooled_s2_s4_veryfalse40_dataset.csv"),
    )
    parser.add_argument(
        "--april04-input",
        # TRANSIENT INTERMEDIATE (not shipped): Study 4 fact-checked claim rows.
        default=str(WORK_DIR / "april04_fact_checked_claims_gemini_flash_include_all.csv"),
    )
    parser.add_argument(
        "--study2-input",
        # TRANSIENT INTERMEDIATE (not shipped): Study 2 fact-check queue rows.
        default=str(WORK_DIR / "study2_standard_factcheck_queue_gemini_flash_include_all.csv"),
    )
    parser.add_argument(
        "--output",
        # TRANSIENT INTERMEDIATE (not shipped): pooled claim-role classification input.
        default=str(WORK_DIR / "pooled_claim_role_input.csv"),
    )
    args = parser.parse_args()

    master = load_master(args.master_input)
    study4 = load_april04(args.april04_input, master)
    study2 = load_study2(args.study2_input, master)

    pooled = (
        pl.concat([study2, study4], how="diagonal_relaxed")
        .sort(["study_source", "conversation_id", "turn_order", "message_time", "claim_index_num", "row_key"])
    )

    output_path = Path(args.output)
    output_path.parent.mkdir(parents=True, exist_ok=True)
    pooled.write_csv(output_path)

    counts = (
        pooled.group_by(["study_source", "model_pooled", "direction"])
        .agg(pl.len().alias("n_claims"), pl.col("conversation_id").n_unique().alias("n_conversations"))
        .sort(["study_source", "model_pooled", "direction"])
    )
    print(f"Saved: {output_path}")
    print(f"Rows: {pooled.height:,}")
    print(counts)


if __name__ == "__main__":
    main()
