#!/usr/bin/env python3
from __future__ import annotations

import argparse
import json
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
WORK_DIR.mkdir(parents=True, exist_ok=True)
# Shipped participant-level input.
SHARING_STANCE_DIR = REPO_ROOT / "data" / "api_cached" / "sharing_and_stance"


def pool_model_name(name: str | None) -> str | None:
    if name is None:
        return None
    if name in {"google/gemini-3-pro-preview", "google/gemini-3.1-pro-preview"}:
        return "Gemini"
    if name == "anthropic/claude-opus-4-6":
        return "Claude"
    if name == "x-ai/grok-4":
        return "Grok"
    if name == "openai/gpt-5.2":
        return "GPT-5.2"
    return name


def load_jsonl_dedup(path: Path, key_col: str) -> pl.DataFrame:
    records: list[dict] = []
    if not path.exists():
        return pl.DataFrame()
    with path.open("r", encoding="utf-8") as handle:
        for line in handle:
            line = line.strip()
            if not line:
                continue
            try:
                records.append(json.loads(line))
            except json.JSONDecodeError:
                continue
    if not records:
        return pl.DataFrame()
    df = pl.DataFrame(records, infer_schema_length=10000)
    if key_col in df.columns:
        df = df.unique(subset=[key_col], keep="last")
    return df


def pivot_share_table(df: pl.DataFrame, prefix: str) -> pl.DataFrame:
    cats = [
        "specific_empirical",
        "general_underspecified",
        "inferential_logical",
        "meta_epistemic",
        "low_information",
        "moral_political_evaluation",
    ]
    if df.is_empty():
        cols = ["conversation_id"] + [f"{prefix}_share_{cat}" for cat in cats] + [f"{prefix}_claims_total"]
        return pl.DataFrame(schema={c: pl.Float64 for c in cols})

    counts = (
        df.group_by(["conversation_id", "claim_category_v2"])
        .agg(pl.len().alias("n"))
        .sort(["conversation_id", "claim_category_v2"])
    )
    totals = counts.group_by("conversation_id").agg(pl.col("n").sum().alias(f"{prefix}_claims_total"))
    shares = counts.join(totals, on="conversation_id", how="left").with_columns(
        (pl.col("n") / pl.col(f"{prefix}_claims_total")).alias("share")
    )
    wide = shares.pivot(
        values="share",
        index="conversation_id",
        columns="claim_category_v2",
        aggregate_function="first",
    )
    for cat in cats:
        if cat not in wide.columns:
            wide = wide.with_columns(pl.lit(0.0).alias(cat))
    wide = wide.select(["conversation_id"] + cats).rename({cat: f"{prefix}_share_{cat}" for cat in cats})
    return totals.join(wide, on="conversation_id", how="left").fill_null(0.0)


def main() -> None:
    parser = argparse.ArgumentParser(
        description="Build a conversation-complete analysis dataset once fact-checking is done."
    )
    parser.add_argument(
        "--participant-input",
        # SHIPPED: study4_sharing_analysis_merged.csv.gz -> data/api_cached/sharing_and_stance/
        default=str(SHARING_STANCE_DIR / "study4_sharing_analysis_merged.csv.gz"),
    )
    parser.add_argument(
        "--taxonomy-input",
        # TRANSIENT INTERMEDIATE (not shipped): claim taxonomy classifications (paid API).
        default=str(WORK_DIR / "april04_claim_classifications_v2_gemini_flash.csv"),
    )
    parser.add_argument(
        "--queue-input",
        # TRANSIENT INTERMEDIATE (not shipped): factcheck queue (eligible claim rows).
        default=str(WORK_DIR / "april04_factcheck_queue_gemini_flash_include_all.csv"),
    )
    parser.add_argument(
        "--factcheck-jsonl",
        # TRANSIENT INTERMEDIATE (not shipped): per-claim fact-check veracity JSONL.
        default=str(WORK_DIR / "april04_fact_checked_claims_gemini_flash_include_all.jsonl"),
    )
    parser.add_argument(
        "--focal-veracity-input",
        default="",
        help="Optional CSV with one row per conversation containing focal_claim_veracity outputs.",
    )
    parser.add_argument(
        "--output",
        # TRANSIENT INTERMEDIATE (not shipped): conversation-complete accuracy dataset.
        default=str(WORK_DIR / "april04_complete_conversation_accuracy_dataset.csv"),
    )
    parser.add_argument(
        "--status-output",
        # TRANSIENT INTERMEDIATE (not shipped): per-conversation accuracy completeness status.
        default=str(WORK_DIR / "april04_complete_conversation_accuracy_status.csv"),
    )
    args = parser.parse_args()

    participant_df = pl.read_csv(args.participant_input, infer_schema_length=10000).select(
        [
            "ResponseId",
            "modelName",
            "modelName_raw",
            "direction",
            "full_condition",
            "valid_core",
            "conSummary",
            "conRestatement",
            "aligned_belief_change",
            "aligned_stance_change",
            "aligned_new_minus_old_weighted",
            "belief_rating_pre_4",
            "belief_rating_post_4",
            "share_pre_4",
            "share_post_4",
            "share_original_post_now_4",
        ]
    ).rename({"ResponseId": "conversation_id"})

    participant_df = participant_df.with_columns(
        pl.col("modelName").map_elements(pool_model_name, return_dtype=pl.String).alias("model_pooled"),
        pl.when(pl.col("valid_core").cast(pl.Utf8) == "True").then(True).otherwise(False).alias("valid_core"),
    )

    taxonomy_df = pl.read_csv(args.taxonomy_input, infer_schema_length=10000).with_columns(
        pl.col("model_name").map_elements(pool_model_name, return_dtype=pl.String).alias("model_pooled")
    )
    all_comp = pivot_share_table(taxonomy_df, "all")

    queue_df = pl.read_csv(args.queue_input, infer_schema_length=10000).with_columns(
        pl.col("model_name").map_elements(pool_model_name, return_dtype=pl.String).alias("model_pooled")
    )
    queue_status = queue_df.group_by("conversation_id").agg(
        pl.len().alias("eligible_queue_n"),
        pl.col("message_id").n_unique().alias("eligible_message_n"),
    )
    queue_comp = pivot_share_table(queue_df, "queue")

    fact_df = load_jsonl_dedup(Path(args.factcheck_jsonl), "row_key")
    if fact_df.is_empty():
        fact_status = pl.DataFrame(schema={
            "conversation_id": pl.String,
            "factcheck_scored_n": pl.Int64,
            "factcheck_attempted_n": pl.Int64,
            "mean_veracity": pl.Float64,
            "median_veracity": pl.Float64,
            "pct_false_claims": pl.Float64,
            "pct_very_low_claims": pl.Float64,
            "pct_true_claims": pl.Float64,
            "true_claims": pl.Int64,
            "false_claims": pl.Int64,
            "other_claims": pl.Int64,
        })
    else:
        fact_df = fact_df.with_columns(
            pl.col("veracity_score").cast(pl.Float64, strict=False),
        )
        attempted = fact_df.group_by("conversation_id").agg(pl.len().alias("factcheck_attempted_n"))
        scored = fact_df.filter(pl.col("request_status") == "success", pl.col("veracity_score").is_not_null())
        fact_status = (
            scored.group_by("conversation_id")
            .agg(
                pl.len().alias("factcheck_scored_n"),
                pl.col("veracity_score").mean().alias("mean_veracity"),
                pl.col("veracity_score").median().alias("median_veracity"),
                (pl.col("veracity_score") < 50).mean().alias("pct_false_claims"),
                (pl.col("veracity_score") < 33).mean().alias("pct_very_low_claims"),
                (pl.col("veracity_score") >= 67).mean().alias("pct_true_claims"),
                (pl.col("veracity_score") >= 67).sum().alias("true_claims"),
                (pl.col("veracity_score") < 33).sum().alias("false_claims"),
                ((pl.col("veracity_score") >= 33) & (pl.col("veracity_score") < 67)).sum().alias("other_claims"),
            )
            .join(attempted, on="conversation_id", how="left")
        )

    df = (
        participant_df.join(queue_status, on="conversation_id", how="left")
        .join(queue_comp, on="conversation_id", how="left")
        .join(all_comp, on="conversation_id", how="left")
        .join(fact_status, on="conversation_id", how="left")
    )

    if args.focal_veracity_input:
        focal_path = Path(args.focal_veracity_input)
        if focal_path.exists():
            focal_df = (
                pl.read_csv(focal_path, infer_schema_length=10000)
                .unique(subset=["conversation_id"], keep="last")
                .sort("conversation_id")
            )
            if "conversation_id" in focal_df.columns:
                df = df.join(focal_df, on="conversation_id", how="left")

    fill_zero_cols = [
        "eligible_queue_n",
        "eligible_message_n",
        "queue_claims_total",
        "all_claims_total",
        "factcheck_attempted_n",
        "factcheck_scored_n",
        "true_claims",
        "false_claims",
        "other_claims",
    ]
    fill_zero_cols.extend([col for col in df.columns if col.startswith("queue_share_")])
    fill_zero_cols.extend([col for col in df.columns if col.startswith("all_share_")])
    existing_fill_cols = [col for col in fill_zero_cols if col in df.columns]
    if existing_fill_cols:
        df = df.with_columns([pl.col(col).fill_null(0) for col in existing_fill_cols])

    df = df.with_columns(
        (pl.col("eligible_queue_n") > 0).alias("has_eligible_queue"),
        (pl.col("factcheck_scored_n") == pl.col("eligible_queue_n")).alias("factcheck_queue_complete"),
        (pl.col("factcheck_scored_n") / pl.when(pl.col("eligible_queue_n") > 0).then(pl.col("eligible_queue_n")).otherwise(None)).alias("factcheck_queue_coverage"),
        ((pl.col("eligible_queue_n") > 0) & (pl.col("factcheck_scored_n") == pl.col("eligible_queue_n"))).alias("ready_for_accuracy_analysis"),
        (pl.col("mean_veracity") / 10).alias("mean_veracity_10"),
        (pl.col("true_claims") / 10).alias("true_claims_10"),
        (pl.col("false_claims") / 10).alias("false_claims_10"),
        (pl.col("other_claims") / 10).alias("other_claims_10"),
        ((1 + pl.col("eligible_queue_n")).log()).alias("log1p_eligible_queue_n"),
        ((1 + pl.col("all_claims_total")).log()).alias("log1p_all_claims_total"),
    )

    output_path = Path(args.output)
    output_path.parent.mkdir(parents=True, exist_ok=True)
    df.write_csv(output_path)

    status_summary = df.select(
        [
            "conversation_id",
            "model_pooled",
            "direction",
            "valid_core",
            "eligible_queue_n",
            "factcheck_attempted_n",
            "factcheck_scored_n",
            "factcheck_queue_complete",
            "factcheck_queue_coverage",
            "ready_for_accuracy_analysis",
        ]
    )
    Path(args.status_output).parent.mkdir(parents=True, exist_ok=True)
    status_summary.write_csv(args.status_output)

    print(f"Saved: {output_path}")
    print(f"Saved: {args.status_output}")
    print(f"Rows: {df.height:,}")
    print(
        "Ready-for-analysis conversations: "
        f"{df.filter(pl.col('ready_for_accuracy_analysis')).height:,}"
    )


if __name__ == "__main__":
    main()
