#!/usr/bin/env python3
from __future__ import annotations

import argparse
from pathlib import Path

import pandas as pd


def _repo_root(start: Path) -> Path:
    p = start.resolve()
    for cand in [p, *p.parents]:
        if (cand / "data").is_dir() and (cand / "code").is_dir():
            return cand
    raise RuntimeError("repo root (dir containing data/ and code/) not found")


REPO_ROOT = _repo_root(Path(__file__))
# Transient intermediates for this pipeline (not shipped). Keep old basenames here.
WORK_DIR = REPO_ROOT / "output" / "provenance_work" / "claim_factcheck"
# Shipped final outputs.
CLAIM_DATASETS_DIR = REPO_ROOT / "data" / "api_cached" / "claim_datasets"
COMPLIANCE_APE_DIR = REPO_ROOT / "data" / "api_cached" / "compliance_ape"


def rename_prefixed(df: pd.DataFrame, prefix: str, skip: set[str]) -> pd.DataFrame:
    rename_map = {col: f"{prefix}{col}" for col in df.columns if col not in skip}
    return df.rename(columns=rename_map)


def main() -> None:
    parser = argparse.ArgumentParser(description="Build a clean April 4 master analysis dataset.")
    parser.add_argument(
        "--overall",
        # TRANSIENT INTERMEDITE (not shipped): conversation-complete accuracy dataset with focal
        # item, produced upstream by the factcheck pipeline (paid API). Kept in the working dir.
        default=str(WORK_DIR / "april04_complete_conversation_accuracy_dataset_with_focal_item_final.csv"),
    )
    parser.add_argument(
        "--portfolio",
        # SHIPPED: claim_role_portfolio_all_studies.csv -> data/api_cached/claim_datasets/
        # (written by build_claim_role_portfolio_dataset.py).
        default=str(CLAIM_DATASETS_DIR / "claim_role_portfolio_all_studies.csv"),
    )
    parser.add_argument(
        "--ape",
        # SHIPPED: study4_compliance_ape_refusal.csv -> data/api_cached/compliance_ape/
        default=str(COMPLIANCE_APE_DIR / "study4_compliance_ape_refusal.csv"),
    )
    parser.add_argument(
        "--claims",
        # TRANSIENT INTERMEDIATE (not shipped): per-claim annotated rows from the role pipeline.
        default=str(WORK_DIR / "pooled_claim_role_claim_level_annotated.csv"),
    )
    parser.add_argument(
        "--output-conversation",
        # SHIPPED: study4_master_analysis_dataset.csv -> data/api_cached/claim_datasets/
        default=str(CLAIM_DATASETS_DIR / "study4_master_analysis_dataset.csv"),
    )
    parser.add_argument(
        "--output-claim-level",
        # TRANSIENT INTERMEDIATE (not shipped): claim-level master companion.
        default=str(WORK_DIR / "april04_master_claim_level_dataset.csv"),
    )
    parser.add_argument(
        "--output-dictionary",
        # TRANSIENT INTERMEDIATE (not shipped): data dictionary for the master datasets.
        default=str(WORK_DIR / "april04_master_analysis_data_dictionary.csv"),
    )
    args = parser.parse_args()

    overall = pd.read_csv(args.overall)
    portfolio = pd.read_csv(args.portfolio)
    ape = pd.read_csv(args.ape)
    claims = pd.read_csv(args.claims)

    # Base conversation-level file: all April 4 rows with existing outcomes and overall accuracy metrics.
    overall["conversation_id"] = overall["conversation_id"].astype(str)
    overall["direction"] = overall["direction"].astype(str)
    overall["model_pooled"] = overall["model_pooled"].astype(str)

    # Keep base file relatively readable by dropping obvious duplicated merge artifacts.
    drop_from_overall = [
        "valid_core_right",
        "modelName_right",
        "direction_right",
        "conSummary_right",
        "conRestatement_right",
    ]
    overall = overall.drop(columns=[c for c in drop_from_overall if c in overall.columns])

    portfolio = portfolio.loc[portfolio["study_source"] == "Study4"].copy()
    portfolio["conversation_id"] = portfolio["conversation_id"].astype(str)
    portfolio["direction"] = portfolio["direction"].astype(str)
    portfolio["model_pooled"] = portfolio["model_pooled"].astype(str)
    portfolio = rename_prefixed(
        portfolio,
        prefix="role_",
        skip={"conversation_id", "direction", "model_pooled"},
    )

    ape["human_id"] = ape["human_id"].astype(str)
    ape["direction"] = ape["direction"].astype(str)
    ape["model_pooled"] = ape["model_pooled"].astype(str)
    ape = ape.rename(columns={"human_id": "conversation_id"})
    ape = rename_prefixed(
        ape,
        prefix="ape_",
        skip={"conversation_id", "direction", "model_pooled"},
    )

    conversation = overall.merge(
        portfolio,
        on=["conversation_id", "model_pooled", "direction"],
        how="left",
        validate="one_to_one",
    ).merge(
        ape,
        on=["conversation_id", "model_pooled", "direction"],
        how="left",
        validate="one_to_one",
    )

    conversation["has_role_portfolio"] = conversation["role_study_source"].notna()
    conversation["has_ape_compliance"] = conversation["ape_compliance_status"].notna()
    conversation["ape_is_compliant_attempt"] = conversation["ape_compliance_status"].eq("compliant_attempt")
    conversation["ape_is_refusal"] = conversation["ape_compliance_status"].eq("refusal")
    conversation["ape_is_off_direction_or_no_attempt"] = conversation["ape_compliance_status"].eq("off_direction_or_no_attempt")

    conv_out = Path(args.output_conversation)
    conv_out.parent.mkdir(parents=True, exist_ok=True)
    conversation.to_csv(conv_out, index=False)

    # Claim-level companion: Study4 rows plus attached APE conversation-level compliance signals.
    claims = claims.loc[claims["study_source"] == "Study4"].copy()
    claims["conversation_id"] = claims["conversation_id"].astype(str)
    claims["direction"] = claims["direction"].astype(str)
    claims["model_pooled"] = claims["model_pooled"].astype(str)

    claim_ape_cols = [
        "conversation_id",
        "model_pooled",
        "direction",
        "ape_attempt_binary",
        "ape_refusal_binary",
        "ape_persuasion_score_5",
        "ape_specificity_score_5",
        "ape_compliance_status",
        "ape_refusal_parse_ok",
        "ape_attempt_parse_ok",
    ]
    claims = claims.merge(
        ape[claim_ape_cols],
        on=["conversation_id", "model_pooled", "direction"],
        how="left",
        validate="many_to_one",
    )

    claim_out = Path(args.output_claim_level)
    claims.to_csv(claim_out, index=False)

    # Simple data dictionary.
    dict_rows = []
    for source_name, df in [
        ("conversation_master", conversation),
        ("claim_level_master", claims),
    ]:
        for col in df.columns:
            dict_rows.append(
                {
                    "dataset": source_name,
                    "column": col,
                    "dtype": str(df[col].dtype),
                }
            )
    dictionary = pd.DataFrame(dict_rows)
    dictionary.to_csv(args.output_dictionary, index=False)

    print(f"Saved conversation master: {conv_out}")
    print(f"Rows: {len(conversation):,}")
    print(f"Saved claim-level master: {claim_out}")
    print(f"Rows: {len(claims):,}")
    print(f"Saved dictionary: {args.output_dictionary}")


if __name__ == "__main__":
    main()
