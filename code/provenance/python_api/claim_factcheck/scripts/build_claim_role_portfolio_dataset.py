#!/usr/bin/env python3
from __future__ import annotations

import argparse
from pathlib import Path

import numpy as np
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


def truth_bin_from_score(x):
    if pd.isna(x):
        return np.nan
    if x < 33:
        return "False"
    if x < 67:
        return "Mostly False"
    return "True"


def subset_mean(df: pd.DataFrame, mask: pd.Series, col: str = "veracity_score"):
    vals = df.loc[mask, col].dropna()
    return vals.mean() if len(vals) else np.nan


def subset_pct(df: pd.DataFrame, mask: pd.Series, predicate):
    vals = df.loc[mask, "veracity_score"].dropna()
    if len(vals) == 0:
        return np.nan
    return predicate(vals).mean()


def subset_min(df: pd.DataFrame, mask: pd.Series):
    vals = df.loc[mask, "veracity_score"].dropna()
    return vals.min() if len(vals) else np.nan


def subset_bottom3_mean(df: pd.DataFrame, mask: pd.Series):
    vals = df.loc[mask, "veracity_score"].dropna().sort_values()
    if len(vals) == 0:
        return np.nan
    return vals.head(3).mean()


def subset_count(df: pd.DataFrame, mask: pd.Series):
    return int(mask.fillna(False).sum())


def compute_features(group: pd.DataFrame) -> dict:
    group = group.sort_values(
        ["turn_order", "message_time", "claim_index_num", "row_key"],
        kind="mergesort",
    ).reset_index(drop=True)

    first = group.iloc[0]
    direct = group["directness_to_focal"] == "direct"
    indirect = group["directness_to_focal"] == "indirect"
    background = group["directness_to_focal"] == "background"
    supports = group["stance_to_focal"] == "supports"
    opposes = group["stance_to_focal"] == "opposes"
    neutral = group["stance_to_focal"] == "neutral"

    if first["direction"] == "bunk":
        aligned = supports
        counteraligned = opposes
    else:
        aligned = opposes
        counteraligned = supports

    aligned_direct = aligned & direct
    counteraligned_direct = counteraligned & direct
    aligned_indirect = aligned & indirect
    counteraligned_indirect = counteraligned & indirect
    direct_any = direct

    unique_turns = (
        group["turn_order"]
        .dropna()
        .sort_values(kind="mergesort")
        .drop_duplicates()
        .tolist()
    )
    first_turn = unique_turns[0] if unique_turns else np.nan
    first_two_turns = unique_turns[:2]
    first_turn_aligned_direct = aligned_direct & (group["turn_order"] == first_turn)
    first_two_turn_aligned_direct = aligned_direct & group["turn_order"].isin(first_two_turns)

    ordered_aligned_direct = group.loc[aligned_direct].copy()

    out = {
        "conversation_id": first["conversation_id"],
        "study_source": first["study_source"],
        "model_pooled": first["model_pooled"],
        "model_name": first["model_name"],
        "direction": first["direction"],
        "aligned_belief_change": first["aligned_belief_change"],
        "belief_rating_pre_4": first["belief_rating_pre_4"],
        "conRestatement": first["conRestatement"],
        "conSummary": first["conSummary"],
        "focal_claim_veracity": first["focal_claim_veracity"],
        "focal_claim_checkability": first["focal_claim_checkability"],
        "truth_bin": first["truth_bin"],
        "claim_role_total_n": int(len(group)),
        "direct_n": subset_count(group, direct),
        "indirect_n": subset_count(group, indirect),
        "background_n": subset_count(group, background),
        "aligned_direct_n": subset_count(group, aligned_direct),
        "counteraligned_direct_n": subset_count(group, counteraligned_direct),
        "aligned_indirect_n": subset_count(group, aligned_indirect),
        "counteraligned_indirect_n": subset_count(group, counteraligned_indirect),
        "direct_mean_veracity": subset_mean(group, direct_any),
        "direct_pct_very_false40": subset_pct(group, direct_any, lambda s: s < 40),
        "aligned_direct_mean_veracity": subset_mean(group, aligned_direct),
        "aligned_direct_pct_very_false40": subset_pct(group, aligned_direct, lambda s: s < 40),
        "counteraligned_direct_mean_veracity": subset_mean(group, counteraligned_direct),
        "counteraligned_direct_pct_very_false40": subset_pct(group, counteraligned_direct, lambda s: s < 40),
        "aligned_direct_min_veracity": subset_min(group, aligned_direct),
        "aligned_direct_bottom3_mean_veracity": subset_bottom3_mean(group, aligned_direct),
        "aligned_direct_any_below20": int(subset_count(group, aligned_direct & (group["veracity_score"] < 20)) > 0),
        "aligned_direct_any_below40": int(subset_count(group, aligned_direct & (group["veracity_score"] < 40)) > 0),
        "aligned_direct_n_below40": subset_count(group, aligned_direct & (group["veracity_score"] < 40)),
        "aligned_direct_true_n": subset_count(group, aligned_direct & (group["veracity_score"] >= 67)),
        "aligned_direct_false_n": subset_count(group, aligned_direct & (group["veracity_score"] < 33)),
        "aligned_direct_mid_n": subset_count(group, aligned_direct & (group["veracity_score"] >= 33) & (group["veracity_score"] < 67)),
        "counteraligned_direct_true_n": subset_count(group, counteraligned_direct & (group["veracity_score"] >= 67)),
        "counteraligned_direct_false_n": subset_count(group, counteraligned_direct & (group["veracity_score"] < 33)),
        "counteraligned_direct_mid_n": subset_count(group, counteraligned_direct & (group["veracity_score"] >= 33) & (group["veracity_score"] < 67)),
        "first_direct_aligned_claim_veracity": ordered_aligned_direct["veracity_score"].iloc[0] if len(ordered_aligned_direct) else np.nan,
        "first_direct_aligned_claim_turn_order": ordered_aligned_direct["turn_order"].iloc[0] if len(ordered_aligned_direct) else np.nan,
        "first3_direct_aligned_mean_veracity": ordered_aligned_direct["veracity_score"].head(3).mean() if len(ordered_aligned_direct) else np.nan,
        "first3_direct_aligned_pct_very_false40": ((ordered_aligned_direct["veracity_score"].head(3) < 40).mean() if len(ordered_aligned_direct) else np.nan),
        "first_turn_aligned_direct_n": subset_count(group, first_turn_aligned_direct),
        "first_turn_aligned_direct_true_n": subset_count(group, first_turn_aligned_direct & (group["veracity_score"] >= 67)),
        "first_turn_aligned_direct_false_n": subset_count(group, first_turn_aligned_direct & (group["veracity_score"] < 33)),
        "first_turn_aligned_direct_mid_n": subset_count(group, first_turn_aligned_direct & (group["veracity_score"] >= 33) & (group["veracity_score"] < 67)),
        "first_turn_direct_aligned_mean_veracity": subset_mean(group, first_turn_aligned_direct),
        "first_turn_direct_aligned_pct_very_false40": subset_pct(group, first_turn_aligned_direct, lambda s: s < 40),
        "first_two_turn_aligned_direct_n": subset_count(group, first_two_turn_aligned_direct),
        "first_two_turn_aligned_direct_true_n": subset_count(group, first_two_turn_aligned_direct & (group["veracity_score"] >= 67)),
        "first_two_turn_aligned_direct_false_n": subset_count(group, first_two_turn_aligned_direct & (group["veracity_score"] < 33)),
        "first_two_turn_aligned_direct_mid_n": subset_count(group, first_two_turn_aligned_direct & (group["veracity_score"] >= 33) & (group["veracity_score"] < 67)),
        "first_two_turn_direct_aligned_mean_veracity": subset_mean(group, first_two_turn_aligned_direct),
        "first_two_turn_direct_aligned_pct_very_false40": subset_pct(group, first_two_turn_aligned_direct, lambda s: s < 40),
        "topic_normalized_aligned_direct_veracity": subset_mean(group, aligned_direct) - first["focal_claim_veracity"] if not pd.isna(first["focal_claim_veracity"]) else np.nan,
    }

    out["any_intended_movement"] = int((first["aligned_belief_change"] if pd.notna(first["aligned_belief_change"]) else np.nan) > 0)
    out["positive_intended_magnitude"] = first["aligned_belief_change"] if out["any_intended_movement"] == 1 else np.nan
    return out


def main() -> None:
    parser = argparse.ArgumentParser(
        description="Build conversation-level claim-role portfolios from labeled claim rows."
    )
    parser.add_argument(
        "--input",
        # TRANSIENT INTERMEDIATE (not shipped): pooled claim-role labeled rows from the
        # role-classification stage (paid API). Kept in the working dir.
        default=str(WORK_DIR / "pooled_claim_role_labeled.csv"),
    )
    parser.add_argument(
        "--output-conversation",
        # SHIPPED: claim_role_portfolio_all_studies.csv -> data/api_cached/claim_datasets/
        default=str(CLAIM_DATASETS_DIR / "claim_role_portfolio_all_studies.csv"),
    )
    parser.add_argument(
        "--output-claim-level",
        # TRANSIENT INTERMEDIATE (not shipped): per-claim annotated companion.
        default=str(WORK_DIR / "pooled_claim_role_claim_level_annotated.csv"),
    )
    parser.add_argument(
        "--status-output",
        # TRANSIENT INTERMEDIATE (not shipped): per-conversation completeness status.
        default=str(WORK_DIR / "pooled_claim_role_portfolio_status.csv"),
    )
    args = parser.parse_args()

    df = pd.read_csv(args.input)
    required = [
        "conversation_id",
        "row_key",
        "direction",
        "model_pooled",
        "veracity_score",
        "claim_text",
        "stance_to_focal",
        "directness_to_focal",
    ]
    missing = [col for col in required if col not in df.columns]
    if missing:
        raise SystemExit(f"Input missing required columns: {', '.join(missing)}")

    for col in [
        "turn_order",
        "message_time",
        "claim_index_num",
        "veracity_score",
        "aligned_belief_change",
        "belief_rating_pre_4",
        "focal_claim_veracity",
        "focal_claim_checkability",
    ]:
        if col in df.columns:
            df[col] = pd.to_numeric(df[col], errors="coerce")

    if "truth_bin" not in df.columns:
        df["truth_bin"] = df["focal_claim_veracity"].apply(truth_bin_from_score)

    df["stance_to_focal"] = df["stance_to_focal"].fillna("")
    df["directness_to_focal"] = df["directness_to_focal"].fillna("")
    df["claim_role_labeled"] = (df["stance_to_focal"] != "") & (df["directness_to_focal"] != "")
    df["aligned_to_direction"] = np.where(
        ((df["direction"] == "bunk") & (df["stance_to_focal"] == "supports"))
        | ((df["direction"] == "debunk") & (df["stance_to_focal"] == "opposes")),
        1,
        0,
    )
    df["counteraligned_to_direction"] = np.where(
        ((df["direction"] == "bunk") & (df["stance_to_focal"] == "opposes"))
        | ((df["direction"] == "debunk") & (df["stance_to_focal"] == "supports")),
        1,
        0,
    )

    total_counts = df.groupby("conversation_id").size().rename("claim_role_total_n")
    labeled_counts = df.groupby("conversation_id")["claim_role_labeled"].sum().rename("claim_role_labeled_n")
    status = pd.concat([total_counts, labeled_counts], axis=1).reset_index()
    status["claim_role_complete"] = status["claim_role_total_n"] == status["claim_role_labeled_n"]

    labeled_df = df.loc[df["claim_role_labeled"]].copy()
    portfolio = pd.DataFrame([compute_features(group) for _, group in labeled_df.groupby("conversation_id", sort=False)])
    portfolio = portfolio.merge(status, on="conversation_id", how="left")
    portfolio["ready_for_role_analysis"] = portfolio["claim_role_complete"]
    portfolio["belief_rating_pre_4_centered"] = portfolio["belief_rating_pre_4"] - portfolio["belief_rating_pre_4"].mean(skipna=True)

    scale_10_cols = [
        "belief_rating_pre_4",
        "belief_rating_pre_4_centered",
        "focal_claim_veracity",
        "focal_claim_checkability",
        "direct_mean_veracity",
        "aligned_direct_mean_veracity",
        "counteraligned_direct_mean_veracity",
        "aligned_direct_bottom3_mean_veracity",
        "first_direct_aligned_claim_veracity",
        "first3_direct_aligned_mean_veracity",
        "first_turn_direct_aligned_mean_veracity",
        "first_two_turn_direct_aligned_mean_veracity",
        "topic_normalized_aligned_direct_veracity",
    ]
    pct_cols = [
        "direct_pct_very_false40",
        "aligned_direct_pct_very_false40",
        "counteraligned_direct_pct_very_false40",
        "first3_direct_aligned_pct_very_false40",
        "first_turn_direct_aligned_pct_very_false40",
        "first_two_turn_direct_aligned_pct_very_false40",
    ]
    count_cols = [
        "direct_n",
        "indirect_n",
        "background_n",
        "aligned_direct_n",
        "counteraligned_direct_n",
        "aligned_indirect_n",
        "counteraligned_indirect_n",
        "aligned_direct_n_below40",
        "aligned_direct_true_n",
        "aligned_direct_false_n",
        "aligned_direct_mid_n",
        "counteraligned_direct_true_n",
        "counteraligned_direct_false_n",
        "counteraligned_direct_mid_n",
        "first_turn_aligned_direct_n",
        "first_turn_aligned_direct_true_n",
        "first_turn_aligned_direct_false_n",
        "first_turn_aligned_direct_mid_n",
        "first_two_turn_aligned_direct_n",
        "first_two_turn_aligned_direct_true_n",
        "first_two_turn_aligned_direct_false_n",
        "first_two_turn_aligned_direct_mid_n",
    ]
    for col in scale_10_cols:
        if col in portfolio.columns:
            portfolio[f"{col}_10"] = portfolio[col] / 10.0
    for col in pct_cols:
        if col in portfolio.columns:
            portfolio[f"{col}_10"] = portfolio[col] * 10.0
    for col in count_cols:
        if col in portfolio.columns:
            portfolio[f"{col}_10"] = portfolio[col] / 10.0

    claim_output = Path(args.output_claim_level)
    conv_output = Path(args.output_conversation)
    status_output = Path(args.status_output)
    for path in [claim_output, conv_output, status_output]:
        path.parent.mkdir(parents=True, exist_ok=True)

    df.to_csv(claim_output, index=False)
    portfolio.to_csv(conv_output, index=False)
    status.to_csv(status_output, index=False)

    print(f"Saved: {claim_output}")
    print(f"Saved: {conv_output}")
    print(f"Saved: {status_output}")
    print(f"Conversations in portfolio: {len(portfolio):,}")
    print(f"Ready for role analysis: {int(portfolio['ready_for_role_analysis'].sum()):,}")


if __name__ == "__main__":
    main()
