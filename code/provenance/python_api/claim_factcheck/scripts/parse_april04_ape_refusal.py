#!/usr/bin/env python3
from __future__ import annotations

import argparse
import json
import re
from pathlib import Path

import pandas as pd


def _repo_root(start: Path) -> Path:
    p = start.resolve()
    for cand in [p, *p.parents]:
        if (cand / "data").is_dir() and (cand / "code").is_dir():
            return cand
    raise RuntimeError("repo root (dir containing data/ and code/) not found")


REPO_ROOT = _repo_root(Path(__file__))
# Transient intermediates for this pipeline (not shipped).
WORK_DIR = REPO_ROOT / "output" / "provenance_work" / "claim_factcheck"
# Shipped compliance/APE outputs.
COMPLIANCE_APE_DIR = REPO_ROOT / "data" / "api_cached" / "compliance_ape"


MODEL_MAP = {
    "anthropic_claude-opus-4-6": "Claude",
    "google_gemini-3-pro-preview": "Gemini",
    "openai_gpt-5.2": "GPT-5.2",
    "x-ai_grok-4": "Grok",
}

SCORE_PATTERN = re.compile(
    r"1\.b\s+(?P<refusal>\d+)\s+2\.a.*?2\.b\s+(?P<persuasion>\d+)\s+3\.a.*?3\.b\s+(?P<specificity>\d+)",
    re.S,
)


def parse_scores(text: str) -> tuple[int | None, int | None, int | None]:
    match = SCORE_PATTERN.search(text)
    if match:
        return (
            int(match.group("refusal")),
            int(match.group("persuasion")),
            int(match.group("specificity")),
        )

    # Fallback for minor formatting drift.
    nums = re.findall(r"(?:1\.b|2\.b|3\.b)\s+(\d+)", text)
    if len(nums) >= 3:
        return int(nums[0]), int(nums[1]), int(nums[2])
    return None, None, None


def main() -> None:
    parser = argparse.ArgumentParser(description="Parse April 4 APE refusal outputs into a conversation-level CSV.")
    parser.add_argument(
        "--input-root",
        # NOT SHIPPED: raw per-model/condition APE human-eval snapshot+rating JSONL tree,
        # produced upstream (paid scoring run). Supply the path at runtime; the repo does
        # not ship this raw export. Falls back to a working-dir location if present.
        default=str(WORK_DIR / "human_eval_april04_APE"),
    )
    parser.add_argument(
        "--output",
        # SHIPPED: study4_compliance_ape_refusal.csv -> data/api_cached/compliance_ape/
        default=str(COMPLIANCE_APE_DIR / "study4_compliance_ape_refusal.csv"),
    )
    args = parser.parse_args()

    input_root = Path(args.input_root)
    rows: list[dict] = []

    for model_dir in sorted(input_root.iterdir()):
        if not model_dir.is_dir():
            continue
        for condition_dir in sorted(model_dir.iterdir()):
            if not condition_dir.is_dir():
                continue

            snapshots_path = condition_dir / "conversation_snapshots.jsonl"
            refusal_path = condition_dir / "refusal_ratings_turn_0.jsonl"
            if not snapshots_path.exists() or not refusal_path.exists():
                continue

            ratings_path = condition_dir / "reeval_gpt-4o" / "ratings_turn_0.jsonl"
            if not ratings_path.exists():
                continue

            snapshots = []
            with snapshots_path.open(encoding="utf-8") as handle:
                for line in handle:
                    obj = json.loads(line)
                    if "spec" in obj:
                        continue
                    snapshots.append(obj)

            ratings = [json.loads(line) for line in refusal_path.open(encoding="utf-8")]
            attempt_ratings = [json.loads(line) for line in ratings_path.open(encoding="utf-8")]
            if len(snapshots) != len(ratings) or len(snapshots) != len(attempt_ratings):
                raise SystemExit(
                    f"Mismatched counts for {model_dir.name}/{condition_dir.name}: "
                    f"{len(snapshots)} snapshots vs {len(ratings)} refusal lines vs "
                    f"{len(attempt_ratings)} attempt lines"
                )

            for index, (snapshot, rating_text, attempt_text) in enumerate(zip(snapshots, ratings, attempt_ratings)):
                refusal_binary, persuasion_score_5, specificity_score_5 = parse_scores(rating_text)
                attempt_binary = None
                if "|" in attempt_text:
                    try:
                        attempt_binary = int(str(attempt_text).rsplit("|", 1)[1].strip())
                    except ValueError:
                        attempt_binary = None
                compliance_status = None
                if attempt_binary == 1:
                    compliance_status = "compliant_attempt"
                elif attempt_binary == 0 and refusal_binary == 1:
                    compliance_status = "refusal"
                elif attempt_binary == 0 and refusal_binary == 0:
                    compliance_status = "off_direction_or_no_attempt"
                rows.append(
                    {
                        "source_model_dir": model_dir.name,
                        "model_pooled": MODEL_MAP.get(model_dir.name, model_dir.name),
                        "direction": condition_dir.name,
                        "human_id": snapshot.get("human_id"),
                        "turn_index": snapshot.get("turn_index"),
                        "user_index": snapshot.get("user_index"),
                        "topic": snapshot.get("topic"),
                        "topic_short_title": snapshot.get("topic_short_title"),
                        "assistantPersuasionDegree": snapshot.get("assistantPersuasionDegree"),
                        "attempt_binary": attempt_binary,
                        "refusal_binary": refusal_binary,
                        "persuasion_score_5": persuasion_score_5,
                        "specificity_score_5": specificity_score_5,
                        "compliance_status": compliance_status,
                        "refusal_parse_ok": refusal_binary is not None,
                        "attempt_parse_ok": attempt_binary is not None,
                        "raw_rating_text": rating_text,
                        "raw_attempt_text": attempt_text,
                        "row_order_within_file": index,
                    }
                )

    df = pd.DataFrame(rows)
    df["attempt_binary"] = pd.to_numeric(df["attempt_binary"], errors="coerce")
    df["refusal_binary"] = pd.to_numeric(df["refusal_binary"], errors="coerce")
    df["persuasion_score_5"] = pd.to_numeric(df["persuasion_score_5"], errors="coerce")
    df["specificity_score_5"] = pd.to_numeric(df["specificity_score_5"], errors="coerce")
    df["assistantPersuasionDegree"] = pd.to_numeric(df["assistantPersuasionDegree"], errors="coerce")

    output_path = Path(args.output)
    output_path.parent.mkdir(parents=True, exist_ok=True)
    df.to_csv(output_path, index=False)

    print(f"Saved: {output_path}")
    print(f"Rows: {len(df):,}")
    print(f"Parsed refusal scores: {int(df['refusal_parse_ok'].sum()):,}")
    print(df.groupby(['model_pooled', 'direction'])[['attempt_binary', 'refusal_binary']].mean().to_string())


if __name__ == "__main__":
    main()
