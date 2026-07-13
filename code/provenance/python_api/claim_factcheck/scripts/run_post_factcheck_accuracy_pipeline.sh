#!/bin/zsh
set -euo pipefail

# Resolve REPO_ROOT (dir containing both data/ and code/) by walking up from this script.
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PIPELINE_DIR="$(dirname "$SCRIPT_DIR")"   # ships scripts/, codebooks/, .env
REPO_ROOT="$SCRIPT_DIR"
while [[ "$REPO_ROOT" != "/" && ! ( -d "$REPO_ROOT/data" && -d "$REPO_ROOT/code" ) ]]; do
  REPO_ROOT="$(dirname "$REPO_ROOT")"
done
ROOT="$PIPELINE_DIR"
# cd into the pipeline dir so the relative scripts/*.py calls below resolve.
cd "$ROOT"

# TRANSIENT INTERMEDIATES (not shipped): pipeline working dir.
WORK="$REPO_ROOT/output/provenance_work/claim_factcheck"
mkdir -p "$WORK"

RUN_FOCAL_VERACITY="${RUN_FOCAL_VERACITY:-0}"
FOCAL_CSV="${FOCAL_CSV:-$WORK/april04_focal_claim_veracity_item.csv}"
FOCAL_JSONL="${FOCAL_JSONL:-$WORK/april04_focal_claim_veracity_item.jsonl}"
# SHIPPED input: study4_sharing_analysis_merged.csv.gz -> data/api_cached/sharing_and_stance/
PARTICIPANT_INPUT="${PARTICIPANT_INPUT:-$REPO_ROOT/data/api_cached/sharing_and_stance/study4_sharing_analysis_merged.csv.gz}"

# 1. Optionally score focal conspiracy veracity one row per conversation.
# score_focal_claim_veracity.py was pruned; the maintained successor is
# code/provenance/python_api/focal_veracity/score_focal_statement_veracity.py.
if [[ "$RUN_FOCAL_VERACITY" == "1" ]]; then
  if [ -f scripts/score_focal_claim_veracity.py ]; then
    python3 scripts/score_focal_claim_veracity.py \
      --input "$PARTICIPANT_INPUT" \
      --output-jsonl "$FOCAL_JSONL" \
      --output-csv "$FOCAL_CSV"
  else
    echo "skipping score_focal_claim_veracity.py (pruned; see python_api/focal_veracity/ for the maintained module)"
  fi
fi

# 2. Build the conversation-complete dataset from:
#    - participant-level outcomes
#    - taxonomy outputs
#    - eligibility / queue file
#    - fully-scored fact-check JSONL
#    - optional focal-conspiracy veracity scores
if [[ -f "$FOCAL_CSV" ]]; then
  python3 scripts/build_complete_conversation_analysis_dataset.py \
    --participant-input "$PARTICIPANT_INPUT" \
    --focal-veracity-input "$FOCAL_CSV"
else
  python3 scripts/build_complete_conversation_analysis_dataset.py \
    --participant-input "$PARTICIPANT_INPUT"
fi

# 3. Run the final accuracy-by-condition analysis stack.
# NOTE: analyze_complete_conversation_accuracy_models.R is an exploratory analysis script that was
# pruned from this provenance bundle (see scripts/README_PIPELINE.md). I/O routes to the working dir.
if [ -f scripts/analyze_complete_conversation_accuracy_models.R ]; then
  Rscript scripts/analyze_complete_conversation_accuracy_models.R \
    "$WORK/april04_complete_conversation_accuracy_dataset.csv" \
    "$WORK/april04_complete_conversation_accuracy_models"
else
  echo "skipping analyze_complete_conversation_accuracy_models.R (pruned exploratory step; not in this bundle)"
fi
