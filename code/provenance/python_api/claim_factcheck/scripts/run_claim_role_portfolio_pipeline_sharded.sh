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
# SHIPPED final: portfolio dataset -> data/api_cached/claim_datasets/claim_role_portfolio_all_studies.csv
CLAIM_DATASETS_DIR="$REPO_ROOT/data/api_cached/claim_datasets"
mkdir -p "$CLAIM_DATASETS_DIR"

CLAIMS_PER_REQUEST="${CLAIMS_PER_REQUEST:-6}"
CLAIM_ROLE_CONCURRENT_REQUESTS="${CLAIM_ROLE_CONCURRENT_REQUESTS:-4}"
CLAIM_ROLE_BATCH_SIZE="${CLAIM_ROLE_BATCH_SIZE:-8}"
CLAIM_ROLE_SHARD_SIZE="${CLAIM_ROLE_SHARD_SIZE:-150}"

INPUT_CSV="$WORK/pooled_claim_role_input.csv"
LABEL_CSV="$WORK/pooled_claim_role_labeled.csv"
# SHIPPED final output:
PORTFOLIO_CSV="$CLAIM_DATASETS_DIR/claim_role_portfolio_all_studies.csv"
PORTFOLIO_STATUS_CSV="$WORK/pooled_claim_role_portfolio_status.csv"
CLAIM_LEVEL_CSV="$WORK/pooled_claim_role_claim_level_annotated.csv"

SHARD_INPUT_DIR="$WORK/claim_role_shards/inputs"
SHARD_JSONL_DIR="$WORK/claim_role_shards/jsonl"

mkdir -p "$SHARD_INPUT_DIR" "$SHARD_JSONL_DIR"

python3 scripts/build_pooled_claim_role_input.py \
  --output "$INPUT_CSV"

python3 scripts/make_claim_row_shards.py \
  --input "$INPUT_CSV" \
  --output-dir "$SHARD_INPUT_DIR" \
  --mode message_id \
  --shard-size "$CLAIM_ROLE_SHARD_SIZE" \
  --prefix "claim_role"

for shard in "$SHARD_INPUT_DIR"/claim_role_*.csv; do
  shard_base="$(basename "$shard" .csv)"
  out_jsonl="$SHARD_JSONL_DIR/${shard_base}.jsonl"
  out_csv="$SHARD_JSONL_DIR/${shard_base}.csv"
  if [[ -f "$out_jsonl" ]]; then
    echo "Skipping existing shard: ${shard_base}"
    continue
  fi
  python3 scripts/classify_claim_role_relative_to_focal.py \
    --input "$shard" \
    --output-jsonl "$out_jsonl" \
    --output-csv "$out_csv" \
    --claims-per-request "$CLAIMS_PER_REQUEST" \
    --concurrent-requests "$CLAIM_ROLE_CONCURRENT_REQUESTS" \
    --batch-size "$CLAIM_ROLE_BATCH_SIZE"
done

python3 scripts/materialize_claim_role_csv.py \
  --input "$INPUT_CSV" \
  --jsonl "$SHARD_JSONL_DIR"/*.jsonl \
  --output "$LABEL_CSV"

python3 scripts/build_claim_role_portfolio_dataset.py \
  --input "$LABEL_CSV" \
  --output-conversation "$PORTFOLIO_CSV" \
  --output-claim-level "$CLAIM_LEVEL_CSV" \
  --status-output "$PORTFOLIO_STATUS_CSV"

# NOTE: analyze_claim_role_portfolios.R is an exploratory analysis script that was pruned from
# this provenance bundle (see scripts/README_PIPELINE.md). Output prefix routes to the working dir.
if [ -f scripts/analyze_claim_role_portfolios.R ]; then
  Rscript scripts/analyze_claim_role_portfolios.R \
    "$PORTFOLIO_CSV" \
    "$WORK/claim_role_portfolio_models"
else
  echo "skipping analyze_claim_role_portfolios.R (pruned exploratory step; not in this bundle)"
fi
