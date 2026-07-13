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
# TRANSIENT INTERMEDIATES (not shipped): pipeline working dir.
WORK="$REPO_ROOT/output/provenance_work/claim_factcheck"
mkdir -p "$WORK"
# SHIPPED input: study2_standard_clean.csv.gz -> data/processed_s1s3/
PARTICIPANT_INPUT="$REPO_ROOT/data/processed_s1s3/study2_standard_clean.csv.gz"
# NOT SHIPPED: legacy Study 2 pre-existing fact-check data stream (produced upstream / paid run).
# The repo does not ship this raw stream; supply it at runtime. Falls back to the working dir.
FACTCHECK_INPUT="${FACTCHECK_INPUT:-$WORK/data_stream_factcheck.csv}"

# cd into the pipeline dir so the relative scripts/*.py calls below resolve.
cd "$ROOT"

RUN_FOCAL_VERACITY="${RUN_FOCAL_VERACITY:-0}"
QUEUE_POLICY="${QUEUE_POLICY:-include_all}"
QUEUE_MIN_CONFIDENCE="${QUEUE_MIN_CONFIDENCE:-0.0}"
CLAIMS_PER_REQUEST="${CLAIMS_PER_REQUEST:-4}"
CLAIM_CODING_CONCURRENT_REQUESTS="${CLAIM_CODING_CONCURRENT_REQUESTS:-2}"
CLAIM_CODING_API_BATCH_SIZE="${CLAIM_CODING_API_BATCH_SIZE:-4}"
ELIGIBILITY_CONCURRENT_REQUESTS="${ELIGIBILITY_CONCURRENT_REQUESTS:-2}"
ELIGIBILITY_BATCH_SIZE="${ELIGIBILITY_BATCH_SIZE:-4}"
TAXONOMY_SHARD_SIZE="${TAXONOMY_SHARD_SIZE:-200}"
ELIGIBILITY_SHARD_SIZE="${ELIGIBILITY_SHARD_SIZE:-150}"

CLAIM_ROWS_CSV="$WORK/study2_standard_claim_rows_from_existing_factcheck.csv"
CLAIM_CLASS_CSV="$WORK/study2_standard_claim_classifications_v2_gemini_flash.csv"
ELIG_CSV="$WORK/study2_standard_factcheck_eligibility_gemini_flash.csv"
QUEUE_CSV="$WORK/study2_standard_factcheck_queue_gemini_flash_${QUEUE_POLICY}.csv"
FOCAL_JSONL="$WORK/study2_standard_focal_claim_veracity_item.jsonl"
FOCAL_CSV="$WORK/study2_standard_focal_claim_veracity_item.csv"
DATASET_CSV="$WORK/study2_standard_complete_conversation_accuracy_dataset.csv"
STATUS_CSV="$WORK/study2_standard_complete_conversation_accuracy_status.csv"

TAXONOMY_SHARD_DIR="$WORK/study2_standard_shards/taxonomy_inputs"
TAXONOMY_JSONL_DIR="$WORK/study2_standard_shards/taxonomy_jsonl"
ELIGIBILITY_SHARD_DIR="$WORK/study2_standard_shards/eligibility_inputs"
ELIGIBILITY_JSONL_DIR="$WORK/study2_standard_shards/eligibility_jsonl"

rm -rf "$TAXONOMY_SHARD_DIR" "$TAXONOMY_JSONL_DIR" "$ELIGIBILITY_SHARD_DIR" "$ELIGIBILITY_JSONL_DIR"
mkdir -p "$TAXONOMY_SHARD_DIR" "$TAXONOMY_JSONL_DIR" "$ELIGIBILITY_SHARD_DIR" "$ELIGIBILITY_JSONL_DIR"

# build_legacy_study_claim_rows.py was pruned from this bundle (legacy Study-2
# recode input builder; see README_PIPELINE.md). This runner requires it.
if [ ! -f scripts/build_legacy_study_claim_rows.py ]; then
  echo "ERROR: scripts/build_legacy_study_claim_rows.py was pruned from this bundle;" >&2
  echo "this legacy Study-2 recode runner cannot be replayed without it." >&2
  exit 1
fi
python3 scripts/build_legacy_study_claim_rows.py \
  --factcheck-input "$FACTCHECK_INPUT" \
  --participant-input "$PARTICIPANT_INPUT" \
  --study-label Standard \
  --output "$CLAIM_ROWS_CSV"

python3 scripts/make_claim_row_shards.py \
  --input "$CLAIM_ROWS_CSV" \
  --output-dir "$TAXONOMY_SHARD_DIR" \
  --mode claim_text \
  --shard-size "$TAXONOMY_SHARD_SIZE" \
  --prefix "taxonomy"

for shard in "$TAXONOMY_SHARD_DIR"/taxonomy_*.csv; do
  shard_base="$(basename "$shard" .csv)"
  python3 scripts/classify_claims_v2.py \
    --input "$shard" \
    --output-jsonl "$TAXONOMY_JSONL_DIR/${shard_base}.jsonl" \
    --output-csv "$TAXONOMY_JSONL_DIR/${shard_base}.csv" \
    --claims-per-request "$CLAIMS_PER_REQUEST" \
    --concurrent-requests "$CLAIM_CODING_CONCURRENT_REQUESTS" \
    --api-batch-size "$CLAIM_CODING_API_BATCH_SIZE"
done

python3 scripts/materialize_claim_classifications_csv.py \
  --input "$CLAIM_ROWS_CSV" \
  --jsonl "$TAXONOMY_JSONL_DIR"/*.jsonl \
  --output "$CLAIM_CLASS_CSV"

python3 scripts/make_claim_row_shards.py \
  --input "$CLAIM_CLASS_CSV" \
  --output-dir "$ELIGIBILITY_SHARD_DIR" \
  --mode message_id \
  --shard-size "$ELIGIBILITY_SHARD_SIZE" \
  --prefix "eligibility"

for shard in "$ELIGIBILITY_SHARD_DIR"/eligibility_*.csv; do
  shard_base="$(basename "$shard" .csv)"
  python3 scripts/classify_factcheck_eligibility.py \
    --input "$shard" \
    --output-jsonl "$ELIGIBILITY_JSONL_DIR/${shard_base}.jsonl" \
    --output-csv "$ELIGIBILITY_JSONL_DIR/${shard_base}.csv" \
    --claims-per-request "$CLAIMS_PER_REQUEST" \
    --concurrent-requests "$ELIGIBILITY_CONCURRENT_REQUESTS" \
    --batch-size "$ELIGIBILITY_BATCH_SIZE"
done

python3 scripts/materialize_eligibility_csv.py \
  --input "$CLAIM_CLASS_CSV" \
  --jsonl "$ELIGIBILITY_JSONL_DIR"/*.jsonl \
  --output "$ELIG_CSV"

python3 scripts/build_factcheck_queue.py \
  --input "$ELIG_CSV" \
  --output "$QUEUE_CSV" \
  --queue-policy "$QUEUE_POLICY" \
  --min-confidence "$QUEUE_MIN_CONFIDENCE"

if [[ "$RUN_FOCAL_VERACITY" == "1" ]]; then
  # pruned; the maintained successor is python_api/focal_veracity/score_focal_statement_veracity.py
  python3 scripts/score_focal_claim_veracity.py \
    --input "$PARTICIPANT_INPUT" \
    --all-rows \
    --output-jsonl "$FOCAL_JSONL" \
    --output-csv "$FOCAL_CSV"
fi

if [[ -f "$FOCAL_CSV" ]]; then
  python3 scripts/build_legacy_existing_factcheck_accuracy_dataset.py \
    --participant-input "$PARTICIPANT_INPUT" \
    --all-claims-input "$CLAIM_CLASS_CSV" \
    --queue-input "$QUEUE_CSV" \
    --focal-veracity-input "$FOCAL_CSV" \
    --output "$DATASET_CSV" \
    --status-output "$STATUS_CSV"
else
  python3 scripts/build_legacy_existing_factcheck_accuracy_dataset.py \
    --participant-input "$PARTICIPANT_INPUT" \
    --all-claims-input "$CLAIM_CLASS_CSV" \
    --queue-input "$QUEUE_CSV" \
    --output "$DATASET_CSV" \
    --status-output "$STATUS_CSV"
fi
