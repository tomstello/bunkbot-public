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

# The final dataset builder below (build_legacy_existing_factcheck_accuracy_dataset.py)
# was pruned from this provenance bundle (see README_PIPELINE.md). Fail fast BEFORE
# any paid API stage rather than after.
if [ ! -f scripts/build_legacy_existing_factcheck_accuracy_dataset.py ]; then
  echo "ERROR: scripts/build_legacy_existing_factcheck_accuracy_dataset.py was pruned from" >&2
  echo "this bundle; this legacy Study-2 runner cannot be replayed end to end." >&2
  exit 1
fi

# cd into the pipeline dir so the relative scripts/*.py calls below resolve.
cd "$ROOT"

QUEUE_POLICY="${QUEUE_POLICY:-include_all}"
QUEUE_MIN_CONFIDENCE="${QUEUE_MIN_CONFIDENCE:-0.0}"
CLAIMS_PER_REQUEST="${CLAIMS_PER_REQUEST:-4}"
ELIGIBILITY_CONCURRENT_REQUESTS="${ELIGIBILITY_CONCURRENT_REQUESTS:-2}"
ELIGIBILITY_BATCH_SIZE="${ELIGIBILITY_BATCH_SIZE:-4}"

CLAIM_CLASS_CSV="$WORK/study2_standard_claim_classifications_v2_gemini_flash.csv"
ELIG_CSV="$WORK/study2_standard_factcheck_eligibility_gemini_flash.csv"
QUEUE_CSV="$WORK/study2_standard_factcheck_queue_gemini_flash_${QUEUE_POLICY}.csv"
DATASET_CSV="$WORK/study2_standard_complete_conversation_accuracy_dataset.csv"
STATUS_CSV="$WORK/study2_standard_complete_conversation_accuracy_status.csv"

ELIGIBILITY_SHARD_DIR="$WORK/study2_standard_shards/eligibility_inputs"
ELIGIBILITY_JSONL_DIR="$WORK/study2_standard_shards/eligibility_jsonl"

mkdir -p "$ELIGIBILITY_JSONL_DIR"

if [[ ! -f "$CLAIM_CLASS_CSV" ]]; then
  echo "Missing taxonomy CSV: $CLAIM_CLASS_CSV" >&2
  exit 1
fi

if [[ ! -d "$ELIGIBILITY_SHARD_DIR" ]] || [[ -z "$(ls -A "$ELIGIBILITY_SHARD_DIR" 2>/dev/null)" ]]; then
  python3 scripts/make_claim_row_shards.py \
    --input "$CLAIM_CLASS_CSV" \
    --output-dir "$ELIGIBILITY_SHARD_DIR" \
    --mode message_id \
    --shard-size 150 \
    --prefix "eligibility"
fi

for shard in "$ELIGIBILITY_SHARD_DIR"/eligibility_*.csv; do
  shard_base="$(basename "$shard" .csv)"
  out_jsonl="$ELIGIBILITY_JSONL_DIR/${shard_base}.jsonl"
  out_csv="$ELIGIBILITY_JSONL_DIR/${shard_base}.csv"
  if [[ -f "$out_jsonl" ]]; then
    echo "Skipping existing shard: ${shard_base}"
    continue
  fi
  python3 scripts/classify_factcheck_eligibility.py \
    --input "$shard" \
    --output-jsonl "$out_jsonl" \
    --output-csv "$out_csv" \
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

python3 scripts/build_legacy_existing_factcheck_accuracy_dataset.py \
  --participant-input "$PARTICIPANT_INPUT" \
  --all-claims-input "$CLAIM_CLASS_CSV" \
  --queue-input "$QUEUE_CSV" \
  --output "$DATASET_CSV" \
  --status-output "$STATUS_CSV"
