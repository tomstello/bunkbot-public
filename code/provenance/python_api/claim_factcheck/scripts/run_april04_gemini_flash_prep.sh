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
RESULTS="$REPO_ROOT/output/provenance_work/claim_factcheck"
mkdir -p "$RESULTS"

set -a
source "$ROOT/.env"
set +a

python3 "$ROOT/scripts/classify_claims_v2.py" \
  --input "$RESULTS/april04_extracted_claim_rows.csv" \
  --output-jsonl "$RESULTS/april04_claim_classifications_v2_gemini_flash.jsonl" \
  --output-csv "$RESULTS/april04_claim_classifications_v2_gemini_flash.csv" \
  --model "openrouter/google/gemini-3-flash-preview" \
  --claims-per-request 4 \
  --concurrent-requests 12 \
  --api-batch-size 24 \
  --timeout 120

python3 "$ROOT/scripts/classify_factcheck_eligibility.py" \
  --input "$RESULTS/april04_claim_classifications_v2_gemini_flash.csv" \
  --output-jsonl "$RESULTS/april04_factcheck_eligibility_gemini_flash.jsonl" \
  --output-csv "$RESULTS/april04_factcheck_eligibility_gemini_flash.csv" \
  --model "openrouter/google/gemini-3-flash-preview" \
  --claims-per-request 8 \
  --concurrent-requests 12 \
  --batch-size 24 \
  --timeout 120 \
  --context-chars 900

python3 "$ROOT/scripts/build_factcheck_queue.py" \
  --input "$RESULTS/april04_factcheck_eligibility_gemini_flash.csv" \
  --output "$RESULTS/april04_factcheck_queue_gemini_flash.csv" \
  --queue-policy include_all
