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
  --output "$RESULTS/april04_factcheck_queue_gemini_flash_include_all.csv" \
  --queue-policy include_all

python3 "$ROOT/scripts/fact_check_claims.py" \
  --input "$RESULTS/april04_factcheck_queue_gemini_flash_include_all.csv" \
  --output-jsonl "$RESULTS/april04_fact_checked_claims_gemini_flash_include_all.jsonl" \
  --output-csv "$RESULTS/april04_fact_checked_claims_gemini_flash_include_all.csv" \
  --model "openrouter/perplexity/sonar-pro" \
  --concurrent-requests 6 \
  --batch-size 12 \
  --timeout 180 \
  --context-chars 900

# summarize_factcheck_results.py (reporting-only) was pruned from this bundle
# (see README_PIPELINE.md); run the summary step only if a local copy exists.
if [ -f "$ROOT/scripts/summarize_factcheck_results.py" ]; then
  python3 "$ROOT/scripts/summarize_factcheck_results.py" \
    --input "$RESULTS/april04_fact_checked_claims_gemini_flash_include_all.csv" \
    --conversation-output "$RESULTS/april04_conversation_veracity_summary_gemini_flash_include_all.csv" \
    --cell-output "$RESULTS/april04_cell_veracity_summary_gemini_flash_include_all.csv"
else
  echo "skipping summarize_factcheck_results.py (pruned reporting step; not in this bundle)"
fi
