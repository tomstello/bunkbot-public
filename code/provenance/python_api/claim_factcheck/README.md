# Portable Claim Extraction + Fact-Checking Toolkit

This is a self-contained toolkit for running the same basic pipeline on a new
human-AI dialogue dataset in a different folder or project.

It is designed for cases where:

- you have AI assistant messages from conversations,
- you want to extract discrete claims from those messages,
- you optionally want to classify those claims with the `claim_category_v2`
  taxonomy,
- you optionally want to add a fact-check eligibility / materiality triage
  stage before spending on search-based fact checking,
- you optionally want to fact-check the extracted claims with a
  search-augmented model.

The package does not depend on any other files in the current workspace.

## Paths in this repo (provenance build)

These scripts have been re-pointed for the `bunkbot-public` repo. Every script resolves the
repo root (the directory containing both `data/` and `code/`) at runtime and reads/writes
relative to it. In particular:

- **Shipped inputs** are read from `data/...` under the repo root, e.g.
  - Study 2 participants: `data/processed_s1s3/study2_standard_clean.csv.gz`
  - Study 4 participants (merged): `data/api_cached/sharing_and_stance/study4_sharing_analysis_merged.csv.gz`
  - Claim-role portfolio (shipped final, also an input to the master builder):
    `data/api_cached/claim_datasets/claim_role_portfolio_all_studies.csv`
  - APE refusal/compliance (shipped final): `data/api_cached/compliance_ape/study4_compliance_ape_refusal.csv`
- **Shipped final outputs** are written to `data/api_cached/<subdir>/`:
  - `study4_compliance_ape_refusal.csv` -> `compliance_ape/`
  - `claim_role_portfolio_all_studies.csv` -> `claim_datasets/`
  - `study4_master_analysis_dataset.csv` -> `claim_datasets/`
- **Transient intermediates** (the `results/...` files below) are written to a working dir,
  `output/provenance_work/claim_factcheck/`, and keep their original basenames. They are NOT
  shipped and are produced only when the (paid) API pipeline is run end to end.
- **Not shipped, supply at runtime**: the raw APE human-eval snapshot/rating tree
  (`parse_april04_ape_refusal.py --input-root`), the legacy Study 2 fact-check data stream
  (`data_stream_factcheck.csv`), and the upstream-built `claim_accuracy_pooled_s2_s4.csv`
  master (shipped at `data/api_cached/claim_datasets/`, but produced by a pruned upstream builder).

Below, `results/<name>` refers to that working dir (`output/provenance_work/claim_factcheck/<name>`).

## What It Includes

- `scripts/extract_claims.py`
  Extracts factual claims from assistant messages.
- `scripts/classify_claims_v2.py`
  Applies the six-category `claim_category_v2` taxonomy plus
  `granular_subtype`.
- `scripts/fact_check_claims.py`
  Fact-checks extracted claims with message context.
- `scripts/classify_factcheck_eligibility.py`
  Labels each extracted claim for checkability, materiality, and whether it
  should enter the fact-check queue.
- `scripts/build_factcheck_queue.py`
  Builds the filtered claim queue that should actually be sent to the
  fact-check model.
- `scripts/run_pipeline.py`
  Convenience runner for the standard sequence. (Two auxiliary steps it once
  called — `validate_messages.py` input validation and
  `summarize_factcheck_results.py` reporting — were pruned in the 81→27 script
  consolidation (see `scripts/README_PIPELINE.md`); the runner skips them when
  absent. The same consolidation removed the `build_social_sharing_messages.py`
  input bridge — see `scripts/build_april04_master_analysis_dataset.py` and
  `scripts/build_complete_conversation_analysis_dataset.py` for the maintained
  route from the merged sharing CSV.)
- `codebooks/claim_category_v2_codebook.csv`
  The current portable claim-coding codebook.
- `templates/messages_template.csv`
  Example input format.
- `AGENT_START_HERE.md`
  A cold-start handoff for another coding agent.

## Expected Input

Put a long-format CSV at `data/messages.csv`.

Required columns:

- `conversation_id`
- `message_id`
- `role`
- `content`

Recommended columns:

- `turn_index`
- `condition`
- `study_id`
- `participant_id`
- `message_timestamp`
- `topic`

The scripts filter to `role == "assistant"`, so you can include user turns too
if you want to preserve context in the raw file.

See `INPUT_SCHEMA.md`.

## Quick Start

1. Create a virtual environment and install dependencies.

```bash
cd code/provenance/python_api/claim_factcheck
python3 -m venv .venv
source .venv/bin/activate
pip install -r requirements.txt
```

2. Add your OpenRouter key.

```bash
cp .env.example .env
```

Then edit `.env` and set `OPENROUTER_API_KEY`.

3. Put your dialogue file at `data/messages.csv` (long format; see
`templates/messages_template.csv` and `INPUT_SCHEMA.md`).

4. Run the extraction step.

```bash
python3 scripts/extract_claims.py --input data/messages.csv
```

5. Optionally classify extracted claims with the v2 taxonomy.

```bash
python3 scripts/classify_claims_v2.py \
  --input results/extracted_claim_rows.csv \
  --codebook codebooks/claim_category_v2_codebook.csv
```

6. Optionally fact-check the extracted claims.

Recommended sequence before fact-checking:

```bash
python3 scripts/classify_factcheck_eligibility.py \
  --input results/claim_classifications_v2.csv

python3 scripts/build_factcheck_queue.py \
  --input results/factcheck_eligibility.csv \
  --output results/factcheck_queue.csv
```

```bash
python3 scripts/fact_check_claims.py \
  --input results/factcheck_queue.csv
```

Or run the common sequence:

```bash
python3 scripts/run_pipeline.py --input data/messages.csv
```

## Output Files

- `results/extracted_claims.jsonl`
  One record per assistant message.
- `results/extracted_claims_messages.csv`
  Message-level extraction results.
- `results/extracted_claim_rows.csv`
  One row per extracted claim.
- `results/claim_classifications_v2.jsonl`
  One record per unique claim text coded by the taxonomy.
- `results/claim_classifications_v2.csv`
  Classification merged back onto the claim rows.
- `results/factcheck_eligibility.jsonl`
  Row-level checkability/materiality labels.
- `results/factcheck_eligibility.csv`
  Eligibility labels merged back onto the claim rows.
- `results/factcheck_queue.csv`
  Filtered row-level claims selected for the expensive fact-check stage.
- `results/fact_checked_claims.jsonl`
  One record per claim row with veracity output.
- `results/fact_checked_claims.csv`
  Fact-check output merged back onto the claim rows.
- `results/conversation_veracity_summary.csv`
  Conversation-level veracity summary.
- `results/cell_veracity_summary.csv`
  Model-by-condition veracity summary.

## Default Models

- Claim extraction:
  `openrouter/openai/gpt-5-mini`
- Claim coding:
  `openrouter/openai/gpt-5-mini`
- Eligibility / materiality triage:
  `openrouter/openai/gpt-5-mini`
- Fact checking:
  `openrouter/perplexity/sonar-pro`

Override them in `.env` or with CLI flags.

## Cost / Practical Notes

- Claim extraction is usually manageable.
- Claim coding and eligibility triage are cheaper than fact checking.
- Fact checking can get expensive very quickly because it runs one request per
  queued claim row and uses a search-augmented model.

Recommended workflow:

1. Validate input.
2. Run extraction on a small pilot first.
3. Inspect claim rows.
4. Run taxonomy coding and eligibility triage.
5. Inspect the size of `results/factcheck_queue.csv`.
6. If the queue looks clean, decide whether you want:
   - coding only,
   - fact-checking only,
   - both.
7. Run fact-checking only after confirming queue volume.

## Resume Behavior

All long-running stages write JSONL incrementally and can resume. If a JSONL
output already exists, successful prior records are skipped on restart.

## Scope Limits

This package does not attempt to normalize your raw study export. It expects a
clean long-format `messages.csv`. Another agent can adapt your raw export to
that schema using the template and input guide included here.
