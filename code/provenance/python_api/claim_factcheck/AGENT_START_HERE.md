# Agent Start Here

> Operating notes for an AI coding assistant (or a human) re-running this
> toolkit on a new dataset; kept as provenance for how the pipeline is driven.

Use this package when you have a new study or project with human-AI dialogues
and need to reproduce the claim-extraction / claim-coding / fact-checking
workflow in a fresh folder.

## First Rules

- Do not assume the raw project export already matches the required schema.
- Do not start the expensive fact-checking step before checking extraction
  volume.
- Treat `data/messages.csv` as the canonical input file for this package.

## What You Should Do

1. Read `README.md`.
2. Read `INPUT_SCHEMA.md`.
3. Inspect the user's source data.
4. Transform the source data into `data/messages.csv`.
5. Sanity-check `data/messages.csv` against `INPUT_SCHEMA.md` (the
   `validate_messages.py` helper was pruned from this bundle).

6. Run a small extraction pilot first:

```bash
python3 scripts/extract_claims.py --input data/messages.csv --limit 25
```

7. Inspect:

- `results/extracted_claims_messages.csv`
- `results/extracted_claim_rows.csv`

8. If extraction looks good, rerun extraction on the full dataset.
9. If the user wants claim-content coding, run:

```bash
python3 scripts/classify_claims_v2.py \
  --input results/extracted_claim_rows.csv \
  --codebook codebooks/claim_category_v2_codebook.csv
```

10. If the user wants veracity fact-checking, estimate cost from the number of
    fact-check-eligible rows first, then run:

```bash
python3 scripts/classify_factcheck_eligibility.py \
  --input results/claim_classifications_v2.csv

python3 scripts/build_factcheck_queue.py \
  --input results/factcheck_eligibility.csv \
  --output results/factcheck_queue.csv
```

11. Only then run:

```bash
python3 scripts/fact_check_claims.py --input results/factcheck_queue.csv
```

## Required Input Columns

- `conversation_id`
- `message_id`
- `role`
- `content`

The scripts process only assistant messages.

## Recommended QA Checks

- `message_id` should be unique at the message level.
- `content` should be clean plaintext, not HTML blobs if avoidable.
- Assistant rows should contain the actual model output, not the whole dialogue
  transcript concatenated into one cell unless that is genuinely your unit of
  analysis.
- If the extraction pilot yields too many vague fragments, fix the input before
  scaling up.

## Decision Rules

- If the project only needs claim extraction, stop after
  `results/extracted_claim_rows.csv`.
- If the project needs content coding but not veracity, run classification and
  skip fact checking.
- If the project needs veracity, do not send raw extracted claims directly to
  search-based fact checking. Run eligibility triage first.
- If cost is a concern, do not run fact checking until the user explicitly wants
  it.

## Important Note

This package uses the current `claim_category_v2` taxonomy developed for the
Bunkbot studies, but the toolkit itself is otherwise portable and should work
for other human-AI dialogue studies.
