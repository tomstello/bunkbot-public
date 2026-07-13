# Focal-statement veracity pipeline

Answers: **what fraction of the focal conspiracies our participants entertained are
actually false?** — and produces the per-participant veracity scores used for the
false-conspiracy subset analyses.

## What is scored, and why

The judge scores the **AI restatement** (`conRestatement`): the single declarative
sentence that (a) states the conspiracy claim cleanly and (b) is the exact
proposition the participant's 0–100 belief ratings anchored on. The participant's
own free-text description is passed as **context only** — it is typically hedged
("It is something I wonder about…") and fact-checking it would score a hedge, not
the claim.

## What was wrong with the superseded earlier runs (and is fixed here)

The two earlier artifacts (`conspiracy_veracity_5.jsonl`, 0–5 scale, S1–3 only;
`*_focal_claim_veracity_item.*`, 0–100, S2+S4 only, scored on the full 1,840-row
S4 production master) also scored `conRestatement`, but:

1. **Denial-phrased restatements** (`category == "denies"`, ~6–9% per study) were
   scored as if they asserted a conspiracy — a true denial ("there is no
   conspiracy; the participant has none in mind") scored HIGH veracity and
   inflated the apparent share of true conspiracies. Here the judge first
   classifies `statement_type` (conspiracy_claim / official_account / no_claim);
   headline fraction-false statistics are computed over genuine conspiracy claims
   only, with a flipped-denials sensitivity in the R module.
2. **Two incompatible scales and incomplete coverage** (no 0–100 scores for S1/S3).
   Here: one 0–100 scale, all four studies.
3. **Not restricted to the analytic samples.** Here: inputs are built by
   `extract_inputs.R` from the engine's analytic frames (1,092 / 814 / 818 / 1,272).
4. **No validation.** The R module (`code/R/ext_focal_veracity.R`) reports
   agreement between these scores and both superseded artifacts as robustness,
   and `human_audit_template.csv` supports a stratified hand-check.

## Run

```bash
Rscript code/provenance/python_api/focal_veracity/extract_inputs.R   # stage 0, API-free
cd code/provenance/python_api/focal_veracity
python3 score_focal_statement_veracity.py --dry-run                  # inspect prompts
python3 score_focal_statement_veracity.py --limit 20                 # pilot
python3 score_focal_statement_veracity.py                            # full (~4.0k calls, resumable)
```

Key: `OPENROUTER_API_KEY` in `focal_veracity/.env`, `../claim_factcheck/.env`, or
`prompts/.env`. Judge: `openrouter/perplexity/sonar-pro` (override with
`FOCAL_VERACITY_MODEL`), temperature 0, strict JSON, 3 retries, resumable JSONL.

## Outputs (shipped, frozen measurement data)

`data/api_cached/focal_veracity/study{N}_{regime}_focal_statement_veracity.csv`
(+ `focal_statement_veracity_all_studies.csv`): one row per analytic participant —
`statement_type`, `veracity_score` (0–100), `checkability`, `label`, `rationale`,
model id, prompt hash, timestamp. Read by `code/R/ext_focal_veracity.R`.
