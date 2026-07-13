# Fact-checking pipeline — the documented S1–S4 flow

These scripts produced the cached claim annotations under `data/api_cached/{claim_veracity,
claim_labels,claim_datasets}/` (formerly `claim_counts/claim_labels/claim_conv`). Pruned to the
pipeline **used in the paper** — the exploratory `analyze_*`/`plot_*` one-offs, figure scripts,
social-sharing topic scripts, and legacy re-processing have been removed (81 → 27 scripts).
Shipped as provenance; not run by the API-free reproduction.

Paths are repo-rooted: each script resolves the repo root (the dir containing both `data/` and
`code/`) at runtime. Shipped finals land in `data/api_cached/<subdir>/` under the new
vocabulary; transient stage intermediates (the `results/...` files) land in the working dir
`output/provenance_work/claim_factcheck/` and keep their original basenames.

## One pipeline, applied identically to all four studies

Every stage runs at **temperature 0** with the same model for all of S1–S4. The
current `config.py` / `.env.example` defaults are `gpt-5-mini` for the LLM coding
stages; the models that actually produced each **shipped** annotation are recorded
per row in the data files themselves (extraction = `openrouter/openai/gpt-4.1`;
the harmonized role pass = `openrouter/openai/gpt-5.4-mini`; veracity =
`openrouter/perplexity/sonar-pro`) and take precedence over these defaults. Some
runner scripts also show interim per-run overrides (e.g. the `*_gemini_flash*`
prep artifacts), which were superseded by the harmonized labels that ship.

| Stage | Script | Default model (`config.py`) |
|---|---|---|
| 1. Claim extraction | `extract_claims.py` | `openrouter/openai/gpt-5-mini` |
| 2. Substantiveness + category (v2 codebook) | `classify_claims_v2.py` | `gpt-5-mini` |
| 3. Fact-check eligibility | `classify_factcheck_eligibility.py` | `gpt-5-mini` |
| 4. Claim role (stance + directness to the focal proposition) | `classify_claim_role_relative_to_focal.py` | `gpt-5-mini` |
| 5. Claim veracity (0–100) | `fact_check_claims.py` | `openrouter/perplexity/sonar-pro` |

Aggregation/IO: `build_factcheck_queue.py`, `make_claim_row_shards.py`,
`materialize_{eligibility,claim_role,claim_classifications}_csv.py`,
`build_{claim_role_portfolio,pooled_claim_role_input,april04_master_analysis,
complete_conversation_analysis}_dataset.py`, `jsonl_to_csv.py`, `utils.py`.

Orchestration: `run_pipeline.py` (main), `run_claim_role_portfolio_pipeline_sharded.sh`,
`run_april04_full_factcheck.sh` (Study 4). APE turn-0 refusal/compliance parse:
`parse_april04_ape_refusal.py`.

## Inputs — the same screened samples as the paper

The pipeline runs on the **analytic samples after the paper's screening filters** (equivocal
focal description + baseline belief in the 25–75 window; see Methods/SI): the
`data/processed_s1s3/study{1_jailbroken,2_standard,3_truth_constrained}_clean.csv` files for
S1–3 (formerly `clean_Study{1,2,3}_cd_filt_direction.csv`) and the screened S4 conversations.
The resulting per-claim labels carry `study_source` and share **one identical schema** across
all four studies (`claim_text`, `direction`, `model_pooled`, `veracity_score`,
`stance_to_focal`, `directness_to_focal`, …), so the four studies are pooled on a common basis
(`read_claim_labels()` in `code/bunkbot_helpers.R` reads them and binds).

## The one study-specific step

Study 2's claims had pre-existing fact-checks from the earlier study; the
`run_study2_standard_existing_factcheck_recode*.sh` /
`run_study2_standard_existing_factcheck_eligibility_from_taxonomy.sh` scripts **re-harmonize**
those into the common schema above (hence the `s2s4_…_harmonized.csv` naming). The
codebook and output schema are identical to the other studies — only the entry point
differs (per-row model IDs in the shipped files are authoritative, as above).

`run_april04_gemini_flash_prep.sh` and `run_post_factcheck_accuracy_pipeline.sh` /
`model_accuracy_relation_suite.R` are kept as the S4 prep + accuracy-table builders that feed
the per-conversation datasets in `data/api_cached/claim_datasets/` (formerly `claim_conv/`).
Note: the `…_with_focal_item_final.csv` variant that two of these entry points default to
was produced by a builder pruned in the script consolidation; the kept pipeline builds the
non-`_with_focal_item_final` variant (pass `--participant-input`/args explicitly to re-run).
