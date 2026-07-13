# explanation_coding — multi-model coding of the persuasion open-ended items

Rigorous, multi-model LLM coding of the two open-ended persuasion questions asked of every
participant in Studies 1–4 (`Persuasive_oe`, `Notpersuasive_oe`), using a refined version of the
15-theme taxonomy from the legacy explanation-coding script (not shipped). Built to mirror the
`stance_v2` module's conventions (litellm → OpenRouter panel, temp 0, JSON-schema output, resumable
per-model JSONL caches, consensus + reliability).

**What's new vs. the legacy script:** one multi-label call per response (not 15 calls), a 5-model
panel with full inter-rater reliability instead of a single GPT-4o, no outcome leakage (the coder is
not told whether belief changed), explicit empty/meta handling, and both fields coded against the
full taxonomy. See `codebook.md` for the instrument and `taxonomy.py` for the canonical definitions.

## Panel
`anthropic/claude-sonnet-4.6`, `openai/gpt-5.2`, `google/gemini-3.1-pro-preview`,
`x-ai/grok-4.3`, `deepseek/deepseek-v3.2` (all via OpenRouter). Needs `OPENROUTER_API_KEY`
in the environment (preferred), or a local `.env` at the repo root / `code/` (see
`.env.example`). No real key is shipped.

## Paths
Scripts resolve the **repo root** automatically (the directory containing both `data/` and
`code/`), so they run from anywhere via `Rscript`/`python3`. They read shipped inputs from
`data/` and write:
- **shipped caches** (`analysis_frame.csv`, `*_item_ids.txt`, `*_consolidated.csv`) →
  `data/api_cached/explanation_coding/`. (The transient build inputs —
  `explanation_inputs.jsonl`, `explanation_roster.csv`, per-model `scores_<model>.jsonl` —
  are **not shipped**; they are rebuilt from the shipped raw data when the pipeline is re-run.);
- **derived analysis CSVs, reliability reports, and figures** → `output/provenance_work/explanation_coding/`
  (and `…/figures/`). These are not part of the shipped `data/` layout.

Inputs read from the shipped layout:
- S1–3 cleaned subject files → `data/processed_s1s3/study{1_jailbroken,2_standard,3_truth_constrained}_clean.csv.gz`
- S4 raw Qualtrics export → `data/raw_qualtrics/study4_social_sharing_raw.csv.gz`
- `export_analysis_frame.R` reuses the canonical builders in `code/bunkbot_helpers.R`.

## Files
| file | role |
|---|---|
| `taxonomy.py` | canonical 15 themes + response_quality + system prompt + JSON schema (source of truth) |
| `codebook.md` | human-readable codebook |
| `build_explanation_inputs.py` | → `explanation_inputs.jsonl` (scored items) + `explanation_roster.csv` |
| `export_analysis_frame.R` | → `analysis_frame.csv` (screened S1–4 frame: outcome, compliance) |
| `make_analysis_allowlist.py` | → `analysis_item_ids.txt` (items whose participant is in the frame) |
| `make_pilot_sample.py` | → `pilot_item_ids.txt` (stratified pilot) |
| `score_explanation.py` | the 5-model multi-label scorer → `scores_<model>.jsonl` |
| `consolidate_explanation.py` | majority-vote consensus → `explanation_consolidated.csv` |
| `summarize_explanation_reliability.py` | reliability report → `output/provenance_work/explanation_coding/explanation_reliability_*` |
| `analyze_explanations.R` | merge + prevalence + figures → `output/provenance_work/explanation_coding/explanation_theme_numbers.csv`, `…/figures/` |

Outputs/caches live in `data/api_cached/explanation_coding/`; reports/figures in
`output/provenance_work/explanation_coding/` (`…/figures/`).

## Run order
```bash
# 1. build inputs + screened frame + allowlist
python3 build_explanation_inputs.py
Rscript  export_analysis_frame.R
python3 make_analysis_allowlist.py

# 2. validate the panel emits valid JSON
python3 score_explanation.py --probe

# 3. pilot: score → consolidate → reliability, inspect, refine taxonomy.py if needed
python3 make_pilot_sample.py
python3 score_explanation.py                 --items pilot_item_ids.txt
python3 consolidate_explanation.py           --items pilot_item_ids.txt --out pilot_consolidated.csv
python3 summarize_explanation_reliability.py --items pilot_item_ids.txt --out-prefix explanation_pilot

# 4. full analysis-sample run (resumable; re-run safely to recover from interruptions)
python3 score_explanation.py        --items analysis_item_ids.txt
python3 consolidate_explanation.py  --items analysis_item_ids.txt
python3 summarize_explanation_reliability.py --items analysis_item_ids.txt

# 5. (optional) test-retest stability
shuf analysis_item_ids.txt | head -200 > data/api_cached/explanation_coding/retest_item_ids.txt
python3 score_explanation.py --replicate 1 --items retest_item_ids.txt

# 6. analysis + figures
Rscript analyze_explanations.R
```

## Notes
- **Resumable:** each model's JSONL is append-only and keyed by `sha256(model, prompt_version,
  replicate, item)`. Re-running skips already-OK items; safe after interruption.
- **Prompt versioning:** bump `PROMPT_VERSION` in `score_explanation.py` after any change to
  `taxonomy.py` so old/new codings don't mix (consolidate/reliability filter by `--prompt-version`).
- **Scope:** scoring is restricted to participants in the screened analysis frame
  (`analysis_item_ids.txt`, ~7.5k items). `explanation_inputs.jsonl` holds all substantive
  treatment-arm responses if you want to extend coverage.
