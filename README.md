# Bunkbot — reproduction package

**"AI can effectively promote conspiracies unless it is truth constrained."**

This repository reproduces **every number, table, and figure** reported in the main text
and Supplementary Information of the Bunkbot paper — four experiments, three of them
pre-registered (Studies 1–4, *N* = 3,996), testing whether large language models can both **instil
("bunk")** and **dispel ("debunk")** conspiracy beliefs — end to end from de-identified
data and cached model annotations.

Everything is produced from two documents — [`results_methods.Rmd`](results_methods.Rmd)
(main-text Methods + Results) and [`supplement/Bunkbot_SI.Rmd`](supplement/Bunkbot_SI.Rmd) (the
Supplementary Information) — that **share one live recompute** (`code/R/build_all_numbers.R` →
`ALL_NUMBERS`). Each pulls every in-text value, table cell, and figure from that single table.
**No rendered number is hard-coded and no "answer" file is ever read at render time** — if a
number appears in either document, an expression over the data produced it, so a main-text
value can never disagree with its SI counterpart. (The developer tripwire `code/dev_qa.R`
keeps a small set of audited anchor values, but those exist to *check* the recompute, never
to feed the documents.)

The SI carries the analyses that map directly onto claims in the paper, plus their robustness
tests.

```
make results              # render results_methods.Rmd  -> output/{html,pdf,docx}   (~10–20 min first run, no API key)
make supplement           # render the supplement       -> output/{html,pdf,docx}   (no API key)
make test                 # developer regression check (code/dev_qa.R)
make manuscript-check     # verify the Word manuscript's statistics against the recompute (needs a local manuscript copy)
make manuscript-wire      # write "<manuscript> (wired).docx" with corrections as tracked changes
```

The Word manuscript itself is wired to the same recompute: every reported
statistic has an anchored entry in [`code/manuscript_wiring/`](code/manuscript_wiring/)
(see its README), so `make manuscript-check` proves the prose agrees with the
pipeline and `make manuscript-wire` redlines anything that drifts. The manuscript
docx is **not distributed** with this repository — those two targets are for
checking a local copy (`--docx <path>`).

## The studies

| Study | Label | Model | Pre-registration |
|---|---|---|---|
| 1 | Jailbroken | GPT-4o with guardrails removed | AsPredicted #218585 |
| 2 | Standard | GPT-4o, default safeguards | AsPredicted #224184 |
| 3 | Truth-Constrained | GPT-4o instructed to use only accurate arguments | **none — exploratory** |
| 4 | Frontier models | Claude Opus 4.6, Gemini 3.1 Pro, GPT-5.2, Grok 4 (2×4 design, + social-media sharing) | AsPredicted #282517 |

> **Study 3 was not preregistered.** Studies 1, 2, and 4 are preregistered (AsPredicted
> numbers above; PDFs in [`pre-regs/`](pre-regs/)). Study 3 is exploratory.

## API-free by default

The default reproduction needs **no API key and makes no network calls.** Every
LLM-derived input — claim extraction/veracity, claim role/stance, Attempt-to-Persuade
(APE) compliance, 5-rater post-stance, and topic embeddings — is read from the frozen
files committed under [`data/api_cached/`](data/api_cached/). These cached annotations are
*measurement data*, on the same footing as a survey response: regenerating them needs paid
model access, so they are shipped, not recomputed, on the default path.

| Input family | Folder | Reproducible offline? |
|---|---|---|
| Raw survey responses (S1–3 processed + raw, S4 raw Qualtrics; gzipped) | `data/processed_s1s3/` + `data/raw_qualtrics/` | yes (this *is* the raw data) |
| Qualtrics instruments (4 QSFs) | `data/survey_definitions/` | yes (parsed as text) |
| APE compliance (all 4 studies) + coverage-gap rescore | `data/api_cached/compliance_ape/` | frozen judgments — ship + cite |
| 5-rater post-stance + restatement orientation | `data/api_cached/sharing_and_stance/` | needs paid API (OpenRouter) |
| Per-claim role / veracity labels + claim census | `data/api_cached/{claim_labels,claim_datasets,claim_veracity}/` | needs paid API (Perplexity sonar-pro) |
| Topic embeddings + assignments | `data/api_cached/topic_modeling/` | embeddings need paid API; assignments derive offline from them |
| Explanation-theme codes | `data/api_cached/explanation_coding/` | needs paid API |
| Human gold-coding + classifier reliability | `data/validation/` | frozen judgments — ship + cite |

Regenerating any cached family is an **explicit, key-gated opt-in** — see
`make regenerate-api`, the provenance pipelines under
[`code/provenance/`](code/provenance/), and [`prompts/.env.example`](prompts/.env.example).
**Never commit an API key.** A gitleaks config ([`.gitleaks.toml`](.gitleaks.toml)) and
`make scan` are provided.

## Layout

```
results_methods.Rmd     main-text Methods + Results, driven by ALL_NUMBERS
supplement/             Bunkbot_SI.Rmd + sections/ — the Supplementary Information (same ALL_NUMBERS)
figures/manuscript/     make_figures.R — the 3 main-text data figures (Figs 2–4) + figure1/ (Fig 1 toolchain) + CAPTIONS.md
code/
  bunkbot_helpers.R     data + inference engine (all 4 studies; API-free)
  R/                    numbers layer (study-symmetric) + SI ext_* modules + access/figures
    build_all_numbers.R assembles the one all_numbers table (single plain function, no pipeline framework)
    numbers_s4.R        S4 number generator — a PEER of compute_s13_numbers (no privileged S4 file)
  sections/             child documents of results_methods.Rmd (methods + per-study results)
  figures/              placeholder (.gitkeep); the manuscript figures live in top-level figures/manuscript/
  dev_qa.R              developer regression tripwire (never read by the document)
  provenance/python_api the pipelines that PRODUCED the cached annotations (read-only)
data/                   raw_qualtrics/ + processed_s1s3/ + survey_definitions/ + api_cached/ + validation/
                        (committed; files that would exceed GitHub's 100 MB limit ship gzipped — see CATALOGUE.md)
prompts/                persuader/evaluator prompt YAML + claim/explanation codebooks
pre-regs/               3 AsPredicted PDFs (Study 3 has none)
output/                 rendered artefacts (gitignored)
```

## Data ethics & de-identification

All four studies were run with participant consent under MIT COUHES protocol E-6485
(exempt determination); participants were debriefed (including a corrective debunking
conversation after bunking arms).
The shipped data are de-identified: Qualtrics identity/geolocation fields are removed,
panel respondent IDs are replaced by salted-hash pseudonyms, and free-text
self-identifications are redacted — see the **De-identification** note at the top of
[`CATALOGUE.md`](CATALOGUE.md) for exactly what was transformed. Conversation
transcripts are participants' own words about conspiracy beliefs; do not attempt to
re-identify participants.

## How a number is produced

`code/R/build_all_numbers.R` sources the engine and every numbers/extension module,
builds `all_numbers` (Studies 1–4 + pooled) from `data/`, and exposes it to the document
as `ALL_NUMBERS`. Prose then pulls single values, e.g.

```r
est("belief_change", model = "Jailbroken", direction = "Bunking")   # 13.68
```

`code/dev_qa.R` cross-checks the live recompute against a small set of audited anchor
values. Those anchors are an external tripwire **only**; the rendered document never reads
them.

## Requirements

R (≥ 4.5) with the packages in `renv.lock` (`Rscript -e 'renv::restore()'`), plus pandoc
and a LaTeX engine (xelatex) for PDF. Key packages: tidyverse, sandwich, car, dbscan,
lme4/lmerTest, emmeans, marginaleffects, broom(.mixed), jsonlite, scales, patchwork,
ggtext, knitr, kableExtra, bookdown, yaml.

Python is optional for the API provenance and manuscript-wiring utilities. Their direct
dependencies and API-client compatibility are pinned in `requirements-python.txt`
(`python3 -m pip install -r requirements-python.txt`). The default R reproduction does not
require Python or an API key.

## Citation

See [`CITATION.cff`](CITATION.cff).
