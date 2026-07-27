# Manuscript figures — output + code

This folder is the self-contained bundle of the **four main-text figures**. Figures
2–4 are data plots produced by one R script (`make_figures.R`); Figure 1 is a
hand-laid-out example-transcript infographic built by a separate Python/HTML
toolchain in [`figure1/`](figure1/).

## Contents

| File | Manuscript | What it shows |
|------|------------|---------------|
| `figure1/figure1_transcript.png` | **Figure 1** | Turn-based case study of one real bunking conversation (Study 2, standard GPT-4o, "chemtrails"): baseline → bunking dialogue (fabrications underlined) → corrective debrief, with the participant's 0–100 belief 49% → 99% → 10%. Built by the Python toolchain in `figure1/` (see its README) |
| `fig_study1_merged.{png,pdf}` | **Figure 2** | Study 1 (jailbroken GPT-4o): belief-change trajectory, exceedance curves, AI perceptions, trust/GCBS shifts (4 panels a–d) |
| `figure3_ATE_and_veracity_aligned.{png,pdf}` | **Figure 3** | Studies 1–3: per-variant bunk/debunk belief ATE forest + focal-relevant-claim veracity violins + low-veracity claim-count bars |
| `figure4_belief_and_posting.{png,pdf}` | **Figure 4** | Study 4 (four frontier models): per-model forests for belief change (a), increase in posting the AI's side (b), decrease in posting the opposing side (c) |
| `make_figures.R` | — | The R script that produces Figures 2–4 (PNG + PDF) |
| `CAPTIONS.md` | — | Nature-style captions for Figures 2–4 (Figure 1's caption lives in the manuscript) |

> **Figure 1** uses a different toolchain (Python + headless browser, not R) and its
> own data input; see [`figure1/README.md`](figure1/README.md). `make_figures.R`
> produces only Figures 2–4.

## Regenerating the figures

From the **repository root** (no API key required; this is the `make figures` target):

```bash
Rscript figures/manuscript/make_figures.R      # or: make figures
```

The script bootstraps the analysis engine the same way the analyses and SI do:

- If `output/_all_numbers.rds` exists (the cached recompute), it loads it — fast,
  a few seconds.
- Otherwise it calls `build_all_numbers()` and recomputes everything from
  `data/` (raw + de-identified) and `data/api_cached/` (frozen model annotations) —
  a few minutes, still API-free.

PNG + PDF for each of Figures 2–4 are written **into this folder**
(`figures/manuscript/`).

## Notes

- **Style.** These are the print-ready ("polished") figures: no in-figure titles or
  subtitles (that detail lives in `CAPTIONS.md`), Helvetica Neue typography, a
  high-contrast raspberry/blue palette, dot-and-interval displays, and export at a
  double-column print width (7.2 in). Data sources, estimators and sample
  definitions are identical to the analyses and SI — only styling/layout differ.
- **Figure 2 panel c/d.** The shipped Study-1 figure uses the original-style bar
  chart with flat CIs in panel c plus a coefficient plot in panel d.
- **Colours.** Bunking = raspberry `#C7375A`, debunking = blue `#0868AC`. Belief is
  on a 0–100 scale (reverse-coded; higher = more conspiracy belief); change is
  signed toward the AI's assigned position in both arms.
- **Models (Study 4):** Claude Opus 4.6, Gemini 3.1 Pro, Grok 4, GPT-5.2.
