# Code Ocean capsule setup

This repository is designed to be imported into a Code Ocean Compute Capsule
without API credentials. The default run uses the de-identified survey data and
frozen model annotations committed under `data/`.

## Import

For a Nature Portfolio submission, create the account or capsule through the
publisher-specific invitation link supplied by Nature. In the Open Science
Library, choose **Copy from Git** and enter:

`https://github.com/tomstello/bunkbot-public.git`

Import the final paper-associated branch or merge it to the repository's default
branch before importing. Code Ocean maps the repository's `code/` and `data/`
folders to the capsule's corresponding folders. Other top-level files appear in
the capsule UI but are not mounted during a Reproducible Run. For that reason,
the executable document, figure, and deployed-prompt sources are mirrored under
`code/code_ocean_sources/`; `code/run` assembles the ordinary repository layout
in `/scratch` before rendering.

## Environment

Select the newest Python and R (JupyterLab/RStudio) starter environment available
in Code Ocean (R 4.4.2 as of July 2026). The environment must include:

- the R packages and versions in `renv.lock`;
- Pandoc;
- XeLaTeX, `texlive-latex-extra`, `texlive-plain-generic`, and recommended
  LaTeX fonts;
- standard build tools plus the Linux development libraries needed by `curl`,
  `openssl`, `xml2`, `nloptr`, `ragg`, `systemfonts`, and `textshaping`.

Add the required R packages through Code Ocean's **R (CRAN)** package manager,
using the versions recorded in `renv.lock`. Also add `irr` 0.84.1, `mice` 3.18.0,
`psych` 2.5.6, and `rstan` 2.32.7, which are used by the reliability and Study 4
multiple-imputation diagnostics. Add Pandoc and the XeLaTeX packages through
`apt-get`. Code Ocean's post-install stage cannot access capsule folders, so it
cannot restore the repository lockfile directly.

The reproducible run itself makes no network calls and requires no secrets.

## Run

Set `code/run` as the capsule's run file. It:

1. performs one live API-free numerical rebuild;
2. renders the reproducible main Results/Methods document;
3. renders the Supplementary Information;
4. rebuilds the manuscript figures; and
5. copies the rendered artifacts and a SHA-256 manifest to `/results`.

The numerical cache is deliberately excluded from `/results`; it is a large
intermediate artifact that is rebuilt from the shipped inputs.

Whenever the canonical R Markdown or figure sources change, refresh the mounted
Code Ocean mirror before committing:

```bash
code/sync_code_ocean_sources.sh
```

## Recommended resources

Use a CPU instance with at least 16 GB RAM and allow at least 60 minutes for the
first reproducible run. The causal-forest and multiple-imputation components are
the most computationally intensive steps.

## Metadata

- **Title:** Bunkbot: reproduction package for “AI can effectively promote
  conspiracies unless it is truth constrained”
- **Description:** API-free reproduction package for four experiments testing
  whether large language models can instil and dispel conspiracy beliefs.
- **License:** MIT for code; CC BY 4.0 for data and documentation.
- **Authors and keywords:** copy from `CITATION.cff`.

Keep the capsule private during peer review. After acceptance, submit the tested
capsule for Code Ocean publication and DOI minting.

## Nature availability text

Before a DOI is issued:

> Code and de-identified data sufficient to reproduce all reported analyses,
> tables, and figures are available to editors and reviewers in a private Code
> Ocean Compute Capsule. The capsule performs an API-free reproducible run from
> frozen inputs.

After publication, replace the private-capsule wording with the Code Ocean DOI
and cite the versioned capsule in the reference list.
