# Conspiracy-topic taxonomy (embedding-based, pooled across Studies 1–4)

Rebuilds the conspiracy-topic classification with an embedding approach adapted
from an earlier internal pipeline, upgraded to
**Gemini Embedding 2** and run **once over all four studies pooled**. Supersedes the
old S1–3-only `dbscan_cluster_name` (left in place, no longer relied on).

All paths are repo-rooted: each script resolves `REPO_ROOT` by walking up to the directory
that contains both `data/` and `code/` (i.e. `bunkbot-public/`). Run with `Rscript` from
anywhere.

## Pipeline
1. `assemble_corpus.R` → `pooled_conspiracy_corpus.csv` (written **here**, THIS_DIR-relative,
   since `pooled_conspiracy_topics.R` reads it from here) — every participant's conspiracy
   restatement (`conRestatement`, fallback `conSummary`) across S1–S4 (N = 3,930), tagged
   with study / variant (S1–3 regime or S4 model) / condition / aligned belief change.
   - Shipped inputs read: `data/processed_s1s3/study{1_jailbroken,2_standard,3_truth_constrained}_clean.csv`
     and `data/api_cached/sharing_and_stance/study4_sharing_analysis_merged.csv`.
   - **Not shipped** (kept + flagged in the script): the S1–3 analysis frame `d` (old
     `code/figure_revamp/data_s13.R`, not in this repo) and the S4 analytic-strict frame
     (`s4_analytic_strict.csv`, built at runtime by `build_s4_data()` in
     `code/bunkbot_helpers.R`). To run standalone, materialise the S4-strict frame to
     `output/provenance_work/conspiracy_topics/s4_analytic_strict.csv` and provide `d`.
2. `pooled_conspiracy_topics.R` — embed via `google/gemini-embedding-2-preview` (OpenRouter
   `/embeddings`, 3072-dim, `taskType` clustering; key read from `.openrouter_key`; cached to
   the shipped `data/api_cached/topic_modeling/topic_embeddings_gemini.rds`) → PCA(30) →
   HDBSCAN grid (minPts 8–30). Selection prefers the **most retained clusters** (each ≥
   `MIN_CLUSTER_N`, default 20) under a 0.55 noise ceiling → minPts = 10, **18 named topics +
   ~47% Mixed/Unclassified**. Cluster ids are deterministic; labels assigned from 30 central
   exemplars via `LABEL_OVERRIDES`.
   - Re-run: `Rscript pooled_conspiracy_topics.R` (embeddings cached, so seconds).
   - Retune granularity: `BB_MIN_CLUSTER_N=30 Rscript ...` (fewer/larger) or `=15` (more/smaller).
   - Writes the shipped join key `data/api_cached/topic_modeling/topic_assignments.csv`
     (consumed by the analysis pipeline via `paths$topic_assignments`); all other outputs are
     THIS_DIR-relative working files.

## Outputs
- `data/api_cached/topic_modeling/topic_assignments.csv` (**shipped**) — minimal join key:
  response_id, study, cluster_id, topic. Consumed by the analysis pipeline.
- `pooled_with_clusters.csv` (this dir) — per-participant: response_id, study, variant,
  condition, belief_change, cluster_id, topic. **This is the unified, folded-in analysis frame.**
- `topic_by_study.csv`, `topic_by_condition.csv`, `topic_by_model_s4.csv`, `topic_effects.csv`,
  `cluster_exemplars.csv`, `hdbscan_grid_summary.csv` (all this dir, working files).

## Folding back into the analysis pipeline
- The shipped `data/api_cached/topic_modeling/topic_assignments.csv` is the canonical join key,
  read by the analysis pipeline via `paths$topic_assignments` (see `code/bunkbot_helpers.R`).
- **S1–3:** left-join `topic_assignments.csv` onto the S1–3 frame, so `d` carries a `topic`
  column (S1 605 / S2 426 / S3 450 named; rest Mixed).
- **S4:** join the (runtime-built) S4 analytic-strict frame to `topic_assignments.csv` by
  `ResponseId = response_id`:
  ```r
  ta <- readr::read_csv(paths$topic_assignments)
  s4 <- s4_strict |> dplyr::left_join(ta, by = c("ResponseId" = "response_id"))
  ```
- The old `dbscan_cluster_name` (11 clusters, S1–3 only) is superseded.

## Figures
- `fig_topic_composition.{png,pdf}` (`fig_topic_composition.R`) — topic composition heatmap by
  study/model; the 4 S4-model columns are near-identical (model randomized → balance check),
  the substantive variation is across studies (COVID falling, Moon Landing/AI/Epstein rising).
- `fig_topic_effects.{png,pdf}` (`fig_topic_effects.R`) — bunk-vs-debunk belief change by topic,
  pooled across studies (topics ≥ 40), ordered by the bunk−debunk gap. Model-level slicing is
  unestimable (median S4 topic×model×condition cell n = 3; topic×model interaction p = .59).
  Story: Area 51 is the one topic where bunking wins (+12.4 vs +4.4); Moon Landing is
  near-unbunkable (+1.2, CI crosses 0, vs debunking +13.4) — the "truthful ammunition" gradient.

Both figure scripts read the THIS_DIR working file `pooled_with_clusters.csv` but still
`source()` the figure house-style helper `code/figure_revamp/theme_bunkbot.R` and write to
`figures/revamp/` — **neither the helper nor that output tree is shipped in bunkbot-public**.
The references are kept and flagged with `TODO(provenance)` in the scripts; supply
`theme_bunkbot.R` (and an output dir) to render these figures.
