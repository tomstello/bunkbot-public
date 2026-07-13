# Stance v2.2 reliability report

3690 items x 5 raters (claude-sonnet-4.6, gpt-5.2, gemini-3.1-pro, grok-4.3, deepseek-v3.2), prompt v2.2, canonical affirms-phrased focal claims.

## Stance scores (0-100)

- Krippendorff's alpha (interval): **0.910**
- ICC(2,k): **0.982**
- pairwise r: mean **0.918**, range 0.885-0.949

## Categorical fields

- stance_category: alpha (nominal) **0.847**; unanimous 54%; >=4/5 75%
- response_type: alpha (nominal) **0.872**; unanimous 71%; >=4/5 84%
- focal_relevance: alpha (nominal) **0.819**; unanimous 84%; >=4/5 93%

## Score dispersion by consensus response type

- assertion: mean cross-rater SD 6.5 (n=2140)
- question_raising: mean cross-rater SD 4.7 (n=495)
- uncertainty_statement: mean cross-rater SD 3.9 (n=366)
- mixed_assertion_question: mean cross-rater SD 6.6 (n=328)
- meta_task: mean cross-rater SD 6.8 (n=20)
- unclassifiable: mean cross-rater SD 11.6 (n=7)
- declines_to_post: mean cross-rater SD 2.3 (n=4)

## Within-rater test-retest (200 duplicate items)

- claude: score r = **0.993**, category agreement 96%, |diff|<=5 in 97% (n=200)
- gpt: score r = **0.966**, category agreement 86%, |diff|<=5 in 85% (n=200)
- gemini: score r = **0.996**, category agreement 96%, |diff|<=5 in 96% (n=200)
- grok: score r = **0.963**, category agreement 86%, |diff|<=5 in 88% (n=200)
- deepseek: score r = **0.919**, category agreement 83%, |diff|<=5 in 85% (n=183)

## Leave-one-rater-out consensus stability

- without claude: r(consensus, LOO-consensus) = 0.994
- without gpt: r(consensus, LOO-consensus) = 0.994
- without gemini: r(consensus, LOO-consensus) = 0.993
- without grok: r(consensus, LOO-consensus) = 0.994
- without deepseek: r(consensus, LOO-consensus) = 0.993

## v1 (Gemini Flash, single) vs v2.2 consensus

- r = **0.923** over 3319 jointly scored items
- exactly-50 share: v1 26.4% -> v2.2 14.9%
- v2.2 not_applicable: 371 items (10.1%) — meta/off-topic posts that v1 scored as fake-neutral 50s

## Consensus response-type distribution

- pre: assertion 927 (50%), question_raising 379 (21%), mixed_assertion_question 249 (13%), uncertainty_statement 224 (12%), declines_to_post 45 (2%), meta_task 11 (1%), unclassifiable 10 (1%)
- post: assertion 1290 (70%), uncertainty_statement 143 (8%), meta_task 134 (7%), question_raising 130 (7%), mixed_assertion_question 80 (4%), declines_to_post 43 (2%), unclassifiable 25 (1%)

## Human gold validation

One human coder hand-coded a stratified gold sample of 151 posts (enriched for hard cases:
questions, meta posts, and high-dispersion items). Against the ensemble consensus: stance-score
Pearson r = 0.731, ICC(2,1) = 0.725, MAE = 12.0 pts (n = 119 applicable); pro-/anti-conspiracy
direction agreement 86% (kappa 0.60, n = 95); applicability (scoreable vs not) agreement 97%
(kappa 0.92). See `stance_gold_validation_report.md` (run `code/stance_gold_validation.R`).
