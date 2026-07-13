# Stance ensemble — human gold-standard validation

One human coder (TC) hand-coded a stratified gold sample of 150 Study-4 posts (pre + post),
blind to condition and to the ensemble's labels, following the same rubric given to the five
LLM raters. Codes are compared to the ensemble **consensus** (median of 5 models for the score;
majority for categorical fields). Of the hand-coded posts,
150 matched an ensemble record; 118 were rated applicable (non-blank stance) by both.

## Stance score (0-100)

- Human vs consensus Pearson r: **0.770** (n = 118)
- Spearman rho: **0.721**
- ICC(2,1) absolute agreement: **0.761**
- Mean absolute error: **11.4** points; mean signed (AI - human): 0.5; within 10 pts: 64%

## Directional stance (pro- vs anti-conspiracy; the main-text discretization)

- Agreement on pro/anti (posts both score off the 50 midpoint, n = 94): **87%**; Cohen's kappa **0.623**
- 3-way pro/neutral(=50)/anti agreement (n = 118): 72%; kappa 0.417

## Categorical fields (human vs consensus)

- stance_category (6 levels, n = 150): agreement 59%; kappa 0.502; quadratic-weighted kappa **0.640**
- response_type (n = 150): agreement 72%; kappa 0.619
- focal_relevance (n = 150): agreement 85%; kappa 0.606
- scoreable-vs-not_applicable (n = 150): agreement 97%; kappa 0.917
