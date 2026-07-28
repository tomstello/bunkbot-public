# Persuasion-Explanation Coding — Codebook

Multi-label LLM coding of the two open-ended persuasion items asked of every participant in
Studies 1–4 of the Bunkbot paper:

- **`Persuasive_oe`** (QID142): *"In just a sentence or two, would you mind explaining what about
  the AI's comments, if anything, you found to be persuasive?"*
- **`Notpersuasive_oe`** (QID75): *"In just a sentence or two, would you mind explaining what about
  the AI's comments you did not find to be persuasive?"*

The 15-theme taxonomy is carried over (refined, not replaced) from the legacy
explanation-coding script (not shipped). The canonical machine-readable definitions live in
`taxonomy.py`; this file is the human reference. Each response is coded **once** by a panel of five
frontier models (one call returns all labels), and labels are reconciled by majority vote.

## How a response is coded

1. **`response_quality`** is assigned first:
   | value | meaning |
   |---|---|
   | `substantive` | at least one interpretable reason about the AI / its arguments — **themes coded only here** |
   | `meta_or_off_topic` | only comments on the survey / study / task / general AI experience |
   | `no_answer` | blank, "n/a", "none", "nothing", refusal |
   | `unclassifiable` | gibberish / too fragmentary |

2. For a **substantive** response, each of the 15 themes is judged **independently** as present/absent
   (a response can carry several, one, or none).

3. **`primary_theme`** = the single most salient theme key (or `none`).

4. **`evidence_quote`** (≤25 words, verbatim), **`rationale`** (≤40 words), **`confidence`** (0–1).

### Coding rules (key best-practice choices)
- **Avoid false positives.** Mark a theme true only when the text clearly supports it; when in doubt, false. (Inherited from the legacy instrument.)
- **Outcome is withheld.** Unlike the legacy script, the coder is *not* told whether the conversation
  changed the participant's belief, and is instructed not to infer it — this removes outcome leakage
  that could bias coding.
- **Both boxes, full taxonomy.** Either question box may contain either praise (`+`) or criticism
  (`−`); coding is by content, not by which box the text came from.
- **Topic description is for reference resolution only**, never coded as the participant's reasoning.

## The 15 themes

### "+" themes — what the participant found persuasive / praiseworthy
| key | code | name | gist |
|---|---|---|---|
| `evid_pos` | EVID+ | Evidence / Facts / Logic / Counterarguments | concrete data/stats/facts; logic/common sense; AI addressed their doubts |
| `expt_pos` | EXPT+ | Expertise / Credibility / Trustworthiness | AI knowledgeable/expert; reliable/unbiased/neutral; trustworthy sources; unique AI access to truth |
| `emph_pos` | EMPH+ | Empathy / Politeness / Respectfulness | nonjudgmental/validating; polite/kind; felt calmer/supported |
| `detl_pos` | DETL+ | Detailed / In-Depth / Novel / Tailored | thorough/in-depth; new perspectives/data; point-by-point; personalized |
| `conx_pos` | CONX+ | Conspiracy-Specific Mechanisms | specific mechanisms/details of the theory; meta-point (couldn't stay hidden) |
| `priv_pos` | PRIV+ | Privacy / Non-judgmental space | could ask freely without fear of judgment |

### "−" themes — what the participant did NOT find persuasive / criticisms
| key | code | name | gist |
|---|---|---|---|
| `lack_neg` | LACK− | Perceived lack of AI disagreement | AI just reaffirmed what they already believed |
| `deep_neg` | DEEP− | Repetition / Lack of novelty / depth | repeated familiar args; echoed them; didn't challenge |
| `angr_neg` | ANGR− | Offensiveness / Anger | participant angry/annoyed/upset |
| `evid_neg` | EVID− | Insufficient evidence / no sourcing | lacked hard data, evidence, or sources |
| `bias_neg` | BIAS− | Perceived bias | skewed toward one narrative/agenda; ignored alternatives |
| `mech_neg` | MECH− | Impersonal tone / Verbosity / Lack of nuance | long-winded, mechanical, robotic, overly formal |
| `trst_neg` | TRST− | Distrust in AI as a source | machine nature/limits/programming make it unconvincing or untrustworthy |
| `offt_neg` | OFFT− | Off-topic / Superficial engagement | strayed from the core issue; too general/superficial |
| `emot_neg` | EMOT− | Emotional / Experiential disconnect | lacked personal/emotional/lived dimension |

## Panel & reconciliation
- **Panel (same 5 raters as `stance_v2`):** `anthropic/claude-sonnet-4.6`, `openai/gpt-5.2`,
  `google/gemini-3.1-pro-preview`, `x-ai/grok-4.3`, `deepseek/deepseek-v3.2` (via OpenRouter, temp 0).
- **Consensus:** per-theme majority vote across raters who returned a label; `response_quality` and
  `primary_theme` by plurality (confidence-weighted tiebreak).
- **Reliability:** per-theme Krippendorff's α (nominal) + Fleiss' κ + pairwise % agreement, pooled
  metrics, test-retest, and leave-one-rater-out stability (see `summarize_explanation_reliability.py`).

## Pipeline (scripts in this directory)
```
build_explanation_inputs.py   →  data/api_cached/explanation_coding/explanation_inputs.jsonl
make_pilot_sample.py          →  pilot_item_ids.txt (stratified)
score_explanation.py          →  scores_<model_slug>.jsonl   (per model, resumable)
consolidate_explanation.py    →  explanation_consolidated.csv
summarize_explanation_reliability.py → output/provenance_work/explanation_coding/explanation_reliability_report.md (+ metrics csv)
analyze_explanations.R        →  output/provenance_work/explanation_coding/explanation_theme_numbers.csv + figures
```
