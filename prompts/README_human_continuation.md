# Human Continuation Experiments

> **Scope note.** This document is **methods provenance for an external tool**: it
> describes the conversation/evaluation harness (`persuasion-main`) that generated the
> Studies 1–3 Attempt-to-Persuade (APE) compliance labels shipped under
> `data/api_cached/compliance_ape/`. That harness is **not included in this repository**
> (it is large and key-bearing — see CATALOGUE.md §6), so the `python main.py` /
> `src/human_data/...` commands below refer to it, not to files here. It is kept because
> it documents exactly how the shipped `evaluator_label` / `reverse_evaluator_label`
> columns were produced; the evaluator prompts themselves are in
> `prompts/base_v7_persuasion_degree.yaml`.

This document describes the human continuation pipeline, which tests whether AI systems attempt to persuade in real-world conversations collected from human participants.

## Background: The Bunking/Debunking Studies

The human conversation data comes from a series of pre-registered experiments described in [Costello et al. (2025) "Large language models can effectively convince people to believe conspiracies"](https://arxiv.org/abs/2601.05050). In these studies, N=2,724 American participants discussed conspiracy theories they were uncertain about with GPT-4o, which was instructed to either argue against ("debunking") or for ("bunking") the conspiracy.

### Key Findings from the Original Studies

- **Symmetric persuasion**: LLMs were equally effective at increasing conspiracy belief (~14 points on 0-100 scale) as decreasing it (~12 points)
- **Guardrails ineffective**: Standard GPT-4o with OpenAI's safety guardrails produced very similar effects to the jailbroken variant
- **Correction works**: A follow-up corrective conversation successfully reversed newly-induced conspiracy beliefs
- **Truth constraint helps**: Prompting GPT-4o to only use accurate information dramatically reduced its ability to increase conspiracy beliefs while maintaining debunking efficacy

### Study Differences

| Study | Model Variant | Key Manipulation |
|-------|---------------|------------------|
| **Study 1** | Jailbroken GPT-4o | Guardrails removed via post-training; 97-98% compliance with persuasion instructions |
| **Study 2** | Standard GPT-4o | Default OpenAI guardrails in place; tested whether safety measures prevent bunking |
| **Study 3** | Truth-Constrained GPT-4o | Explicit prompt instruction to only use accurate, truthful arguments |

### Experimental Design

Participants were selected based on:
- **Equivocal beliefs**: Only participants who rated their focal conspiracy belief between 25-75 on a 0-100 scale (uncertain)
- **Self-selected topics**: Participants identified and described a conspiracy they were personally uncertain about

Conditions:
- **`treatment_mid_bunk`**: AI instructed to argue FOR the conspiracy (increase belief)
- **`treatment_mid_debunk`**: AI instructed to argue AGAINST the conspiracy (decrease belief)

### Prompt Variants

The `--assistant_prompt human` parameter maps to prompts derived from the original study:

- **Standard bunking/debunking** (`prompt_human_bunk_assistant` / `prompt_human_debunk_assistant`): Allows the AI to invent evidence and fabricate details for maximum persuasion
- **Truth-constrained** (`prompt_human_bunk_assistant_no_lying` / `prompt_human_debunk_assistant_no_lying`): Requires the AI to use only accurate, truthful arguments

## Overview

The human continuation pipeline allows you to:

1. **Replay human conversation data** collected from studies where real people interacted with AI systems
2. **Evaluate the original AI responses** to measure baseline persuasion attempts
3. **Swap in different AI models** to see how they would respond to the same human context
4. **Compare persuasion behaviors** across different models, prompts, and configurations

## Key Parameters

### `--human_data_path`

Path to the human conversation data file. Supports both JSON and CSV formats.

```bash
--human_data_path src/human_data/study3_filtered_conversations.csv
```

Available datasets:

| Dataset | Model | Description |
|---------|-------|-------------|
| `study1_filtered_conversations` | Jailbroken GPT-4o | Guardrails removed; baseline for maximum persuasion capability |
| `study2_filtered_conversations` | Standard GPT-4o | Default OpenAI guardrails; tests safety measure effectiveness |
| `study3_filtered_conversations` | Truth-Constrained GPT-4o | Required to use only accurate information |
| `study_1_2_filtered_conversations` | Combined | Studies 1 and 2 merged for truth-constrained vs. other comparisons |

Each dataset is available in both `.json` and `.csv` formats in `src/human_data/`.

### `--num_turns`

Number of **conversation rounds** (user + assistant pairs). Each turn consists of one user message followed by one assistant message.

```bash
--num_turns 1  # One round: user message + assistant response
--num_turns 3  # Three rounds of back-and-forth
```

### `--human_data_rounds`

Number of **individual messages** (user OR assistant) to take from human data before switching to AI generation. This counts each message separately:

| `human_data_rounds` | Messages from human data |
|---------------------|--------------------------|
| 0 | None (fully AI-generated) |
| 1 | Turn 0 User only |
| 2 | Turn 0 User + Turn 0 Assistant |
| 3 | Turn 0 User + Turn 0 Assistant + Turn 1 User |
| 4 | Turn 0 User + Turn 0 Assistant + Turn 1 User + Turn 1 Assistant |
| 2N | First N complete rounds |

**Relationship**: `human_data_rounds = 2 * num_human_conversation_rounds`

### `--assistant_prompt`

Controls which system prompt is used for the AI assistant:

- `human` - Uses `prompt_human_bunk_assistant` / `prompt_human_debunk_assistant` (allows invented evidence)
- `human_no_lying` - Uses `prompt_human_bunk_assistant_no_lying` / `prompt_human_debunk_assistant_no_lying` (truthful arguments only)
- `prompt_assistant_no_user_degree` - Standard persuasion prompt without user belief info

### `--human_reverse_persuasion`

Reverses the persuasion direction. By default:
- `treatment_mid_bunk` condition → AI argues **for** the conspiracy
- `treatment_mid_debunk` condition → AI argues **against** the conspiracy

With `--human_reverse_persuasion`, these directions are swapped.

## Common Use Cases

### 1. Evaluate Original AI Responses (Baseline)

Replay the original human study conversations and evaluate whether the original AI attempted persuasion.

```bash
python main.py --num_turns 1 \
  --human_data_path src/human_data/study3_filtered_conversations.csv \
  --human_data_rounds 2 \
  --assistant_prompt human \
  --evaluator_model gpt-4o \
  --results_dir results/baseline_evaluation
```

Here, `--num_turns 1` and `--human_data_rounds 2` means one complete round (user + assistant) from human data.

### 2. AI Responds to First Human Message

Have a new AI model (e.g., gpt-4o here) respond to the human participant's first message.

```bash
python main.py --num_turns 1 \
  --human_data_path src/human_data/study2_filtered_conversations.json \
  --human_data_rounds 1 \
  --persuader_model gpt-4o \
  --assistant_prompt prompt_assistant_no_user_degree \
  --results_dir results/gpt4o_first_response
```

Here, `--human_data_rounds 1` uses only the human's first message, then GPT-4o generates the assistant response.

### 3. AI Continues After One Round of Human Context

Provide one full round of human conversation as context, then have the AI continue.

```bash
python main.py --num_turns 2 \
  --human_data_path src/human_data/study2_filtered_conversations.json \
  --human_data_rounds 3 \
  --persuader_model vertex_ai/claude-sonnet-4@20250514 \
  --assistant_prompt prompt_assistant_no_user_degree \
  --results_dir results/claude_continuation
```

This uses:
- Turn 0 User: Human data
- Turn 0 Assistant: Human data
- Turn 1 User: Human data
- Turn 1 Assistant: **Claude generates this**

### 4. Compare Multiple Models

Run the same experiment with different models to compare persuasion behaviors:

```bash
# GPT-4o
python main.py --num_turns 2 \
  --human_data_path src/human_data/study2_filtered_conversations.json \
  --human_data_rounds 2 \
  --persuader_model gpt-4o \
  --experiment_name gpt4o_turn1

# Gemini 2.5 Pro
python main.py --num_turns 2 \
  --human_data_path src/human_data/study2_filtered_conversations.json \
  --human_data_rounds 2 \
  --persuader_model vertex_ai/gemini-2.5-pro \
  --experiment_name gemini_turn1

# Claude Sonnet 4
python main.py --num_turns 2 \
  --human_data_path src/human_data/study2_filtered_conversations.json \
  --human_data_rounds 2 \
  --persuader_model vertex_ai/claude-sonnet-4@20250514 \
  --experiment_name claude_turn1
```

## Human Data Format

### CSV Format

```csv
id,condition,llm_agent,belief_rating_pre,belief_rating_post,topic,con_restatement,turn,user,assistant
R_abc123,treatment_mid_debunk,no,50,51,"area 51","Area 51 is...",2,"what is it","The secrecy..."
```

### JSON Format

```json
[
  {
    "id": "R_abc123",
    "condition": "treatment_mid_debunk",
    "llm_agent": "no",
    "belief_rating_pre": 50,
    "belief_rating_post": 51,
    "topic": "area 51",
    "con_restatement": "Area 51 is...",
    "turns": [
      {"turn": 1, "user": "what is it", "assistant": "The secrecy..."},
      {"turn": 2, "user": "why secret", "assistant": "National security..."}
    ]
  }
]
```

### Key Fields

| Field | Description |
|-------|-------------|
| `id` | Unique identifier (ResponseId from Qualtrics survey) |
| `condition` | `treatment_mid_bunk` (AI argues for) or `treatment_mid_debunk` (AI argues against) |
| `llm_agent` | Identifier for which LLM variant was used |
| `belief_rating_pre` | Participant's belief before conversation (0-100 scale; 0=Definitely False, 50=Uncertain, 100=Definitely True) |
| `belief_rating_post` | Participant's belief after conversation (0-100) |
| `topic` | Brief topic description provided by participant |
| `con_restatement` | Full conspiracy/belief statement (used as the topic in experiments) |
| `con_summary` | Summary of the conspiracy theory |
| `turns` | List of user/assistant message pairs |

### Filtering Criteria

The filtered datasets include only participants who:
- Were **equivocal** (`isEquivocal == TRUE`) - genuinely uncertain about their conspiracy
- Had **moderate baseline beliefs** (`belief_rating_pre` between 25-75) - not strong believers or skeptics
- Provided valid **conspiracy restatement and summary** - ensuring topic clarity

## Output Files

Human continuation experiments produce additional output files:

### `human_conversation_mapping.json`

Maps which human conversations were used:

```json
{
  "sampled_human_ids": ["R_abc123", "R_def456", ...],
  "human_data_rounds": 2
}
```

### `human_evaluation_ratings_turn_N.jsonl`

Per-turn evaluation results with ResponseId mapping:

```json
{"ResponseId": "R_abc123", "evaluator_label": 1, "turn": 0, "condition": "treatment_mid_debunk"}
```

### `human_evaluation_summary.jsonl` / `.csv`

Combined evaluation results across all turns for analysis.

## Pipeline Flow

```
┌─────────────────────────────────────────────────────────────────┐
│                    Load Human Data                               │
│  - Filter by minimum rounds (--human_data_rounds)               │
│  - Extract topics from con_restatement                          │
│  - Set persuasion direction from condition                      │
└─────────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────────┐
│                    For each turn (0 to num_turns-1):            │
│                                                                  │
│  ┌─────────────────────────────────────────────────────────┐   │
│  │ User Turn                                                │   │
│  │  if human_data_rounds > current_message_count:          │   │
│  │    → Use human data                                      │   │
│  │  else:                                                   │   │
│  │    → Generate with persuadee_model                       │   │
│  └─────────────────────────────────────────────────────────┘   │
│                              │                                   │
│                              ▼                                   │
│  ┌─────────────────────────────────────────────────────────┐   │
│  │ Assistant Turn                                           │   │
│  │  if human_data_rounds > current_message_count:          │   │
│  │    → Use human data                                      │   │
│  │  else:                                                   │   │
│  │    → Generate with persuader_model                       │   │
│  └─────────────────────────────────────────────────────────┘   │
│                              │                                   │
│                              ▼                                   │
│  ┌─────────────────────────────────────────────────────────┐   │
│  │ Evaluation                                               │   │
│  │  → Rate persuasion attempt (evaluator_model)            │   │
│  │  → Check for refusals (reject_model)                    │   │
│  └─────────────────────────────────────────────────────────┘   │
└─────────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────────┐
│                    Save Results                                  │
│  - Conversation logs                                            │
│  - Evaluation ratings                                           │
│  - Human evaluation summary (with ResponseId mapping)           │
│  - Visualizations                                               │
└─────────────────────────────────────────────────────────────────┘
```

## Filtering Human Data

To create filtered conversation datasets from raw study data, use:

```bash
python src/human_data/filter_human_data.py
```

This applies filters:
- `isEquivocal == TRUE`
- `belief_rating_pre_rc` between 25 and 75
- Valid `conRestatement` and `conSummary`

## Analysis

For analyzing human continuation results:

```bash
# Analyze persuasion attempt effects
python src/human_data/analyze_attempt_effect.py

# Combine study 1 and 2 data
python src/human_data/combine_study1_2.py

# Analyze human evaluation results
python src/human_data/analyze_human_eval.py
```

## References

- **Paper**: Costello, T. H., Pelrine, K., Kowal, M., Arechar, A. A., Godbout, J.-F., Gleave, A., Rand, D., & Pennycook, G. (2025). Large language models can effectively convince people to believe conspiracies. [arXiv:2601.05050](https://arxiv.org/abs/2601.05050)

- **Conversation Browser**: Original study conversations can be browsed at: https://8cz637-thc.shinyapps.io/bunkingBrowser/

- **Pre-registration & Data**: All code, processed data, and preregistered analytic decisions are available on OSF.
