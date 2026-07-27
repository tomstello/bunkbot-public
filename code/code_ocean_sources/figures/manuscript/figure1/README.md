# Figure 1 — example bunking transcript (output + code)

Figure 1 is a **graphical case study**, not a data plot: it lays out one real
participant's conversation (**Study 2, standard/guardrailed GPT-4o**, the "chemtrails"
conspiracy) as a compact, caption-free, two-column chat infographic. The participant's
own 0–100 confidence that the claim is true is shown at three points — **49% baseline →
99% after the bunking chat → 10% after the corrective debrief** — and the claims the
model fabricated are underlined inline (red) and keyed as fact-checked-false. It is
built from an HTML template rendered to a PNG with a headless browser — a different
toolchain from the R script that makes Figures 2–4.

> **Provenance correction.** Earlier copies of this figure (and the LaTeX caption)
> labelled it "Study 1 / jailbroken GPT-4o." That is wrong: participant
> `R_5QGGSlaD13tJwt6` appears only in `study2_standard_clean.csv.gz` with
> `condition = treatment_mid_bunk` — the **standard, guardrailed** GPT-4o in **Study 2**,
> bunking arm. (The standard-model framing is also the stronger story: even the default
> guardrailed model drove belief 49 → 99.)

## The figure

`create_final_visual.py` builds **the manuscript figure** (`figure1_transcript.png`):
compact, caption-free; two columns; strict turn-based (participant ↔ GPT-4o) chat;
slider readouts for the three belief ratings; **light-red** bunking bubbles + light-green
debrief bubbles; **four** fact-checked-false claims underlined inline, spread across the
model's first two replies (the second selectively quoted with " … "). It produces the
belief arc 49 → 99 → 10. `screenshot_final_visual.py` renders `final_visual.html` →
`figure1_transcript.png`.

> A fuller, portrait "detailed" variant was explored during design but was not adopted
> and is not included here.

## Contents

| File | Role |
|------|------|
| `figure1_transcript.png` | **Figure 1** (the manuscript image) |
| `final_visual.html` | the rendered HTML the PNG is a screenshot of |
| `create_final_visual.py` | builds the manuscript figure |
| `screenshot_final_visual.py` | screenshots `final_visual.html` → `figure1_transcript.png` (headless Chromium, `device_scale_factor=3.0`, full page) |

## Data inputs

The build joins two files on `ResponseId` (`TARGET_ID = "R_5QGGSlaD13tJwt6"`):

- **Primary transcript + baseline/post belief** — the persuasion-corpus CSV
  `transcripts and related information.csv` (≈31 MB; **not bundled**, held by the
  authors — size + raw transcript content).
- **Post-debrief belief + the debrief turns** — the in-repo
  `data/processed_s1s3/study2_standard_clean.csv.gz` (fields `belief_rating_debrf_4`,
  `debrief_content_assistant_1..4`, `debrief_content_user_2..4`).

Both paths are resolved automatically (the script searches upward from its own
location). If the external transcripts CSV is absent, the script **falls back to the
in-repo `study2_standard_clean.csv.gz` for everything**, so it still builds from public
data alone.

## Rebuilding

Requires Python with `playwright` (and its Chromium download):

```bash
pip install playwright && playwright install chromium
python create_final_visual.py        # -> final_visual.html
python screenshot_final_visual.py     # -> figure1_transcript.png
```

The scripts resolve all paths relative to themselves, so they can be run from any
working directory. `final_visual.html` pulls the Google "Manrope" web font and the two
chat avatars (a GPT-4 image and a participant icon) from the network, so the screenshot
step needs network access (it already waits for `networkidle`). The chat bubbles flatten
the models' light markdown (`###`, `**`) to clean prose and truncate long AI turns at
sentence boundaries.

> **Placement note for submission:** this figure is designed to be reproduced at **full
> page width (~180 mm)**; do not down-scale it to a single column, or the smallest
> labels drop below ~5 pt.

## Provenance note

Figure 1 predates the R figure pipeline (Figs 2–4) and was developed in this
separate HTML-to-screenshot toolchain; the code here is the original build.
