# Manuscript wiring — the Word draft mirrors the recompute

The manuscript stays a Word document (that's where the formatting and the
co-author workflow live), but every reported statistic in it is **wired** to the
same live recompute that drives the SI: `build_all_numbers()` → `ALL_NUMBERS`.

> **Authors-only tooling.** The manuscript docx is not distributed with the
> public repository, and neither are the wiring working files
> (`manifest_spec.csv`, `unwired.md`) — so `make manuscript-check` /
> `manuscript-wire` only run on an authors' checkout. The toolchain and
> `wiring_map.yaml` ship so the verification method itself is public.

```
                 code/manuscript_wiring/manifest_spec.csv     (key -> block/term/format)
ALL_NUMBERS  ->  code/manuscript_numbers.R                    (pull + format each key)
                 -> output/manuscript_numbers.csv             (key -> value_formatted)

                 code/manuscript_wiring/wiring_map.yaml       (key -> anchor in the docx)
docx + manifest -> code/manuscript_wiring/check_manuscript.py
                 -> output/manuscript_check_report.{md,csv}   (check mode)
                 -> "<manuscript> (wired).docx"               (redline/clean modes)
```

## Everyday use

```bash
make manuscript-check   # refresh manifest, classify every wired value:
                        #   OK / STALE / NOT_FOUND / AMBIGUOUS / NO_KEY
make manuscript-wire    # same, then write "<manuscript> (wired).docx" where each
                        # STALE value is a Word tracked change (author "Bunkbot
                        # pipeline"). The original docx is NEVER modified.
```

Open the wired copy in Word, review the tracked changes, accept what you agree
with, and carry the accepted text into the working draft (or keep editing the
wired copy — it IS the draft plus corrections). Rejecting every change provably
reconstructs the original text.

## The three layers

1. **`manifest_spec.csv`** — one row per manuscript value: a stable dotted `key`
   (e.g. `s1.debunk_belief.mean`), the `ALL_NUMBERS` selection (`block`,
   `filters`, `stat_col` — same uniqueness rule as `num()`: the filters must
   select exactly one row), and the print `format` (`1dp`, `int_comma`, `p`, …).
   `source_type=ext_needed` rows are quantities not yet in `ALL_NUMBERS`; they
   are skipped with a warning until ported into an additive engine module
   (`code/R/ext_manuscript_s13.R` / `ext_manuscript_s4meth.R`) — never side-computed.
2. **`wiring_map.yaml`** — one entry per sentence/stat-cluster: a verbatim
   `anchor` from the docx text layer with `{{placeholder}}`s replacing the
   numeric tokens, each placeholder bound to a manifest key. Anchors must match
   exactly once in the document; matching tolerates whitespace/dash/quote
   variants and Word's run splitting (values split mid-number across runs).
   P-value placeholders capture the comparator too (`= .23`, `< .001`).
3. **`check_manuscript.py`** — stdlib zip/XML surgery (house style:
   `code/postprocess_word_si.py`). Modes: `candidates` (numeric-token inventory
   for map building), `check`, `redline`, `clean`, `figures`.

## Figures

Figs 2–4 embedded in the docx are compared to the current
`figures/manuscript/*.png` renders **pixel-wise** (byte differences from
recompression are ignored). Genuinely drifted panels are swapped into the wired
copy automatically (image swaps cannot be tracked changes — they are listed in
the tool output). Fig 1 (the `figure1/` transcript toolchain) is verify-only.
Status 2026-07-06: all four embedded figures are pixel-identical to the
pipeline renders.

## When prose is edited

Co-author edits can break anchors — those entries surface as `NOT_FOUND` in the
next `make manuscript-check` (never silently). Fix by updating the entry's
`anchor` to the new sentence text (keep the placeholders). New reported numbers
need a manifest row + a map entry; numbers deliberately not wired are listed
with reasons in `unwired.md` (authors-only, not distributed).

## Verification invariants

- `pandoc --track-changes=reject -t plain` on the wired copy == the same
  extraction from the original (byte-identical text).
- `--track-changes=accept` yields exactly the manifest values.
- Re-running `check` against an accept-all'd wired copy reports 0 STALE.
- `make test` (dev_qa anchors) passes before and after any engine extension.
