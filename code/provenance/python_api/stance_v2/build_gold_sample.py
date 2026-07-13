"""Build the human gold-standard coding sheets for stance-v2 validation.

~250 posts stratified over the v1 failure modes, plus all dispersion-flagged
pilot items. Coder A codes all items; Coder B codes a random 60-item overlap
(human-human reliability ceiling). Sheets are blinded: no model scores, no
condition, no assigned model. Row order randomized (seeded).

Writes (shipped under data/validation/gold_coding/):
    stance_gold_coderA.csv
    stance_gold_coderB.csv
    CODING_INSTRUCTIONS.md
    gold_item_ids.txt   (for later joins)
"""

from __future__ import annotations

import csv
import json
import random
import re
from pathlib import Path

HERE = Path(__file__).resolve().parent


def _repo_root(start: Path) -> Path:
    p = start.resolve()
    for cand in [p, *p.parents]:
        if (cand / "data").is_dir() and (cand / "code").is_dir():
            return cand
    raise RuntimeError("repo root (dir containing data/ and code/) not found")


REPO_ROOT = _repo_root(Path(__file__))
# Transient stance inputs / pilot consolidation live in the working dir.
WORK_DIR = REPO_ROOT / "output" / "provenance_work" / "stance_v2"
V2_DIR = WORK_DIR
# Shipped human gold-coding sheets + instructions.
OUT_DIR = REPO_ROOT / "data" / "validation" / "gold_coding"

META_RX = re.compile(r"\b(this survey|this study|the survey|this ai|the ai|chatbot|experiment)\b",
                     re.IGNORECASE)

STRATA = [
    ("v1_50", 60, lambda it: float(it["v1_score"]) == 50),
    ("v1_extreme", 30, lambda it: float(it["v1_score"]) in (0.0, 100.0)),
    ("question", 50, lambda it: "?" in it["post_text"]),
    ("meta", 20, lambda it: META_RX.search(it["post_text"]) is not None),
    ("random", 83, lambda it: True),
]

CODE_COLUMNS = [
    ("focal_relevance", "focal_claim / related_conspiracy / other_topic / no_propositional_content"),
    ("response_type", "assertion / question_raising / mixed_assertion_question / "
                      "uncertainty_statement / declines_to_post / meta_task / unclassifiable"),
    ("stance_category", "argues_against / leans_against / neutral_uncommitted / "
                        "mixed_both_sides / leans_for / argues_for / not_applicable"),
    ("stance_score", "0-100 integer, BLANK if not_applicable"),
    ("sarcasm_or_irony", "0 / 1"),
    ("notes", "optional"),
]

INSTRUCTIONS = """# Gold-set coding instructions (stance v2 validation)

You are rating what each social media post communicates to a public audience about the
participant's focal conspiracy claim. Do not judge whether the claim is true. Rate only
the post; the participant background column is there solely to resolve references.

For each row, fill the last six columns:

1. **focal_relevance** — is the post about the focal claim shown in that row?
   `focal_claim` (even obliquely), `related_conspiracy` (different but related claim),
   `other_topic`, `no_propositional_content` (no topic content at all, e.g. comments
   about the survey or the AI experience).
2. **response_type** — dominant mode: `assertion` (makes/endorses claims, including
   approving reports of others' claims), `question_raising` (mainly poses questions),
   `mixed_assertion_question`, `uncertainty_statement` (mainly the author's own
   indecision), `declines_to_post` (refuses to write/share a post about the topic,
   e.g. "I wouldn't post about this"), `meta_task` (about the survey/AI/task),
   `unclassifiable`.
3. **stance_category** — `argues_for` (~80-100), `leans_for` (~60-79),
   `neutral_uncommitted` (~45-55), `mixed_both_sides` (score the net impression),
   `leans_against` (~21-40), `argues_against` (~0-20), or `not_applicable` (ONLY if
   focal_relevance is other_topic / no_propositional_content, or the post declines
   to post).
4. **stance_score** — 0-100 integer consistent with the category; leave BLANK iff
   not_applicable. Posts about the focal claim always get a score, including pure
   questions.
5. **sarcasm_or_irony** — 1 if the stance is conveyed by sarcasm/irony (score the
   implied stance).
6. **notes** — anything worth flagging.

Conventions (same as the machine rubric):
- Rhetorical / premise-accepting questions: score the implicature, in either direction
  ("Did the government facilitate 9/11?" posted publicly invites doubt of the official
  account -> leans_for, question_raising; "Sure, and the moon is made of cheese too,
  right?" mocks the conspiracy -> leans/argues against). A genuinely neutral
  information request is neutral_uncommitted ~50.
- Partial endorsement counts: endorsing key evidence or a documented weaker version of
  the claim, presented as supporting the focal claim, leans for; conceding documented
  elements while rejecting the focal extension leans against. Score net communicated
  support for the focal claim.
- Reported speech: score the post's communicative endorsement (enthusiastic
  amplification endorses; explicit distancing leans against).
- Posts relaying what the AI said, with apparent acceptance: score the accepted content.
- 50 is NOT a dumping ground: it means genuinely balanced/uncommitted content about the
  claim. Off-topic / contentless posts are not_applicable with a blank score.

Code independently: do not discuss items with the other coder until both sheets are done.
"""


def main() -> None:
    rng = random.Random(20260612)
    inputs_path = V2_DIR / "stance_v22_inputs.jsonl"
    if not inputs_path.exists():
        inputs_path = V2_DIR / "stance_v2_inputs.jsonl"
    items = {json.loads(l)["item_id"]: json.loads(l) for l in open(inputs_path)}

    frozen = OUT_DIR / "gold_item_ids.txt"
    if frozen.exists():
        # gold_item_ids.txt is already in (shuffled) coder order; coder B's
        # overlap is whatever the existing coder-B sheet contains
        chosen = [l.strip() for l in open(frozen) if l.strip()]
        b_csv = OUT_DIR / "stance_gold_coderB.csv"
        overlap = {r["item_id"] for r in csv.DictReader(open(b_csv))
                   if r["item_id"] and not r["item_id"].startswith("<")}
        print(f"reusing frozen gold sample: {len(chosen)} items, overlap {len(overlap)}")
        _write_outputs(items, chosen, overlap)
        return

    chosen: list[str] = []
    chosen_set: set[str] = set()

    # all dispersion-flagged pilot items first
    pilot_csv = V2_DIR / "stance_v2_pilot_consolidated.csv"
    if pilot_csv.exists():
        for r in csv.DictReader(open(pilot_csv)):
            if r["dispersion_flag"] == "True":
                chosen.append(r["item_id"])
                chosen_set.add(r["item_id"])
        print(f"pilot dispersion-flagged: +{len(chosen)}")

    pool_all = list(items.values())
    for name, n, pred in STRATA:
        pool = [it for it in pool_all if pred(it) and it["item_id"] not in chosen_set]
        pre = [it for it in pool if it["timepoint"] == "pre"]
        post = [it for it in pool if it["timepoint"] == "post"]
        take = rng.sample(pre, min(n // 2, len(pre))) + rng.sample(post, min(n - n // 2, len(post)))
        for it in take:
            chosen.append(it["item_id"])
            chosen_set.add(it["item_id"])
        print(f"{name}: +{len(take)}")
    print(f"gold total: {len(chosen)}")

    rng.shuffle(chosen)
    overlap = set(rng.sample(chosen, 60))
    _write_outputs(items, chosen, overlap)


def claim_of(it: dict) -> str:
    return it.get("focal_claim") or it.get("focal_claim_restatement", "")


def _write_outputs(items: dict, chosen: list[str], overlap: set) -> None:
    OUT_DIR.mkdir(parents=True, exist_ok=True)
    (OUT_DIR / "CODING_INSTRUCTIONS.md").write_text(INSTRUCTIONS)
    (OUT_DIR / "gold_item_ids.txt").write_text("\n".join(chosen) + "\n")

    base_cols = ["item_id", "timepoint", "focal_claim", "participant_background", "post_text"]
    code_cols = [c for c, _ in CODE_COLUMNS]

    def write_sheet(path: Path, ids: list[str]) -> None:
        with open(path, "w", newline="") as f:
            w = csv.writer(f)
            w.writerow(base_cols + code_cols)
            w.writerow(["", "", "", "", "<- read CODING_INSTRUCTIONS.md first"] +
                       [hint for _, hint in CODE_COLUMNS])
            for item_id in ids:
                it = items[item_id]
                background = (f"topic: {it['participant_topic_description']} || "
                              f"reasons: {it['participant_reasons']}")
                w.writerow([it["item_id"], it["timepoint"], claim_of(it),
                            background, it["post_text"]] + [""] * len(code_cols))

    write_sheet(OUT_DIR / "stance_gold_coderA.csv", chosen)
    write_sheet(OUT_DIR / "stance_gold_coderB.csv", [i for i in chosen if i in overlap])
    print(f"coder A: {len(chosen)} items; coder B overlap: {len(overlap)} items -> {OUT_DIR}")


if __name__ == "__main__":
    main()
