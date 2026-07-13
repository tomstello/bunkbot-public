# Gold-set coding instructions (stance v2 validation)

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
