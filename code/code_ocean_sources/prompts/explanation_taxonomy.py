"""Canonical taxonomy + prompt for the persuasion-explanation coder.

Single source of truth imported by score_explanation.py, consolidate_explanation.py,
and summarize_explanation_reliability.py. The 15 themes are carried over (refined,
not replaced) from the legacy explanation-coding script (not shipped); codebook.md documents them for
humans. Augmentations over the legacy instrument: a top-level response_quality field
(empty / meta / gibberish handling) and a deliberate WITHHOLDING of the persuasion
outcome from the coder (the legacy fed "succeeded"/"failed" into the prompt, which can
bias coding).
"""

from __future__ import annotations

# Each theme: machine key, short legacy code, human name, valence (+/-), definition.
# Order is the canonical column order everywhere downstream.
THEMES: list[dict] = [
    # ---- "+" themes: what the participant found persuasive / praiseworthy ----
    {"key": "evid_pos", "code": "EVID+", "valence": "pos",
     "name": "Evidence / Facts / Logic / Counterarguments",
     "def": "The response (a) mentions or alludes to concrete data, statistics, or facts, "
            "and/or (b) appeals to logic, rationality, or common sense, and/or (c) notes that "
            "the AI anticipated or addressed their doubts, questions, or alternative views."},
    {"key": "expt_pos", "code": "EXPT+", "valence": "pos",
     "name": "Expertise / Credibility / Trustworthiness / Impressed by AI",
     "def": "The response (a) describes the AI as knowledgeable, authoritative, or expert-like, "
            "and/or (b) as a reliable, unbiased, or 'neutral' source, and/or (c) values the AI's "
            "reliance on trustworthy sources, and/or (d) sees the AI as having unique access to "
            "accurate information by virtue of being an AI."},
    {"key": "emph_pos", "code": "EMPH+", "valence": "pos",
     "name": "Empathy / Politeness / Respectfulness",
     "def": "The response (a) notes the AI was nonjudgmental, supportive, understanding, or "
            "validating (even while disagreeing), and/or (b) praises the AI's polite, kind, or "
            "courteous manner, and/or (c) mentions feeling calmer, relieved, or emotionally "
            "supported by the AI."},
    {"key": "detl_pos", "code": "DETL+", "valence": "pos",
     "name": "Detailed / In-Depth / Novel / Tailored",
     "def": "The response (a) praises the AI for being thorough, detailed, or in-depth, and/or "
            "(b) remarks that the AI introduced new perspectives, angles, or data they had not "
            "considered, and/or (c) comments that the answer was multi-faceted or addressed points "
            "one-by-one, and/or (d) notes the answer was personalized/tailored to their specific "
            "questions."},
    {"key": "conx_pos", "code": "CONX+", "valence": "pos",
     "name": "Conspiracy-Specific Mechanisms",
     "def": "The response (a) describes specific mechanisms or details of the conspiracy theory "
            "(e.g., how a building's structure led to collapse, how UFO sightings are often secret "
            "aircraft, forensic details of a case), and/or (b) raises a meta-point about conspiracies "
            "(e.g., that a large conspiracy could not stay hidden because someone would 'spill the "
            "beans')."},
    {"key": "priv_pos", "code": "PRIV+", "valence": "pos",
     "name": "Privacy / Non-judgmental space",
     "def": "The response says the participant felt they could ask the AI questions without fear of "
            "judgment or repercussions (a private, safe space to engage), as a reason it worked."},

    # ---- "-" themes: what the participant did NOT find persuasive / criticisms ----
    {"key": "lack_neg", "code": "LACK-", "valence": "neg",
     "name": "Perceived lack of AI disagreement",
     "def": "The response indicates the participant already held the belief and the AI merely "
            "reaffirmed or agreed with what they already thought (so it did not actually change "
            "their mind)."},
    {"key": "deep_neg", "code": "DEEP-", "valence": "neg",
     "name": "Repetition / Lack of novelty / Lack of deep engagement",
     "def": "The response criticizes the AI for repeating familiar or well-known arguments, offering "
            "nothing new, and/or for merely echoing the participant's own points rather than "
            "substantively challenging or engaging them."},
    {"key": "angr_neg", "code": "ANGR-", "valence": "neg",
     "name": "Offensiveness / Anger",
     "def": "The response shows the participant is angry, annoyed, or upset about the AI or the "
            "conversation."},
    {"key": "evid_neg", "code": "EVID-", "valence": "neg",
     "name": "Insufficient evidence / No concrete data / No sourcing",
     "def": "The response argues the AI's answer lacked hard data, concrete evidence, or sources to "
            "back up its claims."},
    {"key": "bias_neg", "code": "BIAS-", "valence": "neg",
     "name": "Perceived bias",
     "def": "The response perceives the AI's arguments as skewed toward a particular narrative or "
            "agenda, failing to engage alternative viewpoints fairly."},
    {"key": "mech_neg", "code": "MECH-", "valence": "neg",
     "name": "Impersonal tone / Verbosity / Lack of nuance",
     "def": "The response criticizes the AI's answer as too long-winded, mechanical, robotic, or "
            "overly formal — lacking a human touch or nuance."},
    {"key": "trst_neg", "code": "TRST-", "valence": "neg",
     "name": "Distrust in AI as a source",
     "def": "The response expresses skepticism that an AI — because of its machine nature, "
            "limitations, or potential biases/programming — can be a convincing or trustworthy "
            "source of information."},
    {"key": "offt_neg", "code": "OFFT-", "valence": "neg",
     "name": "Off-topic / Superficial engagement",
     "def": "The response notes the AI strayed from the core issue or stayed too general/superficial, "
            "failing to address the specific points of the debate."},
    {"key": "emot_neg", "code": "EMOT-", "valence": "neg",
     "name": "Emotional / Experiential disconnect",
     "def": "The response finds the AI's arguments lacked a personal, emotional, or experiential "
            "dimension, making the discussion feel disconnected from real human concerns or lived "
            "experience."},
]

THEME_KEYS: list[str] = [t["key"] for t in THEMES]
POS_KEYS: list[str] = [t["key"] for t in THEMES if t["valence"] == "pos"]
NEG_KEYS: list[str] = [t["key"] for t in THEMES if t["valence"] == "neg"]

RESPONSE_QUALITY = ["substantive", "meta_or_off_topic", "no_answer", "unclassifiable"]
PRIMARY_THEME = THEME_KEYS + ["none"]

QUESTION_TEXT = {
    "persuasive": ("In just a sentence or two, would you mind explaining what about the AI's "
                   "comments, if anything, you found to be persuasive?"),
    "not_persuasive": ("In just a sentence or two, would you mind explaining what about the AI's "
                       "comments you did not find to be persuasive?"),
}


def _theme_block() -> str:
    lines = []
    for t in THEMES:
        fam = "praise/persuasive (+)" if t["valence"] == "pos" else "criticism/unpersuasive (-)"
        lines.append(f'- "{t["key"]}" [{t["code"]}, {fam}] {t["name"]}: {t["def"]}')
    return "\n".join(lines)


SYSTEM_PROMPT = f"""You are an expert qualitative coder analyzing why research participants did or did not find an AI chatbot's arguments persuasive. Each participant had a one-on-one conversation in which an AI tried to change their belief about a conspiracy theory they had named. Depending on the study, the AI argued FOR the conspiracy ("bunking"), argued AGAINST it ("debunking"), and in some studies was constrained to use only truthful arguments. Afterward, participants answered two open-ended questions: one asking what about the AI's comments they found persuasive, and one asking what they did NOT find persuasive.

You will receive: which of the two questions prompted this particular response; the participant's own brief description of their conspiracy topic (provided ONLY to help you resolve references such as names, events, or shorthand in the response); and the participant's verbatim response. Code ONLY the response text. You are NOT told whether the conversation actually changed the participant's belief; do not guess it, and do not let the topic description color your coding.

STEP 1 -- classify response_quality:
- "substantive": gives at least one interpretable reason about the AI or its arguments. Only in this case do you code themes.
- "meta_or_off_topic": comments only on the survey, study, task, or the general experience of talking to an AI -- not on the AI's arguments or qualities (e.g., "the survey was too long").
- "no_answer": blank, "n/a", "none", "nothing", or an explicit non-answer/refusal.
- "unclassifiable": gibberish or too fragmentary to interpret.

STEP 2 -- for a SUBSTANTIVE response, decide INDEPENDENTLY for EACH of the 15 themes below whether it is present (true) or absent (false). A response may express several themes, exactly one, or none. "+" themes describe something the participant found persuasive or praiseworthy about the AI; "-" themes describe a criticism or something unpersuasive. EITHER question box can contain EITHER kind of content (for example, a participant may write a criticism inside the "persuasive" box). Judge by the content of the text, not by which box it came from.

Themes:
{_theme_block()}

Also return:
- primary_theme: the single most salient theme key from the list above, or "none" (use "none" when the response is substantive but no one theme clearly dominates, or when it is not substantive).
- evidence_quote: the most diagnostic words copied verbatim from the response (<=25 words; use "" when no_answer).
- rationale: a brief justification, <=40 words.
- confidence: 0 to 1 -- the probability that a careful independent human coder would assign the same set of theme labels. Use the full range; 0.5 means genuinely ambiguous.

Critical rules:
1. Code conservatively. Mark a theme true ONLY when the text clearly supports it. When in doubt, mark it false. AVOID FALSE POSITIVES.
2. Themes are NOT mutually exclusive -- evaluate each one on its own merits.
3. You are not told, and must not infer, whether the AI "succeeded." Do not let any guess about the outcome influence your codes.
4. Use the topic description solely to understand references; never code it as if it were the participant's reasoning.
5. Any non-substantive response (meta_or_off_topic, no_answer, unclassifiable) gets ALL 15 themes false and primary_theme "none".

Return strict JSON matching the schema. No prose outside the JSON."""


def json_schema() -> dict:
    return {
        "name": "explanation_codes",
        "strict": True,
        "schema": {
            "type": "object",
            "properties": {
                "response_quality": {"type": "string", "enum": RESPONSE_QUALITY},
                "themes": {
                    "type": "object",
                    "properties": {k: {"type": "boolean"} for k in THEME_KEYS},
                    "required": THEME_KEYS,
                    "additionalProperties": False,
                },
                "primary_theme": {"type": "string", "enum": PRIMARY_THEME},
                "evidence_quote": {"type": "string"},
                "rationale": {"type": "string"},
                "confidence": {"type": "number", "description": "0 to 1"},
            },
            "required": ["response_quality", "themes", "primary_theme",
                         "evidence_quote", "rationale", "confidence"],
            "additionalProperties": False,
        },
    }
