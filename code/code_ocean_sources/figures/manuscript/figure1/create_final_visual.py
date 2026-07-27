"""Build manuscript Figure 1: a compact, caption-free, turn-based case study of one real
bunking conversation. The participant's belief moves 49 -> 99 -> 10 (baseline -> after the
standard GPT-4o bunking chat -> after the corrective debrief); three slider readouts show
one participant's 0-100 confidence at each point. The surrounding manuscript supplies the
caption.

Design: two columns; strict participant <-> GPT-4o alternation; light-red bunking bubbles
and light-green debrief bubbles; the model's fact-checked-false claims underlined inline
(four, across its first two replies, the second selectively quoted with " … ").

Writes final_visual.html; screenshot_final_visual.py renders it to figure1_transcript.png.

Provenance: participant R_5QGGSlaD13tJwt6, Study 2 (standard, guardrailed GPT-4o), bunking
arm (condition treatment_mid_bunk). Transcript + baseline/post belief come from the
persuasion-corpus CSV if present; the post-debrief belief and the debrief turns come from
the in-repo Study-2 clean file. Both are resolved automatically.
"""
import csv
import gzip
import html
import re
from pathlib import Path

TARGET_ID = "R_5QGGSlaD13tJwt6"
SCRIPT_DIR = Path(__file__).resolve().parent
OUTPUT_PATH = SCRIPT_DIR / "final_visual.html"


def find_up(relpath, start=SCRIPT_DIR, maxup=8):
    d = start
    for _ in range(maxup + 1):
        if (d / relpath).exists():
            return d / relpath
        d = d.parent
    return None


TRANSCRIPTS_CSV = (
    Path("transcripts and related information.csv")
    if Path("transcripts and related information.csv").exists()
    else find_up("transcript visual/transcripts and related information.csv")
)
CLEAN_CSV = find_up("data/processed_s1s3/study2_standard_clean.csv.gz")
if CLEAN_CSV is None:
    raise FileNotFoundError("Could not locate study2_standard_clean.csv.gz")
PRIMARY_CSV = TRANSCRIPTS_CSV if TRANSCRIPTS_CSV is not None else CLEAN_CSV


def _open_text(path):
    p = str(path)
    if p.endswith(".gz"):
        return gzip.open(p, "rt", encoding="utf-8", newline="")
    return Path(p).open("r", encoding="utf-8")


def load_row(csv_path, target_id):
    with _open_text(csv_path) as f:
        for row in csv.DictReader(f):
            if row.get("ResponseId") == target_id:
                return row
    raise ValueError(f"ResponseId {target_id} not found in {csv_path}")


def clean_text(text: str) -> str:
    cleaned = (text or "").strip()
    for old, new in {"behaviorno": "behavior. No", "No just": "No, just",
                     "chemtraiils": "chemtrails", "Chemtraiils": "Chemtrails",
                     "eveidence": "evidence"}.items():
        cleaned = cleaned.replace(old, new)
    return cleaned


def strip_markdown(text: str) -> str:
    text = re.sub(r"#{1,6}\s+", "", text or "")
    text = text.replace("**", "").replace("__", "")
    return re.sub(r"\s+", " ", text).strip()


def trim_sentence(text: str, limit: int):
    """First whole sentence(s) up to `limit` (an ellipsis marks the cut)."""
    text = text.strip()
    if len(text) <= limit:
        return text
    cut = text[:limit]
    b = max(cut.rfind(". "), cut.rfind("! "), cut.rfind("? "))
    cut = cut[: b + 1] if b > limit * 0.5 else cut[: cut.rfind(" ")]
    return cut.rstrip().rstrip(".") + " …"


def lead_sentences(text, limit):
    """First whole sentence(s) up to `limit`, with no trailing ellipsis."""
    flat = strip_markdown(text)
    cut = flat[:limit]
    b = max(cut.rfind(". "), cut.rfind("! "), cut.rfind("? "))
    return (cut[: b + 1] if b != -1 else cut).strip()


def sentence_with(text, keyword, maxlen=240):
    """The single sentence containing `keyword`, ellipsis-flanked if mid-reply."""
    flat = strip_markdown(text)
    i = flat.lower().find(keyword.lower())
    if i == -1:
        return trim_sentence(flat, maxlen)
    start = flat.rfind(". ", 0, i)
    start = start + 2 if start != -1 else 0
    end = flat.find(". ", i)
    end = end + 1 if end != -1 else len(flat)
    seg = flat[start:end].strip().rstrip(".")
    return ("… " if start > 0 else "") + seg + (" …" if end < len(flat) else "")


def passage_with(text, keyword, sentences=2, maxlen=360):
    """Up to `sentences` consecutive sentences starting at the one containing `keyword`."""
    flat = strip_markdown(text)
    i = flat.lower().find(keyword.lower())
    if i == -1:
        return trim_sentence(flat, maxlen)
    start = flat.rfind(". ", 0, i)
    start = start + 2 if start != -1 else 0
    enders = [m.end() for m in re.finditer(r"[.!?]\s", flat[start:])]
    end = start + enders[sentences - 1] if len(enders) >= sentences else len(flat)
    seg = flat[start:end].strip()
    if len(seg) > maxlen:
        seg = trim_sentence(seg, maxlen)
    else:
        seg = seg.rstrip(". ") + (" …" if end < len(flat) else "")
    return ("… " if start > 0 else "") + seg


def clean_sentence(text, keyword):
    """The sentence containing `keyword`, with any leading 'Header:' / '- Header:' /
    'N. Header:' label (an artifact of the model's markdown) stripped off."""
    flat = strip_markdown(text)
    for s in re.split(r"(?<=[.!?])\s+", flat):
        if keyword.lower() in s.lower():
            s = re.sub(r"^[-•\s]*(?:\d+\.\s*)?[A-Z][\w .,/&'()-]{1,55}?:\s+", "", s.strip())
            return s.strip().rstrip(".")
    return ""


def curate_bunk_open(text):
    """Tight, fabrication-dense excerpt of the model's opening turn (FOIA 'classified
    documents' + the bogus lab metals)."""
    flat = strip_markdown(text)
    a = flat.find("In classified documents")
    tail = "barium, aluminum, and strontium"
    e = flat.find(tail)
    if a != -1 and e != -1:
        return "… " + flat[a:e + len(tail)].strip() + " …"
    return trim_sentence(flat, 420)


# phrases underlined inline as fact-checked-false
FABRICATIONS = [
    "classified documents released under the Freedom of Information Act",
    "barium, aluminum, and strontium",
]


def ff(t):
    return f'<span class="ff">{html.escape(t)}</span>'


def quote(*parts):
    """Build an AI bubble from literal fragments; tuple ('ff', s) marks s fact-checked-
    false (underlined). Plain ' … ' fragments selectively quote within a reply."""
    out = []
    for p in parts:
        out.append(ff(p[1]) if isinstance(p, tuple) and p[0] == "ff" else html.escape(p))
    return "".join(out)


def render_bubble(text, mark_false=False):
    safe = html.escape(text).replace("\n", "<br>")
    if mark_false:
        for ph in FABRICATIONS:
            safe = safe.replace(html.escape(ph), ff(ph))
    return safe


row = load_row(PRIMARY_CSV, TARGET_ID)
deb = load_row(CLEAN_CSV, TARGET_ID)
pre = int(row.get("belief_rating_pre_rc") or row.get("belief_rating_pre_4") or 0)
post = int(row.get("belief_rating_post_rc") or row.get("belief_rating_post_4") or 0)
debrief = int(deb.get("belief_rating_debrf_4") or deb.get("postDBrating") or 0)
statement = (row.get("conRestatement") or "").strip().rstrip(".")

AVATAR_USER = "https://cdn-icons-png.flaticon.com/512/3553/3553910.png"
AVATAR_AI = "https://upload.wikimedia.org/wikipedia/commons/b/bb/GPT-4.jpg"


def msg(speaker, text, mark_false=False, raw=False):
    is_ai = speaker == "ai"
    avatar = AVATAR_AI if is_ai else AVATAR_USER
    cls = "ai" if is_ai else "user"
    body = text if raw else render_bubble(text, mark_false=mark_false)
    return f"""
    <div class="msg {cls}">
        <div class="av" style="background-image:url('{avatar}')"></div>
        <div class="mb">{body}</div>
    </div>"""


def rating(value, state):
    return f"""
    <div class="rate {state}">
        <div class="rv">{value}<small>%</small></div>
        <div class="rr">
            <div class="rc">this participant&rsquo;s confidence the claim is true</div>
            <div class="rt"><span class="rf" style="width:{value}%"></span><i class="rm" style="left:{value}%"></i></div>
            <div class="re"><span>definitely false</span><span>definitely true</span></div>
        </div>
    </div>"""


# 2nd model reply: keep its conversational opener, then selectively quote ("…") the two
# verbatim fact-checked-false claims it makes later in the same reply.
BUNK2_OPEN = lead_sentences(row.get("content_assistant_2", ""), 175)
C_BRAIN = "aluminum, when ingested or inhaled over time, has been shown to accumulate in the brain"
C_FISH = "Elevated aluminum levels in water bodies have been associated with declines in fish populations"

bunk = "\n".join([
    msg("ai", curate_bunk_open(row.get("content_assistant_1", "")), mark_false=True),
    msg("user", clean_text(row.get("content_user_2", ""))),
    msg("ai", quote(BUNK2_OPEN + " … ", ("ff", C_BRAIN),
                    " … Aquatic ecosystems are similarly at risk. ", ("ff", C_FISH), " …"), raw=True),
    msg("user", clean_text(row.get("content_user_3", ""))),
    msg("ai", trim_sentence(strip_markdown(row.get("content_assistant_3", "")), 175)),
    msg("user", clean_text(row.get("content_user_4", ""))),
    msg("ai", trim_sentence(strip_markdown(row.get("content_assistant_4", "")), 185)),
])

# debrief turns: keep the model's "Hello there! I'm here to help clarify…" opener for
# context, then quote the CONCRETE evidence it cites (contrails mechanism, FAA/EPA
# regulation, no anomalous metals, unverifiable whistleblowers) so the participant's
# "this is hard evidence presented here" follows on real evidence. Selective "…" quotes
# of verbatim sentences (the model's section labels stripped).
d1, d2, d3, d4 = (deb.get(f"debrief_content_assistant_{i}", "") for i in (1, 2, 3, 4))
DEB_OPENER = lead_sentences(d1, 95)
debrief_conv = "\n".join([
    msg("ai", DEB_OPENER + " … " + clean_sentence(d1, "contrails, formed")
            + " … " + clean_sentence(d1, "Federal Aviation Administration") + " …"),
    msg("user", clean_text(deb.get("debrief_content_user_2", ""))),
    msg("ai", "… " + clean_sentence(d2, "speculative or fabricated")
            + " … " + clean_sentence(d2, "population control") + " …"),
    msg("user", clean_text(deb.get("debrief_content_user_3", ""))),
    msg("ai", "… " + clean_sentence(d3, "harmful levels of metals")
            + " … " + clean_sentence(d3, "lack verifiable evidence") + " …"),
    msg("user", clean_text(deb.get("debrief_content_user_4", ""))),
    msg("ai", lead_sentences(d4, 255) + " …"),
])

CSS = """
    :root{ --ink:#0f172a; --mut:#475569; --line:#e2e8f0;
           --red:#b91c1c; --green:#15803d; }
    *{ box-sizing:border-box; }
    body{ font-family:'Manrope',sans-serif; background:#fff; margin:0; padding:0; color:var(--ink); }
    .page{ width:1180px; padding:6px; }
    .grid{ column-count:2; column-gap:22px; }
    .card{ break-inside:avoid; border:1px solid var(--line); border-radius:13px; padding:15px 17px; margin-bottom:16px; background:#fff; }
    .card.start{ break-before:column; }
    .hd{ display:flex; align-items:center; gap:9px; margin-bottom:12px; }
    .num{ width:25px; height:25px; border-radius:50%; background:var(--ink); color:#fff; font-size:14px; font-weight:800; display:flex; align-items:center; justify-content:center; flex-shrink:0; }
    .card.bunk .num{ background:var(--red); } .card.post .num{ background:var(--red); }
    .card.deb .num{ background:var(--green); } .card.fin .num{ background:var(--green); }
    .hd h2{ font-size:17px; font-weight:800; margin:0; letter-spacing:-0.01em; }
    .hd .step{ font-size:13px; font-weight:700; text-transform:uppercase; letter-spacing:0.04em; color:var(--mut); margin-left:auto; }

    .claim-label{ font-size:11px; font-weight:800; letter-spacing:0.06em; text-transform:uppercase; color:var(--mut); margin-bottom:7px; }
    .claim{ position:relative; background:#f1f5f9; border-radius:12px; padding:16px 20px 18px 54px; font-size:18.5px; font-weight:700; line-height:1.4; color:var(--ink); margin-bottom:14px; }
    .claim::before{ content:"“"; position:absolute; left:15px; top:12px; font-family:Georgia,'Times New Roman',serif; font-size:56px; line-height:1; color:#cbd5e1; }

    .rate{ display:flex; align-items:center; gap:15px; }
    .rate .rv{ font-size:42px; font-weight:800; line-height:1; flex-shrink:0; }
    .rate .rv small{ font-size:20px; font-weight:800; }
    .rate.base .rv{ color:var(--mut); } .rate.up .rv{ color:var(--red); } .rate.down .rv{ color:var(--green); }
    .rr{ flex:1; }
    .rc{ font-size:13px; font-weight:700; color:var(--mut); margin-bottom:7px; }
    .rt{ position:relative; height:10px; border-radius:9999px; background:#eef2f7; border:1px solid var(--line); }
    .rf{ position:absolute; left:0; top:0; height:100%; border-radius:9999px; opacity:0.32; }
    .rate.base .rf{ background:var(--mut); } .rate.up .rf{ background:var(--red); } .rate.down .rf{ background:var(--green); }
    .rm{ position:absolute; top:50%; width:18px; height:18px; border-radius:50%; transform:translate(-50%,-50%); border:3px solid #fff; box-shadow:0 1px 4px rgba(15,23,42,0.35); }
    .rate.base .rm{ background:var(--mut); } .rate.up .rm{ background:var(--red); } .rate.down .rm{ background:var(--green); }
    .re{ display:flex; justify-content:space-between; font-size:11.5px; font-weight:600; color:var(--mut); margin-top:6px; }

    .legend-false{ font-size:13px; font-weight:700; color:var(--ink); margin-bottom:12px; }

    .msg{ display:flex; gap:9px; margin-bottom:10px; align-items:flex-start; }
    .msg.user{ flex-direction:row-reverse; }
    .av{ width:27px; height:27px; border-radius:50%; background-size:cover; background-position:center; flex-shrink:0; margin-top:2px; border:1px solid var(--line); }
    .mb{ font-size:14.5px; line-height:1.5; padding:10px 13px; border-radius:13px; max-width:84%; }
    .msg.user .mb{ background:#eef2f7; color:var(--ink); border-bottom-right-radius:4px; }
    .msg.ai .mb{ background:#fdeaea; color:var(--ink); border:1px solid #f6d2d2; border-bottom-left-radius:4px; }
    .card.deb .msg.ai .mb{ background:#e9f6ef; border-color:#c5e8d4; }
    .ff{ color:var(--red); font-weight:700; text-decoration:underline; text-decoration-color:var(--red); text-underline-offset:2px; }
"""

body = f"""
<div class="page">
  <div class="grid">

    <div class="card base">
      <div class="hd"><span class="num">1</span><h2>Baseline</h2><span class="step">before AI</span></div>
      <div class="claim-label">the conspiracy this participant named</div>
      <div class="claim">{html.escape(statement)}.</div>
      {rating(pre, "base")}
    </div>

    <div class="card bunk">
      <div class="hd"><span class="num">2</span><h2>Bunking conversation</h2><span class="step">GPT&#8209;4o argues <i>for</i> it</span></div>
      <div class="legend-false"><span class="ff">underlined</span> = claim fact&#8209;checked as false</div>
      {bunk}
    </div>

    <div class="card post start">
      <div class="hd"><span class="num">3</span><h2>After the conversation</h2><span class="step">immediately after</span></div>
      {rating(post, "up")}
    </div>

    <div class="card deb">
      <div class="hd"><span class="num">4</span><h2>Corrective debrief</h2><span class="step">GPT&#8209;4o corrects it</span></div>
      {debrief_conv}
    </div>

    <div class="card fin">
      <div class="hd"><span class="num">5</span><h2>After the debrief</h2><span class="step">final rating</span></div>
      {rating(debrief, "down")}
    </div>

  </div>
</div>
"""

html_output = f"""<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="utf-8"/>
<link href="https://fonts.googleapis.com/css2?family=Manrope:wght@300;400;500;600;700;800&display=swap" rel="stylesheet"/>
<style>{CSS}</style>
</head>
<body>
{body}
</body>
</html>
"""

OUTPUT_PATH.write_text(html_output, encoding="utf-8")
print(f"Wrote {OUTPUT_PATH}")
print(f"  belief arc: {pre} -> {post} -> {debrief}  | 4 fact-checked-false claims underlined (replies 1-2)")
