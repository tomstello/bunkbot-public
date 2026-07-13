"""Generate self-contained HTML coding apps for the stance gold-set task.

One file per coder (A: full 245-item sample; B: 60-item overlap). Each app
presents items one at a time with all rating context, enforces the rubric's
consistency rules, autosaves progress to localStorage, and exports a CSV.

Blinding: no v1 scores, no machine ratings, no condition/model information.

Writes data/validation/gold_coding/stance_gold_coderA.html (+ coderB).
"""

from __future__ import annotations

import json
from pathlib import Path

HERE = Path(__file__).resolve().parent


def _repo_root(start: Path) -> Path:
    p = start.resolve()
    for cand in [p, *p.parents]:
        if (cand / "data").is_dir() and (cand / "code").is_dir():
            return cand
    raise RuntimeError("repo root (dir containing data/ and code/) not found")


REPO_ROOT = _repo_root(Path(__file__))
# Transient stance inputs live in the working dir.
WORK_DIR = REPO_ROOT / "output" / "provenance_work" / "stance_v2"
V2_DIR = WORK_DIR
# Shipped human gold-coding sheets + the rendered coder apps.
GOLD_DIR = REPO_ROOT / "data" / "validation" / "gold_coding"

FOCAL_RELEVANCE = [
    ("focal_claim", "Focal claim", "about the focal conspiracy claim, even obliquely"),
    ("related_conspiracy", "Related conspiracy", "a different but related conspiracy/claim"),
    ("other_topic", "Other topic", "about something else entirely"),
    ("no_propositional_content", "No content", "no topic-relevant content at all"),
]
RESPONSE_TYPE = [
    ("assertion", "Assertion", "makes/endorses claims (incl. approving reports of others' claims)"),
    ("question_raising", "Question-raising", "mainly poses questions, invites others to wonder"),
    ("mixed_assertion_question", "Mixed assertion + question", "substantively both"),
    ("uncertainty_statement", "Uncertainty statement", "mainly the author's own indecision"),
    ("declines_to_post", "Declines to post", "refuses to write/share a post about the topic"),
    ("meta_task", "Meta / task", "about the survey, the AI, or the task itself"),
    ("unclassifiable", "Unclassifiable", "gibberish or too fragmentary"),
]
STANCE_CATEGORY = [
    ("argues_for", "Argues FOR", "~80-100: asserts/strongly implies the claim is true"),
    ("leans_for", "Leans for", "~60-79: suggestive support, premise-accepting questions"),
    ("neutral_uncommitted", "Neutral / uncommitted", "~45-55: no direction; pure info request"),
    ("mixed_both_sides", "Mixed (both sides)", "score the net impression"),
    ("leans_against", "Leans against", "~21-40: suggestive doubt, tentative mundane explanations"),
    ("argues_against", "Argues AGAINST", "~0-20: asserts/strongly implies the claim is false"),
    ("not_applicable", "Not applicable", "no rateable stance content (score stays blank)"),
]

INSTRUCTIONS_HTML = """
<p>You are rating what each social media post <b>communicates to a public audience</b> about the
participant's focal conspiracy claim. Do not judge whether the claim is true. Rate only the post;
the participant background is there solely to resolve references (names, events, shorthand).</p>
<p><b>Conventions:</b></p>
<ul>
<li><b>Rhetorical / premise-accepting questions:</b> score the implicature, in either direction.
"Did the government facilitate 9/11?" posted publicly invites doubt of the official account
(leans for, question-raising). "Sure, and the moon is made of cheese too, right?" mocks the
conspiracy (leans/argues against). A genuinely neutral information request is neutral (~50).</li>
<li><b>Partial endorsement counts:</b> endorsing key evidence or a documented weaker version of
the claim, presented as supporting the focal claim, leans for; conceding documented elements
while rejecting the focal extension leans against. Score net communicated support.</li>
<li><b>Reported speech:</b> score the post's communicative endorsement (enthusiastic
amplification endorses; explicit distancing leans against).</li>
<li><b>Posts relaying what the AI said</b>, with apparent acceptance: score the accepted content.</li>
<li><b>Declining to post</b> ("I wouldn't post about this") = response type "Declines to post";
stance is then Not applicable automatically.</li>
<li><b>50 is not a dumping ground:</b> it means genuinely balanced/uncommitted content about the
claim. Off-topic or contentless posts are Not applicable with a blank score.</li>
<li><b>Sarcasm/irony:</b> score the implied stance and tick the flag.</li>
</ul>
<p>Your progress autosaves in this browser. You can close the file and reopen it later on the
same computer. When finished (or to back up partway), click <b>Export CSV</b> and send the
downloaded file back. Please code independently &mdash; no discussing items with other coders
until both are done.</p>
"""

TEMPLATE = """<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>Stance coding — __CODER__</title>
<style>
:root { --ink:#1a1d23; --muted:#6b7280; --line:#e5e7eb; --accent:#2563eb; --ok:#16a34a;
        --bg:#f6f7f9; --card:#ffffff; --warn:#b91c1c; }
* { box-sizing:border-box; }
body { margin:0; font-family:-apple-system,BlinkMacSystemFont,"Segoe UI",Roboto,Helvetica,Arial,sans-serif;
       color:var(--ink); background:var(--bg); line-height:1.45; }
.wrap { max-width:860px; margin:0 auto; padding:16px 20px 80px; }
header { display:flex; align-items:center; justify-content:space-between; gap:12px;
         padding:10px 0; flex-wrap:wrap; }
h1 { font-size:17px; margin:0; }
.progress { font-size:13px; color:var(--muted); }
.bar { height:6px; background:var(--line); border-radius:3px; overflow:hidden; margin:4px 0 14px; }
.bar > div { height:100%; background:var(--accent); width:0%; transition:width .2s; }
.card { background:var(--card); border:1px solid var(--line); border-radius:10px;
        padding:18px 20px; margin-bottom:14px; }
.badge { display:inline-block; font-size:11px; font-weight:600; letter-spacing:.04em;
         text-transform:uppercase; padding:2px 8px; border-radius:10px; margin-right:6px; }
.badge.pre { background:#dbeafe; color:#1e40af; } .badge.post { background:#dcfce7; color:#166534; }
.label { font-size:11px; font-weight:600; letter-spacing:.05em; text-transform:uppercase;
         color:var(--muted); margin:14px 0 4px; }
.claim { font-size:15px; font-weight:600; }
.summary, .background { font-size:13px; color:var(--muted); }
details { margin-top:6px; } summary { cursor:pointer; font-size:13px; color:var(--accent); }
.post { font-size:17px; background:#fffbeb; border:1px solid #fde68a; border-radius:8px;
        padding:14px 16px; margin-top:6px; white-space:pre-wrap; }
fieldset { border:none; margin:0 0 6px; padding:0; }
.opts { display:flex; flex-wrap:wrap; gap:6px; }
.opt { border:1px solid var(--line); border-radius:8px; padding:6px 10px; cursor:pointer;
       font-size:13px; background:#fff; user-select:none; }
.opt:hover { border-color:var(--accent); }
.opt.sel { background:var(--accent); border-color:var(--accent); color:#fff; }
.opt.disabled { opacity:.4; pointer-events:none; }
.hint { font-size:12px; color:var(--muted); min-height:16px; margin-top:3px; }
.scorebox { display:flex; align-items:center; gap:12px; margin-top:4px; }
input[type=range] { flex:1; }
input[type=number] { width:74px; font-size:16px; padding:5px 8px; border:1px solid var(--line);
                     border-radius:6px; }
.row { display:flex; gap:16px; align-items:center; flex-wrap:wrap; }
textarea { width:100%; min-height:44px; border:1px solid var(--line); border-radius:6px;
           padding:8px; font:inherit; font-size:13px; }
.btn { font-size:14px; font-weight:600; border:none; border-radius:8px; padding:10px 18px;
       cursor:pointer; }
.btn.primary { background:var(--accent); color:#fff; }
.btn.ghost { background:#fff; border:1px solid var(--line); color:var(--ink); }
.btn:disabled { opacity:.45; cursor:default; }
.nav { display:flex; gap:10px; align-items:center; justify-content:space-between; margin-top:10px; }
.err { color:var(--warn); font-size:13px; min-height:18px; margin-top:6px; }
.gridmap { display:flex; flex-wrap:wrap; gap:3px; margin-top:8px; }
.cell { width:14px; height:14px; border-radius:3px; background:var(--line); cursor:pointer; }
.cell.done { background:var(--ok); } .cell.cur { outline:2px solid var(--accent); }
.screen-start .card { padding:24px 28px; }
input.name { font-size:15px; padding:8px 10px; border:1px solid var(--line); border-radius:6px; }
.saved { color:var(--ok); font-size:12px; }
.checkbox { display:flex; gap:8px; align-items:center; font-size:14px; cursor:pointer; }
.endcard { text-align:center; padding:40px 20px; }
kbd { background:#eef; border-radius:4px; padding:0 5px; font-size:12px; }
</style>
</head>
<body>
<div class="wrap">
<header>
  <h1>Stance gold coding — sheet __CODER__</h1>
  <div class="row">
    <span class="progress" id="progress"></span>
    <button class="btn ghost" id="btn-instructions">Instructions</button>
    <button class="btn ghost" id="btn-export">Export CSV</button>
  </div>
</header>
<div class="bar"><div id="barfill"></div></div>

<div id="screen-start" class="screen-start">
  <div class="card">
    <h2 style="margin-top:0">Before you start</h2>
    <div id="instructions-body">__INSTRUCTIONS__</div>
    <p class="label">Your name or initials (saved with every rating)</p>
    <input class="name" id="coder-name" placeholder="e.g., TC">
    <p class="err" id="start-err"></p>
    <button class="btn primary" id="btn-start">Start coding</button>
  </div>
</div>

<div id="screen-item" style="display:none">
  <div class="card">
    <div><span class="badge" id="tp-badge"></span>
         <span class="summary" id="task-wording"></span></div>
    <p class="label">Focal conspiracy claim</p>
    <div class="claim" id="claim"></div>
    <div class="summary" id="claim-summary"></div>
    <details><summary>Participant background (reference only — do not rate this)</summary>
      <div class="background" id="background"></div></details>
    <p class="label">The post you are rating</p>
    <div class="post" id="post"></div>
  </div>

  <div class="card">
    <p class="label" style="margin-top:0">1 · What is the post about? <span style="color:var(--warn)">*</span></p>
    <fieldset class="opts" id="fr-opts"></fieldset><div class="hint" id="fr-hint"></div>
    <p class="label">2 · Response type <span style="color:var(--warn)">*</span></p>
    <fieldset class="opts" id="rt-opts"></fieldset><div class="hint" id="rt-hint"></div>
    <p class="label">3 · Stance toward the focal claim <span style="color:var(--warn)">*</span></p>
    <fieldset class="opts" id="sc-opts"></fieldset><div class="hint" id="sc-hint"></div>
    <p class="label">4 · Stance score (0 = argues claim is false · 50 = neutral · 100 = argues claim is true)</p>
    <div class="scorebox">
      <input type="range" id="score-range" min="0" max="100" step="1" value="50">
      <input type="number" id="score-num" min="0" max="100" step="1" placeholder="–">
    </div>
    <div class="hint" id="score-hint"></div>
    <p class="label">5 · Flags & notes</p>
    <div class="row">
      <label class="checkbox"><input type="checkbox" id="sarcasm"> sarcasm / irony carries the stance</label>
    </div>
    <textarea id="notes" placeholder="optional notes"></textarea>
    <p class="err" id="item-err"></p>
    <div class="nav">
      <button class="btn ghost" id="btn-prev">&larr; Back</button>
      <span class="saved" id="saved-flash"></span>
      <button class="btn primary" id="btn-save">Save &amp; next &rarr;</button>
    </div>
  </div>
  <div class="card">
    <p class="label" style="margin-top:0">Item map (green = coded, click to jump)</p>
    <div class="gridmap" id="gridmap"></div>
  </div>
</div>

<div id="screen-end" style="display:none">
  <div class="card endcard">
    <h2>All items coded 🎉</h2>
    <p>Click below to download your ratings, then send the CSV file back.</p>
    <button class="btn primary" id="btn-export2">Export CSV</button>
    <p class="summary" style="margin-top:14px">You can still revisit any item via the map or Back button.</p>
    <div class="gridmap" id="gridmap2" style="justify-content:center"></div>
  </div>
</div>
</div>

<script>
const ITEMS = __DATA__;
const SHEET = "__CODER__";
const FR = __FR__;
const RT = __RT__;
const SC = __SC__;
const LSKEY = "stance_gold::" + SHEET;

let state = { coder:"", idx:0, codes:{} };
try { const s = JSON.parse(localStorage.getItem(LSKEY)); if (s && s.codes) state = s; } catch(e){}

const $ = id => document.getElementById(id);
const esc = t => { const d = document.createElement("div"); d.textContent = t==null?"":String(t); return d.innerHTML; };

let cur = null; // working copy of the current item's code

function blankCode(){ return { focal_relevance:null, response_type:null, stance_category:null,
  stance_score:null, sarcasm_or_irony:false, notes:"" }; }

function persist(){ localStorage.setItem(LSKEY, JSON.stringify(state)); }

function nCoded(){ return Object.keys(state.codes).length; }

function renderChips(containerId, options, selected, onpick, disabledSet){
  const box = $(containerId); box.innerHTML = "";
  options.forEach(([val, label, hint]) => {
    const el = document.createElement("div");
    el.className = "opt" + (selected===val ? " sel":"") +
                   (disabledSet && disabledSet.has(val) ? " disabled":"");
    el.textContent = label;
    el.title = hint;
    el.onclick = () => onpick(val, hint);
    box.appendChild(el);
  });
}

function scoreEnabled(){ return cur.stance_category !== "not_applicable" && cur.stance_category !== null; }

function syncScoreUI(){
  const on = scoreEnabled();
  $("score-range").disabled = !on; $("score-num").disabled = !on;
  if (!on) { $("score-num").value = ""; $("score-hint").textContent =
    cur.stance_category==="not_applicable" ? "score stays blank for Not applicable" : "pick a stance category first"; }
  else {
    if (cur.stance_score==null) { $("score-num").value=""; $("score-range").value=50;
      $("score-hint").textContent="move the slider or type a number"; }
    else { $("score-num").value=cur.stance_score; $("score-range").value=cur.stance_score;
      $("score-hint").textContent=""; }
  }
}

function napAllowed(){
  return ["other_topic","no_propositional_content"].includes(cur.focal_relevance) ||
         cur.response_type==="declines_to_post";
}

function renderItem(){
  const it = ITEMS[state.idx];
  cur = Object.assign(blankCode(), state.codes[it.item_id] || {});
  $("tp-badge").textContent = it.timepoint === "pre" ? "before AI conversation" : "after AI conversation";
  $("tp-badge").className = "badge " + it.timepoint;
  $("task-wording").textContent = it.task_wording;
  $("claim").textContent = it.focal_claim;
  $("claim-summary").textContent = "";
  $("background").innerHTML = "<p><b>Their topic description:</b> " + esc(it.participant_topic_description) +
    "</p><p><b>Their stated reasons for/against:</b> " + esc(it.participant_reasons) + "</p>";
  $("post").textContent = it.post_text;
  $("sarcasm").checked = !!cur.sarcasm_or_irony;
  $("notes").value = cur.notes || "";
  $("item-err").textContent = "";
  $("saved-flash").textContent = state.codes[it.item_id] ? "previously saved" : "";
  redrawChips(); syncScoreUI(); drawProgress(); drawMap("gridmap");
  window.scrollTo(0,0);
}

function redrawChips(){
  renderChips("fr-opts", FR, cur.focal_relevance, (v,h)=>{ cur.focal_relevance=v; $("fr-hint").textContent=h;
    if (cur.stance_category==="not_applicable" && !napAllowed()) cur.stance_category=null;
    redrawChips(); syncScoreUI(); });
  renderChips("rt-opts", RT, cur.response_type, (v,h)=>{ cur.response_type=v; $("rt-hint").textContent=h;
    if (v==="declines_to_post"){ cur.stance_category="not_applicable"; cur.stance_score=null; }
    else if (cur.stance_category==="not_applicable" && !napAllowed()) cur.stance_category=null;
    redrawChips(); syncScoreUI(); });
  const scDisabled = new Set();
  if (!napAllowed()) scDisabled.add("not_applicable");
  const scLocked = cur.response_type==="declines_to_post";
  renderChips("sc-opts", SC, cur.stance_category, (v,h)=>{
    if (scLocked) { $("sc-hint").textContent="locked to Not applicable while response type is Declines to post"; return; }
    cur.stance_category=v; $("sc-hint").textContent=h;
    if (v==="not_applicable") cur.stance_score=null;
    redrawChips(); syncScoreUI(); }, scDisabled);
}

$("score-range").addEventListener("input", e=>{ if(scoreEnabled()){ cur.stance_score=+e.target.value;
  $("score-num").value=e.target.value; $("score-hint").textContent=""; }});
$("score-num").addEventListener("input", e=>{ const v=e.target.value;
  if(scoreEnabled() && v!==""){ cur.stance_score=Math.max(0,Math.min(100,Math.round(+v)));
  $("score-range").value=cur.stance_score; $("score-hint").textContent=""; }});

function validate(){
  if (!cur.focal_relevance) return "Pick what the post is about (1).";
  if (!cur.response_type) return "Pick a response type (2).";
  if (!cur.stance_category) return "Pick a stance category (3).";
  if (cur.stance_category==="not_applicable"){
    if (!napAllowed()) return "Not applicable needs Other topic / No content / Declines to post.";
  } else {
    if (cur.stance_score==null || isNaN(cur.stance_score)) return "Enter a stance score (4).";
  }
  return null;
}

$("btn-save").onclick = () => {
  const err = validate(); if (err){ $("item-err").textContent = err; return; }
  cur.sarcasm_or_irony = $("sarcasm").checked; cur.notes = $("notes").value.trim();
  const it = ITEMS[state.idx];
  state.codes[it.item_id] = JSON.parse(JSON.stringify(cur));
  // advance to next uncoded, else next index, else end screen
  let next = ITEMS.findIndex((x,i)=> i>state.idx && !state.codes[x.item_id]);
  if (next === -1) next = ITEMS.findIndex(x=> !state.codes[x.item_id]);
  if (next === -1){ persist(); showEnd(); return; }
  state.idx = next; persist(); renderItem();
};
$("btn-prev").onclick = () => { if (state.idx>0){ state.idx--; persist(); renderItem(); } };

function drawProgress(){
  $("progress").textContent = nCoded() + " / " + ITEMS.length + " coded";
  $("barfill").style.width = (100*nCoded()/ITEMS.length) + "%";
}
function drawMap(id){
  const g = $(id); g.innerHTML = "";
  ITEMS.forEach((it,i)=>{ const c=document.createElement("div");
    c.className = "cell" + (state.codes[it.item_id]?" done":"") + (i===state.idx?" cur":"");
    c.title = (i+1) + (state.codes[it.item_id] ? " (coded)":"");
    c.onclick = ()=>{ state.idx=i; persist(); showItem(); };
    g.appendChild(c); });
}
function showItem(){ $("screen-start").style.display="none"; $("screen-end").style.display="none";
  $("screen-item").style.display="block"; renderItem(); }
function showEnd(){ $("screen-item").style.display="none"; $("screen-end").style.display="block";
  drawProgress(); drawMap("gridmap2"); }

$("btn-start").onclick = () => {
  const name = $("coder-name").value.trim();
  if (!name){ $("start-err").textContent = "Please enter your name or initials."; return; }
  state.coder = name; persist(); showItem();
};
$("btn-instructions").onclick = () => { $("screen-item").style.display="none";
  $("screen-end").style.display="none"; $("screen-start").style.display="block";
  $("coder-name").value = state.coder || ""; window.scrollTo(0,0); };

function csvEscape(v){ v = v==null? "": String(v);
  return /[",\\n]/.test(v) ? '"' + v.replace(/"/g,'""') + '"' : v; }
function exportCSV(){
  const cols = ["item_id","timepoint","focal_relevance","response_type","stance_category",
                "stance_score","sarcasm_or_irony","notes","coder","sheet"];
  const lines = [cols.join(",")];
  ITEMS.forEach(it=>{ const c = state.codes[it.item_id]; if (!c) return;
    lines.push([it.item_id, it.timepoint, c.focal_relevance, c.response_type, c.stance_category,
      c.stance_score==null? "": c.stance_score, c.sarcasm_or_irony?1:0, c.notes,
      state.coder, SHEET].map(csvEscape).join(",")); });
  const blob = new Blob([lines.join("\\n")+"\\n"], {type:"text/csv"});
  const a = document.createElement("a"); a.href = URL.createObjectURL(blob);
  a.download = "stance_gold_" + SHEET + "_" + (state.coder||"coder").replace(/\\W+/g,"") + ".csv";
  a.click();
}
$("btn-export").onclick = exportCSV; $("btn-export2").onclick = exportCSV;

if (state.coder){ showItem(); } // returning coder: jump straight back in
drawProgress();
</script>
</body>
</html>
"""


def build(coder: str, item_ids: list[str], items: dict[str, dict]) -> None:
    data = []
    for iid in item_ids:
        it = items[iid]
        data.append({
            "item_id": it["item_id"],
            "timepoint": it["timepoint"],
            "task_wording": it["task_wording"],
            "focal_claim": it.get("focal_claim") or it.get("focal_claim_restatement", ""),
            "participant_topic_description": it["participant_topic_description"],
            "participant_reasons": it["participant_reasons"],
            "post_text": it["post_text"],
            # deliberately excluded: v1_score, orientation flags (blinding)
        })
    html = (TEMPLATE
            .replace("__CODER__", coder)
            .replace("__INSTRUCTIONS__", INSTRUCTIONS_HTML)
            .replace("__DATA__", json.dumps(data, ensure_ascii=False))
            .replace("__FR__", json.dumps([[v, l, h] for v, l, h in FOCAL_RELEVANCE]))
            .replace("__RT__", json.dumps([[v, l, h] for v, l, h in RESPONSE_TYPE]))
            .replace("__SC__", json.dumps([[v, l, h] for v, l, h in STANCE_CATEGORY])))
    out = GOLD_DIR / f"stance_gold_{coder}.html"
    out.write_text(html)
    print(f"{coder}: {len(data)} items -> {out} ({out.stat().st_size//1024} KB)")


def main() -> None:
    inputs = V2_DIR / "stance_v22_inputs.jsonl"
    if not inputs.exists():
        inputs = V2_DIR / "stance_v2_inputs.jsonl"
    items = {json.loads(l)["item_id"]: json.loads(l) for l in open(inputs)}
    order = [l.strip() for l in open(GOLD_DIR / "gold_item_ids.txt") if l.strip()]
    import csv as _csv
    b_ids = [r["item_id"] for r in _csv.DictReader(open(GOLD_DIR / "stance_gold_coderB.csv"))
             if r["item_id"] and not r["item_id"].startswith("<")]
    # the second row of the sheet is the hint row with empty item_id; filtered above
    build("coderA", order, items)
    build("coderB", [i for i in order if i in set(b_ids)], items)


if __name__ == "__main__":
    main()
