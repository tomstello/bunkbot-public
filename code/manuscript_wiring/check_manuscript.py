#!/usr/bin/env python3
"""check_manuscript.py — wire the Word manuscript to the ALL_NUMBERS recompute.

The manuscript's statistics are literal run text (no fields/content controls), and
Word splits single values across runs (italics, subscripts, even mid-number, e.g.
"12." + "1"). This tool therefore works on word/document.xml with a paragraph-level
text model: concatenate every <w:t> in a paragraph, keep a char->xml-span map, match
anchors against the joined text, and project matches back onto runs for surgery.

Inputs
  wiring_map.yaml         anchor templates: literal prose with {{name}} placeholders,
                          each placeholder bound to a manifest key
  output/manuscript_numbers.csv   the value manifest written by code/manuscript_numbers.R
                          (columns: key, value_raw, value_formatted, block, filters, note)

Modes
  candidates   enumerate numeric tokens + context (map-building aid) -> CSV
  check        classify every map entry OK / STALE / NOT_FOUND / AMBIGUOUS / NO_KEY
               -> output/manuscript_check_report.{md,csv}
  redline      write a NEW "<stem> (wired).docx" where every stale value is a Word
               tracked change (w:del + w:ins, author "Bunkbot pipeline"); the input
               file is never modified. Rejecting all changes restores the original.
  clean        same, but values replaced silently (final pre-submission compile)
  figures      compare embedded media vs figures/manuscript renders; with --swap
               (or during redline/clean) drifted PNGs are replaced in the output copy

Conventions honored: never edit the author's docx in place; tracked changes must
be cleanly rejectable; stdlib zip surgery in the style of
code/postprocess_word_si.py.
"""

from __future__ import annotations

import argparse
import csv
import datetime
import html
import io
import os
import re
import sys
import zipfile

try:
    import yaml
except ImportError:  # map can also be JSON if PyYAML is absent
    yaml = None
import json

# ---------------------------------------------------------------------------
# repo layout
# ---------------------------------------------------------------------------

def repo_root(start: str | None = None) -> str:
    d = os.path.abspath(start or os.path.dirname(__file__))
    for _ in range(8):
        if os.path.exists(os.path.join(d, "code", "R", "build_all_numbers.R")):
            return d
        d = os.path.dirname(d)
    sys.exit("cannot locate repo root (need code/R/build_all_numbers.R)")

ROOT = repo_root()
DEFAULT_DOCX = os.path.join(
    ROOT, "AI can effectively promote conspiracies unless it is truth constrained "
          "LATEST DRAFT (wired, citations fixed).docx")
DEFAULT_MAP = os.path.join(ROOT, "code", "manuscript_wiring", "wiring_map.yaml")
DEFAULT_PROSE = os.path.join(ROOT, "code", "manuscript_wiring", "prose_fixes.yaml")
DEFAULT_MANIFEST = os.path.join(ROOT, "output", "manuscript_numbers.csv")
REPORT_MD = os.path.join(ROOT, "output", "manuscript_check_report.md")
REPORT_CSV = os.path.join(ROOT, "output", "manuscript_check_report.csv")

TC_AUTHOR = "Bunkbot pipeline"

# ---------------------------------------------------------------------------
# document model: paragraphs -> runs -> chars, with char -> xml-span projection
# ---------------------------------------------------------------------------

P_RE = re.compile(r"<w:p(?:\s[^>]*)?>.*?</w:p>", re.S)
R_RE = re.compile(r"<w:r(?:\s[^>]*)?>.*?</w:r>", re.S)
T_RE = re.compile(r"<w:t(?:\s[^>]*)?>(.*?)</w:t>", re.S)
RPR_RE = re.compile(r"<w:rPr>.*?</w:rPr>", re.S)
PSTYLE_RE = re.compile(r'<w:pStyle w:val="([^"]+)"')
# one unescaped char per token: entity or literal char
TOK_RE = re.compile(r"&[a-zA-Z]+;|&#x?[0-9A-Fa-f]+;|.", re.S)


class Char:
    __slots__ = ("ch", "xs", "xe", "run_idx")

    def __init__(self, ch, xs, xe, run_idx):
        self.ch, self.xs, self.xe, self.run_idx = ch, xs, xe, run_idx


class Run:
    __slots__ = ("xs", "xe", "rpr")

    def __init__(self, xs, xe, rpr):
        self.xs, self.xe, self.rpr = xs, xe, rpr


class Para:
    __slots__ = ("xs", "xe", "chars", "runs", "text", "style")

    def __init__(self, xs, xe, chars, runs, style):
        self.xs, self.xe, self.chars, self.runs, self.style = xs, xe, chars, runs, style
        self.text = "".join(c.ch for c in chars)


def parse_document(xml: str) -> list[Para]:
    paras = []
    for pm in P_RE.finditer(xml):
        chars, runs = [], []
        style_m = PSTYLE_RE.search(pm.group(0)[:400])
        for rm in R_RE.finditer(xml, pm.start(), pm.end()):
            rpr_m = RPR_RE.search(xml, rm.start(), rm.end())
            rpr = rpr_m.group(0) if rpr_m and rpr_m.start() < rm.end() else ""
            run_idx = len(runs)
            runs.append(Run(rm.start(), rm.end(), rpr))
            for tm in T_RE.finditer(xml, rm.start(), rm.end()):
                body = tm.group(1)
                base = tm.start(1)
                for tok in TOK_RE.finditer(body):
                    chars.append(Char(html.unescape(tok.group(0)),
                                      base + tok.start(), base + tok.end(), run_idx))
        paras.append(Para(pm.start(), pm.end(),
                          chars, runs, style_m.group(1) if style_m else ""))
    return paras


# ---------------------------------------------------------------------------
# anchor templates -> regex over paragraph text
# ---------------------------------------------------------------------------

# numeric token, optionally led by a p-value comparator (convention: placeholders
# for p-values are written "p {{p}}" and capture "= .23" / "< .001" whole, so the
# manifest's formatted p carries the comparator); tolerant of the three minus/dash
# characters co-authors and Word produce
NUMTOK = r"(?:[<>=]\s*)?[+−–-]?(?:\d[\d,]*(?:\.\d+)?|\.\d+)"
PLACEHOLDER_RE = re.compile(r"\{\{(\w+)\}\}")

_CHAR_CLASSES = {
    "-": "[-−–]", "−": "[-−–]", "–": "[-−–]",
    "'": "['‘’]", "‘": "['‘’]", "’": "['‘’]",
    '"': '["“”]', "“": '["“”]', "”": '["“”]',
    "χ": "[χ\U0001d6d8\U0001d712]",  # chi variants
    "\U0001d6d8": "[χ\U0001d6d8\U0001d712]",
}


def anchor_regex(anchor: str) -> re.Pattern:
    out, pos = [], 0
    for m in PLACEHOLDER_RE.finditer(anchor):
        out.append(_literal(anchor[pos:m.start()]))
        out.append(f"(?P<{m.group(1)}>{NUMTOK})")
        pos = m.end()
    out.append(_literal(anchor[pos:]))
    return re.compile("".join(out))


def _literal(s: str) -> str:
    parts = []
    for ch in s:
        if ch.isspace():
            if parts and parts[-1] == r"[\s   ]+":
                continue
            parts.append(r"[\s   ]+")
        elif ch in _CHAR_CLASSES:
            parts.append(_CHAR_CLASSES[ch])
        else:
            parts.append(re.escape(ch))
    return "".join(parts)


def norm_value(v: str) -> str:
    """Comparison normal form for a numeric token as printed."""
    v = v.replace("−", "-").replace("–", "-")
    v = re.sub(r"\s+", " ", v.strip())
    v = re.sub(r"([<>=])\s*", r"\1 ", v)  # '<.001' == '< .001', '=.23' == '= .23'
    return v


# ---------------------------------------------------------------------------
# manifest + map
# ---------------------------------------------------------------------------

def load_manifest(path: str) -> dict[str, dict]:
    if not os.path.exists(path):
        return {}
    with open(path, newline="", encoding="utf-8") as fh:
        return {row["key"]: row for row in csv.DictReader(fh)}


def load_map(path: str) -> list[dict]:
    with open(path, encoding="utf-8") as fh:
        data = yaml.safe_load(fh) if yaml and path.endswith((".yaml", ".yml")) \
            else json.load(fh)
    entries = data["entries"] if isinstance(data, dict) else data
    seen = set()
    for e in entries:
        if e["id"] in seen:
            sys.exit(f"duplicate map id: {e['id']}")
        seen.add(e["id"])
        names = set(PLACEHOLDER_RE.findall(e["anchor"]))
        bound = set(e.get("values", {}))
        if names != bound:
            sys.exit(f"map entry {e['id']}: placeholders {sorted(names)} != bound keys {sorted(bound)}")
    return entries


# ---------------------------------------------------------------------------
# check
# ---------------------------------------------------------------------------

def load_prose(path: str) -> list[dict]:
    if not os.path.exists(path):
        return []
    with open(path, encoding="utf-8") as fh:
        data = yaml.safe_load(fh) if yaml else json.load(fh)
    return (data or {}).get("fixes", []) if isinstance(data, dict) else (data or [])


def prose_status(paras, fix):
    """-> (status, para, (a, b)). PROSE_PENDING = old sentence still present.

    The replacement is checked FIRST: for append-style fixes the `find` text is
    a substring of `replace`, so a doc that already carries the rewrite would
    otherwise re-match `find` and be classified (and re-applied) forever.
    """
    if any(re.search(_literal(fix["replace"]), p.text) for p in paras):
        return "PROSE_DONE", None, None
    old = [(p, m) for p in paras for m in re.finditer(_literal(fix["find"]), p.text)]
    if len(old) > 1:
        return "PROSE_AMBIGUOUS", None, None
    if len(old) == 1:
        p, m = old[0]
        return "PROSE_PENDING", p, (m.start(), m.end())
    return "PROSE_UNKNOWN", None, None


def find_entry(paras: list[Para], entry: dict):
    """Return (status, para, match). status in {ok, NOT_FOUND, AMBIGUOUS}."""
    rx = anchor_regex(entry["anchor"])
    hits = [(p, m) for p in paras for m in rx.finditer(p.text)]
    if not hits:
        return "NOT_FOUND", None, None
    if len(hits) > 1:
        return "AMBIGUOUS", None, None
    return "ok", hits[0][0], hits[0][1]


def run_check(paras, entries, manifest):
    rows = []
    for e in entries:
        status, para, m = find_entry(paras, e)
        if status != "ok":
            rows.append(dict(entry=e["id"], placeholder="", key="", status=status,
                             doc_value="", expected="",
                             context=e["anchor"][:90]))
            continue
        for name, key in e.get("values", {}).items():
            doc_val = norm_value(m.group(name))
            man = manifest.get(key)
            if man is None:
                st, exp = "NO_KEY", ""
            else:
                exp = norm_value(man["value_formatted"])
                st = "OK" if doc_val == exp else "STALE"
            rows.append(dict(entry=e["id"], placeholder=name, key=key, status=st,
                             doc_value=doc_val, expected=exp,
                             context=para.text[max(0, m.start(name) - 45):m.end(name) + 45]))
    return rows


def write_report(rows, md_path, csv_path):
    order = {"STALE": 0, "PROSE_PENDING": 1, "NOT_FOUND": 2, "AMBIGUOUS": 3,
             "PROSE_AMBIGUOUS": 3, "PROSE_UNKNOWN": 4, "NO_KEY": 5,
             "PROSE_DONE": 6, "OK": 7}
    rows = sorted(rows, key=lambda r: (order[r["status"]], r["entry"]))
    counts = {}
    for r in rows:
        counts[r["status"]] = counts.get(r["status"], 0) + 1
    os.makedirs(os.path.dirname(csv_path), exist_ok=True)
    with open(csv_path, "w", newline="", encoding="utf-8") as fh:
        w = csv.DictWriter(fh, fieldnames=list(rows[0].keys()) if rows else
                           ["entry", "placeholder", "key", "status", "doc_value", "expected", "context"])
        w.writeheader()
        w.writerows(rows)
    with open(md_path, "w", encoding="utf-8") as fh:
        fh.write("# Manuscript ↔ pipeline check report\n\n")
        fh.write(f"Generated {datetime.datetime.now():%Y-%m-%d %H:%M} — "
                 + ", ".join(f"**{k}: {v}**" for k, v in sorted(counts.items())) + "\n\n")
        for st in ("STALE", "PROSE_PENDING", "NOT_FOUND", "AMBIGUOUS",
                   "PROSE_AMBIGUOUS", "PROSE_UNKNOWN", "NO_KEY"):
            sub = [r for r in rows if r["status"] == st]
            if not sub:
                continue
            fh.write(f"## {st} ({len(sub)})\n\n")
            fh.write("| entry | placeholder | key | doc | pipeline | context |\n|---|---|---|---|---|---|\n")
            for r in sub:
                ctx = r["context"].replace("|", "\\|")
                fh.write(f"| {r['entry']} | {r['placeholder']} | {r['key']} | "
                         f"`{r['doc_value']}` | `{r['expected']}` | …{ctx}… |\n")
            fh.write("\n")
        fh.write(f"## OK ({counts.get('OK', 0)})\n\nListed in the CSV.\n")
    return counts


# ---------------------------------------------------------------------------
# surgery: clean replace + tracked-change redline
# ---------------------------------------------------------------------------

def xml_escape(s: str) -> str:
    return s.replace("&", "&amp;").replace("<", "&lt;").replace(">", "&gt;")


class Editor:
    """Collects (xml_start, xml_end, replacement) splices; applies right-to-left."""

    def __init__(self, xml: str):
        self.xml = xml
        self.edits: list[tuple[int, int, str]] = []
        self._next_id = 90001

    def next_id(self) -> int:
        self._next_id += 1
        return self._next_id - 1

    def splice(self, xs, xe, repl):
        self.edits.append((xs, xe, repl))

    def apply(self) -> str:
        spans = sorted(self.edits, key=lambda e: e[0])
        for i in range(1, len(spans)):
            if spans[i][0] < spans[i - 1][1]:
                raise ValueError("overlapping edits")
        out, pos, buf = [], 0, self.xml
        for xs, xe, repl in spans:
            out.append(buf[pos:xs]); out.append(repl); pos = xe
        out.append(buf[pos:])
        return "".join(out)


def replace_clean(ed: Editor, para: Para, a: int, b: int, new_text: str):
    """Replace para.text[a:b] with new_text, preserving run structure."""
    chars = para.chars[a:b]
    ed.splice(chars[0].xs, chars[0].xe, xml_escape(new_text))
    for c in chars[1:]:
        ed.splice(c.xs, c.xe, "")


def redline_paragraph(ed: Editor, para: Para, fixes: list[tuple[int, int, str]]):
    """Rewrite one paragraph's runs so every fix span (a, b, new) becomes
    <w:del> old </w:del><w:ins> new </w:ins>.

    All fixes of a paragraph are applied in ONE pass: each touched run is rebuilt
    once as alternating kept / deleted segments, so several stale values inside a
    single run (e.g. "89.1%, 87.6%, 84.7%") are handled. The <w:ins> is emitted
    right after the <w:del> segment that carries the fix's final character.
    Limitation: touched runs are rebuilt from their text; a non-text child
    (w:tab, w:br) inside a touched run would be dropped — statistics runs don't
    contain these.
    """
    stamp = datetime.datetime.now().strftime("%Y-%m-%dT%H:%M:%SZ")
    fixes = sorted(fixes)
    for (a1, b1, _n1), (a2, _b2, _n2) in zip(fixes, fixes[1:]):
        if b1 > a2:
            raise ValueError("overlapping fixes in one paragraph")
    owner: dict[int, int] = {}
    for fi, (a, b, _new) in enumerate(fixes):
        for i in range(a, b):
            owner[i] = fi
    touched = sorted({para.chars[i].run_idx for i in owner})
    per_run: dict[int, list[int]] = {ri: [] for ri in touched}
    for idx, c in enumerate(para.chars):
        if c.run_idx in per_run:
            per_run[c.run_idx].append(idx)

    for ri in touched:
        run = para.runs[ri]
        groups: list[tuple[int | None, list[int]]] = []
        for idx in per_run[ri]:
            o = owner.get(idx)
            if groups and groups[-1][0] == o:
                groups[-1][1].append(idx)
            else:
                groups.append((o, [idx]))
        parts = []
        for o, idxs in groups:
            text = "".join(para.chars[i].ch for i in idxs)
            if o is None:
                parts.append(_mk_run(run.rpr, text))
                continue
            a, b, new = fixes[o]
            parts.append(
                f'<w:del w:id="{ed.next_id()}" w:author="{TC_AUTHOR}" w:date="{stamp}">'
                + _mk_run(run.rpr, text, tag="w:delText") + "</w:del>")
            if idxs[-1] == b - 1:  # this segment ends the fix -> insert replacement
                ins_rpr = para.runs[para.chars[a].run_idx].rpr
                parts.append(
                    f'<w:ins w:id="{ed.next_id()}" w:author="{TC_AUTHOR}" w:date="{stamp}">'
                    + _mk_run(ins_rpr, new) + "</w:ins>")
        ed.splice(run.xs, run.xe, "".join(parts))


def _mk_run(rpr: str, text: str, tag: str = "w:t") -> str:
    return (f"<w:r>{rpr}<{tag} xml:space=\"preserve\">{xml_escape(text)}</{tag}></w:r>")


def collect_fixes(paras, entries, manifest):
    """-> list of (para, start, end, new_text, entry_id, key) for STALE values."""
    fixes = []
    for e in entries:
        status, para, m = find_entry(paras, e)
        if status != "ok":
            continue
        for name, key in e.get("values", {}).items():
            man = manifest.get(key)
            if man is None:
                continue
            if norm_value(m.group(name)) != norm_value(man["value_formatted"]):
                fixes.append((para, m.start(name), m.end(name),
                              man["value_formatted"], e["id"], key))
    return fixes


def apply_fixes(xml, paras, fixes, redline: bool):
    ed = Editor(xml)
    if not redline:
        for para, a, b, new, _eid, _k in fixes:
            replace_clean(ed, para, a, b, new)
        return ed.apply()
    by_para: dict[int, tuple[Para, list[tuple[int, int, str]]]] = {}
    for para, a, b, new, _eid, _key in fixes:
        by_para.setdefault(para.xs, (para, []))[1].append((a, b, new))
    for para, plist in by_para.values():
        redline_paragraph(ed, para, plist)
    return ed.apply()


# ---------------------------------------------------------------------------
# docx zip round-trip (byte-preserving for untouched entries)
# ---------------------------------------------------------------------------

def rewrite_docx(src: str, dst: str, new_document_xml: str | None,
                 media_swaps: dict[str, bytes]):
    with zipfile.ZipFile(src) as zin:
        items = zin.infolist()
        with zipfile.ZipFile(dst, "w", zipfile.ZIP_DEFLATED) as zout:
            for it in items:
                data = zin.read(it.filename)
                if it.filename == "word/document.xml" and new_document_xml is not None:
                    data = new_document_xml.encode("utf-8")
                elif it.filename in media_swaps:
                    data = media_swaps[it.filename]
                zout.writestr(it, data)


# ---------------------------------------------------------------------------
# figures
# ---------------------------------------------------------------------------

FIGMAP = {
    "word/media/image2.png": "figures/manuscript/fig_study1_merged.png",
    "word/media/image3.png": "figures/manuscript/figure3_ATE_and_veracity_aligned.png",
    "word/media/image4.png": "figures/manuscript/figure4_belief_and_posting.png",
    # image1 = Fig 1 transcript illustration (figures/manuscript/figure1/ toolchain):
    # verify-only, never auto-swapped
    "word/media/image1.png": "figures/manuscript/figure1/figure1_transcript.png",
}
AUTOSWAP = {"word/media/image2.png", "word/media/image3.png", "word/media/image4.png"}


def _pixels_equal(a: bytes, b: bytes):
    """True/False if PIL can compare pixel content; None if PIL unavailable."""
    try:
        from PIL import Image
    except ImportError:
        return None
    ia = Image.open(io.BytesIO(a)).convert("RGBA")
    ib = Image.open(io.BytesIO(b)).convert("RGBA")
    return ia.size == ib.size and ia.tobytes() == ib.tobytes()


def figure_diff(docx: str) -> tuple[list[str], dict[str, bytes]]:
    notes, swaps = [], {}
    with zipfile.ZipFile(docx) as z:
        for member, rel in FIGMAP.items():
            path = os.path.join(ROOT, rel)
            if not os.path.exists(path):
                notes.append(f"{member}: no fresh render at {rel} (run `make figures`)")
                continue
            fresh = open(path, "rb").read()
            embedded = z.read(member)
            if embedded == fresh:
                notes.append(f"{member}: identical to {rel}")
                continue
            same_px = _pixels_equal(embedded, fresh)
            if same_px:
                notes.append(f"{member}: byte-diff but PIXEL-IDENTICAL to {rel} "
                             "(recompression only) — not swapped")
                continue
            tag = "SWAPPED in output" if member in AUTOSWAP else "DIFFERS (verify-only, not swapped)"
            px = "" if same_px is not None else " [PIL absent: byte compare only]"
            notes.append(f"{member}: differs from {rel} "
                         f"({len(embedded):,} vs {len(fresh):,} bytes){px} — {tag}")
            if member in AUTOSWAP:
                swaps[member] = fresh
    return notes, swaps


# ---------------------------------------------------------------------------
# candidates (map-building aid)
# ---------------------------------------------------------------------------

NUM_SCAN = re.compile(r"[+−–-]?(?:\d[\d,]*(?:\.\d+)?|\.\d+)%?")


def run_candidates(paras, out_csv):
    heading = ""
    with open(out_csv, "w", newline="", encoding="utf-8") as fh:
        w = csv.writer(fh)
        w.writerow(["para", "heading", "token", "before", "after"])
        for i, p in enumerate(paras):
            if p.style.lower().startswith("heading") and p.text.strip():
                heading = p.text.strip()[:70]
            for m in NUM_SCAN.finditer(p.text):
                w.writerow([i, heading, m.group(0),
                            p.text[max(0, m.start() - 55):m.start()],
                            p.text[m.end():m.end() + 55]])
    print(f"candidates -> {out_csv}")


# ---------------------------------------------------------------------------
# main
# ---------------------------------------------------------------------------

def main():
    ap = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    ap.add_argument("mode", choices=["candidates", "check", "redline", "clean", "figures"])
    ap.add_argument("--docx", default=DEFAULT_DOCX)
    ap.add_argument("--map", default=DEFAULT_MAP)
    ap.add_argument("--prose", default=DEFAULT_PROSE)
    ap.add_argument("--manifest", default=DEFAULT_MANIFEST)
    ap.add_argument("--out", default=None, help="output docx/csv path")
    ap.add_argument("--swap", action="store_true", help="figures mode: write a copy with swapped media")
    args = ap.parse_args()

    if not os.path.exists(args.docx):
        sys.exit(
            "manuscript docx not found: %s\n"
            "The Word manuscript is not distributed with the public repository "
            "(only its numbers are, via the recompute). Pass --docx <path> to check "
            "a local copy." % args.docx)

    with zipfile.ZipFile(args.docx) as z:
        xml = z.read("word/document.xml").decode("utf-8")
    paras = parse_document(xml)

    if args.mode == "candidates":
        run_candidates(paras, args.out or os.path.join(ROOT, "output", "manuscript_candidates.csv"))
        return

    if args.mode == "figures":
        notes, swaps = figure_diff(args.docx)
        print("\n".join(notes))
        if args.swap and swaps:
            dst = args.out or _default_out(args.docx, "figswap")
            rewrite_docx(args.docx, dst, None, swaps)
            print(f"wrote {dst}")
        return

    entries = load_map(args.map)
    manifest = load_manifest(args.manifest)
    if not manifest:
        print(f"warning: manifest {args.manifest} missing/empty — every key will be NO_KEY",
              file=sys.stderr)

    prose = load_prose(args.prose)

    if args.mode == "check":
        rows = run_check(paras, entries, manifest)
        for f in prose:
            st, _p, _span = prose_status(paras, f)
            rows.append(dict(entry=f["id"], placeholder="(prose)", key="", status=st,
                             doc_value="", expected="", context=f["find"][:90]))
        counts = write_report(rows, REPORT_MD, REPORT_CSV)
        print(f"report -> {REPORT_MD}\n" +
              ", ".join(f"{k}: {v}" for k, v in sorted(counts.items())))
        sys.exit(1 if counts.get("STALE") or counts.get("NOT_FOUND")
                 or counts.get("AMBIGUOUS") or counts.get("PROSE_PENDING") else 0)

    # redline / clean
    fixes = collect_fixes(paras, entries, manifest)
    for f in prose:
        st, p, span = prose_status(paras, f)
        if st == "PROSE_PENDING":
            fixes.append((p, span[0], span[1], f["replace"], f["id"], "prose"))
        elif st != "PROSE_DONE":
            print(f"warning: prose fix {f['id']}: {st}", file=sys.stderr)
    fig_notes, swaps = figure_diff(args.docx)
    if not fixes and not swaps:
        print("nothing to fix: no STALE values, figures identical")
        return
    new_xml = apply_fixes(xml, paras, fixes, redline=(args.mode == "redline")) if fixes else None
    dst = args.out or _default_out(args.docx, "wired")
    rewrite_docx(args.docx, dst, new_xml, swaps)
    print(f"wrote {dst}  ({len(fixes)} value fixes as "
          f"{'tracked changes' if args.mode == 'redline' else 'silent replacements'}, "
          f"{len(swaps)} figure swaps)")
    for line in fig_notes:
        print("  " + line)
    for para, a, b, new, eid, key in fixes:
        old = para.text[a:b]
        print(f"  [{eid}] {key}: {old!r} -> {new!r}")


def _default_out(src: str, tag: str) -> str:
    stem, ext = os.path.splitext(src)
    return f"{stem} ({tag}){ext}"


if __name__ == "__main__":
    main()
