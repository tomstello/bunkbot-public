#!/usr/bin/env python3
"""Rewrite the rendered Word SI's table/figure numbering from bookdown's
section.sequence style ("Table 4.3") to the PDF's flat S-numbering ("Table S17"),
using the crosswalk emitted by code/R/si_crosswalk.R.

Never edits in place: writes <input>_snum.docx next to the input. Fails loudly if
the number of caption rewrites does not match the crosswalk (a signal the render
and crosswalk are out of sync — regenerate the crosswalk from the same render).

Usage: python3 code/postprocess_word_si.py output/Bunkbot_SI.docx
       [--crosswalk output/si_label_crosswalk.csv]
Stdlib only (zipfile + re).
"""
from __future__ import annotations

import argparse
import csv
import re
import shutil
import sys
import zipfile
from pathlib import Path


def _repo_root(start: Path) -> Path:
    p = start.resolve()
    for cand in [p, *p.parents]:
        if (cand / "data").is_dir() and (cand / "code").is_dir():
            return cand
    raise RuntimeError("repo root not found")


REPO_ROOT = _repo_root(Path(__file__))

# strip XML tags between characters of a caption prefix so split runs still match
RUN_SPLIT = r"(?:</w:t>(?:(?!</w:p>).)*?<w:t(?:\s[^>]*)?>)?"


def flexi_pattern(text: str) -> str:
    """Regex matching `text` even if Word split it across text runs."""
    parts = [re.escape(ch) for ch in text]
    return RUN_SPLIT.join(parts)


def main() -> None:
    ap = argparse.ArgumentParser()
    ap.add_argument("docx", nargs="?", default=str(REPO_ROOT / "output" / "Bunkbot_SI.docx"))
    ap.add_argument("--crosswalk", default=str(REPO_ROOT / "output" / "si_label_crosswalk.csv"))
    args = ap.parse_args()

    docx = Path(args.docx)
    cw_path = Path(args.crosswalk)
    if not docx.exists():
        sys.exit(f"not found: {docx}")
    if not cw_path.exists():
        sys.exit(f"crosswalk not found: {cw_path} (run Rscript code/R/si_crosswalk.R first)")

    rows = list(csv.DictReader(open(cw_path, newline="")))
    n_tab = sum(1 for r in rows if r["kind"] == "table")
    n_fig = sum(1 for r in rows if r["kind"] == "figure")

    out_path = docx.with_name(docx.stem + "_snum.docx")
    shutil.copy(docx, out_path)

    with zipfile.ZipFile(docx) as zin:
        xml = zin.read("word/document.xml").decode("utf-8")

    # longest-first so "Table 4.13" is replaced before "Table 4.1"
    rows_sorted = sorted(rows, key=lambda r: -len(r["word_number"]))
    caption_hits = 0
    body_hits = 0
    for r in rows_sorted:
        word_lab = ("Table " if r["kind"] == "table" else "Figure ") + r["word_number"]
        s_lab = ("Table " if r["kind"] == "table" else "Figure ") + r["s_number"]
        # caption occurrences carry a trailing colon; count separately
        pat_cap = re.compile(flexi_pattern(word_lab + ":"), flags=re.S)
        xml, k = pat_cap.subn(s_lab.replace("\\", "\\\\") + ":", xml)
        caption_hits += k
        # body cross-references: word boundary so 4.1 does not eat 4.13 (longest-first
        # ordering already guards this; the boundary guards trailing digits)
        pat_body = re.compile(flexi_pattern(word_lab) + r"(?![.\d])", flags=re.S)
        xml, k = pat_body.subn(s_lab.replace("\\", "\\\\"), xml)
        body_hits += k

    leftovers = re.findall(r"(?:Table|Figure) \d+\.\d+", xml)
    # each caption appears once as a TableCaption/ImageCaption paragraph and once as a
    # w:tblCaption / accessibility attribute, so hits land between 1x and ~2x the crosswalk
    if not (len(rows) <= caption_hits <= 2 * len(rows) + 4):
        sys.exit(f"FAIL: rewrote {caption_hits} captions vs crosswalk {len(rows)} "
                 f"({n_tab} tables + {n_fig} figures) — outside the expected 1-2x band. "
                 f"Leftovers: {leftovers[:10]}")
    if leftovers:
        sys.exit(f"FAIL: {len(leftovers)} section-numbered references survived: {leftovers[:10]}")

    with zipfile.ZipFile(docx) as zin, zipfile.ZipFile(out_path, "w", zipfile.ZIP_DEFLATED) as zout:
        for item in zin.infolist():
            data = xml.encode("utf-8") if item.filename == "word/document.xml" else zin.read(item.filename)
            zout.writestr(item, data)

    # body_hits is counted on the caption-rewritten xml, so it already excludes captions
    print(f"OK: {caption_hits} captions + {body_hits} body refs rewritten -> {out_path.name}")


if __name__ == "__main__":
    main()
