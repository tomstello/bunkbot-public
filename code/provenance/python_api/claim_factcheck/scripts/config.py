from __future__ import annotations

import os
from pathlib import Path

from dotenv import load_dotenv


def _repo_root(start: Path) -> Path:
    p = start.resolve()
    for cand in [p, *p.parents]:
        if (cand / "data").is_dir() and (cand / "code").is_dir():
            return cand
    raise RuntimeError("repo root (dir containing data/ and code/) not found")


# ROOT_DIR is the pipeline package dir (ships scripts/, codebooks/, .env).
ROOT_DIR = Path(__file__).resolve().parent.parent
REPO_ROOT = _repo_root(Path(__file__))
load_dotenv(ROOT_DIR / ".env")

# The codebook SHIPS with the pipeline (script-dir-relative, read-only).
CODEBOOK_DIR = ROOT_DIR / "codebooks"
# DATA_DIR (portable messages.csv input) and RESULTS_DIR (all stage outputs) are TRANSIENT
# pipeline intermediates -> route to the repo working dir; they are not shipped.
WORK_DIR = REPO_ROOT / "output" / "provenance_work" / "claim_factcheck"
DATA_DIR = WORK_DIR / "data"
RESULTS_DIR = WORK_DIR

DATA_DIR.mkdir(parents=True, exist_ok=True)
RESULTS_DIR.mkdir(parents=True, exist_ok=True)
CODEBOOK_DIR.mkdir(parents=True, exist_ok=True)

OPENROUTER_API_KEY = os.environ.get("OPENROUTER_API_KEY", "").strip()
if OPENROUTER_API_KEY:
    os.environ["OPENROUTER_API_KEY"] = OPENROUTER_API_KEY

EXTRACTION_MODEL = os.environ.get(
    "EXTRACTION_MODEL",
    "openrouter/openai/gpt-5-mini",
)
CLAIM_CODING_MODEL = os.environ.get(
    "CLAIM_CODING_MODEL",
    "openrouter/openai/gpt-5-mini",
)
ELIGIBILITY_MODEL = os.environ.get(
    "ELIGIBILITY_MODEL",
    "openrouter/openai/gpt-5-mini",
)
CLAIM_ROLE_MODEL = os.environ.get(
    "CLAIM_ROLE_MODEL",
    ELIGIBILITY_MODEL,
)
FACT_CHECK_MODEL = os.environ.get(
    "FACT_CHECK_MODEL",
    "openrouter/perplexity/sonar-pro",
)
FOCAL_VERACITY_MODEL = os.environ.get(
    "FOCAL_VERACITY_MODEL",
    FACT_CHECK_MODEL,
)

REQUEST_TIMEOUT = int(os.environ.get("LITELLM_REQUEST_TIMEOUT", "120"))
NUM_RETRIES = int(os.environ.get("LITELLM_NUM_RETRIES", "3"))
