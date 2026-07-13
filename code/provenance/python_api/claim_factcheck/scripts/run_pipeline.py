from __future__ import annotations

import argparse
import subprocess
import sys
from pathlib import Path

from config import CODEBOOK_DIR, RESULTS_DIR


def run(cmd: list[str]) -> None:
    print("Running:", " ".join(cmd))
    subprocess.run(cmd, check=True)


def main():
    parser = argparse.ArgumentParser(description="Run the portable claim pipeline.")
    parser.add_argument("--input", required=True, help="Path to messages.csv")
    parser.add_argument("--skip-classification", action="store_true")
    parser.add_argument("--skip-eligibility", action="store_true")
    parser.add_argument("--skip-fact-check", action="store_true")
    parser.add_argument("--include-review", action="store_true")
    args = parser.parse_args()

    py = sys.executable
    scripts_dir = Path(__file__).resolve().parent

    # validate_messages.py was pruned from this provenance bundle (see
    # README_PIPELINE.md); run it only if a local copy is present.
    if (scripts_dir / "validate_messages.py").exists():
        run([py, str(scripts_dir / "validate_messages.py"), "--input", args.input])
    run([py, str(scripts_dir / "extract_claims.py"), "--input", args.input])

    if not args.skip_classification:
        run(
            [
                py,
                str(scripts_dir / "classify_claims_v2.py"),
                "--input",
                str(RESULTS_DIR / "extracted_claim_rows.csv"),
                "--codebook",
                str(CODEBOOK_DIR / "claim_category_v2_codebook.csv"),
            ]
        )

    eligibility_input = str(RESULTS_DIR / "claim_classifications_v2.csv")
    if args.skip_classification:
        eligibility_input = str(RESULTS_DIR / "extracted_claim_rows.csv")

    if not args.skip_eligibility:
        run(
            [
                py,
                str(scripts_dir / "classify_factcheck_eligibility.py"),
                "--input",
                eligibility_input,
            ]
        )
        queue_cmd = [
            py,
            str(scripts_dir / "build_factcheck_queue.py"),
            "--input",
            str(RESULTS_DIR / "factcheck_eligibility.csv"),
            "--output",
            str(RESULTS_DIR / "factcheck_queue.csv"),
        ]
        if args.include_review:
            queue_cmd += ["--queue-policy", "include_review"]
        run(queue_cmd)

    if not args.skip_fact_check:
        fact_input = str(RESULTS_DIR / "extracted_claim_rows.csv")
        if not args.skip_eligibility:
            fact_input = str(RESULTS_DIR / "factcheck_queue.csv")
        run(
            [
                py,
                str(scripts_dir / "fact_check_claims.py"),
                "--input",
                fact_input,
            ]
        )
        # summarize_factcheck_results.py was pruned from this provenance bundle
        # (reporting-only step); run it only if a local copy is present.
        if (scripts_dir / "summarize_factcheck_results.py").exists():
            run(
                [
                    py,
                    str(scripts_dir / "summarize_factcheck_results.py"),
                    "--input",
                    str(RESULTS_DIR / "fact_checked_claims.csv"),
                ]
            )


if __name__ == "__main__":
    main()
