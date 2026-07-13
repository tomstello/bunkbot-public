from __future__ import annotations

import argparse
import asyncio
import json
import random
import time
from datetime import datetime
from pathlib import Path

import litellm
import polars as pl
from litellm import acompletion
from litellm.types.utils import ModelResponse

from config import CLAIM_CODING_MODEL, CODEBOOK_DIR, NUM_RETRIES, REQUEST_TIMEOUT, RESULTS_DIR
from utils import append_jsonl, batched, get_content, load_jsonl

litellm.set_verbose = False

CATEGORY_TO_SPECIFICITY_FLAG = {
    "specific_empirical": "specific_checkable",
    "inferential_logical": "object_level_inference",
    "meta_epistemic": "meta_epistemic",
    "moral_political_evaluation": "evaluative",
    "low_information": "low_information",
    "general_underspecified": "underspecified_empirical",
}


def load_codebook(path: Path) -> tuple[dict[str, dict], str]:
    df = pl.read_csv(path, infer_schema_length=10000)
    rows = df.to_dicts()
    mapping = {row["granular_subtype"]: row for row in rows}
    prompt_lines = []
    for row in rows:
        prompt_lines.append(
            (
                f'{row["granular_subtype"]} | {row["claim_category_v2"]} | '
                f'{row["subtype_label"]} | Definition: {row["definition"]} | '
                f'Example A: "{row["example_1"]}" | Example B: "{row["example_2"]}"'
            )
        )
    return mapping, "\n".join(prompt_lines)


def build_system_prompt(codebook_prompt: str) -> str:
    return f"""You are doing neutral research annotation on claims extracted from AI-generated text. The claims may mention conspiracies, violence, war, terrorism, or other disturbing topics. Do not refuse because of the topic.

Assign exactly one granular subtype code to each claim.

Category logic:
- `specific_empirical`: concrete and checkable as written.
- `general_underspecified`: empirical in spirit, but too broad or underspecified to check as written.
- `inferential_logical`: object-level reasoning about what follows if the theory were true or why a mundane explanation fits better.
- `meta_epistemic`: advice about evidence standards, proof, source comparison, or reasoning method.
- `moral_political_evaluation`: moral, civic, political, or prudential evaluation.
- `low_information`: definitional, banal, tautological, or minimally informative statements.

Granular subtype codebook:
{codebook_prompt}

Return JSON only as an array in claim order.
Each array item must have:
- `id`
- `granular_subtype`
- `confidence`
"""


def normalize_subtype(code: str, codebook: dict[str, dict]) -> str:
    clean = str(code or "").strip().upper()
    if clean in codebook:
        return clean
    normalized = clean.replace("-", "").replace("_", "")
    for valid in codebook:
        if normalized == valid.replace("-", "").replace("_", ""):
            return valid
    return ""


def parse_response(response: ModelResponse, claims: list[str], codebook: dict[str, dict]):
    payload = get_content(response)
    if isinstance(payload, dict):
        payload = [payload]

    if not isinstance(payload, list):
        return [
            (
                claim,
                {
                    "granular_subtype": "PARSE_ERROR",
                    "confidence": -1,
                    "request_status": f"PARSE_ERROR: {str(payload)[:200]}",
                },
            )
            for claim in claims
        ]

    results = []
    for idx, claim in enumerate(claims):
        if idx < len(payload) and isinstance(payload[idx], dict):
            subtype = normalize_subtype(payload[idx].get("granular_subtype", ""), codebook)
            confidence = payload[idx].get("confidence", -1)
            if subtype:
                results.append(
                    (
                        claim,
                        {
                            "granular_subtype": subtype,
                            "confidence": confidence,
                            "request_status": "success",
                        },
                    )
                )
            else:
                results.append(
                    (
                        claim,
                        {
                            "granular_subtype": "PARSE_ERROR",
                            "confidence": -1,
                            "request_status": f"INVALID_SUBTYPE: {payload[idx].get('granular_subtype', '')}",
                        },
                    )
                )
        else:
            results.append(
                (
                    claim,
                    {
                        "granular_subtype": "PARSE_ERROR",
                        "confidence": -1,
                        "request_status": f"MISSING_INDEX_{idx}",
                    },
                )
            )
    return results


def processed_claims(path: Path, model: str) -> set[str]:
    done = set()
    for rec in load_jsonl(path):
        if rec.get("request_status") == "success" and rec.get("classification_model") == model:
            done.add(str(rec.get("claim_text")))
    return done


def enrich(subtype: str, confidence, model: str, codebook: dict[str, dict]) -> dict:
    row = codebook[subtype]
    category = row["claim_category_v2"]
    return {
        "granular_subtype": subtype,
        "claim_category_v2": category,
        "subtype_label": row["subtype_label"],
        "legacy_subtype_v1": row["legacy_subtype_v1"],
        "legacy_claim_category": row["legacy_claim_category"],
        "specificity_flag": CATEGORY_TO_SPECIFICITY_FLAG[category],
        "claim_category_v2_confidence": confidence,
        "claim_category_v2_model": model,
    }


async def classify_batch(claims: list[str], model: str, system_prompt: str, timeout: int):
    messages = [
        {"role": "system", "content": system_prompt},
        {
            "role": "user",
            "content": "\n".join(f'{i + 1}. "{claim}"' for i, claim in enumerate(claims)),
        },
    ]
    return await asyncio.wait_for(
        acompletion(
            model=model,
            messages=messages,
            temperature=0,
            num_retries=NUM_RETRIES,
            timeout=timeout,
        ),
        timeout=timeout,
    )


async def main():
    parser = argparse.ArgumentParser(description="Apply claim_category_v2 coding.")
    parser.add_argument(
        "--input",
        default=str(RESULTS_DIR / "extracted_claim_rows.csv"),
    )
    parser.add_argument(
        "--codebook",
        default=str(CODEBOOK_DIR / "claim_category_v2_codebook.csv"),
    )
    parser.add_argument(
        "--output-jsonl",
        default=str(RESULTS_DIR / "claim_classifications_v2.jsonl"),
    )
    parser.add_argument(
        "--output-csv",
        default=str(RESULTS_DIR / "claim_classifications_v2.csv"),
    )
    parser.add_argument("--model", default=CLAIM_CODING_MODEL)
    parser.add_argument("--claims-per-request", type=int, default=8)
    parser.add_argument("--concurrent-requests", type=int, default=12)
    parser.add_argument("--api-batch-size", type=int, default=24)
    parser.add_argument("--limit", type=int, default=None)
    parser.add_argument("--seed", type=int, default=17)
    parser.add_argument("--timeout", type=int, default=REQUEST_TIMEOUT)
    args = parser.parse_args()

    input_df = pl.read_csv(args.input, infer_schema_length=10000)
    claim_col = "claim_text" if "claim_text" in input_df.columns else "individual_claim"
    if claim_col not in input_df.columns:
        raise SystemExit("Input must contain `claim_text` or `individual_claim`.")

    codebook, codebook_prompt = load_codebook(Path(args.codebook))
    system_prompt = build_system_prompt(codebook_prompt)

    unique_claims = (
        input_df.select(pl.col(claim_col).alias("claim_text"))
        .filter(pl.col("claim_text").is_not_null())
        .filter(pl.col("claim_text").cast(pl.String).str.strip_chars() != "")
        .unique()
    )
    claims = unique_claims["claim_text"].to_list()

    done = processed_claims(Path(args.output_jsonl), args.model)
    if done:
        claims = [claim for claim in claims if claim not in done]

    if args.limit is not None and args.limit < len(claims):
        rng = random.Random(args.seed)
        claims = rng.sample(claims, k=args.limit)

    print(f"Unique claims queued: {len(claims):,}")
    print(f"Model: {args.model}")

    semaphore = asyncio.Semaphore(args.concurrent_requests)
    processed = 0
    success = 0
    start = time.time()

    async def guarded(batch_claims: list[str]):
        async with semaphore:
            try:
                response = await classify_batch(
                    batch_claims,
                    args.model,
                    system_prompt,
                    args.timeout,
                )
                return parse_response(response, batch_claims, codebook)
            except Exception as exc:
                return [
                    (
                        claim,
                        {
                            "granular_subtype": "API_ERROR",
                            "confidence": -1,
                            "request_status": f"EXCEPTION: {exc}",
                        },
                    )
                    for claim in batch_claims
                ]

    for api_batch in batched(
        list(batched(claims, args.claims_per_request)),
        args.api_batch_size,
    ):
        tasks = [guarded(batch) for batch in api_batch]
        for coro in asyncio.as_completed(tasks):
            batch_results = await coro
            for claim_text, result in batch_results:
                record = {
                    "claim_text": claim_text,
                    "classification_model": args.model,
                    "classification_time": datetime.now().strftime("%Y-%m-%dT%H:%M:%S"),
                    **result,
                }
                append_jsonl(args.output_jsonl, record)
                processed += 1
                if result["request_status"] == "success":
                    success += 1
            if processed % 200 == 0 or processed == len(claims):
                rate = processed / max(time.time() - start, 1)
                print(f"[{processed:,}/{len(claims):,}] success={success:,} rate={rate:.1f}/s")

    classification_rows = []
    for rec in load_jsonl(args.output_jsonl):
        if rec.get("request_status") != "success":
            continue
        subtype = normalize_subtype(rec.get("granular_subtype", ""), codebook)
        if not subtype:
            continue
        classification_rows.append(
            {
                "claim_text": rec["claim_text"],
                **enrich(
                    subtype,
                    rec.get("confidence", -1),
                    rec.get("classification_model", args.model),
                    codebook,
                ),
            }
        )

    if classification_rows:
        class_df = pl.DataFrame(classification_rows, infer_schema_length=10000)
    else:
        class_df = pl.DataFrame(
            {
                "claim_text": pl.Series([], dtype=pl.Utf8),
                "granular_subtype": pl.Series([], dtype=pl.Utf8),
                "claim_category_v2": pl.Series([], dtype=pl.Utf8),
                "subtype_label": pl.Series([], dtype=pl.Utf8),
                "legacy_subtype_v1": pl.Series([], dtype=pl.Utf8),
                "legacy_claim_category": pl.Series([], dtype=pl.Utf8),
                "specificity_flag": pl.Series([], dtype=pl.Utf8),
                "claim_category_v2_confidence": pl.Series([], dtype=pl.Int64),
                "claim_category_v2_model": pl.Series([], dtype=pl.Utf8),
            }
        )
    merged = input_df.with_columns(pl.col(claim_col).alias("claim_text")).join(
        class_df,
        on="claim_text",
        how="left",
    )
    merged.write_csv(args.output_csv)
    print(f"Saved: {args.output_jsonl}")
    print(f"Saved: {args.output_csv}")


if __name__ == "__main__":
    asyncio.run(main())
