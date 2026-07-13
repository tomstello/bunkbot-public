from __future__ import annotations

import argparse
import asyncio
import time
from datetime import datetime
from pathlib import Path

import litellm
import polars as pl
from litellm import acompletion
from litellm.types.utils import ModelResponse

from config import FACT_CHECK_MODEL, NUM_RETRIES, REQUEST_TIMEOUT, RESULTS_DIR
from utils import append_jsonl, batched, get_content, load_jsonl

litellm.set_verbose = False

SYSTEM_PROMPT = (
    "You are a meticulous fact-checker. You will be given a CLAIM to fact-check "
    "and some CONTEXT showing the passage it was extracted from. Your task is to "
    "evaluate only the veracity of the claim. Use the context only to resolve "
    "ambiguous references. Do not fact-check the surrounding context itself. "
    "Return JSON only with keys `veracity_score` (integer 0-100) and "
    "`explanation` (brief justification)."
)


def normalize_payload(payload) -> tuple[int, str] | None:
    if not isinstance(payload, dict):
        return None
    score = payload.get("veracity_score")
    explanation = payload.get("explanation")
    try:
        score = int(score)
    except (TypeError, ValueError):
        return None
    return score, str(explanation or "").strip()


async def fact_check_one(row: dict, model: str, timeout: int, context_chars: int):
    claim_text = str(row["claim_text"]).strip()
    context = str(row.get("content", "") or "").strip()[:context_chars]
    messages = [
        {"role": "system", "content": SYSTEM_PROMPT},
        {
            "role": "user",
            "content": f'CLAIM: "{claim_text}"\n\nCONTEXT: {context}',
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


def processed_keys(path: Path, model: str) -> set[tuple[str, str]]:
    done = set()
    for rec in load_jsonl(path):
        if rec.get("request_status") == "success" and rec.get("fact_check_model") == model:
            done.add((str(rec.get("message_id")), str(rec.get("claim_text"))))
    return done


async def main():
    parser = argparse.ArgumentParser(description="Fact-check extracted claim rows.")
    parser.add_argument(
        "--input",
        default=str(RESULTS_DIR / "extracted_claim_rows.csv"),
    )
    parser.add_argument(
        "--output-jsonl",
        default=str(RESULTS_DIR / "fact_checked_claims.jsonl"),
    )
    parser.add_argument(
        "--output-csv",
        default=str(RESULTS_DIR / "fact_checked_claims.csv"),
    )
    parser.add_argument("--model", default=FACT_CHECK_MODEL)
    parser.add_argument("--concurrent-requests", type=int, default=12)
    parser.add_argument("--batch-size", type=int, default=24)
    parser.add_argument("--limit", type=int, default=None)
    parser.add_argument("--timeout", type=int, default=REQUEST_TIMEOUT)
    parser.add_argument("--context-chars", type=int, default=750)
    args = parser.parse_args()

    input_df = pl.read_csv(args.input, infer_schema_length=10000)
    claim_col = "claim_text" if "claim_text" in input_df.columns else "individual_claim"
    if claim_col not in input_df.columns:
        raise SystemExit("Input must contain `claim_text` or `individual_claim`.")
    if "message_id" not in input_df.columns:
        raise SystemExit("Input must contain `message_id`.")

    df = input_df.with_columns(pl.col(claim_col).alias("claim_text"))
    done = processed_keys(Path(args.output_jsonl), args.model)
    if done:
        df = df.filter(
            ~pl.struct(["message_id", "claim_text"]).map_elements(
                lambda row: (str(row["message_id"]), str(row["claim_text"])) in done,
                return_dtype=pl.Boolean,
            )
        )

    if args.limit is not None and args.limit < df.height:
        df = df.head(args.limit)

    rows = df.to_dicts()
    print(f"Claim rows queued: {len(rows):,}")
    print(f"Model: {args.model}")

    semaphore = asyncio.Semaphore(args.concurrent_requests)
    processed = 0
    success = 0
    start = time.time()

    async def guarded(row: dict):
        async with semaphore:
            try:
                response = await fact_check_one(
                    row,
                    args.model,
                    args.timeout,
                    args.context_chars,
                )
                return response, row
            except Exception as exc:
                return exc, row

    for batch in batched(rows, args.batch_size):
        tasks = [guarded(row) for row in batch]
        for coro in asyncio.as_completed(tasks):
            response_or_exc, row = await coro
            record = dict(row)
            record["fact_check_model"] = args.model
            record["fact_check_time"] = datetime.now().strftime("%Y-%m-%dT%H:%M:%S")

            if isinstance(response_or_exc, ModelResponse):
                payload = normalize_payload(get_content(response_or_exc))
                if payload is not None:
                    score, explanation = payload
                    record["veracity_score"] = score
                    record["explanation"] = explanation
                    record["request_status"] = "success"
                    success += 1
                else:
                    record["veracity_score"] = None
                    record["explanation"] = ""
                    record["request_status"] = "PARSE_ERROR"
            else:
                record["veracity_score"] = None
                record["explanation"] = ""
                record["request_status"] = f"EXCEPTION: {response_or_exc}"

            append_jsonl(args.output_jsonl, record)
            processed += 1
            if processed % 50 == 0 or processed == len(rows):
                rate = processed / max(time.time() - start, 1)
                print(f"[{processed:,}/{len(rows):,}] success={success:,} rate={rate:.1f}/s")

    output_rows = [rec for rec in load_jsonl(args.output_jsonl)]
    if output_rows:
        pl.DataFrame(output_rows, infer_schema_length=10000).write_csv(args.output_csv)
    print(f"Saved: {args.output_jsonl}")
    print(f"Saved: {args.output_csv}")


if __name__ == "__main__":
    asyncio.run(main())
