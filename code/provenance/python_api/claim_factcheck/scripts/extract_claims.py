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

from config import DATA_DIR, EXTRACTION_MODEL, REQUEST_TIMEOUT, NUM_RETRIES, RESULTS_DIR
from utils import (
    append_jsonl,
    batched,
    get_content,
    load_jsonl,
    load_messages_csv,
    parse_claim_list,
    validate_messages_df,
)

litellm.set_verbose = False

SYSTEM_PROMPT = (
    "You are doing neutral research annotation on AI-generated study messages. "
    "The text may mention conspiracies, violence, terrorism, war, sexual misconduct, "
    "or other disturbing topics. Do not refuse because of the topic. "
    "Extract only specific factual claims stated in the text, regardless of whether "
    "they are true or false. Do not include opinions, advice, moral evaluations, "
    "or generic rhetoric. You may receive one message or a small batch of messages. "
    "Return JSON only as an array in input order. Each array item must contain "
    "`message_id`, `n_factual_claims`, and `factual_claims`. If there are no specific "
    "factual claims for a message, return `n_factual_claims: 0` and an empty list."
)


def normalize_payload(payload) -> tuple[int, list[str]] | None:
    if not isinstance(payload, dict):
        return None

    claims = payload.get("factual_claims")
    if claims is None:
        claims = payload.get("claims")
    claims = parse_claim_list(claims)

    n_claims = payload.get("n_factual_claims")
    if n_claims is None:
        n_claims = len(claims)

    try:
        n_claims = int(n_claims)
    except (TypeError, ValueError):
        n_claims = len(claims)

    return n_claims, claims


def parse_batch_payload(payload, batch_rows: list[dict]) -> list[tuple[dict, dict]]:
    if isinstance(payload, dict) and len(batch_rows) == 1:
        payload = [payload]

    if not isinstance(payload, list):
        return [
            (
                row,
                {
                    "n_factual_claims": None,
                    "factual_claims": [],
                    "request_status": "PARSE_ERROR",
                },
            )
            for row in batch_rows
        ]

    by_message_id = {}
    for item in payload:
        if not isinstance(item, dict):
            continue
        msg_id = str(item.get("message_id", "")).strip()
        if msg_id:
            by_message_id[msg_id] = item

    results = []
    for idx, row in enumerate(batch_rows):
        item = None
        row_message_id = str(row["message_id"]).strip()
        if row_message_id in by_message_id:
            item = by_message_id[row_message_id]
        elif idx < len(payload) and isinstance(payload[idx], dict):
            item = payload[idx]

        normalized = normalize_payload(item)
        if normalized is None:
            results.append(
                (
                    row,
                    {
                        "n_factual_claims": None,
                        "factual_claims": [],
                        "request_status": "PARSE_ERROR",
                    },
                )
            )
            continue

        n_claims, claims = normalized
        results.append(
            (
                row,
                {
                    "n_factual_claims": n_claims,
                    "factual_claims": claims,
                    "request_status": "success",
                },
            )
        )
    return results


async def extract_batch(batch_rows: list[dict], model: str, timeout: int):
    user_lines = []
    for idx, row in enumerate(batch_rows, start=1):
        user_lines.append(
            f'MESSAGE {idx}\nmessage_id: {row["message_id"]}\ncontent: """{str(row["content"]).strip()}"""'
        )
    messages = [
        {"role": "system", "content": SYSTEM_PROMPT},
        {"role": "user", "content": "\n\n".join(user_lines)},
    ]
    try:
        response = await asyncio.wait_for(
            acompletion(
                model=model,
                messages=messages,
                temperature=0,
                num_retries=NUM_RETRIES,
                timeout=timeout,
            ),
            timeout=timeout,
        )
        return response, batch_rows
    except Exception as exc:
        return exc, batch_rows


def processed_keys(path: Path, model: str) -> set[tuple[str, str]]:
    done = set()
    for rec in load_jsonl(path):
        if rec.get("request_status") != "success":
            continue
        if rec.get("extraction_model") != model:
            continue
        done.add((str(rec.get("message_id")), str(rec.get("content"))))
    return done


def build_claim_rows(path: Path, out_csv: Path) -> None:
    rows = []
    for rec in load_jsonl(path):
        if rec.get("request_status") != "success":
            continue
        claims = parse_claim_list(rec.get("factual_claims"))
        for idx, claim in enumerate(claims, start=1):
            row = dict(rec)
            row["claim_index"] = idx
            row["claim_text"] = claim
            rows.append(row)

    if rows:
        df = pl.DataFrame(rows, infer_schema_length=10000)
    else:
        df = pl.DataFrame({"claim_text": []})
    if "factual_claims" in df.columns:
        df = df.drop("factual_claims")
    df.write_csv(out_csv)


def build_message_csv(path: Path, out_csv: Path) -> None:
    records = load_jsonl(path)
    if not records:
        return
    df = pl.DataFrame(records, infer_schema_length=10000)
    if "factual_claims" in df.columns:
        df = df.with_columns(
            pl.col("factual_claims").map_elements(
                lambda x: " || ".join(parse_claim_list(x)),
                return_dtype=pl.String,
            )
        )
    df.write_csv(out_csv)


async def main():
    parser = argparse.ArgumentParser(description="Extract claims from assistant messages.")
    parser.add_argument("--input", default=str(DATA_DIR / "messages.csv"))
    parser.add_argument(
        "--output-jsonl",
        default=str(RESULTS_DIR / "extracted_claims.jsonl"),
    )
    parser.add_argument(
        "--output-messages-csv",
        default=str(RESULTS_DIR / "extracted_claims_messages.csv"),
    )
    parser.add_argument(
        "--output-claim-rows-csv",
        default=str(RESULTS_DIR / "extracted_claim_rows.csv"),
    )
    parser.add_argument("--model", default=EXTRACTION_MODEL)
    parser.add_argument(
        "--concurrent-requests",
        type=int,
        default=24,
    )
    parser.add_argument(
        "--batch-size",
        type=int,
        default=48,
    )
    parser.add_argument("--messages-per-request", type=int, default=4)
    parser.add_argument("--limit", type=int, default=None)
    parser.add_argument("--seed", type=int, default=17)
    parser.add_argument("--timeout", type=int, default=REQUEST_TIMEOUT)
    args = parser.parse_args()

    input_path = Path(args.input)
    out_jsonl = Path(args.output_jsonl)
    out_messages_csv = Path(args.output_messages_csv)
    out_claim_rows_csv = Path(args.output_claim_rows_csv)
    out_jsonl.parent.mkdir(parents=True, exist_ok=True)

    df = load_messages_csv(input_path)
    errors = validate_messages_df(df)
    if errors:
        raise SystemExit("\n".join(errors))

    df = (
        df.filter(pl.col("role") == "assistant")
        .filter(pl.col("content").is_not_null())
        .filter(pl.col("content").cast(pl.String).str.strip_chars() != "")
    )

    done = processed_keys(out_jsonl, args.model)
    if done:
        df = df.filter(
            ~pl.struct(["message_id", "content"]).map_elements(
                lambda row: (str(row["message_id"]), str(row["content"])) in done,
                return_dtype=pl.Boolean,
            )
        )

    rows = df.to_dicts()
    if args.limit is not None and args.limit < len(rows):
        rng = random.Random(args.seed)
        rows = rng.sample(rows, k=args.limit)

    print(f"Assistant messages queued: {len(rows):,}")
    print(f"Model: {args.model}")

    semaphore = asyncio.Semaphore(args.concurrent_requests)
    processed = 0
    success = 0
    start = time.time()

    message_batches = list(batched(rows, args.messages_per_request))

    async def guarded(batch_rows: list[dict]):
        async with semaphore:
            return await extract_batch(batch_rows, args.model, args.timeout)

    for batch in batched(message_batches, args.batch_size):
        tasks = [guarded(message_batch) for message_batch in batch]
        for coro in asyncio.as_completed(tasks):
            response_or_exc, batch_rows = await coro
            if isinstance(response_or_exc, ModelResponse):
                parsed = parse_batch_payload(get_content(response_or_exc), batch_rows)
            else:
                parsed = [
                    (
                        row,
                        {
                            "n_factual_claims": None,
                            "factual_claims": [],
                            "request_status": f"EXCEPTION: {response_or_exc}",
                        },
                    )
                    for row in batch_rows
                ]

            for row, result in parsed:
                record = dict(row)
                record["extraction_model"] = args.model
                record["extraction_time"] = datetime.now().strftime("%Y-%m-%dT%H:%M:%S")
                record.update(result)
                append_jsonl(out_jsonl, record)
                processed += 1
                if result["request_status"] == "success":
                    success += 1

            if processed % 100 == 0 or processed == len(rows):
                rate = processed / max(time.time() - start, 1)
                print(f"[{processed:,}/{len(rows):,}] success={success:,} rate={rate:.1f}/s")

    build_message_csv(out_jsonl, out_messages_csv)
    build_claim_rows(out_jsonl, out_claim_rows_csv)
    print(f"Saved: {out_jsonl}")
    print(f"Saved: {out_messages_csv}")
    print(f"Saved: {out_claim_rows_csv}")


if __name__ == "__main__":
    asyncio.run(main())
