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

from config import CLAIM_ROLE_MODEL, NUM_RETRIES, REQUEST_TIMEOUT, RESULTS_DIR
from utils import append_jsonl, batched, get_content, load_jsonl

litellm.set_verbose = False

SYSTEM_PROMPT = """You are labeling fact-checked claims from AI persuasion conversations relative to a focal conspiracy belief item.

For each claim, assign:
- `stance_to_focal`: one of `supports`, `opposes`, `neutral`
- `directness_to_focal`: one of `direct`, `indirect`, `background`
- `confidence`: number from 0 to 1

Definitions:
- `supports`: if this claim were true, it would tend to increase confidence in the focal belief-item statement.
- `opposes`: if this claim were true, it would tend to decrease confidence in the focal belief-item statement.
- `neutral`: it does not materially bear on whether the focal belief-item statement is true.

- `direct`: the claim directly asserts the focal statement itself, or one of its core factual premises, causal links, or main counterclaims.
- `indirect`: the claim is relevant supporting or counter-supporting evidence, but not a direct restatement of the focal proposition.
- `background`: scene-setting, generic context, rhetoric, or broad information that is not doing meaningful inferential work on the focal item.

Important rules:
- Label relative to the BELIEF_ITEM_STATEMENT shown to the participant, not relative to the conversation's assigned bunk/debunk direction.
- A claim can be `supports` and `indirect`, or `opposes` and `direct`, etc.
- Use `neutral` for claims that are true-or-false but not actually informative about the focal item.
- Use `background` when the claim is generic context even if it points vaguely in the same direction.

Return JSON only as an array in input order. Each array item must include:
- `row_key`
- `stance_to_focal`
- `directness_to_focal`
- `confidence`
"""

STANCE_VALUES = {"supports", "opposes", "neutral"}
DIRECTNESS_VALUES = {"direct", "indirect", "background"}


def normalize_payload(payload) -> dict | None:
    if not isinstance(payload, dict):
        return None
    stance = str(payload.get("stance_to_focal", "")).strip()
    directness = str(payload.get("directness_to_focal", "")).strip()
    try:
        confidence = float(payload.get("confidence", -1))
    except (TypeError, ValueError):
        return None
    if stance not in STANCE_VALUES or directness not in DIRECTNESS_VALUES:
        return None
    return {
        "stance_to_focal": stance,
        "directness_to_focal": directness,
        "confidence": confidence,
    }


def parse_batch_payload(payload, batch_rows: list[dict]) -> list[tuple[dict, dict]]:
    if isinstance(payload, dict) and len(batch_rows) == 1:
        payload = [payload]

    if not isinstance(payload, list):
        return [(row, {"request_status": "PARSE_ERROR"}) for row in batch_rows]

    by_row_key = {}
    for item in payload:
        if isinstance(item, dict) and str(item.get("row_key", "")).strip():
            by_row_key[str(item["row_key"]).strip()] = item

    results = []
    for idx, row in enumerate(batch_rows):
        item = by_row_key.get(row["row_key"])
        if item is None and idx < len(payload) and isinstance(payload[idx], dict):
            item = payload[idx]
        normalized = normalize_payload(item)
        if normalized is None:
            results.append((row, {"request_status": "PARSE_ERROR"}))
        else:
            normalized["request_status"] = "success"
            results.append((row, normalized))
    return results


def processed_keys(path: Path, model: str) -> set[str]:
    done = set()
    for rec in load_jsonl(path):
        if rec.get("request_status") == "success" and rec.get("claim_role_model") == model:
            done.add(str(rec.get("row_key")))
    return done


def build_batches(rows: list[dict], claims_per_request: int) -> list[list[dict]]:
    grouped: dict[str, list[dict]] = {}
    for row in rows:
        grouped.setdefault(str(row.get("message_id", "")).strip() or row["row_key"], []).append(row)
    batches: list[list[dict]] = []
    for group_rows in grouped.values():
        for batch_rows in batched(group_rows, claims_per_request):
            batches.append(batch_rows)
    return batches


async def label_batch(batch_rows: list[dict], model: str, timeout: int, context_chars: int):
    focal = str(batch_rows[0].get("conRestatement", "") or "").strip()
    summary = str(batch_rows[0].get("conSummary", "") or "").strip()
    content = str(batch_rows[0].get("content", "") or "").strip()[:context_chars]
    direction = str(batch_rows[0].get("direction", "") or "").strip()
    claim_lines = []
    for idx, row in enumerate(batch_rows, start=1):
        claim_lines.append(
            (
                f'{idx}. row_key={row["row_key"]} | '
                f'claim_text="{str(row["claim_text"]).strip()}"'
            )
        )

    messages = [
        {"role": "system", "content": SYSTEM_PROMPT},
        {
            "role": "user",
            "content": (
                f"CONVERSATION_ID: {batch_rows[0].get('conversation_id', '')}\n"
                f"ASSIGNED_DIRECTION: {direction}\n"
                f"BELIEF_ITEM_STATEMENT: {focal}\n"
                f"SUMMARY: {summary}\n"
                f"SOURCE ASSISTANT MESSAGE: {content}\n\n"
                f"CLAIMS TO LABEL:\n" + "\n".join(claim_lines)
            ),
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
    parser = argparse.ArgumentParser(
        description="Classify eligible fact-checked claims by stance and directness relative to the focal belief item."
    )
    parser.add_argument(
        "--input",
        default=str(RESULTS_DIR / "pooled_claim_role_input.csv"),
    )
    parser.add_argument(
        "--output-jsonl",
        default=str(RESULTS_DIR / "claim_role_labels.jsonl"),
    )
    parser.add_argument(
        "--output-csv",
        default=str(RESULTS_DIR / "claim_role_labels.csv"),
    )
    parser.add_argument("--model", default=CLAIM_ROLE_MODEL)
    parser.add_argument("--concurrent-requests", type=int, default=8)
    parser.add_argument("--batch-size", type=int, default=24)
    parser.add_argument("--claims-per-request", type=int, default=8)
    parser.add_argument("--timeout", type=int, default=REQUEST_TIMEOUT)
    parser.add_argument("--context-chars", type=int, default=1200)
    parser.add_argument("--limit", type=int, default=None)
    args = parser.parse_args()

    input_df = pl.read_csv(args.input, infer_schema_length=10000)
    required = ["row_key", "message_id", "claim_text", "content", "conRestatement"]
    missing = [col for col in required if col not in input_df.columns]
    if missing:
        raise SystemExit(f"Input missing required columns: {', '.join(missing)}")

    done = processed_keys(Path(args.output_jsonl), args.model)
    df = input_df
    if done:
        df = df.filter(~pl.col("row_key").is_in(list(done)))
    if args.limit is not None and args.limit < df.height:
        df = df.head(args.limit)

    rows = df.to_dicts()
    print(f"Claim rows queued: {len(rows):,}")
    print(f"Model: {args.model}")

    claim_batches = build_batches(rows, args.claims_per_request)
    semaphore = asyncio.Semaphore(args.concurrent_requests)
    processed = 0
    success = 0
    start = time.time()

    async def guarded(batch_rows: list[dict]):
        async with semaphore:
            try:
                response = await label_batch(
                    batch_rows,
                    args.model,
                    args.timeout,
                    args.context_chars,
                )
                return response, batch_rows
            except Exception as exc:
                return exc, batch_rows

    tasks = [guarded(batch_rows) for batch_rows in claim_batches]
    for coro in asyncio.as_completed(tasks):
        response_or_exc, batch_rows = await coro
        if isinstance(response_or_exc, ModelResponse):
            parsed_rows = parse_batch_payload(get_content(response_or_exc), batch_rows)
            now = datetime.now().strftime("%Y-%m-%dT%H:%M:%S")
            for row, payload in parsed_rows:
                record = dict(row)
                record.update(payload)
                record["claim_role_model"] = args.model
                record["claim_role_time"] = now
                append_jsonl(args.output_jsonl, record)
                if payload.get("request_status") == "success":
                    success += 1
                processed += 1
        else:
            now = datetime.now().strftime("%Y-%m-%dT%H:%M:%S")
            for row in batch_rows:
                record = dict(row)
                record["request_status"] = f"EXCEPTION: {response_or_exc}"
                record["claim_role_model"] = args.model
                record["claim_role_time"] = now
                append_jsonl(args.output_jsonl, record)
                processed += 1

        if processed % 100 == 0 or processed == len(rows):
            rate = processed / max(time.time() - start, 1)
            print(f"[{processed:,}/{len(rows):,}] success={success:,} rate={rate:.1f}/s")

    records = load_jsonl(args.output_jsonl)
    if records:
        pl.DataFrame(records, infer_schema_length=10000).unique(subset=["row_key"], keep="last").sort("row_key").write_csv(args.output_csv)
    print(f"Saved: {args.output_jsonl}")
    print(f"Saved: {args.output_csv}")


if __name__ == "__main__":
    asyncio.run(main())
