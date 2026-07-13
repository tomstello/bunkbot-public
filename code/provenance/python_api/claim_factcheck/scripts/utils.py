from __future__ import annotations

import ast
import json
from pathlib import Path
from typing import Iterable

import polars as pl
from litellm.types.utils import ModelResponse


REQUIRED_MESSAGE_COLUMNS = [
    "conversation_id",
    "message_id",
    "role",
    "content",
]


def batched(items: Iterable, size: int):
    batch = []
    for item in items:
        batch.append(item)
        if len(batch) == size:
            yield batch
            batch = []
    if batch:
        yield batch


def extract_json_candidate(text: str, open_char: str, close_char: str) -> str:
    start = text.find(open_char)
    end = text.rfind(close_char)
    if start == -1 or end == -1 or end < start:
        return text
    return text[start : end + 1]


def parse_json_like(text: str):
    if not isinstance(text, str):
        return text

    clean = text.strip()
    if not clean:
        return ""

    if clean.startswith("```"):
        lines = [
            line
            for line in clean.splitlines()
            if not line.strip().startswith("```")
        ]
        clean = "\n".join(lines).strip()

    candidates = [
        clean,
        extract_json_candidate(clean, "{", "}"),
        extract_json_candidate(clean, "[", "]"),
    ]

    for candidate in candidates:
        for parser in (json.loads, ast.literal_eval):
            try:
                return parser(candidate)
            except (json.JSONDecodeError, SyntaxError, ValueError):
                continue

    return clean


def get_content(response: ModelResponse):
    if not isinstance(response, ModelResponse):
        raise ValueError("response is not a valid ModelResponse")

    if not response.get("choices") or not response.get("choices")[0]:
        return ""

    content = response.get("choices")[0].get("message", {}).get("content", "")
    if not content:
        return ""

    return parse_json_like(content)


def load_jsonl(path: str | Path) -> list[dict]:
    records = []
    path = Path(path)
    if not path.exists():
        return records
    with path.open("r", encoding="utf-8") as handle:
        for line in handle:
            line = line.strip()
            if not line:
                continue
            try:
                records.append(json.loads(line))
            except json.JSONDecodeError:
                continue
    return records


def append_jsonl(path: str | Path, record: dict) -> None:
    path = Path(path)
    with path.open("a", encoding="utf-8") as handle:
        handle.write(json.dumps(record, ensure_ascii=True) + "\n")


def parse_claim_list(value) -> list[str]:
    if value is None:
        return []
    if isinstance(value, list):
        return [str(v).strip() for v in value if str(v).strip()]
    text = str(value).strip()
    if not text:
        return []
    parsed = parse_json_like(text)
    if isinstance(parsed, list):
        return [str(v).strip() for v in parsed if str(v).strip()]
    return [part.strip() for part in text.split(" || ") if part.strip()]


def load_messages_csv(path: str | Path) -> pl.DataFrame:
    return pl.read_csv(path, infer_schema_length=10000)


def validate_messages_df(df: pl.DataFrame) -> list[str]:
    errors = []
    missing = [col for col in REQUIRED_MESSAGE_COLUMNS if col not in df.columns]
    if missing:
        errors.append(f"Missing required columns: {', '.join(missing)}")
        return errors

    if df.height == 0:
        errors.append("Input CSV has zero rows.")
        return errors

    assistant_rows = df.filter(pl.col("role") == "assistant")
    if assistant_rows.height == 0:
        errors.append("No assistant rows found (`role == assistant`).")

    blank_content = assistant_rows.filter(
        pl.col("content").is_null() | (pl.col("content").cast(pl.String).str.strip_chars() == "")
    )
    if blank_content.height > 0:
        errors.append(f"{blank_content.height} assistant rows have blank content.")

    duplicate_ids = (
        assistant_rows.group_by("message_id")
        .agg(pl.len().alias("n"))
        .filter(pl.col("n") > 1)
    )
    if duplicate_ids.height > 0:
        errors.append(
            f"{duplicate_ids.height} duplicated `message_id` values found among assistant rows."
        )

    return errors


def materialize_jsonl_to_csv(jsonl_path: str | Path, csv_path: str | Path) -> None:
    records = load_jsonl(jsonl_path)
    if not records:
        return
    df = pl.DataFrame(records, infer_schema_length=10000)
    for col_name in df.columns:
        if str(df[col_name].dtype).startswith("List("):
            df = df.with_columns(pl.col(col_name).list.join(" || ").alias(col_name))
    df.write_csv(csv_path)
