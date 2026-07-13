#!/usr/bin/env python3
from __future__ import annotations

import argparse
from pathlib import Path

import polars as pl


def main() -> None:
    parser = argparse.ArgumentParser(
        description="Split a claim-row CSV into deterministic shards by unique claim_text or message_id."
    )
    parser.add_argument("--input", required=True)
    parser.add_argument("--output-dir", required=True)
    parser.add_argument(
        "--mode",
        choices=["claim_text", "message_id"],
        default="claim_text",
        help="Shard by unique claim text for taxonomy or by message_id for eligibility.",
    )
    parser.add_argument("--shard-size", type=int, default=200)
    parser.add_argument("--prefix", default="shard")
    args = parser.parse_args()

    input_path = Path(args.input)
    output_dir = Path(args.output_dir)
    output_dir.mkdir(parents=True, exist_ok=True)

    df = pl.read_csv(input_path, infer_schema_length=10000)
    key_col = args.mode
    if key_col not in df.columns:
        raise SystemExit(f"Input is missing required column `{key_col}`.")

    keys = (
        df.select(pl.col(key_col).cast(pl.String))
        .filter(pl.col(key_col).is_not_null())
        .unique()
        .sort(key_col)
        .to_series()
        .to_list()
    )

    if not keys:
        raise SystemExit("No shard keys found.")

    shard_paths: list[Path] = []
    for shard_idx, start in enumerate(range(0, len(keys), args.shard_size)):
        chunk = keys[start : start + args.shard_size]
        shard_df = df.filter(pl.col(key_col).cast(pl.String).is_in(chunk))
        shard_path = output_dir / f"{args.prefix}_{shard_idx:04d}.csv"
        shard_df.write_csv(shard_path)
        shard_paths.append(shard_path)

    manifest = output_dir / f"{args.prefix}_manifest.csv"
    pl.DataFrame(
        {
            "shard_path": [str(p) for p in shard_paths],
            "shard_index": list(range(len(shard_paths))),
        }
    ).write_csv(manifest)

    print(f"Saved shards to: {output_dir}")
    print(f"Mode: {args.mode}")
    print(f"Unique keys: {len(keys):,}")
    print(f"Shards: {len(shard_paths):,}")
    print(f"Manifest: {manifest}")


if __name__ == "__main__":
    main()
