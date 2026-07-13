from __future__ import annotations

import argparse

from utils import materialize_jsonl_to_csv


def main():
    parser = argparse.ArgumentParser(description="Convert JSONL output to CSV.")
    parser.add_argument("--input", required=True)
    parser.add_argument("--output", required=True)
    args = parser.parse_args()
    materialize_jsonl_to_csv(args.input, args.output)


if __name__ == "__main__":
    main()
