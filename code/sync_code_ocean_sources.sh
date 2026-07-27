#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)"
REPO_ROOT="$(CDPATH= cd -- "${SCRIPT_DIR}/.." && pwd)"
DEST="${SCRIPT_DIR}/code_ocean_sources"

rm -rf "${DEST}"
mkdir -p "${DEST}/supplement" "${DEST}/figures"

cp "${REPO_ROOT}/results_methods.Rmd" "${DEST}/results_methods.Rmd"
cp -R "${REPO_ROOT}/supplement/." "${DEST}/supplement/"
cp -R "${REPO_ROOT}/figures/." "${DEST}/figures/"

printf 'Synchronized Code Ocean document and figure sources in %s\n' "${DEST}"
