#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd -- "$(dirname -- "$0")" && pwd)"
LEAN_ROOT="$(cd -- "${SCRIPT_DIR}/.." && pwd)"
MANIFEST_FILE="${LEAN_ROOT}/theorem-manifest.json"
HASH_FILE="${LEAN_ROOT}/theorem-manifest.json.sha256"
SCHEMA_FILE="${LEAN_ROOT}/restriction-kernel-schema.json"
SCHEMA_HASH_FILE="${LEAN_ROOT}/restriction-kernel-schema.json.sha256"

if command -v lake >/dev/null 2>&1; then
  LAKE_BIN="$(command -v lake)"
else
  echo "missing lake executable" >&2
  exit 1
fi

cd "${LEAN_ROOT}"
"${LAKE_BIN}" build MoonlightEGraphProofs
"${LAKE_BIN}" exe theorem-manifest > "${MANIFEST_FILE}"

if command -v sha256sum >/dev/null 2>&1; then
  sha256sum "${MANIFEST_FILE}" | cut -d ' ' -f1 > "${HASH_FILE}"
  sha256sum "${SCHEMA_FILE}" | cut -d ' ' -f1 > "${SCHEMA_HASH_FILE}"
elif command -v shasum >/dev/null 2>&1; then
  shasum -a 256 "${MANIFEST_FILE}" | cut -d ' ' -f1 > "${HASH_FILE}"
  shasum -a 256 "${SCHEMA_FILE}" | cut -d ' ' -f1 > "${SCHEMA_HASH_FILE}"
else
  echo "missing sha256sum or shasum" >&2
  exit 1
fi
