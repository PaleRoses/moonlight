#!/usr/bin/env bash
set -euo pipefail

output="${1:-/tmp/release-root}"
inputs="${2:-/tmp/sheaf-inputs}"
repository_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"

mkdir -p "$inputs"

download() {
  local id="$1" name="$2" expected="$3"
  curl --fail --silent --show-error --location --retry 5 --retry-all-errors \
    "https://drive.usercontent.google.com/download?id=${id}&export=download&confirm=t" \
    --output "${inputs}/${name}"
  local observed
  observed="$(sha256sum "${inputs}/${name}" | awk '{print $1}')"
  if [[ "$observed" != "$expected" ]]; then
    printf 'digest mismatch for %s: expected %s observed %s\n' "$name" "$expected" "$observed" >&2
    exit 1
  fi
}

download 1qymeghRSZ7t6eUSRA9pERGOZk7jYJsko stage3.zip c3f090a1532542890f0d31559d40cbf5eedd1e516d6b7299cba7cc7c4337330c
download 1ER9j9-GQnT4-O-lHv4_xPKu_CMxEnDMH CLAIMS.md 656748d71c65c336baefdc0f1d292b2a6bb1a484f73ba5cfb97327dbbeb7af39
download 14m9BrDhaUBmKJ3A0hDAzuNOQ9JWe9kPB FORMAL_ARGUMENT.md 3af146bb9898e4d719378405fc7a1ba391bfad23a3a5f63f18849ea5b64563b2
download 1VcQ1PBfclp9CqIr7K-fw5qiuZdiHLB1Z locality.py 039fe16187aa0bd52231fc1bdca11efc61be02bc62dbf9d908d109149728782f
download 1-kmZwCkQzq86WjCF3r0ZnZ3LzYMa3Jng test_locality.py 9f712503866a7930c4b808b6221e4b4ced31dd61786ff1b1f839ca53e885b7f4
download 1h-J5H-FBTanXEDbF942mTjBbufpLfOdO THREAT_MODEL.md 57a0eea1fccb7d2e1ae65b7593da7238e292d4147b246e1583d14095c4a577b0
download 1Gnh7iYX_GQuRwkSZJHUCg0ChLifLlgBQ LITERATURE.md fe3116d197a5f010b80799e894acf251e4a9be705caa4281d21adfd489382061
download 1wtrWcEe3VUz6ZStAImMfdto7R-I1nbQJ independent_verify.mjs bdc24d29dc4c1179cea6d61b941ac6d12903edcb2e2f7478cfb8f71264a502aa

python "${repository_root}/artifact/sheaf-dominance/build_release.py" \
  --inputs "$inputs" \
  --template "${repository_root}/artifact/sheaf-dominance/template" \
  --output "$output"

python "$output/docs/check_documentation.py" --root "$output" --source-only
