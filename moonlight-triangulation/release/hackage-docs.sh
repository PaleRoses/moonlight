#!/usr/bin/env bash
set -euo pipefail

# Cut a Hackage-acceptable documentation tarball for moonlight-triangulation.
#
# Cabal cannot do this alone. It writes a Haddock interface per sublibrary and
# hands none of them to the siblings, so the facade's re-exports render as
# unlinked text. It writes one same-named tarball per component, so the
# components overwrite one another. It names the sublibrary Hoogle databases
# with a colon, which Hackage refuses outright.
#
# --haddock-for-hackage is deliberately never used here: it suppresses the
# --read-interface options this script depends on, producing exactly the
# unlinked facade it is meant to prevent. The Hackage layout is assembled below
# instead, from the plain html trees Cabal always writes.
#
# Usage: release/hackage-docs.sh

PKG=moonlight-triangulation
PKG_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
CABAL_FILE="$PKG_DIR/$PKG.cabal"
HACKAGE_HTML_LOCATION='https://hackage.haskell.org/package/$pkg-$version/docs'

[ "$#" = "0" ] || { echo "hackage-docs.sh accepts no arguments" >&2; exit 2; }

[ -f "$CABAL_FILE" ] || { echo "no $PKG.cabal at $PKG_DIR" >&2; exit 1; }

VERSION="$(awk '/^version:/ {print $2; exit}' "$CABAL_FILE")"
# The manifest is the authoritative component list; restating it here would
# manufacture a second owner that goes stale the first time a component moves.
SUBLIBS="$(awk '/^library [a-z]+$/ {print $2}' "$CABAL_FILE")"

[ -n "$VERSION" ] || { echo "could not read version from $CABAL_FILE" >&2; exit 1; }
[ -n "$SUBLIBS" ] || { echo "could not read sublibraries from $CABAL_FILE" >&2; exit 1; }

PROJECT_ROOT="$PKG_DIR"
while [ "$PROJECT_ROOT" != "/" ] && [ ! -f "$PROJECT_ROOT/cabal.project" ]; do
  PROJECT_ROOT="$(dirname "$PROJECT_ROOT")"
done
[ -f "$PROJECT_ROOT/cabal.project" ] || {
  echo "no cabal.project above $PKG_DIR" >&2; exit 1;
}
PROJECT_FILE_NAME=cabal.project.triangulation-dev
PROJECT_FILE="$PROJECT_ROOT/$PROJECT_FILE_NAME"
[ -f "$PROJECT_FILE" ] || {
  echo "no isolated triangulation project at $PROJECT_FILE" >&2; exit 1;
}

DOCS_NAME="$PKG-$VERSION-docs"
STAGE="$(mktemp -d "${TMPDIR:-/tmp}/$DOCS_NAME.XXXXXX")"
DEST="$STAGE/$DOCS_NAME"
HADDOCK_RESPONSES="$STAGE/haddock-responses"
OUT_DIR="${MOONLIGHT_TRIANGULATION_DOCS_OUTPUT_DIRECTORY:-$PROJECT_ROOT/dist-newstyle}"
OUT="$OUT_DIR/$DOCS_NAME.tar.gz"

# @--keep-temp-files@ is required below because the second Haddock pass reuses
# Cabal's exact response files. Cabal also retains package-local object and
# interface scratch beside the ordinary build products, so close that lifetime
# explicitly. This release command is already the exclusive writer of the
# package documentation tree and archive.
cleanup() {
  find "$PROJECT_ROOT/dist-newstyle/build" \
    -type d \
    -path "*/$PKG-$VERSION/*" \
    \( -name 'haddock-objs-*' -o -name 'haddock-his-*' \) \
    -prune -exec rm -rf -- {} + 2>/dev/null || true
  rm -rf "$STAGE"
}
trap cleanup EXIT

mkdir -p "$HADDOCK_RESPONSES" "$OUT_DIR"

# Cabal resolves package locations relative to its invocation directory rather
# than the project file. Run from the owning compiler project and name the
# isolated triangulation closure explicitly; discovering the full workspace
# project here would make documentation assembly rebuild unrelated packages.
cd "$PROJECT_ROOT"

render_hidden_pages() {
  local component_responses="$1"
  local component_package_name="$2"
  local response
  response="$(grep -l "^--package-name=$component_package_name$" \
    "$component_responses"/haddock-response*.txt | head -1)"
  [ -f "$response" ] || {
    echo "missing retained Haddock response for $component_package_name" >&2; exit 1;
  }
  if grep -q '^--hide=' "$response"; then
    local visible_response="$component_responses/visible-response.txt"
    grep -v '^--hide=' "$response" > "$visible_response"
    haddock "@$visible_response" --html >/dev/null
  fi
}

# GHC 9.14's Haddock loads dynamic interfaces for sibling libraries. Keep the
# normal and dynamic interfaces in the same Cabal configuration throughout both
# passes; otherwise a prior static build leaves either a missing or stale
# .dyn_hi file. Internal pages preserve declaration-origin links for public
# re-exports, and Hoogle generation is explicit rather than a Cabal-version
# accident.
echo ">> haddocking sublibraries"
for lib in $SUBLIBS; do
  component_responses="$HADDOCK_RESPONSES/$lib"
  mkdir -p "$component_responses"
  TMPDIR="$component_responses" cabal haddock \
    "$PKG:lib:$lib" \
    --project-file="$PROJECT_FILE_NAME" \
    --enable-shared \
    --haddock-hoogle \
    --haddock-html \
    --haddock-html-location="$HACKAGE_HTML_LOCATION" \
    --haddock-internal \
    --keep-temp-files \
    >/dev/null

  # Cabal asks Haddock to process other-modules but also marks them hidden.
  # Haddock still emits instance-origin links to those modules, producing
  # perfectly polished 404s. Re-render Cabal's own component response without
  # the hide directives while its exact dependency configuration is resident.
  render_hidden_pages "$component_responses" "$PKG:$lib"
done

PKG_BUILD="$(find "$PROJECT_ROOT/dist-newstyle/build" -maxdepth 3 -type d -name "$PKG-$VERSION" | head -1)"
[ -n "$PKG_BUILD" ] || { echo "no build directory for $PKG-$VERSION" >&2; exit 1; }

# The interface URL is "." because the module pages are flattened to a single
# root below. Naming the sublibrary directory instead yields links into
# directories the flattening has already removed.
IFACE_OPTS=""
for lib in $SUBLIBS; do
  iface="$PKG_BUILD/l/$lib/doc/html/$PKG/$lib/$lib.haddock"
  [ -f "$iface" ] || { echo "missing interface for $lib at $iface" >&2; exit 1; }
  IFACE_OPTS="$IFACE_OPTS --read-interface=.,$iface"
done

echo ">> haddocking the facade against $(echo "$SUBLIBS" | wc -w | tr -d ' ') sibling interfaces"
facade_responses="$HADDOCK_RESPONSES/$PKG"
mkdir -p "$facade_responses"
TMPDIR="$facade_responses" cabal haddock \
  "$PKG:lib:$PKG" \
  --project-file="$PROJECT_FILE_NAME" \
  --enable-shared \
  --haddock-hoogle \
  --haddock-html \
  --haddock-html-location="$HACKAGE_HTML_LOCATION" \
  --haddock-options="$IFACE_OPTS" \
  --haddock-internal \
  --keep-temp-files \
  >/dev/null
render_hidden_pages "$facade_responses" "$PKG"

FACADE="$PKG_BUILD/doc/html/$PKG"
[ -f "$FACADE/Moonlight-Triangulation.html" ] || {
  echo "no facade page at $FACADE" >&2; exit 1;
}

echo ">> flattening module pages to one root"
cp -R "$FACADE" "$DEST"
mkdir -p "$DEST/src"
for lib in $SUBLIBS; do
  src="$PKG_BUILD/l/$lib/doc/html/$PKG/$lib"
  [ -d "$src" ] || { echo "missing rendered docs for $lib at $src" >&2; exit 1; }
  find "$src" -maxdepth 1 -name 'Moonlight-*.html' -exec cp -n {} "$DEST"/ \;
  if [ -d "$src/src" ]; then
    find "$src/src" -maxdepth 1 -type f -exec cp -n {} "$DEST/src"/ \;
  fi
  # Hackage refuses any filename carrying a colon, which is precisely how Cabal
  # names a sublibrary's Hoogle database.
  if [ -f "$src/$PKG:$lib.txt" ]; then
    cp -n "$src/$PKG:$lib.txt" "$DEST/$PKG-$lib.txt"
  fi
done

find "$DEST" \( -name '._*' -o -name '.DS_Store' -o -name '*:*' \) -delete

SURVIVING_COLONS="$(find "$DEST" -name '*:*' | wc -l | tr -d ' ')"
[ "$SURVIVING_COLONS" = "0" ] || {
  echo "colon-bearing filenames survived staging" >&2; exit 1;
}

PAGES="$(find "$DEST" -maxdepth 1 -name 'Moonlight-*.html' | wc -l | tr -d ' ')"
# haddock emits the interface URL verbatim, so a "." URL yields "./Module.html".
LINKS="$(grep -o 'href="\.\{0,2\}/\{0,1\}Moonlight-Triangulation-[A-Za-z-]*\.html' \
  "$DEST/Moonlight-Triangulation.html" | wc -l | tr -d ' ')"
echo ">> $PAGES module pages, $LINKS facade cross-links"
[ "$LINKS" -gt 0 ] || {
  echo "facade has no cross-links; --read-interface did not take" >&2; exit 1;
}

# bsdtar attaches com.apple.provenance as a SCHILY.xattr entry, which Hackage
# reports as a non-portable file type. `xattr -c` does not clear provenance;
# refusing it at archive time does. bsdtar accepts both flags and documents
# neither, so capability is probed by invocation rather than read out of --help.
TAR_FLAGS=""
if tar --no-xattrs --no-mac-metadata -cf /dev/null -T /dev/null >/dev/null 2>&1; then
  TAR_FLAGS="--no-xattrs --no-mac-metadata"
elif tar --no-xattrs -cf /dev/null -T /dev/null >/dev/null 2>&1; then
  TAR_FLAGS="--no-xattrs"
fi

echo ">> archiving -> $OUT"
# cabal derives the package name and version from the tarball filename, so the
# archive cannot be renamed.
( cd "$STAGE" && COPYFILE_DISABLE=1 tar $TAR_FLAGS -czf "$OUT" "$DOCS_NAME" )

# Hackage rejects anything that is not a regular file or a directory, and the
# rejection costs a round trip; refuse it here instead.
EXOTIC="$(tar tvzf "$OUT" | awk '{ t = substr($1, 1, 1); if (t != "-" && t != "d") print t }' | sort -u | tr -d '\n')"
[ -z "$EXOTIC" ] || {
  echo "archive carries non-portable entry types: $EXOTIC" >&2; exit 1;
}

echo ">> archive ready; this command does not upload"
