#! /usr/bin/env bash

set -e

INFILE=$(mktemp)
OUTFILE=$(mktemp)

cat > "$INFILE"
pngcrush -q "$@" "$INFILE" "$OUTFILE"
cat "$OUTFILE"

rm "$INFILE"
rm "$OUTFILE"
