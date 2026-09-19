#!/usr/bin/env bash
# Preview a draft post without touching the tracked _site/ tree.
# usage: R/preview-draft.sh posts/<category>/<slug>/index.qmd [--reseed]
set -euo pipefail

cd "$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

post="${1:-}"
if [ -z "$post" ]; then
  echo "usage: R/preview-draft.sh posts/<category>/<slug>/index.qmd [--reseed]" >&2
  exit 1
fi

# Seed the draft output dir so the preview server does not rebuild the whole
# site. rsync -a preserves mtimes, which is what keeps serveFiles quiet.
if [ ! -d _site-draft ] || [ "${2:-}" = "--reseed" ]; then
  echo "seeding _site-draft/ from _site/ ..."
  rsync -a --delete _site/ _site-draft/
fi

exec quarto preview "$post" --profile draft --port 4201
