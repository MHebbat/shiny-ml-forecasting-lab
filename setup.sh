#!/usr/bin/env bash
# Thin wrapper: runs setup.R with Rscript and forwards arguments.
# Usage: ./setup.sh           # full install
#        ./setup.sh --dry-run # preview without installing
set -e
cd "$(dirname "$0")"
exec Rscript setup.R "$@"
