#!/bin/sh
# One command to set up and run everything on macOS and Linux: ./quickstart.sh
# Checks for R, then quickstart.R does the rest (packages, Quarto, data, rendering).
cd "$(dirname "$0")" || exit 1

while ! command -v Rscript >/dev/null 2>&1; do
  echo "R is not installed. Install R from https://cran.r-project.org/."
  printf "Press Enter when it is installed (or Ctrl+C to stop): "
  read -r _ || exit 1
done

exec Rscript quickstart.R
