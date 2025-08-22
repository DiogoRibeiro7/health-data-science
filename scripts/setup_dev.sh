#!/usr/bin/env bash
set -e

# bootstrap local development environment
if [ ! -f "renv.lock" ]; then
  echo "renv.lock not found" >&2
  exit 1
fi

R -e 'install.packages("renv"); renv::restore(prompt = FALSE)'
pip install pre-commit
pre-commit install

echo "Development environment ready."
