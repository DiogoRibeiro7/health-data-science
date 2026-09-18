#!/usr/bin/env bash
set -e

bash "$(dirname "$0")/install_system_deps.sh"
Rscript "$(dirname "$0")/install_r_deps.R" --all --dev

python -m pip install pre-commit
pre-commit install

echo "Development environment ready."
