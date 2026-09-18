#!/usr/bin/env bash
# Simple setup helper that checks R compatibility, installs system
# requirements, and installs package dependencies declared in DESCRIPTION.
set -e
MIN_R=4.0.0

if ! command -v R >/dev/null 2>&1; then
  echo "R is not installed. Please install R >= ${MIN_R}." >&2
  exit 1
fi

CURRENT_R=$(R --version | head -n1 | awk '{print $3}')
if [ "$(printf '%s\n' "$MIN_R" "$CURRENT_R" | sort -V | head -n1)" != "$MIN_R" ]; then
  echo "R ${MIN_R} or higher is required; detected ${CURRENT_R}. Please update R." >&2
  exit 1
fi

echo "Installing system libraries..."
bash "$(dirname "$0")/install_system_deps.sh"

echo "Installing R dependencies from DESCRIPTION..."
Rscript "$(dirname "$0")/install_r_deps.R" || {
  echo "Failed to install R dependencies. Check the messages above for details." >&2
  exit 1
}

echo "Setup complete. You can now install or run the package."
