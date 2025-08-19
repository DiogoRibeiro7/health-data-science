#!/usr/bin/env bash
# Install required system libraries for the analysis scripts.
set -euo pipefail

packages=(
  r-base python3-pip pre-commit
  libcurl4-openssl-dev libssl-dev libxml2-dev
  gdal-bin libgdal-dev libgeos-dev libproj-dev
  libicu-dev cmake
)

echo "Updating package lists..."
sudo apt-get update -y

for pkg in "${packages[@]}"; do
  if dpkg -s "$pkg" >/dev/null 2>&1; then
    echo "$pkg is already installed"
  else
    echo "Installing $pkg..."
    sudo apt-get install -y "$pkg" || {
      echo "Failed to install $pkg. Please install it manually and re-run." >&2
      exit 1
    }
  fi
done

echo "System dependency installation complete."
