#!/usr/bin/env bash
set -euo pipefail
sudo apt-get update -y
sudo apt-get install -y r-base python3-pip pre-commit \
    libcurl4-openssl-dev libssl-dev libxml2-dev \
    gdal-bin libgdal-dev libgeos-dev libproj-dev \
    libicu-dev cmake
