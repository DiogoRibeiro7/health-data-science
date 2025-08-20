#!/usr/bin/env bash
set -e
ARCHIVE=${1:-backup.tar.gz}
tar xzf "$ARCHIVE"
