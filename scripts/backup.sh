#!/usr/bin/env bash
set -e
DIR=${1:-data}
OUT=${2:-backup.tar.gz}
tar czf "$OUT" "$DIR"
