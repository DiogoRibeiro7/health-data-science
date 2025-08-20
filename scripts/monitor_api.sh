#!/usr/bin/env bash
URL=${1:-http://localhost:8000/ping}
if curl -fs "$URL" > /dev/null; then
  echo "API healthy"
else
  echo "API unreachable" >&2
  exit 1
fi
