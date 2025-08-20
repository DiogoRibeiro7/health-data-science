#!/usr/bin/env bash
set -e
IMAGE=${1:-health-data-science}
docker build -t "$IMAGE" .
