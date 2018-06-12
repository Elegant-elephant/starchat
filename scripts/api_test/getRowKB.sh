#!/usr/bin/env bash

ID=${1:-0}
PORT=${2:-8888}
INDEX_NAME=${3:-index_english_0}
ROUTE=${4:-knowledgebase}
# retrieve one or more entries with given ids; ids can be specified multiple times
curl -v -H "Authorization: Basic $(echo -n 'test_user:p4ssw0rd' | base64)" \
  -H "Content-Type: application/json" "http://localhost:${PORT}/${INDEX_NAME}/${ROUTE}?ids=${ID}"

