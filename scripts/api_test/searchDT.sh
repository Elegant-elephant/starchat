#!/usr/bin/env bash

Q="${1:-'cannot access my account'}"
S="${2:-0.0}"
B="${3:-100.0}"
PORT=${4:-8888}
INDEX_NAME=${5:-index_english_0}
curl -v -H "Authorization: Basic $(echo -n 'test_user:p4ssw0rd' | base64)" \
  -H "Content-Type: application/json" -X POST http://localhost:${PORT}/${INDEX_NAME}/decisiontable_search -d "{
	\"queries\": \"${Q}\",
	\"min_score\": ${S},
	\"boost_exact_match_factor\": ${B},
	\"from\": 0,	
	\"size\": 10
}"
