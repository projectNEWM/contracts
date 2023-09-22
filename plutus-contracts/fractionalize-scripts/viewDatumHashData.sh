#!/bin/bash
set -e

curl -H 'project_id: testnetH4w3Pty6JV590eTZ9kuJi9zJ2yFHUdJi' https://cardano-testnet.blockfrost.io/api/v0/scripts/datum/${1} | jq