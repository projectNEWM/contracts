#!/bin/bash

#increment the datum numbers so we can mint the next NFT

cat data/current_datum.json | jq -e '.fields[1].int += 1' > data/current_datum.json.new
cat data/next_datum.json | jq -e '.fields[1].int += 1' > data/next_datum.json.new

mv data/current_datum.json.new data/current_datum.json
mv data/next_datum.json.new data/next_datum.json