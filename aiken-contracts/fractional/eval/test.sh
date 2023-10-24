#!/bin/bash

TITLE="minter.params"
BLUEPRINT=$(jq ".validators[] | select(.title|contains(\"$TITLE\"))" ../plutus.json)

VALIDATOR_HASH=$(echo $BLUEPRINT | jq .hash | sed s/\"//g)
VALIDATOR=$(echo $BLUEPRINT | jq .compiledCode | sed s/\"//g)
VALIDATOR=$(cbor-diag --to hex --from diag <<< "h'$VALIDATOR'")
echo $VALIDATOR

# 2 is for v2 plutus
SCRIPT_CBOR=[2,$VALIDATOR]

# do some thing with the cli and build a tx.draft with fake fee and units

# exact cbor of just the tx
jq -r .cborHex tx.draft > tx.cbor

# get element 0, 13, 18
# create the input cbor

# in the same order as the input
# get all the resolved outputs
# [
#     {
#         0: h'HEXADDRESS', 
#         1: LOVELACE or [LOVELACE, {h'pid': {h'tkn': amt}}], 
#         2: [PURPOSE, 24(h'DATUM CBOR')]
#         3: 24(h'SCRIPT_CBOR')
#     }
# ]

# create output cbor


# then simulate
aiken tx simulate tx.cbor inputs.cbor outputs.cbor