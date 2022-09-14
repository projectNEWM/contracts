#!/usr/bin/bash
set -e

# Check if user gave args
if [ $# -eq 0 ]
  then
    echo "Provide A Datum Path and Token Amount."
    exit
fi
# Check if user gave args
if [ $# -eq 1 ]
  then
    echo "Provide A Datum Path and Token Amount."
    exit
fi

# creates the json file
python3 -c "from worst_case import getWorstCaseFile;getWorstCaseFile('${1}')"

# create the asset string
asset="18446744073709551615"
tokens=$(python3 -c "from worst_case import addTokens;addTokens(${2})")

# test address
address="addr_test1qrxm0qpeek38dflguvrpp87hhewthd0mda44tnd45rjxqdt2s7gj5l4pam3pdeckkp7jwx8dsxelvq3ypv2ggzet9wcsxrp7pu"

echo $(cardano-cli transaction calculate-min-required-utxo \
    --babbage-era \
    --protocol-params-file protocol.json \
    --tx-out-inline-datum-file worst_case.json \
    --tx-out="${address} ${asset}${tokens}" | tr -dc '0-9')