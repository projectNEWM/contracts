#!/usr/bin/bash
set -e

source .node.env

echo -e "\033[1;35m Creating Test Wallets \033[0m" 

ADDR=artist
# payment address keys
cardano-cli address key-gen \
--verification-key-file ${ROOT}/addresses/${ADDR}.vkey \
--signing-key-file      ${ROOT}/addresses/${ADDR}.skey
# wallet address
cardano-cli address build \
--payment-verification-key-file ${ROOT}/addresses/${ADDR}.vkey \
--testnet-magic 42 \
--out-file ${ROOT}/addresses/${ADDR}.addr

ADDR=collat
# payment address keys
cardano-cli address key-gen \
--verification-key-file ${ROOT}/addresses/${ADDR}.vkey \
--signing-key-file      ${ROOT}/addresses/${ADDR}.skey
# wallet address
cardano-cli address build \
--payment-verification-key-file ${ROOT}/addresses/${ADDR}.vkey \
--testnet-magic 42 \
--out-file ${ROOT}/addresses/${ADDR}.addr

ADDR=reference
# payment address keys
cardano-cli address key-gen \
--verification-key-file ${ROOT}/addresses/${ADDR}.vkey \
--signing-key-file      ${ROOT}/addresses/${ADDR}.skey
# wallet address
cardano-cli address build \
--payment-verification-key-file ${ROOT}/addresses/${ADDR}.vkey \
--testnet-magic 42 \
--out-file ${ROOT}/addresses/${ADDR}.addr

ADDR=attacker
# payment address keys
cardano-cli address key-gen \
--verification-key-file ${ROOT}/addresses/${ADDR}.vkey \
--signing-key-file      ${ROOT}/addresses/${ADDR}.skey
# wallet address
cardano-cli address build \
--payment-verification-key-file ${ROOT}/addresses/${ADDR}.vkey \
--testnet-magic 42 \
--out-file ${ROOT}/addresses/${ADDR}.addr

cp test_keys/* ${ROOT}/addresses/