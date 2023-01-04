#!/usr/bin/bash
set -e

source ../.env

#
script_path="../cogno-contract/cogno-contract.plutus"
SCRIPT_ADDRESS=$(${cli} address build --payment-script-file ${script_path} ${network})
#
ft_script_path="../marketplace-contract/marketplace-contract.plutus"
FT_SCRIPT_ADDRESS=$(${cli} address build --payment-script-file ${ft_script_path} ${network})


#
SELLER_ADDRESS=$(cat wallets/seller-wallet/payment.addr)
BUYER_ADDRESS=$(cat wallets/buyer-wallet/payment.addr)
REFERENCE_ADDRESS=$(cat wallets/reference-wallet/payment.addr)
COLLAT_ADDRESS=$(cat wallets/collat-wallet/payment.addr)
# MULTISIG_ADDRESS=$(cat wallets/multisig-wallet/payment.addr)

#
${cli} query protocol-parameters ${network} --out-file tmp/protocol.json
${cli} query tip ${network} | jq

#
echo
echo -e "\033[1;35m Cogno Script Address:" 
echo -e "\n${SCRIPT_ADDRESS}\n";
${cli} query utxo --address ${SCRIPT_ADDRESS} ${network}
echo -e "\033[0m"

#
echo
echo -e "\033[1;35m Marketplace Script Address:" 
echo -e "\n${FT_SCRIPT_ADDRESS}\n";
${cli} query utxo --address ${FT_SCRIPT_ADDRESS} ${network}
echo -e "\033[0m"

#
echo
echo -e "\033[1;36m NEWM Address:" 
echo -e "\n${SELLER_ADDRESS}\n";
${cli} query utxo --address ${SELLER_ADDRESS} ${network}
echo -e "\033[0m"

#
echo
echo -e "\033[1;32m Artist Address:" 
echo -e "\n${BUYER_ADDRESS}\n";
${cli} query utxo --address ${BUYER_ADDRESS} ${network}
echo -e "\033[0m"

#
echo
echo -e "\033[1;34m Reference Address:" 
echo -e "\n \033[1;34m ${REFERENCE_ADDRESS}\n";
${cli} query utxo --address ${REFERENCE_ADDRESS} ${network}
echo -e "\033[0m"

#
echo
echo -e "\033[1;33m Collateral Address:" 
echo -e "\n${COLLAT_ADDRESS}\n";
${cli} query utxo --address ${COLLAT_ADDRESS} ${network}
echo -e "\033[0m"

# echo
# echo -e "\033[1;34m Multisig Address: \033[0m" 
# echo -e "\n \033[1;34m ${MULTISIG_ADDRESS} \033[0m \n";
# ${cli} query utxo --address ${MULTISIG_ADDRESS} ${network}
