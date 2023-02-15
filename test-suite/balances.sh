#!/usr/bin/bash
set -e

source .node.env

#
script_path="contracts/nft-locking-contract.plutus"
script_address=$(${cli} address build --payment-script-file ${script_path} ${network})
#
ft_script_path="contracts/locking-contract.plutus"
ft_script_address=$(${cli} address build --payment-script-file ${ft_script_path} ${network})


#
newm_address=$(cat ${ROOT}/addresses/newm.addr)
artist_address=$(cat ${ROOT}/addresses/artist.addr)
reference_address=$(cat ${ROOT}/addresses/reference.addr)
collat_address=$(cat ${ROOT}/addresses/collat.addr)



echo
echo -e "\033[1;35m NFT Script Address:" 
echo -e "\n${script_address}\n";
${cli} query utxo --address ${script_address} ${network}
echo -e "\033[0m"

#
echo
echo -e "\033[1;35m FT Script Address:" 
echo -e "\n${ft_script_address}\n";
${cli} query utxo --address ${ft_script_address} ${network}
echo -e "\033[0m"

#
echo
echo -e "\033[1;36m NEWM Address:" 
echo -e "\n${newm_address}\n";
${cli} query utxo --address ${newm_address} ${network}
echo -e "\033[0m"

#
echo
echo -e "\033[1;32m Artist Address:" 
echo -e "\n${artist_address}\n";
${cli} query utxo --address ${artist_address} ${network}
echo -e "\033[0m"

#
echo
echo -e "\033[1;34m Reference Address:" 
echo -e "\n \033[1;34m ${reference_address}\n";
${cli} query utxo --address ${reference_address} ${network}
echo -e "\033[0m"

#
echo
echo -e "\033[1;33m Collateral Address:" 
echo -e "\n${collat_address}\n";
${cli} query utxo --address ${collat_address} ${network}
echo -e "\033[0m"