#!/bin/bash

echo -e "\033[1;35m\nStarting Complete Build... \033[0m" 

# get info
poolId=$(cat start_info.json | jq -r .poolId)
rewardPkh=$(cat start_info.json | jq -r .rewardPkh)
rewardSc=$(cat start_info.json | jq -r .rewardSc)

echo -e "\033[1;35m\nUpdating Staking Contract \033[0m"

# store extra stuff in info folder
mkdir -p info

# starter nft data
python3 -c "import binascii;a='${poolId}';s=binascii.unhexlify(a);print([x for x in s])"    > info/pool.id
python3 -c "import binascii;a='${rewardPkh}';s=binascii.unhexlify(a);print([x for x in s])" > info/reward.pkh
python3 -c "import binascii;a='${rewardSc}';s=binascii.unhexlify(a);print([x for x in s])"  > info/reward.sc

# change the pool id
python3 -c "from update_contracts import changePoolId;changePoolId('./src/StakeContract.hs', './src/StakeContract-new.hs', $(cat info/pool.id))"
mv ./src/StakeContract-new.hs ./src/StakeContract.hs

# change payout pkh
python3 -c "from update_contracts import changeRewardPkh;changeRewardPkh('./src/StakeContract.hs', './src/StakeContract-new.hs', $(cat info/reward.pkh))"
mv ./src/StakeContract-new.hs ./src/StakeContract.hs

# change payout sc
python3 -c "from update_contracts import changeRewardSc;changeRewardSc('./src/StakeContract.hs', './src/StakeContract-new.hs', $(cat info/reward.sc))"
mv ./src/StakeContract-new.hs ./src/StakeContract.hs

echo -e "\033[1;35m\nBuilding Staking Contract\n\033[0m"

# remove old data
rm stake-contract.plutus
rm stake.addr
rm stake.hash
rm stake.bytes
rm stake.cert
rm deleg.cert

# build
cabal build -w ghc-8.10.7
cabal run stake-contract

# get stake data
cardano-cli stake-address build --stake-script-file stake-contract.plutus --testnet-magic 123 --out-file stake.addr
cardano-cli transaction policyid --script-file stake-contract.plutus > stake.hash
python3 -c "import binascii;a='$(cat stake.hash)';s=binascii.unhexlify(a);print([x for x in s])" > stake.bytes
cardano-cli stake-address registration-certificate --stake-script-file stake-contract.plutus  --out-file stake.cert
cardano-cli stake-address delegation-certificate --stake-script-file stake-contract.plutus --stake-pool-id ${poolId} --out-file deleg.cert

echo -e "\033[1;36m\nStake Addr: $(cat stake.addr) \033[0m"
echo -e "\033[1;36mStake Hash: $(cat stake.hash) \033[0m"
echo -e "\033[1;36mStake Byte: $(cat stake.bytes) \033[0m"
echo -e "\nStake Cert";cat stake.cert | jq
echo -e "\nDeleg Cert";cat deleg.cert | jq

# add in stake credential into order book
python3 -c "from update_contracts import changeStakeCred;changeStakeCred('./src/OrderBookContract.hs', './src/OrderBookContract-new.hs', $(cat stake.bytes))"
mv ./src/OrderBookContract-new.hs ./src/OrderBookContract.hs

cabal build -w ghc-8.10.7
cabal run order-book-contract
#
cardano-cli address build --payment-script-file order-book-contract.plutus --testnet-magic 2 --out-file validator.addr
cardano-cli transaction policyid --script-file order-book-contract.plutus > validator.hash
python3 -c "import binascii;a='$(cat validator.hash)';s=binascii.unhexlify(a);print([x for x in s])" > validator.bytes
#
echo -e "\nValidator Testnet Address:" $(cat validator.addr)
echo -e "\nValidator Hash:" $(cat validator.hash)
echo -e "\nValidator Bytes:" $(cat validator.bytes)
echo "DONE"