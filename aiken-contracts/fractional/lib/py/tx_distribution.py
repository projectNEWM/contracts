import os
import requests
import json
import sqlite3
from datetime import date

list_of_address = [
    "addr1v976vnc7fsvgw0g8zn3ae5euu9er923t46a7zcz6arpntuqx009fd",
]
###############################################################################
#
# Transactions times are placed into the sqlite db.
#
con = sqlite3.connect('tx.db')
cur = con.cursor()

# Create table
cur.execute('''CREATE TABLE IF NOT EXISTS trxs (tx_hash, tx_index, block_time, address, amount);''')
###############################################################################
#
# Requires Blockfrost API Key.
#
# Check env var then file.
try:
    API_KEY = os.environ['BLOCKFROST_API_KEY']
    headers = { 'Project_id': API_KEY}
except KeyError:
    # Check if the key is inside the blockfrost_api.key file.
    try:
        with open("blockfrost_api.key", "r") as read_content: API_KEY = read_content.read().splitlines()[0]
        headers = { 'Project_id': API_KEY}
    except (FileNotFoundError, IndexError):
        print("API Key Not Found!")
        exit(1)

# Mainnet base URL
mainnet = "https://cardano-mainnet.blockfrost.io/api/v0/"
###############################################################################


# General Get Endpoint
def get(endpoint:str) -> dict:
    """
    Return the json reponse from an endpoint.
    """
    try:
        response = requests.get(endpoint, headers=headers, timeout=180).json()
    except (requests.exceptions.MissingSchema, json.decoder.JSONDecodeError):
        # return empty
        response = {}
    return response


def update_tx_db(asset:str):
    page = 1
    while True:
        print("The current page is {}".format(page))
        # start from the page
        print("get data")
        endpoint = 'assets/{}/transactions?page={}'.format(asset, page)
        response = get(mainnet + endpoint)
        # print(response)

        # Any error should return an empty list.
        try:
            response['error']
            return False
        except (KeyError, TypeError):
            pass # should be good

        # The last page will be an empty list.
        if len(response) == 0:
            print("no response")
            print(response)
            return True

        print('filter data')
        # Append the hash and increment the page.
        for obj in response:
            # get info about the tx
            tx_hash = obj['tx_hash']
            tx_index = obj['tx_index']
            block_time = obj['block_time']

            # get utxos from tx
            endpoint = 'txs/{}/utxos'.format(tx_hash)
            tx_hash_response = get(mainnet + endpoint)

            # Any error should return an empty list.
            try:
                tx_hash_response['error']
                print('tx hash error')
                return False
            except (KeyError, TypeError):
                pass # should be good
            
            inputs = tx_hash_response['inputs']
            
            hot_wallet_flag = False # assume not found
            for utxo in inputs:
                if utxo['address'] in list_of_address:
                    hot_wallet_flag = True
                    break
            
            # create an object of all address with their token amount
            
            token_wallets = {}
            if hot_wallet_flag is True:
                outputs = tx_hash_response['outputs']
                for utxo in outputs:
                    if utxo['address'] not in list_of_address:
                        amount = utxo['amount']
                        for amt in amount:
                            if amt['unit'] == asset:
                                try:
                                    token_wallets[utxo['address']] += amt['quantity']
                                except KeyError:
                                    token_wallets[utxo['address']] = amt['quantity']
            for address in token_wallets:
                amount = token_wallets[address]
                # print(address, amount)
                cur.execute('INSERT INTO trxs (tx_hash, tx_index, block_time, address, amount) VALUES (?, ?, ?, ?, ?)', (tx_hash, tx_index, block_time, address, amount))
                con.commit()
        page += 1



if __name__ == '__main__':
    policy_id = "46e607b3046a34c95e7c29e47047618dbf5e10de777ba56c590cfd5c"
    token_name = "NEWM_0"
    asset = policy_id + token_name.encode('utf-8').hex()

    # start_time = 1640890569
    # end_time   = 1641330000
    # print((end_time - start_time)/60/60/24)

    # cur.execute("SELECT * FROM trxs WHERE block_time >= {} AND block_time <= {};".format(start_time, end_time))
    # everything = cur.fetchall()
    # total = sum([int(val[4]) for val in everything])
    # print(total / pow(10, 6))

    outcome = update_tx_db(asset)
    print(outcome)
