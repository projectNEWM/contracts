import os
import requests
import json
###############################################################################
#
# Requires Blockfrost API Key.
#
# Check env var then file.
try:
    API_KEY = os.environ['BLOCKFROST_API_KEY']
    headers = { 'Project_id': API_KEY }
except KeyError:
    # Check if the key is inside the blockfrost_api.key file.
    try:
        with open("blockfrost_api.key", "r") as read_content: API_KEY = read_content.read().splitlines()[0]
        headers = { 'Project_id': API_KEY }
    except FileNotFoundError:
        exit(1)
    except IndexError:
        exit(1)
    
# mainnet = "https://cardano-mainnet.blockfrost.io/api/v0/"
preprod = "https://cardano-preprod.blockfrost.io/api/v0/"
###############################################################################
#
# Use requests get to return json data.
#
#
def get(endpoint:str) -> dict:
    """
    Return the json reponse from an endpoint.
    """

    try:
        response = requests.get(endpoint, headers=headers).json()
    except (requests.exceptions.MissingSchema, json.decoder.JSONDecodeError):
        response = {}
    return response
###############################################################################
def utxo_slot(utxo: str) -> int:
    endpoint = 'txs/{}'.format(utxo)
    response = get(preprod + endpoint)
    try:
        response['error']
        return -1
    except (KeyError, TypeError):
        pass
    
    return int(response['slot'])

if __name__ == "__main__":
    utxo = "7d3e9f531da314d162f8e887995a54454271d408c99aebbe8a1c65d31472354e"

    print(utxo_slot(utxo))