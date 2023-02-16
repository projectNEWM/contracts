#!/usr/bin/python
"""
All parsing functions required for testing.
"""
import subprocess
import json

def txin(token_wallet, cost, tmp):
    """
    TODO
    
    Construct the txin string, out going addresses list, token amount object,
    and the number of inputs inside the current wallet state.
    """
    string = ['--tx-in']
    amount = {}
    addresses = []
    number = 0
    with open(tmp+"utxo.json", "r") as read_content:
        data = json.load(read_content)
    # store all tokens from utxo
    for d in data:
        # Get the currency
        for currency in data[d]['value']:
            try:
                # Either dict of token data or integer value of lovelace
                if isinstance(data[d]['value'][currency], dict) is True:
                    amount[currency].update(data[d]['value'][currency])
                else:
                    amount[currency] += data[d]['value'][currency]
            except KeyError:
                amount[currency] = data[d]['value'][currency]
        # Increment number of inputs
        number += 1
        # Build txin string
        string.append(d)
        string.append('--tx-in')
        # Get utxo input address
        txhash = d.split('#')[0]
        addr = get_address_from_hash(txhash, token_wallet, cost)
        # Only send to other wallets
        if addr != token_wallet:
            addresses.append(addr)
    return string, addresses, amount, number