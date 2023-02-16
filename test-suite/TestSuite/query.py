#!/usr/bin/python
"""
All cardano-cli query functions required for testing.
"""
import subprocess
import json

def protocol_parameters(cli, network, tmp):
    """
    Query the protocol parameters and save to the tmp folder.
    """
    # Call the cardano-cli query protocol-parameter function
    func = [
        cli,
        'query',
        'protocol-parameters'
    ]
    func += network.split(" ")
    func += [
        '--out-file',
        tmp+'protocol-parameters.json'
    ]
    p = subprocess.Popen(func)
    p.communicate()

def utxo(cli, network, address, tmp):
    """
    Query the utxo from the wallet address and save to the tmp folder.
    """
    func = [
        cli,
        'query',
        'utxo'
    ]
    func += network.split(" ")
    func += [
        '--address',
        address,
        '--out-file',
        tmp+'utxo.json'
    ]
    p = subprocess.Popen(func)
    p.communicate()