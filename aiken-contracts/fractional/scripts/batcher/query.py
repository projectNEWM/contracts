#!/usr/bin/python
"""
All cardano-cli query functions required for testing.
"""
import subprocess

def tip(cli, socket, tmp, network):
    """
    Query the tip of the blockchain then save to a file.
    """
    func = [
        cli,
        'query',
        'tip',
        '--socket-path',
        socket,
        '--out-file',
        tmp+'tip.json'
    ]
    func += network.split(" ")

    # this saves to out file
    p = subprocess.Popen(func)
    p.communicate()

def utxo(cli, socket, network, address, outfile):
    """
    Query the utxo from the wallet address and save to the tmp folder.
    """
    func = [
        cli,
        'query',
        'utxo',
        '--socket-path',
        socket,
    ]
    func += network.split(" ")
    func += [
        '--address',
        address,
        '--out-file',
        outfile
    ]
    
    # this saves to out file
    p = subprocess.Popen(func)
    p.communicate()