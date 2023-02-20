#!/usr/bin/python
"""
All cardano-cli address functions required for testing.
"""
import subprocess
import json

def address_key_hash(cli, verificaiton_key):
    """
    Return the address key hash from a verificaiton key.
    """
    func = [
        cli,
        'address',
        'key-hash',
        '--payment-verification-key-file',
        verificaiton_key
    ]

    p = subprocess.Popen(func, stdout=subprocess.PIPE, shell=False).stdout.read().decode('utf-8').rstrip()
    return p
