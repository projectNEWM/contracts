#!/usr/bin/python
"""
All delay functions required for testing.
"""
import json
from query import tip

def force_block_change(socket):
    """
    A delay function that forces the block on the chain to change. 
    This is useful for waiting for validated functions to hit the 
    chain before moving on.
    """
    root    = ".."
    cli     = "cardano-cli"
    network = "--testnet-magic 1"

    tmp = root+"/tmp/"
    
    block = 0
    
    while True:
        # get current tip
        tip(cli, socket, tmp, network)
        # get the block data
        with open(tmp+"tip.json", "r") as read_content:
            data = json.load(read_content)
        
        next_block = int(data['block'])

        if block == 0:
            block = next_block
        
        elif next_block > block:
            return True

