#!/usr/bin/python
"""
All delay functions required for testing.
"""
import subprocess
import json
import glob
import os
from TestSuite.query import tip

def force_block_change(block=0):
    """
    A delay function that forces the block on the chain to change. 
    This is useful for waiting for validated functions to hit the 
    chain before moving on.
    """
    root    = os.environ['ROOT']
    cli     = os.environ['cli']
    network = os.environ['network']

    tmp = root+"/tmp/"

    # get current tip
    tip(cli, tmp, network)

    # get the block data
    with open(tmp+"tip.json", "r") as read_content:
        data = json.load(read_content)
    
    next_block = int(data['block'])

    if block == 0:
        block = next_block
        return force_block_change(block)
    
    if next_block > block:
        return True
    else:
        return force_block_change(block)

