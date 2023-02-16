#!/usr/bin/python
"""
The Cardano Python Test Suite
"""
import os
from dotenv import load_dotenv
from TestSuite.predicate import check
from TestSuite.Tests.test_tree import test_tree, example_tree, example_tree_with_failures

###############################################################################
# This sections needs to be inside a test function file.
###############################################################################
# Load environment variables from .node.env file
load_dotenv('.node.env')

# Access the environment variable defined in .node.env
root    = os.environ['ROOT']
cli     = os.environ['cli']
network = os.environ['network']
socket  = os.environ['socket']

# Set the CARDANO_NODE_SOCKET_PATH environment variable
os.environ["CARDANO_NODE_SOCKET_PATH"] = socket

# tmp dir
tmp = root+"/tmp/"

# might need this later to figure out pathing for stuff
# relative_path = os.path.relpath(path1, path2)

# Addresses
newm_addr      = open(root+'/addresses/newm.addr').read()
artist_addr    = open(root+'/addresses/artist.addr').read()
collat_addr    = open(root+'/addresses/collat.addr').read()
reference_addr = open(root+'/addresses/reference.addr').read()
multisig1_addr = open(root+'/addresses/multisig1.addr').read()
multisig2_addr = open(root+'/addresses/multisig2.addr').read()
multisig3_addr = open(root+'/addresses/multisig3.addr').read()
nftLock_addr   = open(root+'/addresses/nftLock.addr').read()
nftMint_addr   = open(root+'/addresses/nftMint.addr').read()
ftLock_addr    = open(root+'/addresses/ftLock.addr').read()
ftMint_addr    = open(root+'/addresses/ftMint.addr').read()
###############################################################################

print("\033[96m\nThe Cardano Python Test Suite For NEWM\n\033[0m")

# Run all the tests here
fails = 0
for predicate in example_tree:
    # check if the predice
    result = check(*predicate)
    
    # count the number of failures
    if result is False:
        fails += 1

# Print out helpful text
if fails > 0:
    print(f"\033[91m\nFailed: {fails} Tests\033[0m")
else:
    print("\033[92m\nAll Tests Are Successful!\033[0m")