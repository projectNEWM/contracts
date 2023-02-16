#!/usr/bin/python
"""
The Cardano Python Test Suite
"""
import os
from dotenv import load_dotenv
from TestSuite.predicate import check
from TestSuite.Tests.test_tree import test_tree, example_tree, example_tree_with_failures

from TestSuite.parsing import address_dict
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
addr = root+"/addresses/"

# might need this later to figure out pathing for stuff
# relative_path = os.path.relpath(path1, path2)

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