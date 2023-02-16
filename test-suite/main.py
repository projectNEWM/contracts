#!/usr/bin/python
"""
The Cardano Python Test Suite
"""
import os
from dotenv import load_dotenv
from TestSuite.predicate import run
from TestSuite.Tests.test_group import test_group, example_test_group

from TestSuite.delay import force_block_change
###############################################################################
# This section may need to be inside a test function file.
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

# Run the test group
run(test_group)

# example test group
run(example_test_group)
