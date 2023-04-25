#!/usr/bin/python
"""
The Cardano Python Test Suite
"""
import os
from dotenv import load_dotenv
from TestSuite.predicate import run
from TestSuite.Tests.test_group import test_group, example_test_group

# Load environment variables from .node.env file
load_dotenv('.node.env')

# Set the CARDANO_NODE_SOCKET_PATH environment variable
socket  = os.environ['socket']
os.environ["CARDANO_NODE_SOCKET_PATH"] = socket

# Run the test group
run(test_group)

# # example test group
# run(example_test_group)
