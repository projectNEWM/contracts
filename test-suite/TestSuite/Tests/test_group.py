#!/usr/bin/python
"""
Define test groups as list of test trees. A test group tuple has the form (statement, test_tree).
"""
from TestSuite.Tests.test_tree import test_tree, example_tree, example_tree_with_failures

# main test group for the NEWM contracts
test_group = [
    ("Tokenization And Fractionalization Test Tree", test_tree),
]

example_test_group = [
    ("Example test tree that succeeds", example_tree),
    ("Example test tree with failures", example_tree_with_failures),
]