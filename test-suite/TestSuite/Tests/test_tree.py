#!/usr/bin/python
"""
Define test trees as list of predicate tuples. A predicate tuple has the form (statement, assertion, function).
"""
from TestSuite.predicate import t, f
from TestSuite.delay import force_block_change
from TestSuite.Tests.Tokenize.wrong_newm_key import wrong_newm_key
from TestSuite.Tests.Tokenize.good_transaction import good_transaction

# List of all the tests for the tokenization
test_tree = [
    ("Forced Block Delay", True, force_block_change),
    ("The NEWM key is wrong on a tokenized transaction", ['Script debugging logs: Signing Tx Error', 'Script debugging logs: Signing Tx Error'], wrong_newm_key),
    ("A correct tokenized transaction", "Transaction successfully submitted.", good_transaction),
    ("Forced Block Delay", True, force_block_change),
]

# example test trees with failures
example_tree_with_failures = [
    ("Testing if the True function is True",   True,  t),
    ("Testing if the False function is False", False, f),
    ("Testing if the False function is true",  True,  f),
    ("Testing if the True function is False",  False, t),
]

# example tree with only successes
example_tree = [
    ("Testing if the True function is True",   True,  t),
    ("Testing if the False function is False", False, f),
]