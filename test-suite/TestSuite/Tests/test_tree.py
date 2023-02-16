#!/usr/bin/python
"""
Define test trees as list of predicate tuples.A predicate tuple has the form (statement, assertion, function).
"""
from TestSuite.predicate import t, f

# list of all the tests we want to run from folders in the TestSuite.Tests directory.
test_tree = []

# example test trees with the default True and False functions
example_tree_with_failures = [
    ("Testing if the True function is True",   True,  t),
    ("Testing if the False function is False", False, f),
    ("Testing if the False function is true",  True,  f),
    ("Testing if the True function is False",  False, t),
]

example_tree = [
    ("Testing if the True function is True",   True,  t),
    ("Testing if the False function is False", False, f),
]