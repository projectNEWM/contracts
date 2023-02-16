#!/usr/bin/python
"""
The check predicate function required for testing.
"""
import subprocess
import json
import time

def t(): return True  # Testing True Func
def f(): return False # Testing False Func

def check(statement, outcome, test_function):
    """
    Check that the statement has the correct outcome from the result of a test function.
    """
    start_time = time.time()
    result = test_function()
    end_time = time.time()
    
    if outcome == result:
        time_taken = round(end_time - start_time, 2)
        print('\033[94m', statement,'\033[0m', "\033[92mOK\033[0m", f"\033[95mTime: {time_taken} seconds\033[0m")
        return True
    else:
        print('\033[94m', statement,'\033[0m', "\033[91mFAIL\033[0m")
        return False