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
        print(f"\033[93mExpecting: {outcome} Provided: {result}\033[0m")
        return False

def run(test_group):
    """
    Run all the test trees inside a test group and count how many failures exist. Print out some helpful text.
    """
    print("\033[96m\nThe Cardano Python Test Suite For NEWM\n\033[0m")

    start_time = time.time()
    # Run all the tests here
    fails = 0
    for (statement, test_tree) in test_group:
        print("\n",statement,"\n")
        for predicate in test_tree:
            # check if the predice
            result = check(*predicate)
            
            # count the number of failures
            if result is False:
                fails += 1
    end_time = time.time()
    time_taken = round(end_time - start_time, 2)
    print(f"\033[95mTotal Test Time: {time_taken} seconds\033[0m")
    # Print out helpful text
    if fails > 0:
        print(f"\033[91m\nFailed: {fails} Tests\033[0m")
    else:
        print("\033[92m\nAll Tests Are Successful!\033[0m")