#!/usr/bin/python
"""
Define test trees as list of predicate tuples. A predicate tuple has the form (statement, assertion, function).
"""
from TestSuite.predicate import t, f
from TestSuite.delay import force_block_change
# import tokenize mint tests
from TestSuite.Tests.Tokenize.Mint.wrong_newm_key import wrong_mint_newm_key
from TestSuite.Tests.Tokenize.Mint.wrong_mint_amt import wrong_mint_token_amt
from TestSuite.Tests.Tokenize.Mint.wrong_cont_datum import wrong_mint_cont_datum
from TestSuite.Tests.Tokenize.Mint.wrong_cont_output_amt import wrong_mint_cont_output_amt
from TestSuite.Tests.Tokenize.Mint.wrong_script_input_amt import wrong_mint_script_input_amt
from TestSuite.Tests.Tokenize.Mint.wrong_starter_token import wrong_mint_starter_token
from TestSuite.Tests.Tokenize.Mint.good_mint_transaction import good_mint_transaction
# import fractionalize lock tests
from TestSuite.Tests.Fractional.Lock.wrong_newm_key import wrong_lock_newm_key
from TestSuite.Tests.Fractional.Lock.wrong_mint_amt import wrong_lock_mint_amt
from TestSuite.Tests.Fractional.Lock.wrong_mint_name import wrong_lock_mint_name
from TestSuite.Tests.Fractional.Lock.wrong_cont_datum import wrong_lock_cont_datum
from TestSuite.Tests.Fractional.Lock.wrong_cont_output_amt import wrong_lock_cont_output_amt
from TestSuite.Tests.Fractional.Lock.wrong_input_amt import wrong_lock_input_amt
from TestSuite.Tests.Fractional.Lock.good_lock_transaction import good_lock_transaction
# import solidification unlock tests
from TestSuite.Tests.Fractional.Unlock.wrong_newm_key import wrong_unlock_newm_key
from TestSuite.Tests.Fractional.Unlock.wrong_burn_tkn import wrong_unlock_burn_tkn
from TestSuite.Tests.Fractional.Unlock.wrong_cont_output_amt import wrong_unlock_cont_output_amt
from TestSuite.Tests.Fractional.Unlock.wrong_input_amt import wrong_unlock_input_amt
from TestSuite.Tests.Fractional.Unlock.wrong_burn_amt import wrong_unlock_burn_amt
from TestSuite.Tests.Fractional.Unlock.good_unlock_transaction import good_unlock_transaction
# import tokenize burn tests

# List of all the tests for the tokenization and fractionalization
token_and_fraction_test_tree = [
    ("Forced Block Delay", True, force_block_change),
    ("The NEWM key is wrong on a tokenized transaction", ['Script debugging logs: Signing Tx Error', 'Script debugging logs: Signing Tx Error'], wrong_mint_newm_key),
    ("The wrong mint amount on a tokenized transaction", ['Script debugging logs: NFT Minting Error', 'Script debugging logs: Incorrect Mint Amount'], wrong_mint_token_amt),
    ("The wrong continuing datum on a tokenized transaction", ['Script debugging logs: Invalid Datum Error'], wrong_mint_cont_datum),
    ("The wrong continuing output amount on a tokenized transaction", ['Script debugging logs: Single Output Error'], wrong_mint_cont_output_amt),
    ("The wrong amount of script inputs on a tokenized transaction", ['Script debugging logs: Single Input Error'], wrong_mint_script_input_amt),
    ("The wrong starter token on a script input for a tokenized transaction", ['Script debugging logs: Invalid Starter Tkn', 'Script debugging logs: Invalid Starter Tkn'], wrong_mint_starter_token),
    ("A correct tokenized transaction", "Transaction successfully submitted.", good_mint_transaction),
    ("Forced Block Delay", True, force_block_change),
    ("The NEWM key is wrong on a fractionalized transaction", ['Script debugging logs: Signing Tx Error', 'Script debugging logs: Signing Tx Error'], wrong_lock_newm_key),
    ("The wrong mint amount on a fractionalized transaction", ['Script debugging logs: Mint/Burn/Datum Error'], wrong_lock_mint_amt),
    ("The wrong mint name on a fractionalized transaction", ['Script debugging logs: Minting Error'], wrong_lock_mint_name),
    ("The wrong datum is continuing on a fractionalized transaction", ['Script debugging logs: Invalid Datum Error', 'Script debugging logs: Mint/Burn/Datum Error'], wrong_lock_cont_datum),
    ("The wrong number of continuing outputs on a fractionalized transaction", ['Script debugging logs: Single Output Error'], wrong_lock_cont_output_amt),
    ("The wrong number of datum inputs on a fractionalized transaction", ['Script debugging logs: Single Input Error'], wrong_lock_input_amt),
    ("A correct fractionalized transaction", "Transaction successfully submitted.", good_lock_transaction),
    ("Forced Block Delay", True, force_block_change),
    ("The NEWM key is wrong on a solidify transaction", ['Script debugging logs: Signing Tx Error', 'Script debugging logs: Signing Tx Error'], wrong_unlock_newm_key),
    ("The wrong token name on a solidify transaction", ['Script debugging logs: Burning Error'], wrong_unlock_burn_tkn),
    ("The wrong amount of continuing outputs on a solidify transaction", ['Script debugging logs: Single Output Error'], wrong_unlock_cont_output_amt),
    ("The wrong amount of datum inputs on a solidify transaction", ['Script debugging logs: Single Input Error'], wrong_unlock_input_amt),
    ("The wrong burn amount on a solidify transaction", ['Script debugging logs: Mint/Burn/Datum Error'], wrong_unlock_burn_amt),
    ("A correct solidify transaction", "Transaction successfully submitted.", good_unlock_transaction),
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