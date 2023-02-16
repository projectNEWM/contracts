#!/usr/bin/python
"""
All parsing functions required for testing.
"""
import subprocess
import json
import glob

def address_dict(tmp):
    """
    Create a dictionary of all the addresses inside the tmp directory.
    """
    files = glob.glob(tmp+'*.addr')
    result = {}

    for addr_file in files:
        with open(addr_file, 'r') as f:
            content = f.readline().strip()
            filename = addr_file.split('/')[-1].split('.')[0]  # extract the filename from the full path
            result[filename] = content
    return result

def add_dicts(dict1, dict2):
    """
    Adds two value dictionaries together.
    """
    result = dict1.copy()  # create a copy of the first dictionary
    for key, value in dict2.items():
        if key in result:
            if isinstance(result[key], dict) and isinstance(value, dict):
                # If both values are dictionaries, add their values
                for inner_key, inner_value in value.items():
                    if inner_key in result[key]:
                        result[key][inner_key] += inner_value
                    else:
                        result[key][inner_key] = inner_value
            else:
                # Otherwise, add the values directly
                result[key] += value
        else:
            # If the key doesn't exist in the first dictionary, add it to the result
            result[key] = value
    return result


def compress_dicts(dictionary, result):
    """
    Inputs a value dictionary from the utxo set and sums it into the result dictionary.
    """
    for key, value in dictionary.items():
        if key in result:
            # If the key already exists, sum the values
            for inner_key, inner_value in value.items():
                if inner_key in result[key]:
                    result[key][inner_key] += inner_value
                else:
                    result[key][inner_key] = inner_value
        else:
            # If the key doesn't exist, add it to the result
            result[key] = value
    return result

def subtract_dicts(dict1, dict2):
    """
    Take a total value dictionary and a specific value dictionary and return the difference.
    """
    result = dict1.copy()  # create a copy of the first dictionary
    for key, value in dict2.items():
        if key in result:
            if isinstance(result[key], dict) and isinstance(value, dict):
                # If both values are dictionaries, subtract their values
                for inner_key, inner_value in value.items():
                    if inner_key in result[key]:
                        result[key][inner_key] -= inner_value
            else:
                # Otherwise, subtract the values directly
                result[key] -= value
    return result


def txin(tmp):
    """
    Take in a utxo json file from some wallet and output the tx-in list, the value object, and a datum list.
    """
    with open(tmp+'utxo.json') as f:
        d = json.load(f)

    # The list of keys are all the TxId#Index we are spending.
    tx_in_output = []
    for u in list(d.keys()):
        tx_in_output.append('--tx-in')
        tx_in_output.append(u)

    # The input utxos may have datums attach so we need those too.
    inline_datum_output = []
    # We need the total value so we know what is going where.
    value_object = {}
    for v in list(d.values()):
        inline_datum_output.append(v['inlineDatum'])
        value_object = compress_dicts(v['value'], value_object)
    
    return tx_in_output, inline_datum_output, value_object

