#!/usr/bin/python
"""
All parsing functions required for testing.
"""
import json
import glob

def process_output(address, value_dict):
    # Get the value of the 'lovelace' key in the dictionary
    lovelace_value = str(value_dict['lovelace'])
    
    # Get the values of the nested dictionary and combine them into a string
    nested_dict_values = []
    for key, value in value_dict.items():
        if key != 'lovelace':
            for sub_key, sub_value in value.items():
                nested_dict_values.append(f"{sub_value} {key}.{sub_key}")
    if len(nested_dict_values) == 0:
        # combine the address with the lovelace amount
        output_string = address + " + " + lovelace_value
    else:
        nested_dict_values = " + ".join(nested_dict_values)
        
        # Combine the address wit the lovelace and the dictionary values
        output_string = address + " + " + lovelace_value + " + " + nested_dict_values
    
    return output_string


def worst_case_asset(address):
    return address + " + 18446744073709551615 + 18446744073709551615 b0818471a0e9633ae337cc1dcc7526ebe42286b4ceb3d836ad3a9e73.74686973697361766572796c6f6e67737472696e67666f7274657374696e6773"

def read_json_file(fileName):
    """
    Take in a file name and load the json from the file.
    """
    with open(fileName) as f:
        d = json.load(f)
    return d

def write_json_file(data, fileName):
    """
    Take in some data and dump it into some json file inside tmp.
    """
    with open(fileName, 'w') as outfile:
        json.dump(data, outfile)

def skey_dict(tmp):
    """
    Creates a dictionary of all the secret keys inside the tmp directory.
    """
    files = glob.glob(tmp+'*.skey')
    result = {}

    for skey_file in files:
        filename = skey_file.split('/')[-1].split('.')[0]  # extract the filename from the full path
        result[filename] = skey_file
    return result


# def pkh_dict(cli, tmp):
#     """
#     Creates a dictionary of all the public key hashes inside the tmp directory.
#     """
#     files = glob.glob(tmp+'*.vkey')
#     result = {}
    
#     for vkey_file in files:
#         content = address_key_hash(cli, vkey_file)
#         filename = vkey_file.split('/')[-1].split('.')[0]  # extract the filename from the full path
#         result[filename] = content
#     return result


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
    zeros = []
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

def delete_zeros(dict1):
    zeros = []
    
    for key, value in dict1.items():
        if isinstance(dict1[key], dict) and isinstance(value, dict):
            # If both values are dictionaries, subtract their values
            for inner_key, inner_value in value.items():
                if dict1[key][inner_key] == 0:
                    zeros.append(key)
        else:
            if dict1[key] == 0:
                zeros.append(key)
    for key in zeros:
        del dict1[key]
    return dict1

def txin(tmp):
    """
    Take in a utxo json file from some wallet and output the tx-in list, the value object, and a datum list.
    """
    # get utxo data
    d = read_json_file(tmp+'utxo.json')
    # The list of keys are all the TxId#Index we are spending.
    tx_in_output = []
    for u in list(d.keys()):
        tx_in_output.append('--tx-in')
        tx_in_output.append(u)

    # The input utxos may have datums attach so we need those too.
    inline_datum_output = []
    # We need the total value so we know what is going where.
    value_object_list = []
    for v in list(d.values()):
        value_object = {}
        inline_datum_output.append(v['inlineDatum'])
        value_object = compress_dicts(v['value'], value_object)
        value_object_list.append(value_object)
    
    return tx_in_output, inline_datum_output, value_object_list


def map_to_value(map_obj, scale):
    val_obj = {}
    for value in map_obj['map']:
        pid = value['k']['bytes']
        asset = value['v']['map']
        for token in asset:
            tkn = token['k']['bytes']
            amt = token['v']['int']
        
            if pid in val_obj:
                val_obj[pid] = {tkn:amt}
                val_obj[pid][tkn] += scale*amt
            else:
                val_obj[pid] = {tkn: scale*amt}
    return val_obj

def value_exist_in_value(target, total):
    for target_pid in target:
        for target_tkn in target[target_pid]:
            target_amt = target[target_pid][target_tkn]
            try:
                if total[target_pid][target_tkn] < target_amt:
                    return False
            except KeyError:
                return False
    return True