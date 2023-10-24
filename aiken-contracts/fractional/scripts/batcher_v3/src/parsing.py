import hashlib

def cost_map_to_value_dict(map_obj: dict, scale: int = 1) -> dict:
    """Convert the cost map from the sale datum and scale it into a value
    dictionary that is used in the db.

    Args:
        map_obj (dict): The map type for a plutus datum
        scale (int): The scaling for the value.

    Returns:
        dict: The value dictionary similar to the db
    """
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

def process_output(address: str, value_dict: dict) -> str:
    """This will produce a correctly formatted output given an address and the
    value dictionary that will go along with it.

    Args:
        address (str): The bech32 address
        value_dict (dict): The value that is on the utxo

    Returns:
        str: The output in the "address + lovelace + asset" format.
    """
    # Get the value of the 'lovelace' key in the dictionary
    lovelace_value = str(value_dict['lovelace'])
    
    # Get the values of the nested dictionary and combine them into a string
    nested_dict_values = []
    for key, value in value_dict.items():
        if key != 'lovelace':
            for sub_key, sub_value in value.items():
                nested_dict_values.append(f"{sub_value} {key}.{sub_key}")
    
    # just add in the lovelace else do the assets too
    if len(nested_dict_values) == 0:
        # combine the address with the lovelace amount
        output_string = address + " + " + lovelace_value
    else:
        nested_dict_values = " + ".join(nested_dict_values)
        
        # Combine the address wit the lovelace and the dictionary values
        output_string = address + " + " + lovelace_value + " + " + nested_dict_values
    
    return output_string

def asset_list_to_dict(assets: list) -> dict:
    """Convert the Oura asset list inside a tx output into a value dictionary.

    Args:
        assets (list): The oura list of assets from a tx_output variant

    Returns:
        dict: A value dictionary of the assets.
    """
    # the asset data is in this form
    # "assets": [
    #         {
    #             "policy": "4d6d1192d39a48f4c80edbe52a5c480bec0a4e0711a998ca016b81a5",
    #             "asset": "001bc28001047a0269ba71359397cf9b3dd1124cbed1b1454cee335ab7ef91f8",
    #             "asset_ascii": None,
    #             "amount": 100000000
    #         }
    #     ],
    values = {}
    for asset in assets:
        values[asset['policy']] = {}
        values[asset['policy']][asset['asset']] = asset['amount']
    return values

def key_exists_in_dict(dictionary: dict, key: str) -> bool:
    """Return True if a key exists inside a dictionary else return False.

    Args:
        dictionary (dict): Some dictionary to check
        key (str): The key to be used.

    Returns:
        bool: True if exists else False
    """
    return key in dictionary


def sha3_256(input_string: str) -> str:
    """Compute the hex digest of a sha3_256 hash of some input string.

    Args:
        input_string (str): Some input string to be hashed.

    Returns:
        str: The sha3_256 hash of some string.
    """
    # Calculate the SHA3-256 hash
    return hashlib.sha3_256(str(input_string).encode('utf-8')).hexdigest()