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
