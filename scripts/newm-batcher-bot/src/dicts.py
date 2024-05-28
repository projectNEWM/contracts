# A dict here is
# {pid1: {tkn11: amt11}, pid2: {tkn21: amt21, tkn22: amt22},..lovelace: 1_234_567_890}
# where the element amt from some pid:{tkn:amt} can never be zero and lovelace
# is its own type for some reason as an int and not just the token with an empty
# byte string for a pid and tkn. This is to respect oura.

def add(dict1, dict2):
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
    result = delete_zeros(result)
    return result


def subtract(dict1, dict2):
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
    result = delete_zeros(result)
    return result


def delete_zeros(dict1):
    zeros = []
    # {pid: {tkn: amt}}
    for key, value in dict1.items():
        # this is for tokens
        if isinstance(dict1[key], dict) and isinstance(value, dict):
            # If both values are dictionaries, subtract their values
            for inner_key, _ in value.items():
                if dict1[key][inner_key] == 0:
                    zeros.append(key)
        else:
            # this is lovelace
            if dict1[key] == 0:
                zeros.append(key)
    for key in zeros:
        del dict1[key]
    return dict1


def contains(dict1, dict2):
    for pid, assets in dict2.items():
        if isinstance(assets, dict):
            if pid not in dict1:
                # pid doesn't exist
                return False
            else:
                # pid does exist
                for tkn, amt in dict2[pid].items():
                    if dict1[pid][tkn] >= amt:
                        # go to next enrty
                        continue
                    else:
                        return False
        else:
            # lovelace
            try:
                if dict1[pid] >= dict2[pid]:
                    # go to next entry
                    continue
                else:
                    return False
            except KeyError:
                continue
    return True


