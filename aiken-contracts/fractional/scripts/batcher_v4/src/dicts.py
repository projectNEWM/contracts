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
