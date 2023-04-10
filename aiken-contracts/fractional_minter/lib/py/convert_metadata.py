import json

def string_to_hex(string):
    return string.encode().hex()


def byte_object(string):
    # if string is longer than accepted length then create list of strings
    if len(string) > 64:
        string_list = [string[i:i+64] for i in range(0, len(string), 64)]
        list_object = []
        for value in string_list:
            list_object.append({"bytes": value})
        return {"list": list_object}
    return {"bytes": string}

def int_object(integer):
    return {"int": integer}


def read_metadata_file(file_path):
    with open(file_path) as f:
        data = json.load(f)
    return data  

def write_metadatum_file(file_path, data):
    with open(file_path, "w") as f:
        json.dump(data, f)


def create_metadata_datum(path, tag, pid, tkn, version):
    metadata_datum = {
        "constructor": 0,
        "fields": []
    }
    
    version_object = {"int": version}
    map_object = {"map":[]}
    
    data = read_metadata_file(path)
    
    try:
        metadata = data[tag][pid][tkn]
    except KeyError:
        metadata_datum['fields'].append(map_object)
        metadata_datum['fields'].append(version_object)
        return metadata_datum

    for key in metadata:
        # string conversion
        if isinstance(metadata[key], str):
            map_object["map"].append({"k": string_to_hex(key), "v":byte_object(string_to_hex(metadata[key]))})

        # int conversion
        if isinstance(metadata[key], int):
            map_object["map"].append({"k": string_to_hex(key), "v":int_object(metadata[key])})
        
        # list conversion
        if isinstance(metadata[key], list):
            # default it to the empty list object
            if len(metadata[key]) == 0:
                map_object["map"].append({"k": string_to_hex(key),"v":{"list": []}})
            
            # list of dicts
            elif isinstance(metadata[key][0], dict):
                nested_map = {"map":[]}
                for value in metadata[key]:
                    for nested_key in value:
                        # dict of strings
                        if isinstance(value[nested_key], str):
                            nested_map["map"].append({"k": string_to_hex(nested_key), "v":byte_object(string_to_hex(value[nested_key]))})

                        # dict of ints
                        if isinstance(value[nested_key], int):
                            nested_map["map"].append({"k": string_to_hex(nested_key), "v":int_object(value[nested_key])})
                map_object["map"].append({"k": string_to_hex(key),"v":nested_map})
            
            # list of strings
            elif isinstance(metadata[key][0], str):
                list_object = []
                for value in metadata[key]:
                    list_object.append({"bytes": string_to_hex(value)})
                map_object["map"].append({"k": string_to_hex(key),"v":{"list": list_object}})
            
            # list of ints
            elif isinstance(metadata[key][0], int):
                list_object = []
                for value in metadata[key]:
                    list_object.append({"int": value})
                map_object["map"].append({"k": string_to_hex(key),"v":{"list": list_object}})
    
    metadata_datum['fields'].append(map_object)
    metadata_datum['fields'].append(version_object)
    
    return metadata_datum

    

if __name__ == "__main__":
    
    file_path = "AdaNinjaz-Daisuke.metadata.json"
    datum_path = "AdaNinjaz-Daisuke.metadatUM.json"
    datum = create_metadata_datum(file_path, '721', '<policy_id_hex>', '<asset_name_ascii>', 1)
    
    print(datum)
    # write_metadatum_file(datum_path, datum)
   