import json

def get_json_data(file_path):
    with open(file_path) as file:
        data = json.load(file)
    return data

def byte_obj(value):
    return {"bytes": str(value)}


def int_obj(value):
    return {"int": int(value)}


def create_value_map(data):
    value_map = []
    for pid in data:
        d = {}
        d["k"] = byte_obj(pid)
        d["v"] = {"map":[]}
        for tkn in data[pid]:
            nd = {}
            nd["k"] = byte_obj(tkn)
            amt = data[pid][tkn]
            nd["v"] = int_obj(amt)
            d["v"]["map"].append(nd)
        value_map.append(d)
    return value_map

def map_cost_file(file_path):
    data = get_json_data(file_path)
    v_map = create_value_map(data)
    print(json.dumps(v_map))
    

if __name__ == "__main__":
    test_data = {
        "": {
            "": 2000000
        },
        "682fe60c9918842b3323c43b5144bc3d52a23bd2fb81345560d73f63": {
            "4e45574d": 1000,
            "4e4557": 5000
        }
    }
    
    v_map = create_value_map(test_data)
    print(v_map)
    


