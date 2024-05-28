import json

def load_json_file(filename):
    with open(filename) as f:
        data = json.load(f)
    return data


def concatenate_values(filename):
    data = load_json_file(filename)
    result = ""
    for transaction in data:
        subdata = data[transaction]
        value = subdata["value"]
        for pid in value:
            if pid != "lovelace":
                for tkn in value[pid]:
                    result += f"{value[pid][tkn]} {pid}.{tkn} + "
    print(result[:-3])
