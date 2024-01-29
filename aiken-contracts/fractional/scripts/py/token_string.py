import json


def token_struc(pid: str, tkn: str, amt: int) -> dict:
    return {
        "constructor": 0,
        "fields": [
            {
                "bytes": pid
            },
            {
                "bytes": tkn
            },
            {
                "int": amt
            }
        ]
    }


def list_struc(list_of_token_struc: list) -> dict:
    return {
        "constructor": 0,
        "fields": [
            {
                "list": list_of_token_struc
            }
        ]
    }


def get_token_data(file_path: str) -> dict:
    with open(file_path, 'r') as file:
        data = json.load(file)
    return data


def create_token_string(list_of_token_struc: list) -> str:
    string = ""
    for t in list_of_token_struc:
        token = f"{t['fields'][2]['int']} {t['fields'][0]['bytes']}.{t['fields'][1]['bytes']}"
        if string == "":
            string += token
        else:
            string += " + " + token
    return string


if __name__ == "__main__":
    file_path = "../tmp/script_utxo.json"
    data = get_token_data(file_path)
    list_of_token_struc = []
    for utxo in data:
        value = data[utxo]['value']

        for pid in value:
            if pid != 'lovelace':
                for tkn in value[pid]:
                    amt = value[pid][tkn]
                    token = token_struc(pid, tkn, amt)
                    list_of_token_struc.append(token)

    # print(f"Adding {len(list_of_token_struc)} tokens")
    print(create_token_string(list_of_token_struc))
