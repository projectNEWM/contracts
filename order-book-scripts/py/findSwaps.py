import json
import random

def import_json(file_path):
    with open(file_path, 'r') as file:
        data = json.load(file)
    return data

def write_to_json(data, file_path):
    with open(file_path, 'w') as file:
        json.dump(data, file)

def compare_bytes(list1, list2):
    return list1[0]['bytes'] == list2[0]['bytes'] and list1[1]['bytes'] == list2[1]['bytes']

def effective_price(have, want):
    gamma = 1000000
    scaled = gamma*have
    return scaled // want

def is_in_range(amt, slip, target):

    lowEnd = amt - (amt // slip)
    highEnd = amt + (amt // slip)

    return (lowEnd <= target and target <= highEnd)

def slip(have1,want1,slip1,have2,want2,slip2):
    return is_in_range(have1, slip1, want2) and is_in_range(have2, slip1, want1)

def effective_slip(have1,want1,slip1,have2,want2,slip2):

    a1 = effective_price(have1, want1)
    a2 = effective_price(want2, have2)

    # print('ropices',a1, a2)

    return is_in_range(a1, slip1, a2) and is_in_range(a2, slip1, a1)

def add_unique_tuple(tuple_, list_):
    if tuple_ in list_ or tuple_[::-1] in list_:
        return list_
    else:
        list_.append(tuple_)
        return list_



data = {}
N = 100
for i in range(N):
    if random.random() > 0.5:
        data[abs(hash(str(i)))] = {
            "address": "addr_test1wz5jevtxlujnqhuksat7ag3p9vcyv99lyep39kv4cytttpgtwx0ew",
            "datum": "null",
            "inlineDatum": {
                "constructor": 0,
                "fields": [
                    {
                        "constructor": 0,
                        "fields": [
                            {
                                "bytes": "db7bffc41a43c4d9c31342e3fd457409aeb40302aa52058df374913b"
                            },
                            {
                                "bytes": ""
                            }
                        ]
                    },
                    {
                        "constructor": 0,
                        "fields": [
                            {
                                "bytes": "0ed672eef8d5d58a6fbce91327baa25636a8ff97af513e3481c97c52"
                            },
                            {
                                "bytes": "5468697349734f6e6553746172746572546f6b656e466f7254657374696e6734"
                            },
                            {
                                "int": random.gauss(12345678, 1234567)
                            }
                        ]
                    },
                    {
                        "constructor": 0,
                        "fields": [
                            {
                                "bytes": ""
                            },
                            {
                                "bytes": ""
                            },
                            {
                                "int": abs(random.gauss(1234567, 1234567))
                            }
                        ]
                    },
                    {
                        "constructor": 0,
                        "fields": [
                            {
                                "int": random.randint(10,50)
                            }
                        ]
                    }
                ]
            },
            "inlineDatumhash": "9f092d4eca196a1cc8683ca1ece1ab09268695057da00752b8d00f7e9c5395ae",
            "referenceScript": "null",
            "value": {
                "": {
                    "": 0
                },
                "lovelace": 0
            }
        }
    else:
        data[abs(hash(str(i)))] = {
            "address": "addr_test1wz5jevtxlujnqhuksat7ag3p9vcyv99lyep39kv4cytttpgtwx0ew",
            "datum": "null",
            "inlineDatum": {
                "constructor": 0,
                "fields": [
                    {
                        "constructor": 0,
                        "fields": [
                            {
                                "bytes": "db7bffc41a43c4d9c31342e3fd457409aeb40302aa52058df374913b"
                            },
                            {
                                "bytes": ""
                            }
                        ]
                    },
                    {
                        "constructor": 0,
                        "fields": [
                            {
                                "bytes": ""
                            },
                            {
                                "bytes": ""
                            },
                            {
                                "int": abs(random.gauss(1234567, 1234567))
                            }
                        ]
                    },
                    {
                        "constructor": 0,
                        "fields": [
                            {
                                "bytes": "0ed672eef8d5d58a6fbce91327baa25636a8ff97af513e3481c97c52"
                            },
                            {
                                "bytes": "5468697349734f6e6553746172746572546f6b656e466f7254657374696e6734"
                            },
                            {
                                "int": random.gauss(12345678, 1234567)
                            }
                        ]
                    },
                    {
                        "constructor": 0,
                        "fields": [
                            {
                                "int": random.randint(10,50)
                            }
                        ]
                    }
                ]
            },
            "inlineDatumhash": "9f092d4eca196a1cc8683ca1ece1ab09268695057da00752b8d00f7e9c5395ae",
            "referenceScript": "null",
            "value": {
                "": {
                    "": 0
                },
                "lovelace": 0
            }
        }

# [{'bytes': ''}, {'bytes': ''}, {'int': 48000}]

full_swaps = []
part_swaps = []
for utxo1 in data:
    have1 = data[utxo1]['inlineDatum']['fields'][1]['fields']
    h1 = have1[2]['int']
    want1 = data[utxo1]['inlineDatum']['fields'][2]['fields']
    w1 = want1[2]['int']
    slip1 = data[utxo1]['inlineDatum']['fields'][3]['fields']
    s1 = slip1[0]['int']
    for utxo2 in data:
        if utxo1 == utxo2:
            continue
        have2 = data[utxo2]['inlineDatum']['fields'][1]['fields']
        h2 = have2[2]['int']
        want2 = data[utxo2]['inlineDatum']['fields'][2]['fields']
        w2 = want2[2]['int']
        slip2 = data[utxo2]['inlineDatum']['fields'][3]['fields']
        s2 = slip2[0]['int']


        if compare_bytes(have1, want2):
            eOut = effective_slip(h1,w1,s1,h2,w2,s2)
            if eOut == True:
                out = slip(h1,w1,s1,h2,w2,s2)
                if out is True:
                    # print('FULL',utxo1, utxo2)
                    full_swaps = add_unique_tuple((utxo1, utxo2), full_swaps)
                    # print(have1)
                    # print(have2)
                    print(max(h1/w1, w1/h1))
                else:
                    # print("PART", utxo1, utxo2)
                    part_swaps = add_unique_tuple((utxo1, utxo2), part_swaps)
                    # print(have1)
                    # print(have2)
                    # print(h1/w1, w1/h1)
                    print(max(h1/w1, w1/h1))


print(N)
print(len(full_swaps))
print(len(part_swaps))

