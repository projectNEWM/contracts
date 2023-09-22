import json
from fractions import Fraction as f
from order_book_test import calculate_trade

def read_file(file_path):
    with open(file_path) as f:
        data = json.load(f)
    return data  

def get_that_amt(file_path, this_utxo, that_utxo):
    data = read_file(file_path)
    
    # print(this_data['inlineDatum']['fields'][2]['fields'][1]['fields'])
    this_data = data[this_utxo]
    pn1 = int(this_data['inlineDatum']['fields'][2]['fields'][1]['fields'][0]['int'])
    pd1 = int(this_data['inlineDatum']['fields'][2]['fields'][1]['fields'][1]['int'])
    sn1 = int(this_data['inlineDatum']['fields'][2]['fields'][2]['fields'][0]['int'])
    sd1 = int(this_data['inlineDatum']['fields'][2]['fields'][2]['fields'][1]['int'])
    p1 = f(pn1, pd1).limit_denominator()
    dp1 = f(sn1, sd1).limit_denominator()
    pid1 = this_data['inlineDatum']['fields'][1]['fields'][0]['bytes']
    tkn1 = this_data['inlineDatum']['fields'][1]['fields'][1]['bytes']
    amt1 = this_data['value'][pid1][tkn1]
    # print(f"{amt1} {pid1}.{tkn1} at a price of {p1} with a slippage of {dp1}")
    
    that_data = data[that_utxo]
    pn2 = int(that_data['inlineDatum']['fields'][2]['fields'][1]['fields'][0]['int'])
    pd2 = int(that_data['inlineDatum']['fields'][2]['fields'][1]['fields'][1]['int'])
    sn2 = int(that_data['inlineDatum']['fields'][2]['fields'][2]['fields'][0]['int'])
    sd2 = int(that_data['inlineDatum']['fields'][2]['fields'][2]['fields'][1]['int'])
    p2 = f(pn2, pd2).limit_denominator()
    dp2 = f(sn2, sd2).limit_denominator()
    pid2 = that_data['inlineDatum']['fields'][1]['fields'][0]['bytes']
    tkn2 = that_data['inlineDatum']['fields'][1]['fields'][1]['bytes']
    amt2 = that_data['value'][pid2][tkn2]
    # print(f"{amt2} {pid2}.{tkn2} at a price of {p2} with a slippage of {dp2}")
    
    xp, xg = calculate_trade(amt1,amt2,p1,p2)
    # print("\nReturning Assets:\n")
    if amt2 - xg == 0:
        print(f"{xp} {pid1}.{tkn1}")
        return f"{xp} {pid1}.{tkn1}"
    else:
        print(f"{amt2 - xg} {pid2}.{tkn2} + {xp} {pid1}.{tkn1}")
        return f"{amt2 - xg} {pid2}.{tkn2} + {xp} {pid1}.{tkn1}"

def get_this_amt(file_path, this_utxo, that_utxo):
    data = read_file(file_path)
    
    this_data = data[this_utxo]
    pn1 = int(this_data['inlineDatum']['fields'][2]['fields'][1]['fields'][0]['int'])
    pd1 = int(this_data['inlineDatum']['fields'][2]['fields'][1]['fields'][1]['int'])
    sn1 = int(this_data['inlineDatum']['fields'][2]['fields'][2]['fields'][0]['int'])
    sd1 = int(this_data['inlineDatum']['fields'][2]['fields'][2]['fields'][1]['int'])
    p1 = f(pn1, pd1).limit_denominator()
    dp1 = f(sn1, sd1).limit_denominator()
    pid1 = this_data['inlineDatum']['fields'][1]['fields'][0]['bytes']
    tkn1 = this_data['inlineDatum']['fields'][1]['fields'][1]['bytes']
    amt1 = this_data['value'][pid1][tkn1]
    
    that_data = data[that_utxo]
    pn2 = int(that_data['inlineDatum']['fields'][2]['fields'][1]['fields'][0]['int'])
    pd2 = int(that_data['inlineDatum']['fields'][2]['fields'][1]['fields'][1]['int'])
    sn2 = int(that_data['inlineDatum']['fields'][2]['fields'][2]['fields'][0]['int'])
    sd2 = int(that_data['inlineDatum']['fields'][2]['fields'][2]['fields'][1]['int'])
    p2 = f(pn2, pd2).limit_denominator()
    dp2 = f(sn2, sd2).limit_denominator()
    pid2 = that_data['inlineDatum']['fields'][1]['fields'][0]['bytes']
    tkn2 = that_data['inlineDatum']['fields'][1]['fields'][1]['bytes']
    amt2 = that_data['value'][pid2][tkn2]
    
    xp, xg = calculate_trade(amt1,amt2,p1,p2)
    # print("\nReturning Assets:\n")
    if amt1 - xp == 0:
        print(f"{xg} {pid2}.{tkn2}")
        return f"{xg} {pid2}.{tkn2}"
    else:
        print(f"{amt1 - xp} {pid1}.{tkn1} + {xg} {pid2}.{tkn2}")
        return f"{amt1 - xp} {pid1}.{tkn1} + {xg} {pid2}.{tkn2}"
    

if __name__ == "__main__":
    this=get_this_amt('./data/script_utxo.json', '2473e1180225ae5d03d9874a98e3c5603595309f087e5525e11f2c2df3e71dfc#0', '40d7164999797fe9cc43fe76e3b02c6337c92ee18c846b40d9dadac4f37ceb19#0')
    print(this)
    that=get_that_amt('./data/script_utxo.json', '2473e1180225ae5d03d9874a98e3c5603595309f087e5525e11f2c2df3e71dfc#0', '40d7164999797fe9cc43fe76e3b02c6337c92ee18c846b40d9dadac4f37ceb19#0')
    print(that)