
def get_map(v_map, bundle_size):
    string = ""
    for token in v_map:
        pid = token['k']['bytes']
        for tkn in token['v']['map']:
            name = tkn['k']['bytes']
            amt = int(tkn['v']['int'])
            total = bundle_size * amt
            if pid != "":
                string += f"{total} {pid}.{name} + "
    print(string[:-3])
    

if __name__ == "__main__":
    pass    