from json import load, dump

# '../data/auctionable/auctionable-datum.json'
def getWorstCaseFile(datum_path):
    MAX_BYTE = "5468697349734f6e6553746172746572546f6b656e466f7254657374696e6731"
    MAX_INT  = pow(2, 64) - 1
    
    with open(datum_path, 'r') as f:
        datum = load(f)

    # get the datum data
    data = datum['fields']
    
    # reset the fields
    datum['fields'] = []

    # loop all the data, no lists
    for key in data:
        try:
            key['bytes']
            datum['fields'].append({'bytes':MAX_BYTE})
        except KeyError:pass

        try:
            key['int']
            datum['fields'].append({'int':MAX_INT})
        except KeyError:pass

    # save to a file
    out_file = open("worst_case.json", "w+")
    
    dump(datum, out_file, indent = 6)
    
    out_file.close()

def addTokens(howMany):
    output = ""
    for i in range(howMany):
        thisToken = " + 18446744073709551615 d61e1c1d38fc4e5b0734329a4b7b820b76bb8e0729458c153c4248{}.5468697349734f6e6553746172746572546f6b656e466f7254657374696e67{}".format(str(i).zfill(2), str(i).zfill(2))
        output += thisToken
    print(output)
    return output