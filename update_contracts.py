# '../v2-did-locking-contract/src/V2DidLockingContract.hs'
# '../v2-did-locking-contract/src/V2DidLockingContract-new.hs'
def changeVoteHash(oldPath,newPath,newText):
    pattern = 'voteValidatorHash = PlutusV2.ValidatorHash $ createBuiltinByteString'
    with open(oldPath, 'r') as file1:
        with open(newPath, 'w+') as file2:
            for line in file1:
                if pattern in line:
                    # create teh new addition
                    line = pattern + " " + str(newText) + "\n"
                file2.write(line)

def changeLockHash(oldPath,newPath,newText):
    pattern = 'getValidatorHash = PlutusV2.ValidatorHash $ createBuiltinByteString'
    with open(oldPath, 'r') as file1:
        with open(newPath, 'w+') as file2:
            for line in file1:
                if pattern in line:
                    # create teh new addition
                    line = pattern + " " + str(newText) + "\n"
                file2.write(line)

def changeStartLockPid(oldPath,newPath,newText):
    pattern = 'lockPid = PlutusV2.CurrencySymbol {PlutusV2.unCurrencySymbol = createBuiltinByteString'
    with open(oldPath, 'r') as file1:
        with open(newPath, 'w+') as file2:
            for line in file1:
                if pattern in line:
                    # create teh new addition
                    line = pattern + " " + str(newText) + " }\n"
                file2.write(line)

def changeStartLockTkn(oldPath,newPath,newText):
    pattern = 'lockTkn = PlutusV2.TokenName {PlutusV2.unTokenName = createBuiltinByteString'
    with open(oldPath, 'r') as file1:
        with open(newPath, 'w+') as file2:
            for line in file1:
                if pattern in line:
                    # create teh new addition
                    line = pattern + " " + str(newText) + " }\n"
                file2.write(line)

def changeStartVotePid(oldPath,newPath,newText):
    pattern = 'voteStartPid = PlutusV2.CurrencySymbol { PlutusV2.unCurrencySymbol = createBuiltinByteString'
    with open(oldPath, 'r') as file1:
        with open(newPath, 'w+') as file2:
            for line in file1:
                if pattern in line:
                    # create teh new addition
                    line = pattern + " " + str(newText) + " }\n"
                file2.write(line)

def changeStartVoteTkn(oldPath,newPath,newText):
    pattern = 'voteStartTkn = PlutusV2.TokenName { PlutusV2.unTokenName = createBuiltinByteString'
    with open(oldPath, 'r') as file1:
        with open(newPath, 'w+') as file2:
            for line in file1:
                if pattern in line:
                    # create teh new addition
                    line = pattern + " " + str(newText) + " }\n"
                file2.write(line)

def changeTokenizedPid(oldPath,newPath,newText):
    pattern = 'tokenizedPid = PlutusV2.CurrencySymbol { PlutusV2.unCurrencySymbol = createBuiltinByteString'
    with open(oldPath, 'r') as file1:
        with open(newPath, 'w+') as file2:
            for line in file1:
                if pattern in line:
                    # create teh new addition
                    line = pattern + " " + str(newText) + " }\n"
                file2.write(line)

def changeDelegPkh(oldPath,newPath,newText):
    pattern = 'getPkh = PlutusV2.PubKeyHash { PlutusV2.getPubKeyHash = createBuiltinByteString'
    with open(oldPath, 'r') as file1:
        with open(newPath, 'w+') as file2:
            for line in file1:
                if pattern in line:
                    # create teh new addition
                    line = pattern + " " + str(newText) + " }\n"
                file2.write(line)

def changeMultiPkh(oldPath,newPath,newText1,newText2,newText3):
    pattern1 = 'multiPkh1 = PlutusV2.PubKeyHash { PlutusV2.getPubKeyHash = createBuiltinByteString'
    pattern2 = 'multiPkh2 = PlutusV2.PubKeyHash { PlutusV2.getPubKeyHash = createBuiltinByteString'
    pattern3 = 'multiPkh3 = PlutusV2.PubKeyHash { PlutusV2.getPubKeyHash = createBuiltinByteString'
    with open(oldPath, 'r') as file1:
        with open(newPath, 'w+') as file2:
            for line in file1:
                if pattern1 in line:
                    # create teh new addition
                    line = pattern1 + " " + str(newText1) + " }\n"
                if pattern2 in line:
                    # create teh new addition
                    line = pattern2 + " " + str(newText2) + " }\n"
                if pattern3 in line:
                    # create teh new addition
                    line = pattern3 + " " + str(newText3) + " }\n"
                file2.write(line)