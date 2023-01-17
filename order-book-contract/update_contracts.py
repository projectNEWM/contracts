def changePoolId(oldPath,newPath,newText):
    pattern = 'poolId = PlutusV2.PubKeyHash { PlutusV2.getPubKeyHash = createBuiltinByteString'
    with open(oldPath, 'r') as file1:
        with open(newPath, 'w+') as file2:
            for line in file1:
                if pattern in line:
                    # create teh new addition
                    line = pattern + " " + str(newText) + " }\n"
                file2.write(line)

def changeRewardPkh(oldPath,newPath,newText):
    pattern = 'payoutPkh = PlutusV2.PubKeyHash { PlutusV2.getPubKeyHash = createBuiltinByteString'
    with open(oldPath, 'r') as file1:
        with open(newPath, 'w+') as file2:
            for line in file1:
                if pattern in line:
                    # create teh new addition
                    line = pattern + " " + str(newText) + " }\n"
                file2.write(line)

def changeRewardSc(oldPath,newPath,newText):
    pattern = 'payoutSc = PlutusV2.PubKeyHash { PlutusV2.getPubKeyHash = createBuiltinByteString'
    with open(oldPath, 'r') as file1:
        with open(newPath, 'w+') as file2:
            for line in file1:
                if pattern in line:
                    # create teh new addition
                    line = pattern + " " + str(newText) + " }\n"
                file2.write(line)

def changeStakeCred(oldPath,newPath,newText):
    pattern = 'stakingCred = V2.StakingHash  $ V2.ScriptCredential $ V2.ValidatorHash $ createBuiltinByteString'
    with open(oldPath, 'r') as file1:
        with open(newPath, 'w+') as file2:
            for line in file1:
                if pattern in line:
                    # create teh new addition
                    line = pattern + " " + str(newText) + "\n"
                file2.write(line)