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
    pattern = 'lockValidatorHash = PlutusV2.ValidatorHash $ createBuiltinByteString'
    with open(oldPath, 'r') as file1:
        with open(newPath, 'w+') as file2:
            for line in file1:
                if pattern in line:
                    # create teh new addition
                    line = pattern + " " + str(newText) + "\n"
                file2.write(line)

