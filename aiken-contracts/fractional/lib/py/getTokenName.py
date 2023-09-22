import binascii
import hashlib


def token_name(txHash, index, prefix):
    txBytes = binascii.unhexlify(txHash)
    h = hashlib.new('sha3_256')
    h.update(txBytes)
    txHash = h.hexdigest()
    x = hex(index)[-2:]
    if "x" in x:
        x = x.replace("x", "0")
    txHash = prefix + x + txHash
    print(txHash[0:64])
    return txHash[0:64]

if __name__ == "__main__":
    prefix_100 = "000643b0"
    
    prefix_333 = "0014df10"
    
    prefix_444 = "001bc280"
    
    print('reference', token_name("1FA3625AC5DABFBEDFD80EEDFB5BEA37D8E8D66362C22300C2E4C00951449B18", 0, prefix_100))
    
    print('fractions', token_name("1FA3625AC5DABFBEDFD80EEDFB5BEA37D8E8D66362C22300C2E4C00951449B18", 0, prefix_444))
    
    
    # print(token_name("AB69AAB2EFE96149CA2FC045F8CDFADAA2213E5F71F7F427632AC1216BD6106D", 0, prefix_100) == "000643b00001f1ca8bb6ed0bd798019448bf8b5b9a539958477b53fd86c6d27e")
    
    # print('test', token_name("", 0, ""))