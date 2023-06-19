import hashlib
import binascii

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

if __name__ == "__main__":
    prefix_100 = "000643b0"
    
    prefix_333 = "0014df10"
    
    print('reference', token_name("1e637fd4b1a6a633261a1ba463577d65209dbbe0f7e8ec1fbfedb4c6b1bb926b", 1, prefix_100))
    
    print('fractions', token_name("1e637fd4b1a6a633261a1ba463577d65209dbbe0f7e8ec1fbfedb4c6b1bb926b", 1, prefix_333))
    
    
    print('test', token_name("", 0, ""))