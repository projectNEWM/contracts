import cbor2
import sys

if __name__ == "__main__":
    if len(sys.argv) != 2:
        print("Usage: python hex_to_cbor.py <hex_string>")
        sys.exit(1)

    hex_string = sys.argv[1]
    data = bytes.fromhex(hex_string)
    encoded = cbor2.dumps(data)
    print(encoded.hex())