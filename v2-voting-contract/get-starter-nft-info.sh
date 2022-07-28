#!/bin/bash
set -e

echo "PID"
./python3 -c "import binascii;a='${1}';s=binascii.unhexlify(a);print([x for x in s])"

echo "TKN"
python3 -c "import binascii;a='${2}';s=binascii.unhexlify(a);print([x for x in s])"