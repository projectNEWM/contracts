import os
from dotenv import load_dotenv
from TestSuite.query import protocol_parameters, utxo

# Load environment variables from .node.env file
load_dotenv('.node.env')

# Access the environment variable defined in .node.env
root    = os.environ['ROOT']
cli     = os.environ['cli']
network = os.environ['network']
socket  = os.environ['socket']

# Use the environment variable in your Python code
print(f"The value of ROOT is {root}")
print(f"The value of CLI is {cli}")
print(f"The value of SOCKET is {network}")

# Set the CARDANO_NODE_SOCKET_PATH environment variable
os.environ["CARDANO_NODE_SOCKET_PATH"] = socket

# tmp dir
tmp = root+"/tmp/"

# Addresses
newm_addr      = open(root+'/addresses/newm.addr').read()
artist_addr    = open(root+'/addresses/artist.addr').read()
collat_addr    = open(root+'/addresses/collat.addr').read()
reference_addr = open(root+'/addresses/reference.addr').read()
multisig1_addr = open(root+'/addresses/multisig1.addr').read()
multisig2_addr = open(root+'/addresses/multisig2.addr').read()
multisig3_addr = open(root+'/addresses/multisig3.addr').read()
nftLock_addr   = open(root+'/addresses/nftLock.addr').read()
nftMint_addr   = open(root+'/addresses/nftMint.addr').read()
ftLock_addr    = open(root+'/addresses/ftLock.addr').read()
ftMint_addr    = open(root+'/addresses/ftMint.addr').read()

protocol_parameters(cli, network, tmp)

utxo(cli, network, nftLock_addr, tmp)