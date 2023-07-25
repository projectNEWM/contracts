import subprocess
import json

def tip(socket, file_path, network):
    """
    Query the tip of the blockchain then save to a file.
    """
    func = [
        'cardano-cli',
        'query',
        'tip',
        '--socket-path',
        socket,
        '--out-file',
        file_path
    ]
    func += network.split(" ")

    # this saves to out file
    p = subprocess.Popen(func)
    p.communicate()
    
def get_latest_block_number(socket, file_path, network):
    # get current tip
    tip(socket, file_path, network)
    # get the block data
    with open(file_path, "r") as read_content:
        data = json.load(read_content)
    
    return int(data['block'])