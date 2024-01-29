import json
import subprocess

import requests


def query_tx_mempool(socket, tx_id, file_path, network):
    func = [
        'cardano-cli',
        'query',
        'tx-mempool',
        '--socket-path',
        socket,
        'tx-exists',
        tx_id,
        '--out-file',
        file_path
    ]
    func += network.split(" ")

    # this saves to out file
    p = subprocess.Popen(func)
    p.communicate()


def does_tx_exists_in_mempool(socket, tx_id, file_path, network):
    """
    Check if a tx exists in the nodes local mempool.
    """
    # check the mempool
    query_tx_mempool(socket, tx_id, file_path, network)
    # get the block data
    with open(file_path, "r") as read_content:
        data = json.load(read_content)
    return data['exists']


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


def query_current_block_with_koios(network: bool) -> list[dict]:
    """Uses Koios to query the transaction information from a list of
    transaction hashes. The return order may not match the input order.

    Args:
        hashes (list): The list of tx hashes.
        network (bool): The network flag, mainnet (True) or preprod (False).

    Returns:
        list: A list of transaction information.
    """
    # mainnet and preprod only
    subdomain = "api" if network is True else "preprod"

    headers = {
        'accept': 'application/json',
        'content-type': 'application/json',
    }

    url = 'https://' + subdomain + '.koios.rest/api/v1/block_info'
    # return the tx information for a list of transactions, (the inputs)
    return requests.post(url=url, headers=headers).json()
