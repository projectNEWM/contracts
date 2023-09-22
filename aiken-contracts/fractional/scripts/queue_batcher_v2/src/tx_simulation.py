import json
import os
import subprocess
import tempfile
from typing import Any

import cbor2
import requests


def query_tx_with_koios(hashes: list, network: bool) -> list:
    # mainnet and preprod only
    subdomain = "api" if network is True else "preprod"

    json_data = {
        '_tx_hashes': hashes
    }
    
    headers = {
        'accept': 'application/json',
        'content-type': 'application/json',
    }
    # return the tx information for a list of transactions, (the inputs)
    return requests.post('https://' + subdomain + '.koios.rest/api/v0/tx_info', headers=headers, json=json_data).json()


def to_bytes(s: str) -> bytes:
    # Convert the string to bytes and prepend with 'h' to indicate hexadecimal format
    return bytes.fromhex(s)

def tx_draft_to_resolved(draft: str) -> Any:
    # return the data from the cbor for parsing
    return cbor2.loads(bytes.fromhex(draft))


def from_file(tx_draft_path: str, network: bool) -> dict:
    """Simulate a tx from a tx draft file for some network. 

    Args:
        tx_draft_path (str): The path to the tx.draft file.
        network (bool): The network flag, mainnet (True) or preprod (False)

    Returns:
        dict: A dictionary of the cpu and mem units or an empty dict.
    """
    # get cborHex from tx draft
    with open(tx_draft_path, 'r') as file:
        data = json.load(file)
    cborHex = data['cborHex']
    return from_cbor(cborHex, network)


def from_cbor(tx_cbor: str, network: bool) -> dict:
    """Simulate a tx from tx cbor for some network. 

    Args:
        tx_cbor (str): The transaction cbor.
        network (bool): The network flag, mainnet (True) or preprod (False)

    Returns:
        dict: A dictionary of the cpu and mem units or an empty dict.
    """
    # resolve the data from the cbor
    data = tx_draft_to_resolved(tx_cbor)
    
    # we just need the body here
    txBody = data[0]
    
    # all the types of inputs; tx inputs, collateral, and reference
    inputs = txBody[0] + txBody[13] + txBody[18]
    input_cbor = cbor2.dumps(inputs).hex()
    
    # we now need to loop all the input hashes to resolve their outputs
    tx_hashes = [x[0].hex() for x in inputs]
    result = query_tx_with_koios(tx_hashes, network)
    
    # build out the list of resolved input outputs
    outputs = []
    # the order of the resolved outputs matter so we match to the inputs
    for utxo in inputs:
        tx_hash = utxo[0].hex()
        
        # now find the result for that hash
        for tx in result:
            this_tx_hash = tx['tx_hash']
            if tx_hash != this_tx_hash:
                continue
            
            resolved = {}
            for utxo in tx['outputs']:
                idx = utxo['tx_index']

                # we found it
                if [to_bytes(tx_hash), idx] in inputs:
                    # lets build out the resolved output
                    
                    # assume that anything with a datum is a contract
                    if utxo['inline_datum'] != None:
                        network_flag = "71" if network is True else "70"
                        pkh = network_flag + utxo['payment_addr']['cred']
                        pkh = to_bytes(pkh)
                        resolved[0] = pkh

                        # put the inline datum in the correct format
                        cbor_datum = to_bytes(utxo['inline_datum']['bytes'])
                        resolved[2] = [1, cbor2.CBORTag(24, cbor_datum)]
                    else:
                        # simple payment
                        network_flag = "61" if network is True else "60"
                        pkh = network_flag + utxo['payment_addr']['cred']
                        pkh = to_bytes(pkh)
                        resolved[0] = pkh
                    
                    if utxo['reference_script'] != None:
                        # assume plutus v2 reference scripts
                        cbor_ref = to_bytes(utxo['reference_script']['bytes'])
                        cbor_ref = to_bytes(cbor2.dumps([2, cbor_ref]).hex())
                        resolved[3] = cbor2.CBORTag(24, cbor_ref)
                    
                    # now we need the value element
                    lovelace = utxo['value']
                    assets = utxo['asset_list']
                    
                    # its just an int for lovelace
                    if len(assets) == 0:
                        second = int(lovelace)
                    else:
                        # build out the assets then create the list
                        tkns = {}
                        for asset in assets:
                            tkns[to_bytes(asset['policy_id'])] = {}
                            tkns[to_bytes(asset['policy_id'])][to_bytes(asset['asset_name'])] = int(asset['quantity'])
                        second = [int(lovelace), tkns]
                    resolved[1] = second
                    
                    # append it and go to the next one
                    outputs.append(resolved)
                    break
    
    # get the resolved output cbor
    output_cbor = cbor2.dumps(outputs).hex()
    
    # try to simulate the tx and return the results else an empty dict
    try:
        # use some temp files that get deleted later
        with tempfile.NamedTemporaryFile(mode='w+', delete=False) as temp_tx_file:
            temp_tx_file.write(tx_cbor)
            temp_tx_file_path = temp_tx_file.name
        with tempfile.NamedTemporaryFile(mode='w+', delete=False) as temp_input_file:
            temp_input_file.write(input_cbor)
            temp_input_file_path = temp_input_file.name
        with tempfile.NamedTemporaryFile(mode='w+', delete=False) as temp_output_file:
            temp_output_file.write(output_cbor)
            temp_output_file_path = temp_output_file.name
                    
        output = subprocess.run(
            ['aiken', 'tx', 'simulate', temp_tx_file_path, temp_input_file_path, temp_output_file_path],
            check=True,
            capture_output=True, 
            text=True
        )
            
        os.remove(temp_tx_file_path)
        os.remove(temp_input_file_path)
        os.remove(temp_output_file_path)
        
        return json.loads(output.stdout)
    except subprocess.CalledProcessError as e:
        return {}
        
if __name__ == "__main__":
    """
    
    Pass in some tx.draft file that the cardano-cli created into the tx simulation
    or pass in the cbor from the tx file.
    
    """
    main_script_dir = os.getcwd()
    target_folder = os.path.join(main_script_dir, 'test_data')
    tx_draft_path = target_folder+"/tx1.draft"
    required_units = from_file(tx_draft_path, False)
    print(required_units)
   
        
    
