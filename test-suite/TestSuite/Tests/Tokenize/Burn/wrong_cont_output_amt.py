#!/usr/bin/python
"""
Test tokenization with the wrong amount of continuing outputs.
"""
import os
import copy
from dotenv import load_dotenv
from dotenv import load_dotenv, find_dotenv
from TestSuite.address import address_key_hash
import TestSuite.query as q
import TestSuite.parsing as p
import TestSuite.transaction as t

def wrong_burn_cont_output_amt():
    """
    Build a tokenization transaction that that sends two continuing outputs back to the script.
    """
    # env info
    root    = os.environ['ROOT']
    cli     = os.environ['cli']
    network = os.environ['network']

    tmp  = root+"/tmp/"
    addr = root+"/addresses/"
    
    # get the params
    q.protocol_parameters(cli, network, tmp)

    # get all the addrs and pkhs
    addrs = p.address_dict(addr)
    pkhs  = p.pkh_dict(cli, addr)
    sks   = p.skey_dict(addr)

    # ref utxos
    nft_lock_ref = t.txid(cli, tmp+"tx-tokenized-utxo.signed") + "#1"
    nft_mint_ref = t.txid(cli, tmp+"tx-tokenized-utxo.signed") + "#2"

    # get nft lock utxo data for the contract
    nft_lock_contract_addr = addrs['nftLock']
    q.utxo(cli, network, nft_lock_contract_addr, tmp)
    script_tx_in, script_inline_datum, script_value = p.txin(tmp)

    # find out which one we are talking about here
    counter = 0
    for i in range(len(script_value)):
        if len(list(script_value[i].keys())) == 2:
            counter = i
    start = counter * 2
    if counter == 0:
        script_tx_in = script_tx_in[:2]
    else:
        script_tx_in = script_tx_in[start:start+2]
    # quit()

    # build out the current datum
    p.write_json_file(script_inline_datum[counter], 'data/current_tokenized_datum.json')

    next_script_datum = copy.deepcopy(script_inline_datum[counter])
    next_script_datum['fields'][1]['int'] += 1
    p.write_json_file(next_script_datum, 'data/next_tokenized_datum.json')

    # create script output here
    tokenized_output = p.process_output(nft_lock_contract_addr, script_value[counter])
    bad_tokenized_output = p.process_output(nft_lock_contract_addr, {"lovelace":1234567})
    
    # get the newm addr info
    newm_addr = addrs['newm']
    q.utxo(cli, network, newm_addr, tmp)
    newm_tx_in, newm_inline_datum, newm_value = p.txin(tmp)

    # minting info
    mint_pid   = script_inline_datum[counter]['fields'][0]['bytes']
    mint_tkn   = newm_value[0][mint_pid]
    mint_asset = "-1 " + mint_pid + "." + list(mint_tkn.keys())[0]

    # get the collat addr info
    collat_addr = addrs['collat']
    q.utxo(cli, network, collat_addr, tmp)
    collat_tx_in, collat_inline_datum, collat_value = p.txin(tmp)

    # pkh for signing
    newm_pkh = pkhs['newm']
    collat_pkh = pkhs['collat']

    # multisig signing
    multisig1 = pkhs['multisig1']
    multisig2 = pkhs['multisig2']
    multisig3 = pkhs['multisig3']

    # build the output list
    utxo_out = [
        '--tx-out', tokenized_output,
        '--tx-out-inline-datum-file', 'data/current_tokenized_datum.json',
        '--tx-out', bad_tokenized_output,
        '--tx-out-inline-datum-file', 'data/current_tokenized_datum.json',
    ]

    # build tx object for tx build function
    tx_object = {
        "change_addr": newm_addr,
        "collat_utxo": collat_tx_in[1],
        "utxo_in": newm_tx_in + script_tx_in,
        "spend_ref": nft_lock_ref,
        "spend_redeemer": "data/burn_tokenized_redeemer.json",
        "utxo_out":utxo_out,
        "signers": [newm_pkh, collat_pkh, multisig1, multisig2, multisig3],
        "mint_asset": mint_asset,
        "mint_ref": nft_mint_ref,
        "policy_id": mint_pid,
        "mint_redeemer": "data/current_tokenized_datum.json",
    }

    result = t.build(cli, tmp, network, tx_object)

    lines_with_debugging_logs = [line for line in result.splitlines() if "Script debugging logs:" in line]
    # print(lines_with_debugging_logs)
    return lines_with_debugging_logs


if __name__ == "__main__":
    # Load environment variables from .node.env file
    load_dotenv(find_dotenv('.node.env'), verbose=False)

    # Set the CARDANO_NODE_SOCKET_PATH environment variable
    socket = os.environ['socket']
    os.environ["CARDANO_NODE_SOCKET_PATH"] = socket

    output = wrong_burn_cont_output_amt()
    print(output)
    
