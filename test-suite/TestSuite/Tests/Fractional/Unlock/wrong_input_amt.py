#!/usr/bin/python
"""
Test fractionalization with the wrong amount of datum inputs.
"""
import os
import copy
from dotenv import load_dotenv
from dotenv import load_dotenv, find_dotenv
from TestSuite.address import address_key_hash
import TestSuite.query as q
import TestSuite.parsing as p
import TestSuite.transaction as t

def wrong_unlock_input_amt():
    """
    Build a fractionalization transaction that attempts to spend two datum inputs.
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
    nft_lock_ref = t.txid(cli, tmp+"tx-fractions-utxo.signed") + "#1"
    nft_mint_ref = t.txid(cli, tmp+"tx-fractions-utxo.signed") + "#2"

    # fractional contract addr
    ft_lock_contract_addr = addrs['ftLock']
    q.utxo(cli, network, ft_lock_contract_addr, tmp)
    script_tx_in, script_inline_datum, script_value = p.txin(tmp)
    script_tx_in = script_tx_in[:2]

    bad_script_output = p.process_output(ft_lock_contract_addr, {"lovelace":1234567})


    # minting info
    mint_pid   = script_inline_datum[0]['fields'][0]['bytes']
    mint_tkn   = script_inline_datum[0]['fields'][2]['bytes']
    mint_amt   = -100000000
    mint_asset = f"{mint_amt} " + mint_pid + "." + mint_tkn
    mint_value = {mint_pid:{mint_tkn:mint_amt}}
    # print(mint_asset)

    # get the newm addr info
    newm_addr = addrs['newm']
    q.utxo(cli, network, newm_addr, tmp)
    newm_tx_in, newm_inline_datum, newm_value = p.txin(tmp)

    # send what is on the script utxo back to the newm addr
    newm_output = p.process_output(newm_addr, script_value[0])


    # get the collat addr info
    collat_addr = addrs['collat']
    q.utxo(cli, network, collat_addr, tmp)
    collat_tx_in, collat_inline_datum, collat_value = p.txin(tmp)

    # get the newm addr info
    attacker_addr = addrs['attacker']
    q.utxo(cli, network, attacker_addr, tmp)
    attacker_tx_in, attacker_inline_datum, attacker_value = p.txin(tmp)
    attacker_output = p.process_output(attacker_addr, attacker_value[0])

    # pkh for signing
    newm_pkh = pkhs['newm']
    collat_pkh = pkhs['collat']
    attacker_pkh = pkhs['attacker']


    # build the output list
    utxo_out = [
        '--tx-out', newm_output,
        '--tx-out', attacker_output,
    ]

    # build tx object for tx build function
    tx_object = {
        "change_addr": newm_addr,
        "collat_utxo": collat_tx_in[1],
        "utxo_in": attacker_tx_in + newm_tx_in + script_tx_in,
        "spend_ref": nft_lock_ref,
        "spend_redeemer": "data/fractional_unlock_redeemer.json",
        "utxo_out":utxo_out,
        "signers": [newm_pkh, collat_pkh, attacker_pkh],
        "mint_asset": mint_asset,
        "mint_ref": nft_mint_ref,
        "policy_id": mint_pid,
        "mint_redeemer": "data/current_tokenized_datum.json",
    }

    result = t.build(cli, tmp, network, tx_object)
     # return result
    lines_with_debugging_logs = [line for line in result.splitlines() if "Script debugging logs:" in line]
    # print(lines_with_debugging_logs)
    return lines_with_debugging_logs


if __name__ == "__main__":
    # Load environment variables from .node.env file
    load_dotenv(find_dotenv('.node.env'), verbose=False)

    # Set the CARDANO_NODE_SOCKET_PATH environment variable
    socket = os.environ['socket']
    os.environ["CARDANO_NODE_SOCKET_PATH"] = socket

    output = wrong_unlock_input_amt()
    print(output)
    
