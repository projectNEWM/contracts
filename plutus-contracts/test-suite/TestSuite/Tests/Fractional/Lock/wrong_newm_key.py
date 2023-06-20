#!/usr/bin/python
"""
Test fractionalization with the wrong newm master key.
"""
import os
import copy
from dotenv import load_dotenv
from dotenv import load_dotenv, find_dotenv
from TestSuite.address import address_key_hash
import TestSuite.query as q
import TestSuite.parsing as p
import TestSuite.transaction as t

def wrong_lock_newm_key():
    """
    Build a fractionalization transaction that fails to provide the correct newm master key.
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

    # print(script_inline_datum[0])
    
    datum_data = p.read_json_file('data/current_tokenized_datum.json')
    mint_pid = datum_data['fields'][0]['bytes']
    
    mint_tkn   = script_inline_datum[0]['fields'][1]['bytes']
    tokenized_value = {mint_pid:{mint_tkn:1}}
    new_script_value = p.add_dicts(script_value[0], tokenized_value)

    # create script output here
    fractional_output = p.process_output(ft_lock_contract_addr, new_script_value)
    # print('script output', fractional_output)


    # minting info
    mint_pid   = script_inline_datum[0]['fields'][0]['bytes']
    mint_tkn   = script_inline_datum[0]['fields'][1]['bytes']
    mint_amt   = 100000000
    mint_asset = f"{mint_amt} " + mint_pid + "." + mint_tkn
    mint_value = {mint_pid:{mint_tkn:mint_amt}}
    # print(mint_asset)

    # get the artist addr info
    artist_addr = addrs['artist']
    q.utxo(cli, network, artist_addr, tmp)
    artist_tx_in, artist_inline_datum, artist_value = p.txin(tmp)
    
    # get artist min ada
    artist_output  = artist_addr + " + 5000000 + " + mint_asset
    mint_min_ada = t.calculate_min_lovelace(cli, tmp, '', artist_output)
    artist_output  = artist_addr + f" + {mint_min_ada} + " + mint_asset
    # print("artist output", artist_output)
    # quit()

    # get the collat addr info
    collat_addr = addrs['collat']
    q.utxo(cli, network, collat_addr, tmp)
    collat_tx_in, collat_inline_datum, collat_value = p.txin(tmp)

    # pkh for signing
    artist_pkh = pkhs['artist']
    collat_pkh = pkhs['collat']

    # build the output list
    utxo_out = [
        '--tx-out', fractional_output,
        '--tx-out-inline-datum-file', 'data/fractional_datum.json',
        '--tx-out', artist_output,
    ]

    # build tx object for tx build function
    tx_object = {
        "change_addr": artist_addr,
        "collat_utxo": collat_tx_in[1],
        "utxo_in": artist_tx_in + script_tx_in,
        "spend_ref": nft_lock_ref,
        "spend_redeemer": "data/fractional_lock_redeemer.json",
        "utxo_out":utxo_out,
        "signers": [artist_pkh, collat_pkh],
        "mint_asset": mint_asset,
        "mint_ref": nft_mint_ref,
        "policy_id": mint_pid,
        "mint_redeemer": "data/current_tokenized_datum.json",
        "metadata": "data/example.metadata.json"
    }

    result = t.build(cli, tmp, network, tx_object)
    # print(result)
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

    output = wrong_lock_newm_key()
    print(output)
    