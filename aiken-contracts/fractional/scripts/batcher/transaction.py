#!/usr/bin/python
"""
All cardano-cli transaction functions required for testing.
"""
import subprocess
import json
import os

def txid(cli, tx_file_path):
    """
    Get the tx id of a signed transactions.
    """
    func = [
        cli,
        'transaction',
        'txid',
        '--tx-file',
        tx_file_path
    ]

    p = subprocess.Popen(func, stdout=subprocess.PIPE).stdout.read().decode('utf-8').rstrip()
    return p



def calculate_min_lovelace(cli, tmp, datum, output):
    """
    output="${script_address} + 5000000 + ${asset}"
    """

    func = [
        cli, 
        'transaction', 
        'calculate-min-required-utxo', 
        '--babbage-era',
        '--protocol-params-file',
        tmp+"protocol-parameters.json",
        '--tx-out',
        output
    ]
    if datum:
        func += [
            '--tx-out-inline-datum-file',
            datum
        ]

    calculate_fee = subprocess.Popen(func, stdout=subprocess.PIPE)

    # Set up the second subprocess and connect it to the pipe from the first subprocess
    tr_cmd = ['tr', '-dc', '0-9']
    tr_proc = subprocess.Popen(tr_cmd, stdin=calculate_fee.stdout, stdout=subprocess.PIPE)

    # Read the output of the second subprocess
    output = tr_proc.communicate()[0]

    return int(output)


def signing_keys(signer_keys):
    """
    Create a list of the signing key files. This can not be empty.
    """
    output = []
    for sk in signer_keys:
        output.append('--signing-key-file')
        output.append(sk)
    return output

def build(batcher_tx_in, sale_tx_in, queue_tx_in, batcher_out, sale_out, queue_out):
    """
    Build a transaction and save the fileName into the tmp folder.
    """
    data_ref_utxo = "673532a53fcb4ea99cdb18a745bc67bd611737b11c8736f3c7b8baa41af55416#0"
    script_ref_utxo = "a83a944b8233e826be585816ae69a735d57e41ba503668b7ef6cd629570ab617#1"
    queue_ref_utxo = "7033cff13fb40682f5d5a4073f109682a4ac01c4d55d202e52d323e664fc0536#1"
    collat_utxo = "6e34390c14ea8041c85963cf4b00a4ac900ebfd4e7bbcc9df7ed9345393777f3#0"
    
    collat_pkh = "b834fb41c45bd80e5fd9d99119723637fe9d1e3fc467bc1c57ae9aee"
    batcher_pkh = "e154dbd9ee8685258d7be1d3f374e4c2f1aebeada68707113b1422b0"
    
    sale_execution_unts = "(300000000, 1000000)"
    queue_execution_unts = "(1000000000, 3000000)"
    func = [
    "cardano-cli", "transaction", "build-raw",
    "--babbage-era",
    "--protocol-params-file", "../tmp/protocol.json",
    "--out-file", "../tmp/tx.draft",
    "--tx-in-collateral", collat_utxo,
    "--read-only-tx-in-reference", data_ref_utxo,
    "--tx-in", batcher_tx_in,
    "--tx-in", sale_tx_in,
    "--spending-tx-in-reference", script_ref_utxo,
    "--spending-plutus-script-v2",
    "--spending-reference-tx-in-inline-datum-present",
    "--spending-reference-tx-in-execution-units", sale_execution_unts,
    "--spending-reference-tx-in-redeemer-file", "../data/sale/purchase-redeemer.json",
    "--tx-in", queue_tx_in,
    "--spending-tx-in-reference=" + queue_ref_utxo,
    "--spending-plutus-script-v2",
    "--spending-reference-tx-in-inline-datum-present",
    "--spending-reference-tx-in-execution-units", queue_execution_unts,
    "--spending-reference-tx-in-redeemer-file", "../data/queue/purchase-redeemer.json",
    "--tx-out=" + batcher_out,
    "--tx-out=" + sale_out,
    "--tx-out-inline-datum-file", "../data/sale/sale-datum.json",
    "--tx-out=" + queue_out,
    "--tx-out-inline-datum-file", "../data/queue/queue-datum.json",
    "--required-signer-hash", batcher_pkh,
    "--required-signer-hash", collat_pkh,
    "--fee", '500000'
    ]
    
    print(func)
    
    result = subprocess.run(func, stdout=subprocess.PIPE, stderr=subprocess.STDOUT, text=True)
    return result.stdout.strip()
    

def sign(cli, network, tmp, signing_key_files):
    """
    Sign a transaction with a list of payment keys.
    """
    func = [
        cli,
        'transaction',
        'sign',
        '--tx-body-file',
        tmp+'tx.draft',
        '--tx-file',
        tmp+'tx.signed'
    ]
    func += signing_keys(signing_key_files)
    func += network.split(" ")
    
    p = subprocess.Popen(func)
    p.communicate()


def submit(cli, tmp, network):
    """
    Submit the transaction to the blockchain.
    """
    func = [
        cli,
        'transaction',
        'submit',
        '--tx-file',
        tmp+'tx.signed'
    ]
    func += network.split(" ")

    # this should print a message that I want to catch
    p = subprocess.Popen(func, stdout=subprocess.PIPE).stdout.read().decode('utf-8').rstrip()
    return p