#!/usr/bin/python
"""
All cardano-cli transaction functions required for testing.
"""
import subprocess
import json

def spending_script(ref_utxo, redeemer):
    """
    Create the Plutus V2 spending reference script with inline datum and a redeemer.
    """
    output = [
        '--spending-tx-in-reference',
        ref_utxo,
        '--spending-plutus-script-v2',
        '--spending-reference-tx-in-inline-datum-present',
        '--spending-reference-tx-in-redeemer-file',
        redeemer
    ]
    return output

def minting_script(mint_asset, ref_utxo, policy_id, redeemer):
    """
    Create the Plutus V2 minting reference script with a redeemer.
    """
    output = [
        '--mint',
        mint_asset,
        '--mint-tx-in-reference',
        ref_utxo,
        '--mint-plutus-script-v2' \
        '--policy-id',
        policy_id,
        '--mint-reference-tx-in-redeemer-file',
        redeemer
    ]
    return output

def required_signers(signers):
    """
    Create a list of the required signers from a list of public key hashes.
    """
    if len(signers) == 0:
        return []
    output = []
    for s in signers:
        output.append('--required-signer-hash')
        output.append(s)
    return output

def signing_keys(signer_keys):
    """
    Create a list of the signing key files. This can not be empty.
    """
    output = []
    for sk in signer_keys:
        output.append('--signing-key-file')
        output.append(sk)
    return output

def build(cli, tmp, network, tx_object):
    """
    Build a transaction and save the fileName into the tmp folder.
    """
    func = [
        cli,
        'transaction',
        'build',
        '--protocol-params-file',
        tmp+"protocol-parameters.json",
        '--out-file',
        tmp+'tx.draft',
        '--change-address',
        tx_object['change_addr'],
        '--tx-in-collateral',
        tx_object['collat_utxo'],
    ]
    # This needs to end with the script input
    func += utxo_in

    # spend script stuff
    func += spending_script(tx_object['spend_ref'], tx_object['spend_redeemer'])

    # the datum must fllow the correct output
    func += utxo_out

    # signer sutff
    func += required_signers(tx_object['signers'])
    
    # mint script stuff
    func += minting_script(tx_object['mint_asset'], tx_object['mint_ref'], tx_object['policy_id'], tx_object['mint_redeemer'])
    
    # metadata stuff
    func += ['--metadata-json-file', tmp+ tx_object['metadata.json']]

    # network
    func += network.split(" ")

    # this should be return the fee that can be printed out
    p = subprocess.Popen(func, stdout=subprocess.PIPE).stdout.read().decode('utf-8')
    print(p)


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
    p = subprocess.Popen(func, stdout=subprocess.PIPE).stdout.read().decode('utf-8')
    print(p)