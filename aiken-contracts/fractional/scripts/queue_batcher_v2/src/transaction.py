import subprocess
import os
from typing import Optional

def txid(file_path: str) -> Optional[str]:
    """
    Get the tx id of a signed transactions.
    """
    func = [
        'cardano-cli',
        'transaction',
        'txid',
        '--tx-file',
        file_path
    ]

    p = subprocess.Popen(func, stdout=subprocess.PIPE).stdout
    if p is not None:
        return p.read().decode('utf-8').rstrip()
    return p

def signing_keys(signer_keys):
    """
    Create a list of the signing key files. This can not be empty.
    """
    output = []
    for sk in signer_keys:
        output.append('--signing-key-file')
        output.append(sk)
    return output


# def build_sale(batcher_tx_in, sale_tx_in, queue_tx_in, batcher_out, sale_out, queue_out):
#     """
#     Build a transaction and save the fileName into the tmp folder.
#     """

#     data_ref_utxo = txid('../tmp/referenceable-tx.signed') + "#0"
#     sale_ref_utxo = txid('../tmp/sale-reference-utxo.signed') + "#1"
#     queue_ref_utxo = txid('../tmp/queue-reference-utxo.signed') + "#1"

#     # hardcode this for now
#     collat_utxo = "6e34390c14ea8041c85963cf4b00a4ac900ebfd4e7bbcc9df7ed9345393777f3#0"

#     # hardcode this for now
#     collat_pkh = "b834fb41c45bd80e5fd9d99119723637fe9d1e3fc467bc1c57ae9aee"
#     batcher_pkh = "e154dbd9ee8685258d7be1d3f374e4c2f1aebeada68707113b1422b0"

#     sale_execution_units = "(300000000, 1000000)"
#     queue_execution_units = "(1100000000, 3000000)"

#     script_dir = os.path.dirname(os.path.abspath(__file__))
#     parent_dir = os.path.dirname(script_dir)

#     protocol_file_path = os.path.join(parent_dir, "tmp/protocol.json")
#     out_file_path = os.path.join(parent_dir, "tmp/tx.draft")
#     sale_redeemer_file_path = os.path.join(
#         parent_dir, "data/sale/purchase-redeemer.json")
#     sale_datum_file_path = os.path.join(
#         parent_dir, "data/sale/sale-datum.json")
#     queue_redeemer_file_path = os.path.join(
#         parent_dir, "data/queue/purchase-redeemer.json")
#     queue_datum_file_path = os.path.join(
#         parent_dir, "data/queue/queue-datum.json")

#     func = [
#         "cardano-cli", "transaction", "build-raw",
#         "--babbage-era",
#         "--protocol-params-file", protocol_file_path,
#         "--out-file", out_file_path,
#         "--tx-in-collateral", collat_utxo,
#         "--read-only-tx-in-reference", data_ref_utxo,
#         "--tx-in", batcher_tx_in,
#         "--tx-in", sale_tx_in,
#         "--spending-tx-in-reference", sale_ref_utxo,
#         "--spending-plutus-script-v2",
#         "--spending-reference-tx-in-inline-datum-present",
#         "--spending-reference-tx-in-execution-units", sale_execution_units,
#         "--spending-reference-tx-in-redeemer-file", sale_redeemer_file_path,
#         "--tx-in", queue_tx_in,
#         "--spending-tx-in-reference", queue_ref_utxo,
#         "--spending-plutus-script-v2",
#         "--spending-reference-tx-in-inline-datum-present",
#         "--spending-reference-tx-in-execution-units", queue_execution_units,
#         "--spending-reference-tx-in-redeemer-file", queue_redeemer_file_path,
#         "--tx-out", batcher_out,
#         "--tx-out", sale_out,
#         "--tx-out-inline-datum-file", sale_datum_file_path,
#         "--tx-out", queue_out,
#         "--tx-out-inline-datum-file", queue_datum_file_path,
#         "--required-signer-hash", batcher_pkh,
#         "--required-signer-hash", collat_pkh,
#         "--fee", "600000"
#     ]

#     result = subprocess.run(func, capture_output=True, text=True, check=True)

#     return result.stdout.strip()


# def build_refund(script_tx_in, last_sale_utxo, buyer_out):
#     data_ref_utxo = txid('../tmp/referenceable-tx.signed') + "#0"
#     queue_ref_utxo = txid('../tmp/queue-reference-utxo.signed') + "#1"

#     collat_utxo = "6e34390c14ea8041c85963cf4b00a4ac900ebfd4e7bbcc9df7ed9345393777f3#0"

#     collat_pkh = "b834fb41c45bd80e5fd9d99119723637fe9d1e3fc467bc1c57ae9aee"
#     batcher_pkh = "e154dbd9ee8685258d7be1d3f374e4c2f1aebeada68707113b1422b0"

#     execution_units = '(600000000, 2000000)'

#     script_dir = os.path.dirname(os.path.abspath(__file__))
#     parent_dir = os.path.dirname(script_dir)

#     protocol_file_path = os.path.join(parent_dir, "tmp/protocol.json")
#     out_file_path = os.path.join(parent_dir, "tmp/tx.draft")
#     queue_redeemer_file_path = os.path.join(
#         parent_dir, "data/queue/refund-redeemer.json")

#     func = [
#         'cardano-cli', 'transaction', 'build-raw',
#         '--babbage-era',
#         '--protocol-params-file', protocol_file_path,
#         '--out-file', out_file_path,
#         '--tx-in-collateral', collat_utxo,
#         '--read-only-tx-in-reference', data_ref_utxo,
#         '--read-only-tx-in-reference', last_sale_utxo,
#         '--tx-in', script_tx_in,
#         '--spending-tx-in-reference', queue_ref_utxo,
#         '--spending-plutus-script-v2',
#         '--spending-reference-tx-in-inline-datum-present',
#         '--spending-reference-tx-in-execution-units', execution_units,
#         '--spending-reference-tx-in-redeemer-file', queue_redeemer_file_path,
#         '--tx-out', buyer_out,
#         '--required-signer-hash', batcher_pkh,
#         '--required-signer-hash', collat_pkh,
#         '--fee', '600000'
#     ]

#     result = subprocess.run(func, capture_output=True, text=True, check=True)
#     return result.stdout.strip()


def sign(signed_file_path, network, batcher_skey_path):
    """
    Sign a transaction with a list of payment keys.
    """
    func = [
        'cardano-cli',
        'transaction',
        'sign',
        '--tx-body-file',
        '../tmp/tx.draft',
        '--tx-file',
        signed_file_path,
        '--signing-key-file',
        batcher_skey_path
    ]
    func += network.split(" ")

    # print(func)
    result = subprocess.run(func, capture_output=True, text=True, check=True)
    if result.stderr != "":
        print(result.stderr)
        exit(1)


def submit(network, socket_path, signed_file_path):
    """
    Submit the transaction to the blockchain.
    """
    func = [
        'cardano-cli',
        'transaction',
        'submit',
        '--socket-path', socket_path,
        '--tx-file',
        signed_file_path
    ]
    func += network.split(" ")

    # print(func)
    # result = subprocess.run(func, capture_output=True, text=True, check=False)
    result = subprocess.run(func, stdout=subprocess.PIPE,
                            stderr=subprocess.PIPE, text=True)
    if result.stderr != "":
        print('ERROR:', result.stderr)
        # should this exit be here?
        # exit(1)
    else:
        print(result.stdout.strip())
    return result.stdout.strip()
