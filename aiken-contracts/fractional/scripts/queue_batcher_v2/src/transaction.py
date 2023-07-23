import subprocess
import os

def worst_case_asset(address):
    """This should be max ada, max one token and max another token. This shou
    model the maximum asset for a sale with a singular cost and bundle tokens."""
    return address + " + 18446744073709551615 + 18446744073709551615 b0818471a0e9633ae337cc1dcc7526ebe42286b4ceb3d836ad3a9e73.74686973697361766572796c6f6e67737472696e67666f7274657374696e6773 + 18446744073709551615 c0818471a0e9633ae337cc1dcc7526ebe42286b4ceb3d836ad3a9e73.64686973697361766572796c6f6e67737472696e67666f7274657374696e6773"


def txid(file_path):
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

    p = subprocess.Popen(
        func, stdout=subprocess.PIPE).stdout.read().decode('utf-8').rstrip()
    return p


def calculate_min_lovelace(cli, tmp, datum, output):
    """Calculate the lovelace for some asset and maybe a datum.
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
    tr_proc = subprocess.Popen(
        tr_cmd, stdin=calculate_fee.stdout, stdout=subprocess.PIPE)

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


def build_sale(batcher_tx_in, sale_tx_in, queue_tx_in, batcher_out, sale_out, queue_out):
    """
    Build a transaction and save the fileName into the tmp folder.
    """

    data_ref_utxo = txid('../tmp/referenceable-tx.signed') + "#0"
    sale_ref_utxo = txid('../tmp/sale-reference-utxo.signed') + "#1"
    queue_ref_utxo = txid('../tmp/queue-reference-utxo.signed') + "#1"

    # hardcode this for now
    collat_utxo = "6e34390c14ea8041c85963cf4b00a4ac900ebfd4e7bbcc9df7ed9345393777f3#0"

    # hardcode this for now
    collat_pkh = "b834fb41c45bd80e5fd9d99119723637fe9d1e3fc467bc1c57ae9aee"
    batcher_pkh = "e154dbd9ee8685258d7be1d3f374e4c2f1aebeada68707113b1422b0"

    sale_execution_units = "(300000000, 1000000)"
    queue_execution_units = "(1100000000, 3000000)"

    script_dir = os.path.dirname(os.path.abspath(__file__))
    parent_dir = os.path.dirname(script_dir)

    protocol_file_path = os.path.join(parent_dir, "tmp/protocol.json")
    out_file_path = os.path.join(parent_dir, "tmp/tx.draft")
    sale_redeemer_file_path = os.path.join(
        parent_dir, "data/sale/purchase-redeemer.json")
    sale_datum_file_path = os.path.join(
        parent_dir, "data/sale/sale-datum.json")
    queue_redeemer_file_path = os.path.join(
        parent_dir, "data/queue/purchase-redeemer.json")
    queue_datum_file_path = os.path.join(
        parent_dir, "data/queue/queue-datum.json")

    func = [
        "cardano-cli", "transaction", "build-raw",
        "--babbage-era",
        "--protocol-params-file", protocol_file_path,
        "--out-file", out_file_path,
        "--tx-in-collateral", collat_utxo,
        "--read-only-tx-in-reference", data_ref_utxo,
        "--tx-in", batcher_tx_in,
        "--tx-in", sale_tx_in,
        "--spending-tx-in-reference", sale_ref_utxo,
        "--spending-plutus-script-v2",
        "--spending-reference-tx-in-inline-datum-present",
        "--spending-reference-tx-in-execution-units", sale_execution_units,
        "--spending-reference-tx-in-redeemer-file", sale_redeemer_file_path,
        "--tx-in", queue_tx_in,
        "--spending-tx-in-reference", queue_ref_utxo,
        "--spending-plutus-script-v2",
        "--spending-reference-tx-in-inline-datum-present",
        "--spending-reference-tx-in-execution-units", queue_execution_units,
        "--spending-reference-tx-in-redeemer-file", queue_redeemer_file_path,
        "--tx-out", batcher_out,
        "--tx-out", sale_out,
        "--tx-out-inline-datum-file", sale_datum_file_path,
        "--tx-out", queue_out,
        "--tx-out-inline-datum-file", queue_datum_file_path,
        "--required-signer-hash", batcher_pkh,
        "--required-signer-hash", collat_pkh,
        "--fee", "600000"
    ]

    result = subprocess.run(func, capture_output=True, text=True, check=True)

    return result.stdout.strip()


def build_refund(script_tx_in, last_sale_utxo, buyer_out):
    data_ref_utxo = txid('../tmp/referenceable-tx.signed') + "#0"
    queue_ref_utxo = txid('../tmp/queue-reference-utxo.signed') + "#1"

    collat_utxo = "6e34390c14ea8041c85963cf4b00a4ac900ebfd4e7bbcc9df7ed9345393777f3#0"

    collat_pkh = "b834fb41c45bd80e5fd9d99119723637fe9d1e3fc467bc1c57ae9aee"
    batcher_pkh = "e154dbd9ee8685258d7be1d3f374e4c2f1aebeada68707113b1422b0"

    execution_units = '(600000000, 2000000)'

    script_dir = os.path.dirname(os.path.abspath(__file__))
    parent_dir = os.path.dirname(script_dir)

    protocol_file_path = os.path.join(parent_dir, "tmp/protocol.json")
    out_file_path = os.path.join(parent_dir, "tmp/tx.draft")
    queue_redeemer_file_path = os.path.join(
        parent_dir, "data/queue/refund-redeemer.json")

    func = [
        'cardano-cli', 'transaction', 'build-raw',
        '--babbage-era',
        '--protocol-params-file', protocol_file_path,
        '--out-file', out_file_path,
        '--tx-in-collateral', collat_utxo,
        '--read-only-tx-in-reference', data_ref_utxo,
        '--read-only-tx-in-reference', last_sale_utxo,
        '--tx-in', script_tx_in,
        '--spending-tx-in-reference', queue_ref_utxo,
        '--spending-plutus-script-v2',
        '--spending-reference-tx-in-inline-datum-present',
        '--spending-reference-tx-in-execution-units', execution_units,
        '--spending-reference-tx-in-redeemer-file', queue_redeemer_file_path,
        '--tx-out', buyer_out,
        '--required-signer-hash', batcher_pkh,
        '--required-signer-hash', collat_pkh,
        '--fee', '600000'
    ]

    result = subprocess.run(func, capture_output=True, text=True, check=True)
    return result.stdout.strip()


def sign(signing_key_files, network, signed_file_path):
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
        signed_file_path
    ]
    func += signing_keys(signing_key_files)
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