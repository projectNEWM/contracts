import os
import subprocess

from pycardano import Network
from src import address, datums, dicts, json_file, parsing, transaction


def create_folder_if_not_exists(folder_path: str) -> None:
    if not os.path.exists(folder_path):
        os.makedirs(folder_path)


def build_tx(sale_info, queue_info, batcher_info, constants: dict):
    data_ref_utxo = constants['data_ref_utxo']
    queue_ref_utxo = constants['queue_ref_utxo']

    # hardcode this for now
    batcher_pkh = constants['batcher_pkh']

    # HARDCODE FEE FOR NOW, NEED WAY TO ESITMATE THESE UNITS BETTER
    FEE = 350000
    # this needs to be dynamic here
    FEE_VALUE = {"lovelace": FEE}
    # [{"mem": 988722, "cpu": 360646760}]
    execution_units = '(400000000, 1500000)'

    this_dir = os.path.dirname(os.path.abspath(__file__))
    parent_dir = os.path.dirname(this_dir)

    tmp_folder = os.path.join(parent_dir, "tmp")
    create_folder_if_not_exists(tmp_folder)

    protocol_file_path = os.path.join(parent_dir, "tmp/protocol.json")
    out_file_path = os.path.join(parent_dir, "tmp/tx.draft")

    # queue purchase redeemer and queue datum
    json_file.write(datums.empty(1), "tmp/refund-redeemer.json")
    queue_redeemer_file_path = os.path.join(
        parent_dir, "tmp/refund-redeemer.json")

    queue_datum = queue_info['datum']
    owner_info = queue_datum['fields'][0]['fields']
    buyer_address = address.from_pkh_sc(
        owner_info[0]['bytes'], owner_info[1]['bytes'], Network.TESTNET)

    incentive_data = queue_datum['fields'][3]['fields']
    incentive_value = {incentive_data[0]['bytes']: {
        incentive_data[1]['bytes']: incentive_data[2]['int']}}
    queue_value = queue_info['value']
    qv1 = dicts.subtract(queue_value, FEE_VALUE)
    qv2 = dicts.subtract(qv1, incentive_value)

    batcher_value = batcher_info['value']
    bv1 = dicts.add(batcher_value, incentive_value)
    batcher_out = parsing.process_output(
        constants['batcher_address'], bv1)

    buyer_out = parsing.process_output(buyer_address, qv2)

    # print(batcher_out)
    # print(buyer_out)

    func = [
        'cardano-cli', 'transaction', 'build-raw',
        '--babbage-era',
        '--protocol-params-file', protocol_file_path,
        '--out-file', out_file_path,
        "--tx-in-collateral", constants['collat_utxo'],
        '--read-only-tx-in-reference', data_ref_utxo,
        '--read-only-tx-in-reference', sale_info['txid'],
        "--tx-in", batcher_info['txid'],
        '--tx-in', queue_info['txid'],
        '--spending-tx-in-reference', queue_ref_utxo,
        '--spending-plutus-script-v2',
        '--spending-reference-tx-in-inline-datum-present',
        '--spending-reference-tx-in-execution-units', execution_units,
        '--spending-reference-tx-in-redeemer-file', queue_redeemer_file_path,
        "--tx-out", batcher_out,
        '--tx-out', buyer_out,
        '--required-signer-hash', batcher_pkh,
        '--required-signer-hash', constants['collat_pkh'],
        '--fee', str(FEE)
    ]

    _ = subprocess.run(func, stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True)
    # print(result)

    intermediate_txid = transaction.txid(out_file_path)

    queue_info['txid'] = intermediate_txid + "#1"
    queue_info['value'] = qv1

    batcher_info['txid'] = intermediate_txid + "#0"

    return sale_info, queue_info, batcher_info
