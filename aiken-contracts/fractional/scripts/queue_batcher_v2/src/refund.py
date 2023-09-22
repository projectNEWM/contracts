import os
import subprocess

import src.address as address
import src.datums as datums
import src.dicts as dicts
import src.handle as handle
import src.json_file as json_file
import src.parsing as parsing
import src.transaction as transaction
from pycardano import Network


def build_tx(sale_info, queue_info, batcher_info, constants: dict):
    data_ref_utxo = constants['data_ref_utxo']
    queue_ref_utxo = constants['queue_ref_utxo']
    
    # hardcode this for now
    batcher_pkh = constants['batcher_pkh']
    
    # HARDCODE FEE FOR NOW, NEED WAY TO ESITMATE THESE UNITS BETTER
    FEE = 600000
    # this needs to be dynamic here
    FEE_VALUE = {"lovelace": FEE}
    COLLAT_VALUE = {"lovelace": int(1.5 * FEE)}
    execution_units = '(600000000, 2000000)'

    this_dir = os.path.dirname(os.path.abspath(__file__))
    parent_dir = os.path.dirname(this_dir)
    
    tmp_folder = os.path.join(parent_dir, "tmp")
    handle.create_folder_if_not_exists(tmp_folder)
    
    protocol_file_path = os.path.join(parent_dir, "tmp/protocol.json")
    out_file_path = os.path.join(parent_dir, "tmp/tx.draft")
    
    # queue purchase redeemer and queue datum
    json_file.write(datums.empty(1), "tmp/refund-redeemer.json")
    queue_redeemer_file_path = os.path.join(parent_dir, "tmp/refund-redeemer.json")
    
    queue_datum = queue_info['datum']
    owner_info = queue_datum['fields'][0]['fields']
    buyer_address = address.from_pkh_sc(owner_info[0]['bytes'], owner_info[1]['bytes'], Network.TESTNET)
    
    queue_value = queue_info['value']
    qv1 = dicts.subtract(queue_value, FEE_VALUE)
    
    batcher_value = batcher_info['value']
    batcher_out = parsing.process_output(constants['batcher_address'], batcher_value)
    cv1 = dicts.subtract(batcher_value, COLLAT_VALUE)
    
    
    buyer_out = parsing.process_output(buyer_address, qv1)
    
    # print('\nREFUND')
    # print(buyer_out)
        # "--tx-out-return-collateral", collat_out, "-tx-total-collateral", str(int(1.5*FEE)),
    
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

    # print('\nREFUND FUNC: ',func)
    result = subprocess.run(func, stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True)
    
    intermediate_txid = transaction.txid(out_file_path)
    # print("Purchase TxId:", intermediate_txid)
    
    
    queue_info['txid'] = intermediate_txid + "#1"
    queue_info['value'] = qv1
    
    # sale_info['txid'] = intermediate_txid + "#1"
    
    batcher_info['txid'] = intermediate_txid + "#0"
    
    return sale_info, queue_info, batcher_info