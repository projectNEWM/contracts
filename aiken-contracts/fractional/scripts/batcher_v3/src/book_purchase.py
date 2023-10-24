import os
import subprocess

from src import datums, json_file, transaction


def create_folder_if_not_exists(folder_path: str) -> None:
    if not os.path.exists(folder_path):
        os.makedirs(folder_path)

def build_tx(order1_info, order2_info, batcher_info, constants):
    this_dir = os.path.dirname(os.path.abspath(__file__))
    parent_dir = os.path.dirname(this_dir)
    
    data_ref_utxo = constants['data_ref_utxo']
    order_ref_utxo = constants['order_book_ref_utxo']
    
    tmp_folder = os.path.join(parent_dir, "tmp")
    create_folder_if_not_exists(tmp_folder)
    
    protocol_file_path = os.path.join(parent_dir, "tmp/protocol.json")
    out_file_path = os.path.join(parent_dir, "tmp/tx.draft")
    
    order_book_execution_unts="(950000000, 3000000)"
    order_fee = 720588

# queue purchase redeemer and queue datum
# sale datum stuff
    order1_datum = order1_info['datum']
    json_file.write(order1_datum, "tmp/order1-datum.json")
    order1_datum_file_path = os.path.join(parent_dir, "tmp/order1-datum.json")
    
    order2_datum = order2_info['datum']
    json_file.write(order2_datum, "tmp/order2-datum.json")
    order2_datum_file_path = os.path.join(parent_dir, "tmp/order2-datum.json")
    
    json_file.write(datums.complete_redeemer("", 0), "tmp/purchase1-redeemer.json")
    order1_redeemer_file_path = os.path.join(parent_dir, "tmp/purchase1-redeemer.json")
    
    json_file.write(datums.complete_redeemer("", 0), "tmp/purchase2-redeemer.json")
    order2_redeemer_file_path = os.path.join(parent_dir, "tmp/purchase2-redeemer.json")
    
    func = [
        "cardano-cli", "transaction", "build-raw",
        "--babbage-era",
        "--protocol-params-file", protocol_file_path,
        "--out-file", out_file_path,
        "--tx-in-collateral", constants['collat_utxo'],
        "--read-only-tx-in-reference", data_ref_utxo,
        "--tx-in", batcher_info['txid'],
        "--tx-in", order1_info['txid'],
        "--spending-tx-in-reference", order_ref_utxo,
        "--spending-plutus-script-v2",
        "--spending-reference-tx-in-inline-datum-present",
        "--spending-reference-tx-in-execution-units", order_book_execution_unts,
        "--spending-reference-tx-in-redeemer-file", order1_redeemer_file_path,
        "--tx-in", order2_info['txid'],
        "--spending-tx-in-reference", order_ref_utxo,
        "--spending-plutus-script-v2",
        "--spending-reference-tx-in-inline-datum-present",
        "--spending-reference-tx-in-execution-units", order_book_execution_unts,
        "--spending-reference-tx-in-redeemer-file", order2_redeemer_file_path,
        "--tx-out", batcher_info['output'],
        "--tx-out", order1_info['output'],
        "--tx-out-inline-datum-file", order1_datum_file_path,
        "--tx-out", order2_info['output'],
        "--tx-out-inline-datum-file", order2_datum_file_path,
        "--required-signer-hash", constants['batcher_pkh'],
        "--required-signer-hash", constants['collat_pkh'],
        "--fee", str(order_fee)
    ]

    result = subprocess.run(func, stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True)
    print(result)
    
    # update the sale, queue, and batcher info for the next stepz
    
    intermediate_txid = transaction.txid(out_file_path)
    
    batcher_info['txid'] = intermediate_txid + "#0"
    
    order1_info['txid'] = intermediate_txid + "#1"
    
    order2_info['txid'] = intermediate_txid + "#2"
    
    return order1_info, order2_info, batcher_info
    