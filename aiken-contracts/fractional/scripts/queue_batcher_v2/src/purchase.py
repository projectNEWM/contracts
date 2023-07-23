import os
import src.json_file as json_file
import src.datums as datums

def create_folder_if_not_exists(folder_path: str) -> None:
    if not os.path.exists(folder_path):
        os.makedirs(folder_path)

def build_tx(sale_info, queue_info, batcher_info, constants: dict) -> None:
    """
    Build a transaction and save the fileName into the tmp folder.
    """

    data_ref_utxo = constants['data_ref_utxo']
    sale_ref_utxo = constants['sale_ref_utxo']
    queue_ref_utxo = constants['queue_ref_utxo']


    # hardcode this for now
    batcher_pkh = constants['batcher_pkh']

    # HARDCODE FEE FOR NOW, NEED WAY TO ESITMATE THESE UNITS BETTER
    FEE = 6000000
    sale_execution_units = "(300000000, 1000000)"
    queue_execution_units = "(1100000000, 3000000)"

    this_dir = os.path.dirname(os.path.abspath(__file__))
    parent_dir = os.path.dirname(this_dir)
    
    tmp_folder = os.path.join(parent_dir, "tmp")
    create_folder_if_not_exists(tmp_folder)
    
    protocol_file_path = os.path.join(parent_dir, "tmp/protocol.json")
    out_file_path = os.path.join(parent_dir, "tmp/tx.draft")
    
    # sale purchase redeemer
    json_file.write(datums.empty(0), "tmp/purchase-redeemer.json")
    sale_redeemer_file_path = os.path.join(parent_dir, "tmp/purchase-redeemer.json")
    
    # sale datum stuff
    sale_datum = sale_info['datum']
    json_file.write(sale_datum, "tmp/sale-datum.json")
    sale_datum_file_path = os.path.join(parent_dir, "tmp/sale-datum.json")
    
    # queue purchase redeemer and queue datum
    json_file.write(datums.empty(0), "tmp/purchase-redeemer.json")
    queue_redeemer_file_path = os.path.join(parent_dir, "tmp/purchase-redeemer.json")
    
    # queue datum stuff
    queue_datum = queue_info['datum']
    json_file.write(queue_datum, "tmp/queue-datum.json")
    queue_datum_file_path = os.path.join(parent_dir, "tmp/queue-datum.json")

    batcher_value = batcher_info['value']
    sale_value = sale_info['value']
    queue_value = queue_info['value']
    
    batcher_out = ''
    sale_out = ''
    queue_out = ''

    func = [
        "cardano-cli", "transaction", "build-raw",
        "--babbage-era",
        "--protocol-params-file", protocol_file_path,
        "--out-file", out_file_path,
        "--tx-in-collateral", batcher_info['txid'],
        "--read-only-tx-in-reference", data_ref_utxo,
        "--tx-in", batcher_info['txid'],
        "--tx-in", sale_info['txid'],
        "--spending-tx-in-reference", sale_ref_utxo,
        "--spending-plutus-script-v2",
        "--spending-reference-tx-in-inline-datum-present",
        "--spending-reference-tx-in-execution-units", sale_execution_units,
        "--spending-reference-tx-in-redeemer-file", sale_redeemer_file_path,
        "--tx-in", queue_info['txid'],
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
        "--fee", str(FEE)
    ]

    # result = subprocess.run(func, capture_output=True, text=True, check=True)

    # return result.stdout.strip()