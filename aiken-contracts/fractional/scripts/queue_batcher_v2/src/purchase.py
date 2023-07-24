import os
import src.json_file as json_file
import src.datums as datums
import src.dicts as dicts
import src.parsing as parsing
import subprocess

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
    # this needs to be dynamic here
    FEE_VALUE = {"lovelace": FEE}
    sale_execution_units = "(300000000, 1000000)"
    queue_execution_units = "(1100000000, 3000000)"

    this_dir = os.path.dirname(os.path.abspath(__file__))
    parent_dir = os.path.dirname(this_dir)
    
    tmp_folder = os.path.join(parent_dir, "tmp")
    create_folder_if_not_exists(tmp_folder)
    
    protocol_file_path = os.path.join(parent_dir, "tmp/protocol.json")
    out_file_path = os.path.join(parent_dir, "tmp/tx.draft")
    purchased_tx_signed_path = os.path.join(parent_dir, 'tmp/purchased-tx.signed')
    # refunded_tx_signed_path = os.path.join(parent_dir, 'tmp/refunded-tx.signed')
    
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

    bundle_pid = sale_info['datum']['fields'][1]['fields'][0]['bytes']
    bundle_tkn = sale_info['datum']['fields'][1]['fields'][1]['bytes']
    bundle_amt = sale_info['datum']['fields'][1]['fields'][2]['int']
    wanted_bundle_size = queue_info['datum']['fields'][2]['int']
    current_bundle_amt = sale_info['value'][bundle_pid][bundle_tkn]
    if current_bundle_amt // bundle_amt < wanted_bundle_size:
        number_of_bundles = current_bundle_amt // bundle_amt 
    else:
        number_of_bundles = wanted_bundle_size

    incentive_data = queue_datum['fields'][3]['fields']
    
    bundle_value = {bundle_pid: {bundle_tkn: number_of_bundles * bundle_amt}}
    batcher_value = batcher_info['value']
    sale_value = sale_info['value']
    queue_value = queue_info['value']
    cost_value = parsing.cost_map_to_value_dict(sale_info['datum']['fields'][2], number_of_bundles)
    incentive_value = {incentive_data[0]: {incentive_data[1]: incentive_data[2]}}
    
    sv1 = dicts.add(sale_value, cost_value)
    sv2 = dicts.subtract(sv1, bundle_value)
    
    qv1 = dicts.subtract(queue_value, cost_value)
    qv2 = dicts.add(qv1, bundle_value)
    qv3 = dicts.subtract(qv2, incentive_value)
    qv4 = dicts.subtract(qv3, FEE_VALUE)
    
    bv1 = dicts.add(batcher_value, incentive_value)
    
    batcher_out = parsing.process_output(constants['batcher_address'], bv1)
    print(batcher_out)
    sale_out = parsing.process_output(constants['sale_address'], sv2)
    print(sale_out)
    queue_out = parsing.process_output(constants['queue_address'], qv4)
    print(queue_out)

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
    print("\nFUNC: ", func)
    subprocess.run(func, capture_output=True, text=True, check=True)
    