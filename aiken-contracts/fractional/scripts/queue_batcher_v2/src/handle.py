from src import parsing

def tx_input(db, data):
    context = data['context']
    block_hash = context['block_hash']
    slot = context['slot']
    timestamp = context['timestamp']
    
    # the tx hash of this transaction
    input_utxo = data['tx_input']['tx_id'] + '#' + str(data['tx_input']['index'])
    
    result = db.find_sale_by_utxo(input_utxo)
    if result:
        db.delete_sale_record(result[0])
    
    # utxo_base_64 = base64.urlsafe_b64encode(output_utxo.encode('utf-8')).decode('utf-8')
    utxo_base_64 = parsing.sha3_256(input_utxo)
    if db.is_key_in_queue(utxo_base_64):
        db.delete_queue_record(utxo_base_64)

def tx_output(db, constants, data):
    # print(f"Block Number: {data['context']['block_number']}")
    # do something here
    context = data['context']
    # timestamp for ordering, equal timestamps use the tx_idx to order
    timestamp = context['timestamp']
    tx_idx = context['tx_idx']
    
    output_utxo = context['tx_hash'] + '#' + str(context['output_idx'])
    # utxo_base_64 = base64.urlsafe_b64encode(output_utxo.encode('utf-8')).decode('utf-8')
    utxo_base_64 = parsing.sha3_256(output_utxo)
    
    if data['tx_output']['address'] == constants['queue_address']:
        queue_datum = data['tx_output']['inline_datum']['plutus_data'] if data['tx_output']['inline_datum'] is not None else {}
        tkn = queue_datum['fields'][4]['bytes']
        # convert to dict and add in lovelace
        lovelace = data['tx_output']['amount']
        value_obj = parsing.asset_list_to_dict(data['tx_output']['assets'])
        value_obj['lovelace'] = lovelace
        
        print(f"Queue Output Inside UTxO: {output_utxo} At Timestamp: {timestamp}")
        db.create_queue_record(utxo_base_64, output_utxo, tkn, queue_datum, value_obj, timestamp, tx_idx)
        
    
    if data['tx_output']['address'] == constants['sale_address']:
        sale_datum = data['tx_output']['inline_datum']['plutus_data'] if data['tx_output']['inline_datum'] is not None else {}
        
        # convert to dict and add in lovelace
        lovelace = data['tx_output']['amount']
        value_obj = parsing.asset_list_to_dict(data['tx_output']['assets'])
        value_obj['lovelace'] = lovelace
        
        if parsing.key_exists_in_dict(value_obj, constants['pointer_policy']):
            tkn = list(value_obj[constants['pointer_policy']].keys())[0]
            print(f"Sale Output Inside Block: {output_utxo} At Timestamp: {timestamp}")
            db.create_sale_record(tkn, output_utxo, sale_datum, value_obj)

def rollback(data):
    # do something here
    context = data['context']
    block_hash = context['block_hash']
    slot = context['slot']
    timestamp = context['timestamp']
    
    # if a rollback occurs we need to handle it
    if block_hash is not None:
        print("\nROLLBACK")
        print("DO SOME THING HERE\n")
        print(data)
        exit(1)