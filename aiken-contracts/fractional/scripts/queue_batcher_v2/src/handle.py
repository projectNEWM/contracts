from src import parsing, db_manager_redis, sorting, validate, purchase, refund, transaction
import os

def create_folder_if_not_exists(folder_path: str) -> None:
    if not os.path.exists(folder_path):
        os.makedirs(folder_path)

def tx_input(db: db_manager_redis.DatabaseManager, data: dict, debug:bool = True) -> bool:
    # the tx hash of this transaction
    input_utxo = data['tx_input']['tx_id'] + '#' + str(data['tx_input']['index'])
    
    result = db.find_sale_by_utxo(input_utxo)
    if result:
        if debug is True:
            print(f"Spent Sale Input @ {input_utxo} @ Timestamp {data['context']['timestamp']}")
        db.delete_sale_record(result[0])
        return True
        
    
    utxo_base_64 = parsing.sha3_256(input_utxo)
    if db.is_key_in_queue(utxo_base_64):
        if debug is True:
            print(f"Spent Queue Input @ {input_utxo} @ Timestamp {data['context']['timestamp']}")
        db.delete_queue_record(utxo_base_64)
        return True
        
    
    if db.is_key_in_batcher(utxo_base_64):
        if debug is True:
            print(f"Batcher Input @ {input_utxo} @ Timestamp {data['context']['timestamp']}")
        db.delete_batcher_record(utxo_base_64)
        return True
    return False
        

def tx_output(db: db_manager_redis.DatabaseManager, constants: dict, data: dict, debug:bool = True) -> bool:
    # do something here
    context = data['context']
    # timestamp for ordering, equal timestamps use the tx_idx to order
    timestamp = context['timestamp']
    tx_idx = context['tx_idx']
    
    output_utxo = context['tx_hash'] + '#' + str(context['output_idx'])
    utxo_base_64 = parsing.sha3_256(output_utxo)
    
    if data['tx_output']['address'] == constants['batcher_address']:
        # update the batcher information
        lovelace = data['tx_output']['amount']
        value_obj = parsing.asset_list_to_dict(data['tx_output']['assets'])
        value_obj['lovelace'] = lovelace
        
        if debug is True:
            print(f"Batcher Output @ {output_utxo} @ Timestamp: {timestamp}")
        db.create_batcher_record(utxo_base_64, output_utxo, value_obj)
        return True
    
    
    if data['tx_output']['address'] == constants['queue_address']:
        queue_datum = data['tx_output']['inline_datum']['plutus_data'] if data['tx_output']['inline_datum'] is not None else {}
        tkn = queue_datum['fields'][4]['bytes']
        # convert to dict and add in lovelace
        lovelace = data['tx_output']['amount']
        value_obj = parsing.asset_list_to_dict(data['tx_output']['assets'])
        value_obj['lovelace'] = lovelace
        
        if debug is True:
            print(f"Queue Output @ {output_utxo} @ Timestamp: {timestamp}")
        db.create_queue_record(utxo_base_64, output_utxo, tkn, queue_datum, value_obj, timestamp, tx_idx)
        return True
        
        
    
    if data['tx_output']['address'] == constants['sale_address']:
        sale_datum = data['tx_output']['inline_datum']['plutus_data'] if data['tx_output']['inline_datum'] is not None else {}
        
        # convert to dict and add in lovelace
        lovelace = data['tx_output']['amount']
        value_obj = parsing.asset_list_to_dict(data['tx_output']['assets'])
        value_obj['lovelace'] = lovelace
        
        if parsing.key_exists_in_dict(value_obj, constants['pointer_policy']):
            if debug is True:
                print(f"Sale Output @ {output_utxo} @ Timestamp: {timestamp}")
            
            tkn = list(value_obj[constants['pointer_policy']].keys())[0]
            db.create_sale_record(tkn, output_utxo, sale_datum, value_obj)
            return True
    return False
    
            

def rollback(data:dict) -> bool:
    """TODO"""
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
    return False

def fifo_order(db: db_manager_redis.DatabaseManager) -> dict:
    # get all the queue items and sale items
    sales = db.read_all_sale_records()
    orders = db.read_all_queue_records()
    
    sale_to_order_dict = {}
    
    # there is at least one sale and one order
    if len(sales) >= 1 and len(orders) >= 1:
        # loop sales and loop orders and build out the dictionary
        for sale in sales:
            pointer_token = sale[0]
            sale_to_order_dict[pointer_token] = []
            
            for order in orders:
                order_hash = order[0]
                order_data = order[1]
                tkn = order_data['tkn']
                
                # find an order that points to a sale
                if pointer_token == tkn:
                    # timestap is primary sort var but tx_idx should be use to handle equal timestamps
                    sale_to_order_dict[pointer_token].append((order_hash, order_data['timestamp'], order_data['tx_idx']))
    
    # fifo the queue list per each sale
    return sorting.fifo(sale_to_order_dict)

def order_fulfillment(db: db_manager_redis.DatabaseManager, sorted_sale_to_order_dict: dict, constants: dict) -> None:
    # loop the sorted sales and start batching
    
    # there needs to be at least a single batcher record
    try:
        batcher_info = db.read_all_batcher_records()[0][1]
    except IndexError:
        return
    
    this_dir = os.path.dirname(os.path.abspath(__file__))
    parent_dir = os.path.dirname(this_dir)
    out_file_path = os.path.join(parent_dir, "tmp/tx.draft")
    signed_purchase_tx = os.path.join(parent_dir, "tmp/purchase-tx.signed")
    signed_refund_tx = os.path.join(parent_dir, "tmp/refund-tx.signed")
    batcher_skey_path = os.path.join(parent_dir, "key/batcher.skey")
    collat_skey_path = os.path.join(parent_dir, "key/collat.skey")
    
    for sale in sorted_sale_to_order_dict:
        sale_info = db.read_sale_record(sale)
        sale_orders = sorted_sale_to_order_dict[sale]
        for order in sale_orders:
            order_hash = order[0]
            order_info = db.read_queue_record(order_hash)
            # merklize the sale and order
            tag = parsing.sha3_256(parsing.sha3_256(str(sale)) + parsing.sha3_256(str(order_info)))
            # check if the tag has been seen before
            if db.read_seen_record(tag) is True:
                # lets continue to the next order if we have seen it
                continue
            # check if this sale-order combo hash been seen
            # check the order info for current state
            state = validate.utxo(sale_info, order_info)
            if state is None:
                print("BAD STATE")
                # can not refund nor purchase
                # user must cancel order manually
                continue
            elif state is True:
                # build the purchase tx
                sale_info, order_info, batcher_info = purchase.build_tx(sale_info, order_info, batcher_info, constants)
                # sign tx
                transaction.sign(out_file_path, signed_purchase_tx, constants['network'], batcher_skey_path, collat_skey_path)
                
                # build the refund tx
                refund.build_tx(sale_info, order_info, batcher_info, constants)
                # sign tx
                transaction.sign(out_file_path, signed_refund_tx, constants['network'], batcher_skey_path, collat_skey_path)
                
                # submit tx
                transaction.submit(signed_purchase_tx, constants['socket_path'], constants['network'])
                db.create_seen_record(tag)
                transaction.submit(signed_refund_tx, constants['socket_path'], constants['network'])
                tag = parsing.sha3_256(parsing.sha3_256(str(sale)) + parsing.sha3_256(str(order_info)))
                db.create_seen_record(tag)
            else:
                # print("NEEDS TO BE REFUNDED STATE")
                
                # # build the refund tx
                refund.build_tx(sale_info, order_info, batcher_info, constants)
                # # sign tx
                transaction.sign(out_file_path, signed_refund_tx, constants['network'], batcher_skey_path, collat_skey_path)
                # # submit refund
                transaction.submit(signed_refund_tx, constants['socket_path'], constants['network'])
                db.create_seen_record(tag)
                
                