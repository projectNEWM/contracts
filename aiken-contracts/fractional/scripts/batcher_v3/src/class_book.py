from src import (db_manager_redis, parsing, query, queue_purchase,
                 queue_refund, sorting, transaction, validate)


class Book:
    """Handle all order book logic."""

    @staticmethod
    def tx_input(db: db_manager_redis.DatabaseManager, data: dict, debug: bool = True) -> bool:
            # the tx hash of this transaction
        input_utxo = data['tx_input']['tx_id'] + '#' + str(data['tx_input']['index'])
        
        result = db.find_order_by_utxo(input_utxo)
        if result:
            if debug is True:
                print(f"Spent Order Book Input @ {input_utxo} @ Timestamp {data['context']['timestamp']}")
            db.delete_order_book_record(result[0])
            return True
            
        return False
    
    @staticmethod
    def tx_output(db: db_manager_redis.DatabaseManager, constants: dict, data: dict, debug: bool = True) -> bool:
        # do something here
        context = data['context']
        # timestamp for ordering, equal timestamps use the tx_idx to order
        timestamp = context['timestamp']
        tx_idx = context['tx_idx']
        
        output_utxo = context['tx_hash'] + '#' + str(context['output_idx'])
        utxo_base_64 = parsing.sha3_256(output_utxo)
        
        if data['tx_output']['address'] == constants['order_book_address']:
            order_book_datum = data['tx_output']['inline_datum']['plutus_data'] if data['tx_output']['inline_datum'] is not None else {}
            
            # convert to dict and add in lovelace
            lovelace = data['tx_output']['amount']
            value_obj = parsing.asset_list_to_dict(data['tx_output']['assets'])
            value_obj['lovelace'] = lovelace
            
            if debug is True:
                print(f"Order Book Output @ {output_utxo} @ Timestamp: {timestamp}")
            db.create_order_book_record(utxo_base_64, output_utxo, order_book_datum, value_obj, timestamp, tx_idx)
            return True
                
        return False
    
    @staticmethod
    def fifo_order(db: db_manager_redis.DatabaseManager):
        """Get all the orders in the order book. Find all pairs of orders that 
        fit some optimal criterion. Sort the order dictionary by timestamp and txidx.
        Return the sorted pair of orders.
        """
        # get all the queue items and sale items
        orders = db.read_all_order_book_records()
        return sorting.order_fifo_sort(orders)
    
    @staticmethod
    def fulfillment(db: db_manager_redis.DatabaseManager, sorted_order_to_order_dict: list, constants: dict, debug: bool = True) -> None:
        pass

