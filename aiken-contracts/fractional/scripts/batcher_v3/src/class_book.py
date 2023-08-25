import math
import os
from fractions import Fraction

from src import (db_manager_redis, dicts, parsing, query, queue_purchase,
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
    def fulfillment(db: db_manager_redis.DatabaseManager, sorted_order_to_order_list: list, constants: dict, debug: bool = True) -> None:
        # there needs to be at least a single batcher record
        try:
            batcher_info = db.read_all_batcher_records()[0][1]
        except IndexError:
            return
        
        batcher_txid = batcher_info['txid']
        batcher_value = batcher_info['value']
        batcher_address = constants['batcher_address']
        
        print()
        print(batcher_txid)
        print(batcher_value)
        print(batcher_address)
        
        this_dir = os.path.dirname(os.path.abspath(__file__))
        parent_dir = os.path.dirname(this_dir)
        out_file_path = os.path.join(parent_dir, "tmp/tx.draft")
        mempool_file_path = os.path.join(parent_dir, "tmp/mempool.json")
        signed_purchase_tx = os.path.join(parent_dir, "tmp/book-purchase-tx.signed")
        signed_refund_tx = os.path.join(parent_dir, "tmp/book-refund-tx.signed")
        batcher_skey_path = os.path.join(parent_dir, "key/batcher.skey")
        collat_skey_path = os.path.join(parent_dir, "key/collat.skey")
        

        order_book_execution_unts="(950000000, 3000000)"
        order_fee = {"":{"":720588//2}}
        
        print(f"Assumed Fee Per Order: {order_fee}")
        
        order_book_address = constants["order_book_address"]
        print()
        print(order_book_address)
        for order in sorted_order_to_order_list:
            o1 = db.read_order_book_record(order[0])
            if o1 is None:
                continue
            
            txid1 = o1['txid']
            datum1 = o1['datum']
            
            price1 = datum1['fields'][2]['fields'][1]
            slip1 = datum1['fields'][2]['fields'][2]
            p1 = Fraction(price1['fields'][0]['int'], price1['fields'][1]['int'])
            s1 = Fraction(slip1['fields'][0]['int'], slip1['fields'][1]['int'])
            
            ipid1 = datum1['fields'][3]['fields'][0]['bytes']
            itkn1 = datum1['fields'][3]['fields'][1]['bytes']
            iamt1 = datum1['fields'][3]['fields'][2]['int']
            i1 = {ipid1: {itkn1: iamt1}}
            print(f"Incentive 1: {iamt1} {ipid1}.{itkn1}")
            
            value1 = o1['value']
            
            v11 = dicts.subtract(value1, i1)
            

            print()
            print(txid1)
            print(datum1)
            print(value1)
            print(p1,s1)
            
            o2 = db.read_order_book_record(order[1])
            if o2 is None:
                continue
            
            txid2 = o2['txid']
            datum2 = o2['datum']
            
            price2 = datum2['fields'][2]['fields'][1]
            slip2 = datum2['fields'][2]['fields'][2]
            p2 = Fraction(price2['fields'][0]['int'], price2['fields'][1]['int'])
            s2 = Fraction(slip2['fields'][0]['int'], slip2['fields'][1]['int'])
            
            ipid2 = datum2['fields'][3]['fields'][0]['bytes']
            itkn2 = datum2['fields'][3]['fields'][1]['bytes']
            iamt2 = datum2['fields'][3]['fields'][2]['int']
            i2 = {ipid2: {itkn2: iamt2}}
            print(f"Incentive 2: {iamt2} {ipid2}.{itkn2}")
            
            value2 = o2['value']
            
            v21 = dicts.subtract(value2, i2)
            
            print()
            print(txid2)
            print(datum2)
            print(value2)
            print(p2,s2)
            
            total_incentive = dicts.add(i1, i2)
            print("total incentive:", total_incentive)
            
            # check if the minimum threshold will be met for the potential swap
            geometric_mean_price1 = Fraction(
                int(math.sqrt(p1.numerator * p2.denominator)),
                int(math.sqrt(p1.denominator * p2.numerator))
            )
            geometric_mean_price2 = 1/geometric_mean_price1
            
            # check if the pair works
            have1 = datum1['fields'][1]
            have2 = datum2['fields'][1]
            
            # the prices exist in the correct range
            try:
                amt1 = Fraction(value1[have1['fields'][0]['bytes']][have1['fields'][1]['bytes']])
                print(f"Order 1 Has {amt1} {have1['fields'][0]['bytes']}.{have1['fields'][1]['bytes']}")
                amt2 = Fraction(value2[have2['fields'][0]['bytes']][have2['fields'][1]['bytes']])
                print(f"Order 2 Has {amt2} {have2['fields'][0]['bytes']}.{have2['fields'][1]['bytes']}")
                
            except KeyError:
                continue
            
            get1 = int(amt1*geometric_mean_price2)
            get2 = int(amt2*geometric_mean_price1)
            
            if amt1 - get2 >= Fraction(0):
                getting2 = get2
            else:
                getting2 = amt1
                
            if amt2 - get1 >= Fraction(0):
                getting1 = get1
            else:
                getting1 = amt2
            
            print(f"Order 1 Gets {getting1} {have2['fields'][0]['bytes']}.{have2['fields'][1]['bytes']}")
            print(f"Order 2 Gets {getting2} {have1['fields'][0]['bytes']}.{have1['fields'][1]['bytes']}")
            
            tag = parsing.sha3_256(parsing.sha3_256(str(order[0])) + parsing.sha3_256(str(order[1])))
            print(tag)
            
            # so try to build out the 