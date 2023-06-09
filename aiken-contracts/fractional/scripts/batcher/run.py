import orders
from delay import force_block_change
from query import utxo
from transaction import build
from parsing import compress_dicts, add_dicts, subtract_dicts, process_output, map_to_value, delete_zeros

# def batcher(sale_utxo_path, queue_utxo_path, pointer_pid_path):
#     print("Batching")
#     # current orders to be filled
#     order_book = orders.get_orders(sale_utxo_path, queue_utxo_path, pointer_pid_path)
#     # fifo ordering
#     sorted_order_book = orders.sort_orders(order_book)
    
#     # run batcher
#     for utxo in sorted_order_book:
#         print(f"\nProcessing {utxo}")
#         # print(f"CURRENT SALE UTXO {list(order_book[utxo].keys())[0]}")
#         # do the purchase
#         # do the refund
#         # update new sale utxo in order book

def run():
    root    = ".."
    cli     = "cardano-cli"
    network = "--testnet-magic 1"
    socket  = "/home/logic/Documents/Work/LogicalMechanism/full-node-wallet/node/db-testnet/node.socket"
    tmp = root+"/tmp/"
    
    sale_utxo_path="../tmp/current_sale_utxos.json"
    queue_utxo_path="../tmp/current_queue_utxos.json"
    batcher_utxo_path="../tmp/current_batcher_utxos.json"
    
    pointer_pid_path="../../hashes/pointer_policy.hash"
    
    sale_address = "addr_test1xr4udppuj0wqwt224qe4hcegfqfzs4tlmg4p72vun2nmhlynp7y0mz2xgxfld6ettk7q2nas9hu7u32jchmjnp5spwvq0nezhj"
    queue_address = "addr_test1xr4j7glfh6w0pk5m67ps3ukx3e4fumwte3jktd5385z2f35np7y0mz2xgxfld6ettk7q2nas9hu7u32jchmjnp5spwvqq6rpwt"
    batcher_address = "addr_test1vrs4fk7ea6rg2fvd00sa8um5unp0rt474kngwpc38v2z9vqujprdk"
    
    # current state
    order_book = {}
    
    # things that just got spent
    spent_order_book = {}
    
    # batcher state
    utxo(cli, socket, network, batcher_address, batcher_utxo_path)
    batcher_data = orders.open_file(batcher_utxo_path)
    
    batcher_tx_in = list(batcher_data.keys())[0]
    for o in batcher_data:
        batcher_value = compress_dicts(batcher_data[o]['value'], {})
    
    # start it
    while True:
        print('\nWait For Next Block')
        force_block_change(socket)
        
        print("Find Current State")
        utxo(cli, socket, network, sale_address, sale_utxo_path)
        utxo(cli, socket, network, queue_address, queue_utxo_path)
        
        # current orders wrt the tip
        order_book = orders.get_orders(order_book, sale_utxo_path, queue_utxo_path, pointer_pid_path)
        # fifo
        sorted_order_book = orders.sort_orders(order_book)
        
        for o in sorted_order_book:
            if o in spent_order_book:
                continue
            
            print(f"\nProcessing {o}")
            # process the tx here
            queue_data = orders.open_file(queue_utxo_path)
            queue_value = compress_dicts(queue_data[o]['value'], {})
            
            inval = queue_data[o]['inlineDatum']['fields'][3]['fields']
            bundle_size = int(queue_data[o]['inlineDatum']['fields'][2]['int'])
            incentive = {inval[0]['bytes']:{inval[1]['bytes']:inval[2]['int']}}
            b_out_val = add_dicts(batcher_value, incentive)
            batcher_value = b_out_val
            
            sale_data = orders.open_file(sale_utxo_path)
            sale_utxo = list(order_book[o].keys())[0]
            sale_value = compress_dicts(sale_data[sale_utxo]['value'], {})
            
            bundle = sale_data[sale_utxo]['inlineDatum']['fields'][1]['fields']
            bundle_value = {bundle[0]['bytes']:{bundle[1]['bytes']: bundle_size * bundle[2]['int']}}
            
            
            price = sale_data[sale_utxo]['inlineDatum']['fields'][2]
            print(price)
            price_value = map_to_value(price, bundle_size)
            
            q_out_val = add_dicts(subtract_dicts(subtract_dicts(queue_value, incentive), price_value), bundle_value)
            q_out_val = delete_zeros(q_out_val)
            s_out_val = add_dicts(subtract_dicts(sale_value, bundle_value), price_value)
            s_out_val = delete_zeros(s_out_val)
            
            # add incentive to the batcher output
            batcher_out = process_output(batcher_address, b_out_val)
            
            # add the bundle to the queue output
            queue_out = process_output(queue_address, q_out_val)
            
            # add the profit to the sale outout
            sale_out = process_output(sale_address, s_out_val)
            
            # build the purchase
            build(batcher_tx_in, sale_utxo, o, batcher_out, sale_out, queue_out)
            
            # build the refund
            
            # sign
            
            # submit
            
            # add utxo to the spent dict
            spent_order_book[o] = order_book[o]
        
        # remove any processed orders
        spent_order_book = orders.update_orders(order_book, spent_order_book)
        
        
        
        # current state of chain
        # assume state of the chain

if __name__ == "__main__":
    
    
    try:
        run()
    except KeyboardInterrupt:
        print('exiting')
        exit()