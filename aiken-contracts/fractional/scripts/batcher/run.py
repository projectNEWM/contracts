import orders
def batcher(sale_utxo_path, queue_utxo_path, pointer_pid_path):
    print("STARTING BATCHER")
    # current orders to be filled
    order_book = orders.get_orders(sale_utxo_path, queue_utxo_path, pointer_pid_path)
    # fifo ordering
    sorted_order_book = orders.sort_orders(order_book)
    
    
    print(order_book)
    print(sorted_order_book)
    
    for utxo in sorted_order_book:
        print(f"PROCESSING {utxo}")
        print(f"CURRENT SALE UTXO {list(order_book[utxo].keys())[0]}")
        # do the purchase
        # do the refund
        # update new sale utxo in order book