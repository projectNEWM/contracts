import json
from bf import utxo_slot

def open_file(file_path: str) -> dict:
    data = {}
    with open(file_path, 'r') as file:
        data = json.load(file)
    return data

def cat(file_path: str) -> str:
    with open(file_path, 'r') as file:
        return file.read().strip()

def get_orders(orders, sale_utxo_path, queue_utxo_path, pointer_pid_path):
    # utxo data
    sale_data = open_file(sale_utxo_path)
    queue_data = open_file(queue_utxo_path)
    
    # pointer pid
    pid = cat(pointer_pid_path)
    
    # check for cancels
    keys_to_delete = []
    for o in orders:
        if o not in queue_data:
            keys_to_delete.append(o)

    for key in keys_to_delete:
        del orders[key]
    
    # loop all the queue items
    for q in queue_data:
        if q in orders:
            continue

        # pointer tkn
        tkn = queue_data[q]['inlineDatum']['fields'][4]['bytes']
        
        # loop all the sales
        for s in sale_data:
            try:
                # are you holding the correct pointer?
                if sale_data[s]['value'][pid][tkn] == 1:
                    
                    # get the current slot that utxo was created at
                    slot = utxo_slot(q.split('#')[0])
                    
                    # add to the orders
                    orders[q] = {s:slot}
            except KeyError:
                # wrong pointer order, skip
                pass
    return orders

def update_orders(real, assumed):
    # check for cancels
    keys_to_delete = []
    for o in assumed:
        if o not in real:
            keys_to_delete.append(o)

    if len(keys_to_delete) != 0:
        print("Orders Have Been Removed")
    for key in keys_to_delete:
        del assumed[key]
    return assumed

def sort_orders(orders):
    return sorted(orders, key=lambda k: max(orders[k].values()))


if __name__ == "__main__":
    sale_utxo_path   = "../tmp/current_sale_utxos.json"
    queue_utxo_path  = "../tmp/current_queue_utxos.json"
    pointer_pid_path = "../../hashes/pointer_policy.hash"

    # current orders to be filled
    orders = get_orders(sale_utxo_path, queue_utxo_path, pointer_pid_path)
    # fifo ordering
    sorted_orders = sort_orders(orders)
    
    
    print(orders)
    print(sorted_orders)