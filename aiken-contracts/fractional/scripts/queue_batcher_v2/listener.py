import logging
from flask import Flask, request
import multiprocessing
import subprocess
import json
from src import db_manager_redis, handle, sorting

db = db_manager_redis.DatabaseManager()

# Clear the database
db.clear_database()

# the batcher uses constants defined in the env file
def load_constants():
    with open('env.json', 'r') as f:
        constants = json.load(f)
    return constants

constants = load_constants()

# Disable Flask's default logger
log = logging.getLogger('werkzeug')
log.setLevel(logging.ERROR)

app = Flask(__name__)

@app.route('/webhook', methods=['POST'])
def webhook():
    """The webhook for oura. This is where all the db logic and batcher logic
    needs to go.

    Returns:
        str: A success string
    """
    data = request.get_json()  # Get the JSON data from the request

    try:
        variant = data['variant']
        
        # if a rollback occurs we need to handle it
        if variant == 'RollBack':
            handle.rollback(data)
        
        # tx outputs
        if variant == 'TxOutput':
            handle.tx_output(db, constants, data)
        
        # tx inputs
        if variant == 'TxInput':
            handle.tx_input(db, data)
        
        # there may other things to dump
    except Exception as e:
        return 'Webhook deserialization failure'
    
    # do the orders here
    # get all the queue items and sale items
    sales = db.read_all_sale_records()
    orders = db.read_all_queue_records()
    
    # there is at least one sale and one order
    if len(sales) >= 1 and len(orders) >= 1:
        
        # match the queue items with sale items
        sale_to_order_dict = {}
        
        # loop sales and loop orders and build out the dictionary
        for sale in sales:
            # print('SALE:', sale)
            pointer_token = sale[0]
            sale_data = sale[1]
            sale_to_order_dict[pointer_token] = []
            
            for order in orders:
                order_hash = order[0]
                order_data = order[1]
                tkn = order_data['tkn']
                
                if pointer_token == tkn:
                    sale_to_order_dict[pointer_token].append((order_hash, order_data['timestamp'], order_data['tx_idx']))
                    # print(f"\nThe sale utxo: {sale_data['txid']}")
                    # print(f"The queue utxo: {order_data['txid']}")

    
        # fifo the queue list per each sale
        sorted_sale_to_order_dict = sorting.fifo(sale_to_order_dict)
        
        # loop the sorted sales and start batching
        for sale in sorted_sale_to_order_dict:
            sale_info = db.read_sale_record(sale)
            sale_orders = sorted_sale_to_order_dict[sale]
            for order in sale_orders:
                order_hash = order[0]
                order_info = db.read_queue_record(order_hash)
                # this is a sale to complete
                print(f'\nsale info: {sale_info}')
                print(f'order info: {order_info}')
                
                # do the tx stuff here
                

    # assume success and keep trying until it leaves the db
    # potentially may need intermediate db that tracks mempool
    
    # 
    return 'Webhook received successfully'

def flask_process(start_event):
    start_event.wait()  # Wait until the start event is set
    app.run(host='0.0.0.0', port=8008)

def run_daemon():
    subprocess.run(['oura', 'daemon', '--config', 'daemon.toml'])

def start_processes():
    start_event = multiprocessing.Event()

    # start the webhook
    flask_proc = multiprocessing.Process(target=flask_process, args=(start_event,))
    flask_proc.start()

    # start oura daemon
    daemon_proc = multiprocessing.Process(target=run_daemon)
    daemon_proc.start()

    # Set the start event to indicate that the Flask app is ready to run
    start_event.set()
    try:
        # Wait for both processes to complete
        flask_proc.join()
        daemon_proc.join()
    except KeyboardInterrupt:
        # Handle KeyboardInterrupt (CTRL+C)
        print("\nKeyboardInterrupt detected, terminating processes...")
        print(db.read_all_queue_records())
        print(db.read_all_sale_records())
        flask_proc.terminate()
        daemon_proc.terminate()
        flask_proc.join()
        daemon_proc.join()

if __name__ == '__main__':
    start_processes()
