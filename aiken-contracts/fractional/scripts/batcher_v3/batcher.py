import logging
import multiprocessing
import subprocess

from flask import Flask, request
from src import db_manager_redis, json_file, query
from src.class_handle import Handle

# start the redis database
db = db_manager_redis.DatabaseManager()

# # Clear the database
db.clear_database()

# # load the environment constants
constants = json_file.read("env.json")

# # Disable Flask's default logger
log = logging.getLogger('werkzeug')
log.setLevel(logging.ERROR)

# initial flask
app = Flask(__name__)

# # Use the local/remote socket to get the latest block
latest_block_number = query.get_latest_block_number(constants['socket_path'], 'tmp/tip.json', constants['network'])

# This probably should be in the env file or something
UTXO_DEBUG = True
SUBMIT_DEBUG = False

@app.route('/webhook', methods=['POST'])
def webhook():
    """
        The webhook for oura. This is where all the db logic and batcher logic
    needs to go.

    Returns:
        str: A success string.
    """
    data = request.get_json()  # Get the JSON data from the request
    
    # get all the queue items and sale items in fifo order
    sorted_queue_orders = Handle.Queue.fifo_order(db)
    sorted_book_orders = Handle.Book.fifo_order(db)
    
    block_number = data['context']['block_number']
    db_number = db.read_block_record()
    
    # check for a change in the block number
    if db_number == 0:
        # node just started, let it sync
        db.create_block_record(block_number)
        return 'Webhook received successfully'
    elif block_number == db_number:
        # we are still parsing data from the block
        pass
    else:
        db.create_block_record(block_number)
        if block_number is not None:
            if int(block_number) > latest_block_number:
                print(f"\nBlock Number: {int(block_number) }")
                Handle.Queue.fulfillment(db, sorted_queue_orders, constants, SUBMIT_DEBUG)
                Handle.Book.fulfillment(db, sorted_book_orders, constants, SUBMIT_DEBUG)
            else:
                print(f"Blocks Left To Sync: {latest_block_number - int(block_number) }")
    
    # now lets try to handle the parsing of the data
    try:
        variant = data['variant']
        
        # if a rollback occurs we need to handle it
        if variant == 'RollBack':
            Handle.rollback(data, UTXO_DEBUG)
        
        # tx inputs
        if variant == 'TxInput':
            Handle.Batcher.tx_input(db, data, UTXO_DEBUG)
            Handle.Sale.tx_input(db, data, UTXO_DEBUG)
            Handle.Queue.tx_input(db, data, UTXO_DEBUG)
            Handle.Book.tx_input(db, data, UTXO_DEBUG)
        
        # tx outputs
        if variant == 'TxOutput':
            Handle.Batcher.tx_output(db, constants, data, UTXO_DEBUG)
            Handle.Sale.tx_output(db, constants, data, UTXO_DEBUG)
            Handle.Queue.tx_output(db, constants, data, UTXO_DEBUG)
            Handle.Book.tx_output(db, constants, data, UTXO_DEBUG)
    except Exception:
        return 'Webhook deserialization failure'
    
    # all good
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
        # terminate and join
        flask_proc.terminate()
        daemon_proc.terminate()
        flask_proc.join()
        daemon_proc.join()

if __name__ == '__main__':
    start_processes()
