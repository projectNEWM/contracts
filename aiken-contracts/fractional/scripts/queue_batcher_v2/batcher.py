import logging
from flask import Flask, request
import multiprocessing
import subprocess
from src import db_manager_redis, handle, json_file

# start the redis database
db = db_manager_redis.DatabaseManager()

# Clear the database
db.clear_database()

# load the environment constants
constants = json_file.read("env.json")

# Disable Flask's default logger
log = logging.getLogger('werkzeug')
log.setLevel(logging.ERROR)

# initial flask
app = Flask(__name__)

@app.route('/webhook', methods=['POST'])
def webhook():
    """The webhook for oura. This is where all the db logic and batcher logic
    needs to go.

    Returns:
        str: A success string
    """
    data = request.get_json()  # Get the JSON data from the request
    something_happened_flag = False
    try:
        variant = data['variant']
        # print(data['context']['block_number'])
        # if a rollback occurs we need to handle it
        if variant == 'RollBack':
            something_happened_flag = handle.rollback(data)
        
        # tx outputs
        if variant == 'TxOutput':
            something_happened_flag = handle.tx_output(db, constants, data)
        
        # tx inputs
        if variant == 'TxInput':
            something_happened_flag = handle.tx_input(db, data)
        
        # there may other things to dump
    except Exception as e:
        return 'Webhook deserialization failure'
    
    if something_happened_flag is False:
        return 'Webhook received successfully'
    
    # get all the queue items and sale items in fifo order
    sorted_sale_to_order_dict = handle.fifo_order(db)
        
    # loop the sorted sales and start batching
    handle.order_fulfillment(db, sorted_sale_to_order_dict, constants)
    
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
        print(db.read_all_batcher_records())
        flask_proc.terminate()
        daemon_proc.terminate()
        flask_proc.join()
        daemon_proc.join()

if __name__ == '__main__':
    start_processes()
