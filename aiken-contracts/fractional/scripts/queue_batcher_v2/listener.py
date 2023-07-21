import logging
from flask import Flask, request
import multiprocessing
import subprocess
import json
import base64
from src import parsing, db_manager_redis, handle

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
    # match the queue items with sale items
    # fifo the queue list per each sale
    # do the tx stuff
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

    # Start the Flask app process
    flask_proc = multiprocessing.Process(target=flask_process, args=(start_event,))
    flask_proc.start()

    # Start the daemon process
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
