import logging
import multiprocessing
import os
import subprocess

from flask import Flask, request
from loguru import logger
from src import db_manager_redis, json_file, query
from src.batcher import Batcher
from src.fulfillment import Fulfillment
from src.queue import Queue
from src.sale import Sale
from src.sorting import Sorting

# from src.class_handle import Handle

# start the redis database
db = db_manager_redis.DatabaseManager()

# # Clear the database
db.clear_database()

# initial flask
app = Flask(__name__)

# Get the directory of the currently executing script
script_directory = os.path.dirname(os.path.abspath(__file__))
log_file_path = os.path.join(script_directory, 'app.log')

# Configure log rotation with a maximum file size and retention
logger.add(log_file_path, rotation="1 MB", retention=1)

# # load the environment constants
constants = json_file.read("constants.json")

# Disable Flask's default logger
log = logging.getLogger('werkzeug')
log.setLevel(logging.ERROR)

UTXO_DEBUG = True
SUBMIT_DEBUG = True

# # Use the local/remote socket to get the latest block
latest_block_number = query.get_latest_block_number(constants['socket_path'], 'tmp/tip.json', constants['network'])


@app.route('/webhook', methods=['POST'])
def webhook():
    """The webhook for oura. This is where all the db logic needs to go.

    Returns:
        str: A success/failure string
    """
    data = request.get_json()  # Get the JSON data from the request
    block_number = data['context']['block_number']
    db_number = db.read_block_record()
    # check for a change in the block number
    if db_number is None or db_number == 0:
        # node just started, let it sync
        db.create_block_record(block_number)
        return 'Webhook received successfully'
    elif block_number == db_number:
        # we are still parsing data from the block
        pass
    else:
        db.create_block_record(block_number)
        if int(block_number) > latest_block_number:
            print(f"\nBlock Number: {int(block_number) }")
            sorted_queue_orders = Sorting.fifo(db)
            Fulfillment.orders(db, sorted_queue_orders, constants, SUBMIT_DEBUG)
        else:
            print(f"Blocks Left To Sync: {latest_block_number - int(block_number)}")
        # get all the queue items and sale items in fifo order

    # now lets try to handle the parsing of the data
    try:
        variant = data['variant']

        # if a rollback occurs we need to handle it
        if variant == 'RollBack':
            print("ROLLBACK")

        # tx inputs
        if variant == 'TxInput':
            Batcher.tx_input(db, data, UTXO_DEBUG)
            Sale.tx_input(db, data, UTXO_DEBUG)
            Queue.tx_input(db, data, UTXO_DEBUG)

        # tx outputs
        if variant == 'TxOutput':
            Batcher.tx_output(db, constants, data, UTXO_DEBUG)
            Sale.tx_output(db, constants, data, UTXO_DEBUG)
            Queue.tx_output(db, constants, data, UTXO_DEBUG)

    except Exception:
        return 'Webhook deserialization failure'

    # its all good
    return 'Webhook Successful'


def flask_process(start_event):
    start_event.wait()  # Wait until the start event is set
    app.run(host='0.0.0.0', port=8008)


def run_daemon():
    program_name = "oura"
    # The base directory where user home directories are typically located
    search_directory = "/home"

    # List all subdirectories within the search directory
    user_directories = [
        os.path.join(search_directory, user)
        for user in os.listdir(search_directory)
        if os.path.isdir(os.path.join(search_directory, user))]

    # Iterate through user home directories and check if the program exists
    program_path = ''
    for user_directory in user_directories:
        program_path = os.path.join(
            user_directory, ".cargo", "bin", program_name)
        if os.path.exists(program_path):
            break
        else:
            print('Oura Not Found On System')
            exit(1)
    subprocess.run([program_path, 'daemon', '--config', 'daemon.toml'])


def start_processes():

    # start the processes as events in order
    start_event = multiprocessing.Event()

    # start the webhook
    flask_proc = multiprocessing.Process(
        target=flask_process, args=(start_event,))
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
