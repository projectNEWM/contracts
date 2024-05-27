import logging
import multiprocessing
import os
import subprocess

from flask import Flask, request
from loguru import logger
from src import daemon, db_manager_sqlite3, query, yaml_file
from src.batcher_wallet import Batcher
from src.fulfillment import Fulfillment
from src.queue_contract import Queue
from src.sale_contract import Sale
from src.sorting import Sorting

# # load the environment constants
constants = yaml_file.read("constants.yaml")

# start the sqlite3 database
db = db_manager_sqlite3.DatabaseManagerSQLite()
db.init_status_record(constants)

# initial flask
app = Flask(__name__)

# Get the directory of the currently executing script
script_directory = os.path.dirname(os.path.abspath(__file__))
log_file_path = os.path.join(script_directory, 'app.log')

# Configure log rotation with a maximum file size and retention
logger.add(log_file_path,
           rotation=constants["max_log_file_size"], retention=constants["max_num_log_files"])

# Disable Flask's default logger
log = logging.getLogger('werkzeug')
log.setLevel(logging.ERROR)

# # Use the local/remote socket to get the latest block
latest_block_number = query.get_latest_block_number(
    constants['socket_path'], 'tmp/tip.json', constants['network'], logger)


@app.route('/webhook', methods=['POST'])
def webhook():
    """The webhook for oura. This is where all the db logic needs to go.

    Returns:
        str: A success/failure string
    """
    data = request.get_json()  # Get the JSON data from the request
    block_number = data['context']['block_number']
    block_hash = data['context']['block_hash']
    block_slot = data['context']['slot']

    # Get the current status
    sync_status = db.read_status_record()

    # What the db things is the current block
    db_number = sync_status["block_number"]

    # check for a change in the block number
    if block_number == db_number:
        # we are still parsing data from the block
        pass
    else:
        db.update_status_record(block_number, block_hash, block_slot)
        try:
            if int(block_number) > latest_block_number:
                # log the block
                logger.info(f"Block: {block_number}")
                # sort the queue
                sorted_queue_orders = Sorting.fifo(db)
                # fulfill the orders
                Fulfillment.orders(db, sorted_queue_orders, constants, logger)
            else:
                logger.debug(
                    f"Blocks til tip: {latest_block_number - int(block_number)}")
        except TypeError:
            pass

    # now lets try to handle the parsing of the data
    try:
        variant = data['variant']

        # if a rollback occurs we need to handle it
        if variant == 'RollBack':
            # how do we handle it?
            logger.critical("ROLLBACK")

        # tx inputs
        if variant == 'TxInput':
            Batcher.tx_input(db, data, logger)
            Sale.tx_input(db, data, logger)
            Queue.tx_input(db, data, logger)
            # pass

        # tx outputs
        if variant == 'TxOutput':
            Batcher.tx_output(db, constants, data, logger)
            Sale.tx_output(db, constants, data, logger)
            Queue.tx_output(db, constants, data, logger)
            # pass

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
            logger.error("Oura Not Found On System")
            exit(1)
    subprocess.run([program_path, 'daemon', '--config', 'daemon.toml'])


def start_processes():
    # create the daemon toml file
    sync_status = db.read_status_record()
    # start log
    logger.info(f"Starting Block {sync_status['block_number']} @ Slot {sync_status['timestamp']} With Hash {sync_status['block_hash']}")

    daemon.create_toml_file(
        'daemon.toml', constants['socket_path'], sync_status['timestamp'], sync_status['block_hash'], constants['delay_depth'])

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
        logger.error("KeyboardInterrupt detected, terminating processes...")
        # clean up the db
        db.cleanup()
        # terminate and join
        flask_proc.terminate()
        daemon_proc.terminate()
        flask_proc.join()
        daemon_proc.join()


if __name__ == '__main__':
    start_processes()
    pass
