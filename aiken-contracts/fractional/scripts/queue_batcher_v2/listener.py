import logging
from flask import Flask, request
import multiprocessing
import subprocess
import json
from src import parsing, db_manager

db = db_manager.DatabaseManager("batcher.db")

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
    
    # Process and handle the data as per your requirements
    try:
        # do something here
        variant = data['variant']
        context = data['context']
        block_hash = context['block_hash']
        slot = context['slot']
        timestamp = context['timestamp']
        tx_hash = context['tx_hash']
        
        # if a rollback occurs we need to handle it
        if variant == 'RollBack':
            if block_hash is not None:
                print("ROLLBACK DO SOME THING HERE TO FIX STUFF")
                print(data)
            
        # outputs in questions
        if variant == 'TxOutput':
            output_utxo = context['tx_hash'] + '#' + str(context['output_idx'])
            
            if data['tx_output']['address'] == constants['queue_address']:
                print("QUEUE ITEM")
                print(data)
                print(f'The Transaction {tx_hash} At Time Stamp: {timestamp}')
                print(f'Output: {output_utxo}')
            
            if data['tx_output']['address'] == constants['sale_address']:
                sale_datum = data['tx_output']['inline_datum']['plutus_data'] if data['tx_output']['inline_datum'] is not None else None
                
                # convert to dict and add in lovelace
                lovelace = data['tx_output']['amount']
                value_obj = parsing.asset_list_to_dict(data['tx_output']['assets'])
                value_obj['lovelace'] = lovelace
                
                if parsing.key_exists_in_dict(value_obj, constants['pointer_policy']):
                    tkn = list(value_obj[constants['pointer_policy']].keys())[0]
                    
                    print("SALE IS ON")
                    print(f"Inside Block: {block_hash} At Slot: {slot}")
                    print(f'The Transaction: {tx_hash} At Time Stamp: {timestamp}')
                    print(f'Output: {output_utxo}')
                    print(f"Sale Datum: {sale_datum}")
                    print(f"Current Value: {value_obj}")
                    print(f"Pointer Token: {tkn}")
                else:
                    # we dont need to track it yet
                    print("Sale IS OFF")
                
                
        
        # # inputs in question
        # if variant == 'TxInput':
        #     # the tx hash of this transaction
        #     input_utxo = data['tx_input']['tx_id'] + '#' + str(data['tx_input']['index'])
        #     print(f"Spends: {input_utxo}")
            

    except Exception as e:
        # Log the error message
        print(f'Error: {e}')
        return 'Webhook deserialization failure'
    
    return 'Webhook received successfully'

def flask_process(start_event):
    start_event.wait()  # Wait until the start event is set
    app.run(host='0.0.0.0', port=8008)

def run_daemon():
    # Replace 'oura daemon --config daemon.toml' with the actual command you want to run
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
    # # Start the Flask app process
    # flask_proc = multiprocessing.Process(target=flask_process)
    # flask_proc.start()

    # # Start the daemon process
    # daemon_proc = multiprocessing.Process(target=run_daemon)
    # daemon_proc.start()

    try:
        # Wait for both processes to complete
        flask_proc.join()
        daemon_proc.join()
    except KeyboardInterrupt:
        # Handle KeyboardInterrupt (CTRL+C)
        print("KeyboardInterrupt detected, terminating processes...")
        db.close_connection()
        flask_proc.terminate()
        daemon_proc.terminate()
        flask_proc.join()
        daemon_proc.join()

if __name__ == '__main__':
    start_processes()
