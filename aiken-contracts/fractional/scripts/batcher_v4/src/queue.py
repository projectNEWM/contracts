from src import db_manager_redis, parsing


class Queue:
    """Handle all queue logic."""

    @staticmethod
    def tx_input(db: db_manager_redis.DatabaseManager, data: dict, debug: bool = True) -> bool:
        # the tx hash of this transaction
        input_utxo = data['tx_input']['tx_id'] + \
            '#' + str(data['tx_input']['index'])

        utxo_base_64 = parsing.sha3_256(input_utxo)
        if db.is_key_in_queue(utxo_base_64):
            if debug is True:
                print(
                    f"Spent Queue Input: {input_utxo} @ Timestamp {data['context']['timestamp']}")
            db.delete_queue_record(utxo_base_64)
            return True

        return False

    @staticmethod
    def tx_output(db: db_manager_redis.DatabaseManager, constants: dict, data: dict, debug: bool = True) -> bool:
        context = data['context']
        # timestamp for ordering, equal timestamps use the tx_idx to order
        timestamp = context['timestamp']
        tx_idx = context['tx_idx']

        output_utxo = context['tx_hash'] + '#' + str(context['output_idx'])
        utxo_base_64 = parsing.sha3_256(output_utxo)

        # check if its the queue contract
        if data['tx_output']['address'] == constants['queue_address']:
            queue_datum = data['tx_output']['inline_datum']['plutus_data'] if data['tx_output']['inline_datum'] is not None else {}
            pointer_token = queue_datum['fields'][4]['bytes']
            # convert to dict and add in lovelace
            lovelace = data['tx_output']['amount']
            value_obj = parsing.asset_list_to_dict(data['tx_output']['assets'])
            value_obj['lovelace'] = lovelace

            if debug is True:
                print(f"Queue Output @ {output_utxo} @ Timestamp: {timestamp}")
            db.create_queue_record(
                utxo_base_64, output_utxo, pointer_token, queue_datum, value_obj, timestamp, tx_idx)
            return True

        return False
