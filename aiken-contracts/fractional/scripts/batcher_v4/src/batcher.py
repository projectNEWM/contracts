
from src import db_manager_redis, parsing


class Batcher:
    """Handle all batcher logic."""
    @staticmethod
    def tx_input(db: db_manager_redis.DatabaseManager, data: dict, debug: bool = True) -> bool:
        # the tx hash of this transaction
        input_utxo = data['tx_input']['tx_id'] + \
            '#' + str(data['tx_input']['index'])

        utxo_base_64 = parsing.sha3_256(input_utxo)
        if db.is_key_in_batcher(utxo_base_64):
            if debug is True:
                print(
                    f"Spent Batcher Input @ {input_utxo} @ Timestamp {data['context']['timestamp']}")
            db.delete_batcher_record(utxo_base_64)
            return True
        return False

    @staticmethod
    def tx_output(db: db_manager_redis.DatabaseManager, constants: dict, data: dict, debug: bool = True) -> bool:
        # do something here
        context = data['context']
        # timestamp for ordering, equal timestamps use the tx_idx to order
        timestamp = context['timestamp']

        output_utxo = context['tx_hash'] + '#' + str(context['output_idx'])
        utxo_base_64 = parsing.sha3_256(output_utxo)

        if data['tx_output']['address'] == constants['batcher_address']:
            # update the batcher information
            lovelace = data['tx_output']['amount']
            value_obj = parsing.asset_list_to_dict(data['tx_output']['assets'])
            value_obj['lovelace'] = lovelace

            if debug is True:
                print(
                    f"Batcher Output @ {output_utxo} @ Timestamp: {timestamp}")
            db.create_batcher_record(utxo_base_64, output_utxo, value_obj)
            return True
        return False
