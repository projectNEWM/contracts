
from src import db_manager_sqlite3, parsing


class Batcher:
    """Handle all batcher logic."""
    @staticmethod
    def tx_input(db: db_manager_sqlite3.DatabaseManagerSQLite, data: dict, logger) -> bool:
        # the tx hash of this transaction
        input_utxo = data['tx_input']['tx_id'] + '#' + str(data['tx_input']['index'])
        # sha3_256 hash of the input utxo
        utxo_base_64 = parsing.sha3_256(input_utxo)
        if db.delete_batcher_record(utxo_base_64):
            logger.success(f"Spent Batcher Input @ {input_utxo} @ Timestamp {data['context']['timestamp']}")
            return True
        return False

    @staticmethod
    def tx_output(db: db_manager_sqlite3.DatabaseManagerSQLite, constants: dict, data: dict, logger,) -> bool:
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

            db.create_batcher_record(utxo_base_64, output_utxo, value_obj)
            logger.success(f"Batcher Output @ {output_utxo} @ Timestamp: {timestamp}")
            return True
        return False
