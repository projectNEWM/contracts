
from src import db_manager_sqlite3, parsing


# DatabaseManagerSQLite
class Sale:
    """Handle all sale logic."""

    @staticmethod
    def tx_input(db: db_manager_sqlite3.DatabaseManagerSQLite, data: dict, logger) -> bool:
        # the tx hash of this transaction
        input_utxo = data['tx_input']['tx_id'] + '#' + str(data['tx_input']['index'])

        if db.delete_sale_record_by_txid(input_utxo):
            logger.success(f"Spent Sale Input @ {input_utxo} @ Timestamp {data['context']['timestamp']}")
            return True
        return False

    @staticmethod
    def tx_output(db: db_manager_sqlite3.DatabaseManagerSQLite, constants: dict, data: dict, logger) -> bool:
        # do something here
        context = data['context']

        # timestamp for ordering, equal timestamps use the tx_idx to order
        timestamp = context['timestamp']

        # the utxo
        output_utxo = context['tx_hash'] + '#' + str(context['output_idx'])

        if data['tx_output']['address'] == constants['sale_address']:

            # get the datum
            sale_datum = data['tx_output']['inline_datum']['plutus_data'] if data['tx_output']['inline_datum'] is not None else {}

            # convert to dict and add in lovelace
            lovelace = data['tx_output']['amount']
            value_obj = parsing.asset_list_to_dict(data['tx_output']['assets'])
            value_obj['lovelace'] = lovelace

            # only create a sale if the sale has the pointer token
            if parsing.key_exists_in_dict(value_obj, constants['pointer_policy']):
                tkn = list(value_obj[constants['pointer_policy']].keys())[0]
                logger.success(f"Sale Output @ {output_utxo} @ Timestamp: {timestamp}")
                db.create_sale_record(tkn, output_utxo, sale_datum, value_obj)
                return True
        return False
