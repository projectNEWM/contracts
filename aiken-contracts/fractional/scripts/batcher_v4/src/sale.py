
from src import db_manager_redis, parsing


class Sale:
    """Handle all sale logic."""

    @staticmethod
    def tx_input(db: db_manager_redis.DatabaseManager, data: dict, debug: bool = True) -> bool:
        # the tx hash of this transaction
        input_utxo = data['tx_input']['tx_id'] + \
            '#' + str(data['tx_input']['index'])

        result = db.find_sale_by_utxo(input_utxo)
        if result:
            if debug is True:
                print(
                    f"Spent Sale Input @ {input_utxo} @ Timestamp {data['context']['timestamp']}")
            db.delete_sale_record(result[0])
            return True

        return False

    @staticmethod
    def tx_output(db: db_manager_redis.DatabaseManager, constants: dict, data: dict, debug: bool = True) -> bool:
        # do something here
        context = data['context']
        # timestamp for ordering, equal timestamps use the tx_idx to order
        timestamp = context['timestamp']

        output_utxo = context['tx_hash'] + '#' + str(context['output_idx'])

        if data['tx_output']['address'] == constants['sale_address']:
            sale_datum = data['tx_output']['inline_datum']['plutus_data'] if data['tx_output']['inline_datum'] is not None else {}

            # convert to dict and add in lovelace
            lovelace = data['tx_output']['amount']
            value_obj = parsing.asset_list_to_dict(data['tx_output']['assets'])
            value_obj['lovelace'] = lovelace

            if parsing.key_exists_in_dict(value_obj, constants['pointer_policy']):
                if debug is True:
                    print(
                        f"Sale Output @ {output_utxo} @ Timestamp: {timestamp}")

                tkn = list(value_obj[constants['pointer_policy']].keys())[0]
                db.create_sale_record(tkn, output_utxo, sale_datum, value_obj)
                return True
        return False
