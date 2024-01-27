from src.parsing import cost_map_to_value_dict
import src.value as value
def utxo(sale_info, queue_info):
    # check if the number of bundles exists in sale
    bundle_pid = sale_info['datum']['fields'][1]['fields'][0]['bytes']
    bundle_tkn = sale_info['datum']['fields'][1]['fields'][1]['bytes']
    bundle_amt = sale_info['datum']['fields'][1]['fields'][2]['int']
    # check if cost is inside the queue
    cost_value = cost_map_to_value_dict(sale_info['datum']['fields'][2])
    queue_value = queue_info['value']
    if value.contains(cost_value, queue_value) is True:
        try:
            if sale_info['value'][bundle_pid][bundle_tkn] < bundle_amt:
                return False
            else:
                return True
        except KeyError:
            # should be the empty case
            return False
    else:
        return False