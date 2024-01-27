import os

from src import (db_manager_redis, parsing, query, queue_purchase,
                 queue_refund, transaction)
from src.parsing import cost_map_to_value_dict


def utxo(sale_info, queue_info):
    # check if the number of bundles exists in sale
    bundle_pid = sale_info['datum']['fields'][1]['fields'][0]['bytes']
    bundle_tkn = sale_info['datum']['fields'][1]['fields'][1]['bytes']
    bundle_amt = sale_info['datum']['fields'][1]['fields'][2]['int']
    # check if cost is inside the queue
    cost_value = cost_map_to_value_dict(sale_info['datum']['fields'][2])
    queue_value = queue_info['value']

    if contains(cost_value, queue_value) is True:
        try:
            if sale_info['value'][bundle_pid][bundle_tkn] < bundle_amt:
                # not enough for a bundle
                return False
            else:
                # enough for a bundle
                return True
        except KeyError:
            # should be the empty case
            return False
    else:
        # cost value not in queue value
        return False


def contains(target: dict, total: dict) -> bool:
    """Check if the target value is contained in the total value.

    Args:
        target (dict): The target value dictionary
        total (dict): The total value dictionary

    Returns:
        bool: If contained return True else False
    """
    for target_pid in target:
        for target_tkn in target[target_pid]:
            target_amt = target[target_pid][target_tkn]
            try:
                if total[target_pid][target_tkn] < target_amt:
                    return False
            except KeyError:
                return False
    return True


class Fulfillment:
    """Handle all fulfillment logic."""

    @staticmethod
    def orders(db: db_manager_redis.DatabaseManager, sorted_sale_to_order_dict: dict, constants: dict, debug: bool = True) -> None:
        # loop the sorted sales and start batching

        # there needs to be at least a single batcher record
        try:
            batcher_info = db.read_all_batcher_records()[0][1]
        except IndexError:
            return

        this_dir = os.path.dirname(os.path.abspath(__file__))
        parent_dir = os.path.dirname(this_dir)
        out_file_path = os.path.join(parent_dir, "tmp/tx.draft")
        mempool_file_path = os.path.join(parent_dir, "tmp/mempool.json")
        signed_purchase_tx = os.path.join(
            parent_dir, "tmp/queue-purchase-tx.signed")
        signed_refund_tx = os.path.join(
            parent_dir, "tmp/queue-refund-tx.signed")
        batcher_skey_path = os.path.join(parent_dir, "key/batcher.skey")
        collat_skey_path = os.path.join(parent_dir, "key/collat.skey")

        # the sale here is teh pointer token
        for sale in sorted_sale_to_order_dict:
            sale_info = db.read_sale_record(sale)

            # print("Loop Orders")
            # start fulfilling orders
            sale_orders = sorted_sale_to_order_dict[sale]
            for order in sale_orders:

                # does the order even exist
                order_hash = order[0]
                order_info = db.read_queue_record(order_hash)
                if order_info is None:
                    print("NOTHING FOUND")
                    # a queue item left the queue
                    continue

                # merklize the sale and order
                tag = parsing.sha3_256(parsing.sha3_256(
                    str(sale)) + parsing.sha3_256(str(order_info)))
                # check if the tag has been seen before
                if db.read_seen_record(tag) is True:
                    # print("Seen it")
                    continue

                # check the order info for current state
                state = utxo(sale_info, order_info)
                # print("state", sale_info)
                # print("state", order_info)
                # print("state", state)
                if state is None:
                    print("User Must Remove Queue Item")
                    # user must cancel order manually
                    continue
                elif state is True:
                    # print("Build Purchase")

                    # build the purchase tx
                    sale_info, order_info, batcher_info = queue_purchase.build_tx(
                        sale_info, order_info, batcher_info, constants)
                    # sign tx
                    transaction.sign(out_file_path, signed_purchase_tx,
                                     constants['network'], batcher_skey_path, collat_skey_path)

                    # is the purchase tx already submitted?
                    intermediate_txid = transaction.txid(out_file_path)
                    mempool_check = query.does_tx_exists_in_mempool(
                        constants['socket_path'], intermediate_txid, mempool_file_path, constants['network'])
                    if mempool_check is True:
                        print("Its in the mempool")
                        continue

                    # build the refund tx
                    # print("Build Refund")

                    sale_info, order_info, batcher_info = queue_refund.build_tx(
                        sale_info, order_info, batcher_info, constants)
                    # sign tx
                    transaction.sign(out_file_path, signed_refund_tx,
                                     constants['network'], batcher_skey_path, collat_skey_path)

                    # is the refund tx already submitted?
                    intermediate_txid = transaction.txid(out_file_path)
                    mempool_check = query.does_tx_exists_in_mempool(
                        constants['socket_path'], intermediate_txid, mempool_file_path, constants['network'])
                    if mempool_check is True:
                        continue

                    # submit tx
                    purchase_result, purchase_output = transaction.submit(
                        signed_purchase_tx, constants['socket_path'], constants['network'])
                    # if successful add to the seen record
                    if purchase_result is True:
                        db.create_seen_record(tag)
                    else:
                        continue
                    refund_result, refund_output = transaction.submit(
                        signed_refund_tx, constants['socket_path'], constants['network'])
                    # if successful add to the seen record
                    if refund_result is True:
                        tag = parsing.sha3_256(parsing.sha3_256(
                            str(sale)) + parsing.sha3_256(str(order_info)))
                        db.create_seen_record(tag)

                    # needs better debug
                    if debug is True:
                        print(f"Order: {tag}")
                        print(f"Purchase: {purchase_result}")
                        print(f"Refund: {refund_result}")

                else:
                    # # build the refund tx
                    # print("Build Refund Only")
                    sale_info, order_info, batcher_info = queue_refund.build_tx(
                        sale_info, order_info, batcher_info, constants)
                    # # sign tx
                    transaction.sign(out_file_path, signed_refund_tx,
                                     constants['network'], batcher_skey_path, collat_skey_path)
                    # # submit refund
                    refund_result, refund_output = transaction.submit(
                        signed_refund_tx, constants['socket_path'], constants['network'])
                    if refund_result is True:
                        db.create_seen_record(tag)

                    # needs better debug
                    if debug is True:
                        print(f"Refund: {refund_result}")
