

from src import db_manager_redis


class Sorting:
    """Handle all queue logic."""

    @staticmethod
    def fifo_sort(input_dict: dict) -> dict:
        """This function will do a fifo sort on the sale-order dictionary.
        txHash    timestamp  location-in-block
        ("tx#idx", timestamp, tx_idx)

        Args:
            input_dict (dict): The sales are the keys and the orders are a list.

        Returns:
            dict: A fifo ordered sale-order dictionary.
        """
        # init the new sorted dict
        sorted_dict = {}

        # each sale needs to be ordered
        for key, value_list in input_dict.items():
            # sort by timestamp then by the tx_idx
            sorted_list = sorted(value_list, key=lambda x: (x[1], x[2]))
            sorted_dict[key] = sorted_list

        return sorted_dict

    @staticmethod
    def fifo(db: db_manager_redis.DatabaseManager) -> dict:
        # get all the queue items and sale items
        sales = db.read_all_sale_records()
        orders = db.read_all_queue_records()

        sale_to_order_dict = {}

        # there is at least one sale and one order
        if len(sales) >= 1 and len(orders) >= 1:
            # loop sales and loop orders and build out the dictionary
            for sale in sales:
                pointer_token = sale[0]
                sale_to_order_dict[pointer_token] = []

                for order in orders:
                    order_hash = order[0]
                    order_data = order[1]
                    tkn = order_data['tkn']

                    # find an order that points to a sale
                    if pointer_token == tkn:
                        # timestap is primary sort var but tx_idx should be use to handle equal timestamps
                        sale_to_order_dict[pointer_token].append(
                            (order_hash, order_data['timestamp'], order_data['tx_idx']))

        # fifo the queue list per each sale
        return Sorting.fifo_sort(sale_to_order_dict)
