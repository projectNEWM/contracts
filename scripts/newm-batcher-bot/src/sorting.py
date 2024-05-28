

from src import db_manager_sqlite3


class Sorting:
    """Handle all queue logic."""

    @staticmethod
    def fifo_sort(input_dict: dict) -> dict:
        """This function will do a fifo sort on the sale-order dictionary.
          hash   timestamp  location-in-block
        ("acab", 123456789, 0)

        Args:
            input_dict (dict): The sales are the keys and the orders are a list.

        Returns:
            dict: A fifo ordered sale-order dictionary.
        """
        # init the new sorted dict
        sorted_dict = {}

        # each sale needs to be ordered
        for key, value_list in input_dict.items():
            # sort by incentive amt then timestamp then by the tx_idx
            sorted_list = sorted(value_list, key=lambda x: (-x[3], x[1], x[2]))
            sorted_dict[key] = sorted_list

        return sorted_dict

    @staticmethod
    def fifo(db: db_manager_sqlite3.DatabaseManagerSQLite) -> dict:
        # get all the queue items and sale items
        sales = db.read_all_sale_records()

        sale_to_order_dict = {}

        # there is at least one sale and one order
        if len(sales) >= 1:

            # loop sales and create a dictionary of orders per sale
            for sale in sales:

                # the unique pointer token for the sale
                pointer_token = sale[0]

                # there will be a list of orders for some sale
                sale_to_order_dict[pointer_token] = []

                orders = db.read_all_queue_records(pointer_token)

                # loop the orders
                for order in orders:
                    # unique order hash and the queue data for an order
                    order_hash = order[0]
                    order_data = order[1]
                    try:
                        # check if wallet type is correct and the incetive type is correct
                        _ = order_data['datum']['fields'][0]['fields']
                        incentive_data = order_data['datum']['fields'][2]['fields']
                        incentive_amt = incentive_data[2]['int']

                        # Sort by incentive amount (descending), then by timestamp and tx_idx
                        # Larger incentive amounts get higher priority
                        # a sale to order element is a tuple of
                        # (order hash, timestamp, tx idx, incentive amount)
                        sale_to_order_dict[pointer_token].append((order_hash, order_data['timestamp'], order_data['tx_idx'], incentive_amt))
                    except KeyError:
                        # bad incentive data here
                        continue

        # fifo the queue list per each sale
        return Sorting.fifo_sort(sale_to_order_dict)
