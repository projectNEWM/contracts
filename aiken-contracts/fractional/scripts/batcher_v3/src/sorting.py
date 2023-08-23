def queue_fifo_sort(input_dict: dict) -> dict:
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


if __name__ == "__main__":
    # Example dictionary
    input_dict = {
        'sale1': [
            ('order1', 5, 0),
            ('order2', 5, 2),
            ('order3', 5, 1),
            ('order4', 2, 0)
        ],
        'sale2': [
            ('order1', 4, 0),
            ('order2', 3, 2),
            ('order3', 2, 1),
            ('order4', 1, 0)
        ],
        'sale3': [
            ('order1', 1, 4),
            ('order2', 1, 2),
            ('order3', 1, 3),
            ('order4', 1, 1)
        ]
    }
    
    sorted_dict = {
        'sale1': [
            ('order4', 2, 0),
            ('order1', 5, 0),
            ('order3', 5, 1),
            ('order2', 5, 2),
        ],
        'sale2': [
            ('order4', 1, 0),
            ('order3', 2, 1),
            ('order2', 3, 2),
            ('order1', 4, 0),
        ],
        'sale3': [
            ('order4', 1, 1),
            ('order2', 1, 2),
            ('order3', 1, 3),
            ('order1', 1, 4),
        ]
    }
    result = queue_fifo_sort(input_dict)
    print(result == sorted_dict)