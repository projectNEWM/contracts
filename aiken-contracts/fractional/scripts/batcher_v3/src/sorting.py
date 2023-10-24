import math
from fractions import Fraction


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

def add_unique_tuple(new_tuple, tuple_list):
    for existing_tuple in tuple_list:
        if set(new_tuple) == set(existing_tuple):
            return tuple_list  # Tuple is already in the list

    tuple_list.append(new_tuple)
    return tuple_list

def order_fifo_sort(orders: list) -> list:
    order_pairs = []
    for order1 in orders:
        for order2 in orders:
            if order1 == order2:
                continue
            # check if the pair works
            have1 = order1[1]['datum']['fields'][1]
            want1 = order1[1]['datum']['fields'][2]['fields'][0]
            have2 = order2[1]['datum']['fields'][1]
            want2 = order2[1]['datum']['fields'][2]['fields'][0]
            if have1 == want2 and want1 == have2:
                # the haves and wants work
                # check the price
                
                # remember: price1 ~ 1 / price2
                
                price1 = order1[1]['datum']['fields'][2]['fields'][1]
                slip1 = order1[1]['datum']['fields'][2]['fields'][2]
                p1 = Fraction(price1['fields'][0]['int'], price1['fields'][1]['int'])
                s1 = Fraction(slip1['fields'][0]['int'], slip1['fields'][1]['int'])
                
                price2 = order2[1]['datum']['fields'][2]['fields'][1]
                slip2 = order2[1]['datum']['fields'][2]['fields'][2]
                p2 = Fraction(price2['fields'][0]['int'], price2['fields'][1]['int'])
                s2 = Fraction(slip2['fields'][0]['int'], slip2['fields'][1]['int'])
                
                low1 = p1 - s1
                high1 = p1 + s1
                
                low2 = p2 - s2
                high2 = p2 + s2
                
                if low1 <= (1/p2) <= high1 and low2 <= (1/p1) <= high2:
                    # the prices exist in the correct range
                    try:
                        amt1 = Fraction(order1[1]['value'][have1['fields'][0]['bytes']][have1['fields'][1]['bytes']])
                        amt2 = Fraction(order2[1]['value'][have2['fields'][0]['bytes']][have2['fields'][1]['bytes']])
                    except KeyError:
                        continue
                    
                    # check if the minimum threshold will be met for the potential swap
                    geometric_mean_price1 = Fraction(
                        int(math.sqrt(p1.numerator * p2.denominator)),
                        int(math.sqrt(p1.denominator * p2.numerator))
                    )
                    geometric_mean_price2 = 1/geometric_mean_price1
                    
                    
                    get1 = int(amt1*geometric_mean_price2)
                    get2 = int(amt2*geometric_mean_price1)
                    
                    if amt1 - get2 >= Fraction(0):
                        getting2 = get2
                    else:
                        getting2 = amt1
                        
                    if amt2 - get1 >= Fraction(0):
                        getting1 = get1
                    else:
                        getting1 = amt2
                    
                    have_thres1 = order1[1]['datum']['fields'][2]['fields'][3]['int']
                    want_thres1 = order1[1]['datum']['fields'][2]['fields'][4]['int']
                    
                    have_thres2 = order2[1]['datum']['fields'][2]['fields'][3]['int']
                    want_thres2 = order2[1]['datum']['fields'][2]['fields'][4]['int']
                    
                    # we need to check if both orders are meeting their threshold for trade
                    if getting1 >= want_thres1 and int(amt1) - getting2 >= have_thres1:
                        if getting2 >= want_thres2 and int(amt2) - getting1 >= have_thres2:
                            order_pairs = add_unique_tuple((order1[0],order2[0]), order_pairs)
                        
    # return the sorted list of ordered pairs
    return order_pairs


if __name__ == "__main__":

    d1 = ('1b59221937e6291a52fa9993fd55bc661e9d13cf4d0ad18589b39cc192be443c',
          {'txid': '650941a13b743fbad71f67771772864d1b542e42bfc6b774dddf7bec36f43bfd#0', 
           'datum': {
               'constructor': 0, 'fields': [
                   {'constructor': 0, 'fields': [{'bytes': '2d5fec7bbb8abbe1fb6590db2676389dffab196d212fb2b4b9902dcc'}, {'bytes': ''}]},
                   {'constructor': 0, 'fields': [{'bytes': '79b4b03d86b337231a3a861dba925675bf4edcce3169ad237feb0d14'}, {'bytes': '6c6f76656c616365'}]},
                   {'constructor': 0, 'fields': [{'constructor': 0, 'fields': [{'bytes': '015d83f25700c83d708fbf8ad57783dc257b01a932ffceac9dcd0c3d'}, {'bytes': '43757272656e6379'}]}, 
                                                 {'constructor': 0, 'fields': [{'int': 71331}, {'int': 1000}]}, {'constructor': 0, 'fields': [{'int': 71331}, {'int': 10000}]}, {'int': 0}, {'int': 0}]}, 
                   {'constructor': 0, 'fields': [{'bytes': '698a6ea0ca99f315034072af31eaac6ec11fe8558d3f48e9775aab9d'}, {'bytes': '7444524950'}, {'int': 1000000}]}]},
           'value': {'698a6ea0ca99f315034072af31eaac6ec11fe8558d3f48e9775aab9d': {'7444524950': 1000000}, '79b4b03d86b337231a3a861dba925675bf4edcce3169ad237feb0d14': {'6c6f76656c616365': 25511224}, 'lovelace': 4254130}, 'timestamp': 1692907621, 'tx_idx': 1}
          )
    d2 = ('c0d4cb8c1a7f68f6697049c5854616832022c32a60ebcddaa6fb472334843bae', {'txid': 'c43b627fef06b11d76982a2107c82bce652a9fbdc06c8beddaa04ecfee52d4b7#0', 'datum': {'constructor': 0, 'fields': [{'constructor': 0, 'fields': [{'bytes': '0d28d4a2e4c1504b8bf77f7db89561ca6421eef8ee1ea5a99300e88e'}, {'bytes': ''}]}, {'constructor': 0, 'fields': [{'bytes': '015d83f25700c83d708fbf8ad57783dc257b01a932ffceac9dcd0c3d'}, {'bytes': '43757272656e6379'}]}, {'constructor': 0, 'fields': [{'constructor': 0, 'fields': [{'bytes': '79b4b03d86b337231a3a861dba925675bf4edcce3169ad237feb0d14'}, {
          'bytes': '6c6f76656c616365'}]}, {'constructor': 0, 'fields': [{'int': 139}, {'int': 10000}]}, {'constructor': 0, 'fields': [{'int': 10425}, {'int': 10000000}]}, {'int': 0}, {'int': 0}]}, {'constructor': 0, 'fields': [{'bytes': '698a6ea0ca99f315034072af31eaac6ec11fe8558d3f48e9775aab9d'}, {'bytes': '7444524950'}, {'int': 1000000}]}]}, 'value': {'015d83f25700c83d708fbf8ad57783dc257b01a932ffceac9dcd0c3d': {'43757272656e6379': 12341235}, '698a6ea0ca99f315034072af31eaac6ec11fe8558d3f48e9775aab9d': {'7444524950': 1000000}, 'lovelace': 4241200}, 'timestamp': 1692907621, 'tx_idx': 0})
    items = [d1, d2]
    result = order_fifo_sort(items)
    print(result)
    # Example dictionary
    # input_dict = {
    #     'sale1': [
    #         ('order1', 5, 0),
    #         ('order2', 5, 2),
    #         ('order3', 5, 1),
    #         ('order4', 2, 0)
    #     ],
    #     'sale2': [
    #         ('order1', 4, 0),
    #         ('order2', 3, 2),
    #         ('order3', 2, 1),
    #         ('order4', 1, 0)
    #     ],
    #     'sale3': [
    #         ('order1', 1, 4),
    #         ('order2', 1, 2),
    #         ('order3', 1, 3),
    #         ('order4', 1, 1)
    #     ]
    # }

    # sorted_dict = {
    #     'sale1': [
    #         ('order4', 2, 0),
    #         ('order1', 5, 0),
    #         ('order3', 5, 1),
    #         ('order2', 5, 2),
    #     ],
    #     'sale2': [
    #         ('order4', 1, 0),
    #         ('order3', 2, 1),
    #         ('order2', 3, 2),
    #         ('order1', 4, 0),
    #     ],
    #     'sale3': [
    #         ('order4', 1, 1),
    #         ('order2', 1, 2),
    #         ('order3', 1, 3),
    #         ('order1', 1, 4),
    #     ]
    # }
    # result = queue_fifo_sort(input_dict)
    # print(result == sorted_dict)
