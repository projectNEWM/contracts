import orders
from delay import force_block_change
from query import utxo
from transaction import build_sale, sign, submit, txid, build_refund
from parsing import compress_dicts, add_dicts, subtract_dicts, process_output, map_to_value, delete_zeros, value_exist_in_value
from pycardano import Address, Network, VerificationKeyHash

def run():
    
    DEBUG = False
    
    root    = ".."
    cli     = "cardano-cli"
    network = "--testnet-magic 1"
    socket  = "/home/logic/Documents/Work/LogicalMechanism/full-node-wallet/node/db-testnet/node.socket"
    tmp = root+"/tmp/"
    
    sale_utxo_path="../tmp/current_sale_utxos.json"
    queue_utxo_path="../tmp/current_queue_utxos.json"
    batcher_utxo_path="../tmp/current_batcher_utxos.json"
    
    pointer_pid_path="../../hashes/pointer_policy.hash"
    # pointer pid
    pointer_pid = orders.cat(pointer_pid_path)
    
    sale_address = "addr_test1xrldelw9u8n4depw52lkhnlg34erapvptgcwxaz7tgl856unp7y0mz2xgxfld6ettk7q2nas9hu7u32jchmjnp5spwvqcgsykt"
    queue_address = "addr_test1xqy4383gufhcc3y0g660mtdkyx3x7ayvtavj53m053434synp7y0mz2xgxfld6ettk7q2nas9hu7u32jchmjnp5spwvqvhlqg0"
    batcher_address = "addr_test1vrs4fk7ea6rg2fvd00sa8um5unp0rt474kngwpc38v2z9vqujprdk"
    
    collat_skey = "../wallets/collat-wallet/payment.skey"
    batcher_skey = "../wallets/batcher-wallet/payment.skey"
    
    # current state
    order_book = {}
    
    # things that just got spent
    spent_order_book = {}
    
    # current sale status
    sale_status = {}
    
    # batcher state
    utxo(cli, socket, network, batcher_address, batcher_utxo_path)
    batcher_data = orders.open_file(batcher_utxo_path)
    
    batcher_tx_in = list(batcher_data.keys())[0]
    for o in batcher_data:
        batcher_value = compress_dicts(batcher_data[o]['value'], {})
    
    # start it
    while True:
        print('\nWait For Next Block')
        force_block_change(socket)
        
        print("Find Current State")
        utxo(cli, socket, network, sale_address, sale_utxo_path)
        utxo(cli, socket, network, queue_address, queue_utxo_path)
        
        # current orders wrt the tip
        order_book = orders.get_orders(order_book, sale_utxo_path, queue_utxo_path, pointer_pid_path)
        
        # remove any processed orders
        spent_order_book = orders.update_orders(order_book, spent_order_book)
        
        # fifo
        sorted_order_book = orders.sort_orders(order_book)
        print(f"There Are Potentially {len(sorted_order_book)} Orders")

        # loop all sorted orders
        for o in sorted_order_book:
            queue_utxo = o
            # process the ones not spent yet
            if queue_utxo in spent_order_book:
                continue
            
            # process the tx here
            print(f"\nProcessing {queue_utxo}")
            
            # get the current queue data
            queue_data = orders.open_file(queue_utxo_path)
            queue_value = compress_dicts(queue_data[queue_utxo]['value'], {})
            
            # get the queue item return address
            queue_address_data = queue_data[queue_utxo]['inlineDatum']['fields'][0]
            queue_pkh = queue_address_data['fields'][0]['bytes']
            queue_vkh = VerificationKeyHash(bytes.fromhex(queue_pkh))
            queue_sc = queue_address_data['fields'][1]['bytes']
            if queue_sc == "":
                print("Buyer Has Enterprise Wallet")
                buyer_address = Address(payment_part=queue_vkh, network=Network.TESTNET).encode()
            else:
                print("Buyer Has Stake Wallet")
                queue_kh = VerificationKeyHash(bytes.fromhex(queue_sc))
                buyer_address = Address(payment_part=queue_vkh, staking_part=queue_kh, network=Network.TESTNET).encode()
            
            # the sale the queue item is pointing at
            queue_pointer_tkn = queue_data[queue_utxo]['inlineDatum']['fields'][4]['bytes']
            
            # number of bundles wanted from queue item
            number_of_bundles = int(queue_data[queue_utxo]['inlineDatum']['fields'][2]['int'])
            
            # the incentive from the queue item
            incentive_data = queue_data[queue_utxo]['inlineDatum']['fields'][3]['fields']
            incentive_pid = incentive_data[0]['bytes']
            incentive_tkn = incentive_data[1]['bytes']
            incentive_amt = incentive_data[2]['int']
            incentive_value = {incentive_pid: {incentive_tkn: incentive_amt}}
            
            # does the queue item hold the incentive?
            potential_refund_flag = False
            try:
                if queue_value[incentive_pid][incentive_tkn] != incentive_amt:
                    print("Queue Item Does Not Enough Incentive")
                    continue
            except KeyError:
                print("Queue Item Does Not Hold Incentive")
                potential_refund_flag = True
                
            
            # get the current sale data
            sale_data = orders.open_file(sale_utxo_path)
            
            # check if anyone has pointed at this sale yet
            if queue_pointer_tkn in sale_status:
                # get the assumed status
                print("Sale Already Exists")
                sale_utxo = sale_status[queue_pointer_tkn]['txid']
                sale_value = sale_status[queue_pointer_tkn]['value']
            else:
                print("Creating Sale Data")
                
                # default found to not found
                found = False
                
                # loop all sales and find the sale
                
                for key in sale_data:
                    try:
                        # find the sale with that token
                        if sale_data[key]['value'][pointer_pid][queue_pointer_tkn] == 1:
                            
                            # create a entry for the pointer
                            sale_status[queue_pointer_tkn] = {}
                            sale_status[queue_pointer_tkn]['txid'] = key
                            sale_utxo = key
                            
                            # get the value
                            sale_value = compress_dicts(sale_data[key]['value'], {})
                            sale_status[queue_pointer_tkn]['value'] = sale_value
                            
                            # set flag
                            found = True
                            break
                    
                    # just skip things that don't have the correct pointer
                    except KeyError:
                        continue
            print(f"Current Sale UTXO: {sale_utxo}")
            print(f"Current Batcher UTXO: {batcher_tx_in}")
            # pointing at something that doesn't exist
            if found is False:
                print("A Queue Item Is Pointing At A Non-Existent Sale")
                # this should force a cancel
                continue
            
            if potential_refund_flag is True:
                print(f"Refunding {queue_utxo}")
                q_out_val = subtract_dicts(queue_value, {"lovelace": 600000})
                buyer_out = process_output(buyer_address, q_out_val)
                build_refund(queue_utxo, sale_utxo, buyer_out)
                sign([batcher_skey, collat_skey], network, '../tmp/tx-refund.signed')
                if DEBUG is False: submit(network, socket, '../tmp/tx-refund.signed')
                continue
            
            
            # assume that the sale data is constant
            # get the bundle data from the sale
            # loop all sales and find the sale
            found_sale = False
            for key in sale_data:
                try:
                    # find the sale with that token
                    if sale_data[key]['value'][pointer_pid][queue_pointer_tkn] == 1:
                        bundle_data = sale_data[key]['inlineDatum']['fields'][1]['fields']
                        found_sale = True
                        break
                except KeyError:
                    # print("Can't Find Bundle Information")
                    continue
            if found_sale is False:
                print("Can't Find Sale By Pointer Token")
                continue
            bundle_pid = bundle_data[0]['bytes']
            bundle_tkn = bundle_data[1]['bytes']
            bundle_amt = bundle_data[2]['int']
            
            # check if the sale value has enough for the bundle
            try:
                # see if there is at least one bundle
                if sale_value[bundle_pid][bundle_tkn] >= bundle_amt:
                    # well how many bundles are there?
                    n_bundles = sale_value[bundle_pid][bundle_tkn] // bundle_amt
                    # print(n_bundles)
                    # default to the queue value?
                    if n_bundles >= number_of_bundles:
                        bundle_value = { bundle_pid: {bundle_tkn: number_of_bundles * bundle_amt}}
                    
                    # set the limit to max allowed then
                    else:
                        number_of_bundles = n_bundles
                        bundle_value = { bundle_pid: {bundle_tkn: number_of_bundles * bundle_amt}}
                else:
                    # This needs to trigger the action for the autorefund when the sale doesn't have enough tokens
                    print("Not Enough For A Bundle")
                    print(f"Refunding {queue_utxo}")
                    q_out_val = subtract_dicts(queue_value, {"lovelace": 600000})
                    buyer_out = process_output(buyer_address, q_out_val)
                    build_refund(queue_utxo, sale_utxo, buyer_out)
                    sign([batcher_skey, collat_skey], network, '../tmp/tx-refund.signed')
                    if DEBUG is False: submit(network, socket, '../tmp/tx-refund.signed')
                    continue
            
            except KeyError:
                # This needs to trigger the action for the autorefund when the sale doesn't have enough tokens
                print("No More Tokens Left")
                print(f"Refunding {queue_utxo}")
                q_out_val = subtract_dicts(queue_value, {"lovelace": 600000})
                buyer_out = process_output(buyer_address, q_out_val)
                build_refund(queue_utxo, sale_utxo, buyer_out)
                sign([batcher_skey, collat_skey], network, '../tmp/tx-refund.signed')
                if DEBUG is False: submit(network, socket, '../tmp/tx-refund.signed')
                continue
            
            
            # get the price data from the sale
            found_sale = False
            for key in sale_data:
                try:
                    # find the sale with that token
                    if sale_data[key]['value'][pointer_pid][queue_pointer_tkn] == 1:
                        # print(f"A Price Has Been Found For {queue_utxo}")
                        price = sale_data[key]['inlineDatum']['fields'][2]
                        found_sale = True
                        break
                except KeyError:
                    continue
            if found_sale is False:
                print("Can't Find Sale By Pointer Token")
                continue
            
            # price = sale_data[sale_utxo]['inlineDatum']['fields'][2]
            # print(number_of_bundles)
            price_value = map_to_value(price, number_of_bundles)
            # print(price_value)
            # print(queue_value)
            # Does teh queue have enough for the price
            if value_exist_in_value(price_value, queue_value) is False:
                print("Queue Item Does Not Hold The Payment")
                continue
            
            
            print()
            print(queue_value)
            print(sale_value)
            print(batcher_value)
            
            # update the batcher value with the incentive value
            b_out_val = add_dicts(batcher_value, incentive_value)
            
            # queue out value
            q_out_val = add_dicts(subtract_dicts(subtract_dicts(queue_value, incentive_value), price_value), bundle_value)
            q_out_val = subtract_dicts(q_out_val, {"lovelace": 600000})
            q_out_val = delete_zeros(q_out_val)
            
            # sale out value
            s_out_val = add_dicts(subtract_dicts(sale_value, bundle_value), price_value)
            s_out_val = delete_zeros(s_out_val)
            
            # compute the tx outs
            # add incentive_value to the batcher output
            batcher_out = process_output(batcher_address, b_out_val)
            
            # add the bundle to the queue output
            queue_out = process_output(queue_address, q_out_val)
            
            # add the profit to the sale outout
            sale_out = process_output(sale_address, s_out_val)
            
            print()
            print(batcher_out)
            print(queue_out)
            print(sale_out)
            
            # build the purchase; THIS CHANGES THE TXID: sale and queue
            print("Auto Purchase")
            build_sale(batcher_tx_in, sale_utxo, queue_utxo, batcher_out, sale_out, queue_out)
            sign([batcher_skey, collat_skey], network, '../tmp/tx-purchase.signed')

            print("Chaining Tx")
            id = txid()
            next_sale_txid = id + "#1"
            sale_status[queue_pointer_tkn]['txid'] = next_sale_txid
            sale_status[queue_pointer_tkn]['value'] = s_out_val
            intermediate_queue_utxo = id + "#2"
            batcher_tx_in = id + "#0"
            batcher_value = b_out_val
            
            # add the bundle to the queue output
            
            print("Auto Refund")
            print(f"Refunding {intermediate_queue_utxo}")
            q_out_val = subtract_dicts(q_out_val, {"lovelace": 600000})
            buyer_out = process_output(buyer_address, q_out_val)
            build_refund(intermediate_queue_utxo, next_sale_txid, buyer_out)
            sign([batcher_skey, collat_skey], network, '../tmp/tx-refund.signed')
            
            # submit the tx
            print("Submit Tx")
            if DEBUG is False: submit(network,socket, '../tmp/tx-purchase.signed')
            if DEBUG is False: submit(network,socket, '../tmp/tx-refund.signed')
            
            # add utxo to the spent dict
            spent_order_book[queue_utxo] = order_book[queue_utxo]
            spent_order_book[intermediate_queue_utxo] = {}
        
        
        #
        # go to next order
        #

if __name__ == "__main__":
    
    
    try:
        run()
    except KeyboardInterrupt:
        print('exiting')
        exit()