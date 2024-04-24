import toml


# the delay is the number of block behind the tip. Without proper rollback
# protection a non-zero delay must used. Default is 3, set value in yaml.
def create_toml_file(filename, node_socket, timestamp, block_hash, delay=3):
    data = {
        "source": {
            "type": "N2C",
            "address": ["Unix", node_socket],
            "magic": "preprod",
            "min_depth": delay,
            "intersect": {
                "type": "Point",
                "value": [timestamp, block_hash]
            },
            "mapper": {
                "include_transaction_details": True
            }
        },
        "sink": {
            "type": "Webhook",
            "url": "http://localhost:8008/webhook",
            "timeout": 60000,
            "error_policy": "Exit"
        }
    }

    with open(filename, 'w') as file:
        toml.dump(data, file)
