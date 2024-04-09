import toml


def create_toml_file(filename, node_socket, timestamp, block_hash):
    data = {
        "source": {
            "type": "N2C",
            "address": ["Unix", node_socket],
            "magic": "preprod",
            "min_depth": 0,
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
