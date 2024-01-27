import json


def update_owner(file_path, pkh, sc):
    # Define the updated data object
    updated_data_object = {
        "constructor": 0,
        "fields": [
            {
                "bytes": pkh
            },
            {
                "bytes": sc
            }
        ]
    }

    # Read the data from the file
    with open(file_path, 'r') as file:
        data = json.load(file)

    # Find the index of the data object you want to update
    data['fields'][0] = updated_data_object

    # Write the updated data back to the file
    with open(file_path, 'w') as file:
        json.dump(data, file, indent=2)
