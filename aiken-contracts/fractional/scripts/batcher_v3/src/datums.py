def empty(index: int) -> dict:
  """Creates the empty datum with a specific index

  Args:
      index (int): The constructor index

  Returns:
      dict: The empty datum.
  """
  data = {
    "constructor": index,
    "fields": []
  }
  return data

def complete_redeemer(id: str, idx: int) -> dict:
  """Creates the complete redeemer for the order book.

  Args:
      id (str): The tx id
      idx (int): the tx id index

  Returns:
      dict: The complete redeemer
  """
  data = {
    "constructor": 3,
    "fields": [
      {
        "constructor": 0,
        "fields": [
          {
            "bytes": str(id)
          },
          {
            "int": int(idx)
          }
        ]
      }
    ]
  }
  return data
