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