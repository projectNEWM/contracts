def contains(target: dict, total: dict) -> bool:
    """Check if the target value is contained in the total value.

    Args:
        target (dict): The target value dictionary
        total (dict): The total value dictionary

    Returns:
        bool: If contained return True else False
    """
    for target_pid in target:
        for target_tkn in target[target_pid]:
            target_amt = target[target_pid][target_tkn]
            try:
                if total[target_pid][target_tkn] < target_amt:
                    return False
            except KeyError:
                return False
    return True