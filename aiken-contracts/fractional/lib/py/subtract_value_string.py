def subtract_value(string, first, second):
    values = string.split(' ')
    
    new_first = int(values[0]) - first
    new_second = int(values[3]) - second
    values[0] = str(new_first)
    values[3] = str(new_second)
    print(' '.join(values))