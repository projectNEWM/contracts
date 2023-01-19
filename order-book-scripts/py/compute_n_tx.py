

maximum = 0
for x in range(100):
    for y in range(100):
        if 62000000 >= (1676800 + 1609996)*x + (2701579+ 1936634)*y:
            if x + y > maximum:
                maximum = x+y

print(maximum)