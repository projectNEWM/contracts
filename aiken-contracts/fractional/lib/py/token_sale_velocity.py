import sqlite3
import csv

import matplotlib.pyplot as plt

conn = sqlite3.connect('tx.db')
cursor = conn.cursor()
cursor.execute('SELECT * FROM trxs')
everything = cursor.fetchall()

times = []
begin = everything[0][2]
for val in everything:
    print(val[2]- begin)
    times.append(val[2] - begin)


plt.hist(times)
plt.show()

total = sum([int(val[4]) for val in everything])
print(total / (100*pow(10, 6)))



# conn.close()

# cur.execute("SELECT * FROM trxs WHERE block_time >= {} AND block_time <= {};".format(start_time, end_time))
#     everything = cur.fetchall()
#     total = sum([int(val[4]) for val in everything])
#     print(total / pow(10, 6))