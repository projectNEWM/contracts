import sqlite3

import matplotlib.pyplot as plt

conn = sqlite3.connect('tx.db')
cursor = conn.cursor()
cursor.execute('SELECT * FROM trxs')
everything = cursor.fetchall()

times = []
begin = everything[0][2]
for val in everything:
    times.append(val[2] - begin)


plt.hist(times)
plt.show()
