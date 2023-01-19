import matplotlib.pyplot as plt
import random

have = random.randint(1, 1000000) # Drip
want = random.randint(1,1000000) # Newm

scale = 1000000

price = scale * have / want
slippage = 40
threshold = price / slippage

for h in range(50000, 10000000, 50000): # Newm
    for w in range(50000, 3000000, 50000): # Drip
        p = scale * w / h
        diff = price - p
        slip = random.randint(30, 40)
        thres = p / slip


        if p > price - threshold and p < price + threshold:
            if price > p - thres and price < p + thres:
                print("Your Slippage", slippage, "Their Slippage", slip)
                print("have", h, "want", w, "their price", p)
                print("have", have, "want", want, "your price", price, "difference", diff)
            # plt.scatter(h/10000000, w/3000000, c='g')


        # if diff > above and diff < 0:
        #     print("have", h, "want", w, "their price", p, "your price", price, "difference", diff)
        #     plt.scatter(h/10000000, w/3000000, c='g')
        # elif diff < below and diff > 0:
        #     print("have", h, "want", w, "their price", p, "your price", price, "difference", diff)
        #     plt.scatter(h/10000000, w/3000000, c='b')

        # if p > price - slippage and p < price + slippage:
        #     plt.scatter(h/10000000, w/3000000, c='g')
        # else:
            # plt.scatter(h/10000000, w/3000000, c='r')
# plt.savefig("2d.png")