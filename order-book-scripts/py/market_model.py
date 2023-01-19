import matplotlib.pyplot as plt
import random


def generate_data(nUsers, meanPrice):
    """
    Assuming some price and some simple distributions, generate a data set for N users.
    """

    data = []

    for _ in range(nUsers):
        user  = {}
        price = int(random.gauss(meanPrice, 250000))

        if random.random() > 0.5:
            have  = int(random.gauss(1000000, 250000))
            want  = int(have / (price / pow(10, 6)))
        else:
            want = int(random.gauss(1000000, 250000))
            have = price * want // pow(10, 6)
        
        fee       = random.randint(10000, 400000)
        incentive = random.randint(0, 500000)
        slippage  = random.randint(10, 50)

        user['have']      = have
        user['want']      = want
        user['fee']       = fee
        user['incentive'] = incentive
        user['slippage']  = slippage

        data.append(user)
    
    return data

def calculate_effective_price(user):
    """
    Calculate effective price from the have and want.
    """
    return pow(10, 6) * user['have'] // user['want']

if __name__ == "__main__":
    price = 345678
    n     = 100
    d     = generate_data(n, price)

    # calc eff price
    currentTrades = []
    for i in range(n):
        for j in range(i+1, n):
            userA = d[i]
            userB = d[j]
            # user A data
            pA    = calculate_effective_price(userA)
            lowA  = pA - pA // userA['slippage']
            highA = pA + pA // userA['slippage']

            # user B data
            pB    = calculate_effective_price(userB)
            lowB  = pB - pB // userB['slippage']
            highB = pB + pB // userB['slippage']

            # find prices in slippage
            if pA >= lowB and pA <= highB:
                if pB >= lowA and pB <= highA:
                    diffA          = abs(price - pA)
                    diffB          = abs(price - pB)
                    aveDiff        = int((diffA + diffB)/2)
                    totalFee       = userA['fee'] + userB['fee']
                    totalIncentive = userA['incentive'] + userB['incentive']
                    currentTrades.append((i, j, totalFee, totalIncentive, aveDiff))

    currentTrades.sort(key=lambda y: y[4])
    # currentTrades.reverse()
    print(currentTrades)