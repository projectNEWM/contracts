import random

def effective_price(have, want):
    gamma = 1000000
    scaled = gamma*have
    return scaled // want

def is_in_range(amt, slip, target):

    lowEnd = amt - (amt // slip)
    highEnd = amt + (amt // slip)

    return (lowEnd <= target and target <= highEnd)

def slip(have1,want1,slip1,have2,want2,slip2):
    return is_in_range(have1, slip1, want2) and is_in_range(have2, slip1, want1)

def effective_slip(have1,want1,slip1,have2,want2,slip2):

    a1 = effective_price(have1, want1)
    a2 = effective_price(have2, want2)

    return is_in_range(a1, slip1, a2) and is_in_range(a2, slip1, a1)

for i in range(10000):
    h1 = random.randint(1,2000000)
    w1 = random.randint(1,2000000)
    s1 = random.randint(10,50)
    h2 = random.randint(1,2000000)
    w2 = random.randint(1,2000000)
    s2 = random.randint(10,50)

    eOut = effective_slip(h1,w1,s1,h2,w2,s2)
    if eOut == True:
        out = slip(h1,w1,s1,h2,w2,s2)
        if out is True:
            print("\nfull")
            print(h1,w1,s1)
            print(h2,w2,s2)
        else:
            print("\npartial")
            print(h1,w1,s1)
            print(h2,w2,s2)
