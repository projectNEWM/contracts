from fractions import Fraction as f
from fractions import Fraction
import fractions

def find_best(this_total: fractions.Fraction, average_price: fractions.Fraction, that_total: fractions.Fraction):
    getting = this_total * average_price
    returning = that_total - getting
    if returning >= f(0):
        return this_total.__floor__(), getting.__floor__()
    else:
        amt = that_total / average_price
        return amt.__floor__(), that_total.__floor__()

def sqrt_binary_search(number):
    if number < 0:
        raise ValueError("Square root is not defined for negative numbers.")

    if number == 0:
        return 0

    low = 1
    high = number

    while low <= high:
        mid = (low + high) // 2
        square = mid * mid

        if square == number:
            return mid
        elif square < number:
            low = mid + 1
        else:
            high = mid - 1

    return high  # Return the floor value of the square root    

def calculate_trade(this_amt: int, that_amt: int, this_price:fractions.Fraction, that_price:fractions.Fraction):
    """Determine the amounts that need to be traded between both parties in
    a proper swap, i.e. it pass the is_ok_to_swap validation function.

    Args:
        this_amt (int): The integer quantity of the amount of tokens of the this party.
        that_amt (int): The integer quantity of the amount of tokens of the that party.
        this_price (fractions.Fraction): The fractional price of the this party.
        that_price (fractions.Fraction): The fractional price of the that party.
    """
    this_amt_fraction = f(this_amt)
    that_amt_fraction = f(that_amt)
    
    # geometric mean
    top = this_price.numerator * that_price.denominator
    bot = this_price.denominator * that_price.numerator
    this_average_price = f(sqrt_binary_search(top), sqrt_binary_search(bot))
    that_reciprocal_average_price = pow(this_average_price, -1)
    
    that_paid, that_get_amt = find_best(that_amt_fraction, this_average_price, this_amt_fraction)
    this_paid, this_get_amt = find_best(this_amt_fraction, that_reciprocal_average_price, that_amt_fraction)
    
    # these are teh numbers to be used
    print(f"this pays {this_paid} and gets {this_get_amt}")
    print(f"that pays {that_paid} and gets {that_get_amt}")
    

if __name__ == "__main__":
    # this amt
    x = 3234516
    # that amt
    y = 7322234

    # this price    
    p1 = f(67,2500).limit_denominator()
    dp1 = f(1,500).limit_denominator()
    # that rpice
    p2 = f(35331,1000).limit_denominator()
    dp2 = f(7,3).limit_denominator()
    
    print(float(p1), float(p2))
    
    low1 = p1 - dp1
    high1 = p1 + dp1
    ip2 = pow(p2, -1)
    
    low2 = p2 - dp2
    high2 = p2 + dp2
    ip1 = pow(p1, -1)
    
    # this checks if sale can happen
    print(low1 <= ip2 <= high1)
    print(low2 <= ip1 <= high2)
    
    # the ranges
    print(float(low1), float(ip2), float(high1))
    print(float(low2), float(ip1), float(high2))
    
    calculate_trade(x,y,p1,p2)
    