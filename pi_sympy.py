from sympy import *

x = Symbol("x")
k = Symbol("k")

def pi_000():
    return pi

def pi_001():
    return rad(180)

def pi_002():
    return 4*atan(1)

def pi_005():
    return 2*I*simplify(log((1-I)/(1+I)))

def pi_008():
    return gamma(S(1)/2)**2

def pi_010():
    return (2/diff(erf(x), x).subs(x,0))**2

def pi_027():
    return sqrt(6*zeta(2))

def pi_042():
    return 4*summation((-1)**k/(2*k+1), (k,0,oo))

def pi_046():
    return summation(factorial(k)**2/factorial(2*k+1), (k,0,oo))*3*sqrt(3)/2

def pi_059():
    return integrate(1/(1+x**2), (x,-oo,oo))

def pi_060():
    return integrate(exp(-x**2), (x,-oo,oo))**2

def pi_061():
    return 2*integrate(sqrt(1-x**2), (x,-1,1))

def pi_073():
    return simplify(E*integrate(cos(x)/(1+x**2), (x,-oo,oo)))

def pi_100():
    return sqrt(6*summation(1/k**2, (k,1,oo)))

if __name__ == "__main__":
    for fn in dir():
        if fn.startswith("pi_"):
            print(fn),
            f = globals()[fn]
            v = f()
            if v != pi:
                raise ValueError("is " + str(v) + " really pi?")
            print(v)

