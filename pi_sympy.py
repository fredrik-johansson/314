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

def pi_071():
    return integrate(2*sin(x)/x, (x, 0, oo))

def pi_073():
    return simplify(E*integrate(cos(x)/(1+x**2), (x,-oo,oo)))

def pi_094():
    return limit(2**(4*k + 1)*factorial(k)**4/(2*k + 1)/factorial(2*k)**2, k, oo)

def pi_100():
    return sqrt(6*summation(1/k**2, (k,1,oo)))

def pi_101():
    return acos(-1)

def pi_103():
    return integrate(3*(x**2 + 2)/((x**2 + 1)*(x**2 + 4)), (x, 0, oo))

def pi_104():
    return log(-1)/I

def pi_105():
    return integrate(1/sqrt(1 - x**2), (x, -1, 1))

def pi_106():
    return arg(-1)

def pi_107():
    return simplify(gamma(S(1)/4)*gamma(S(3)/4))/sqrt(2)

def pi_108():
    return simplify(integrate(1/(1 - x**3)**(S(1)/3), (x, 0, 1)))*9/(2*sqrt(3))

def pi_109():
    return (integrate(exp(-x**2)*cos(2*x), (x, 0, oo))*E*2)**2

def pi_110():
    return integrate(1/(1 + x**4), (x, -oo, oo))*sqrt(2)

def pi_111():
    return integrate(x**2/(x**2 + S(1)/16)**2, (x, 0, oo))

if __name__ == "__main__":
    for fn in dir():
        if fn.startswith("pi_"):
            print(fn),
            f = globals()[fn]
            v = f()
            if v != pi:
                raise ValueError("is " + str(v) + " really pi?")
            print(v)

