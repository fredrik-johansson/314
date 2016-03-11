from sympy import *

x = Symbol("x")
k = Symbol("k")

def pi_000():
    return pi

def pi_001():
    return rad(180)

def pi_002():
    return 4*atan(1)

# def pi_003():
#     return simplify(16*acot(5)-4*acot(239))

# def pi_004():
#     return simplify(48*acot(49)+128*acot(57)-20*acot(239)+48*acot(110443))

def pi_005():
    return 2*I*simplify(log((1-I)/(1+I)))

def pi_006():
    return -2*I*asinh(I)

def pi_007():
    return Ci(-oo)/I

def pi_008():
    return gamma(S(1)/2)**2

def pi_009():
    return expand_func(beta(S(1)/2, S(1)/2))

def pi_010():
    return (2/diff(erf(x), x).subs(x,0))**2

def pi_011():
    return solve(sin(x), x)[1]

def pi_012():
    return solve(cos(x), x)[0]*2

# def pi_013():
#     return simplify(-2*I*LambertW(-pi/2))

# def pi_014():
#     return solve(besselj(S(1)/2, x), x)[0]

def pi_015():
    return simplify(3*sqrt(3)/2/hyper([S(-1)/3,S(1)/3],[1],1))

# def pi_016():
#     return simplify(8/(hyper([S(1)/2,S(1)/2],[1],S(1)/2)*gamma(S(3)/4)/gamma(S(5)/4))**2)

# def pi_017():
#     # SymPy doesn't have struvel
#     return simplify(4*(hyper([1],[S(3)/2, 1],1)/struvel(-S(1)/2, 2))**2)

def pi_018():
    return simplify(1/meijerg([[],[]], [[0],[S(1)/2]], 0)**2)

def pi_019():
    # Note 1: simplify doesn't handle erfc -> erf
    # Note 2: SymPy doesn't support the r argument to meijerg, so I removed it
    # (doesn't matter because s is 1)
    return simplify((meijerg([],[2], [1,S(3)/2],[], 1) / erfc(1))**2).rewrite(erf)

def pi_020():
    return simplify((1-E)/meijerg([[1],[S(1)/2]], [[1],[S(1)/2,0]], 1))

# def pi_021():
#     return simplify(sqrt(polygamma(1,S(1)/4)-8*Catalan))

# SymPy doesn't have these functions yet

# def pi_022():
#     return simplify(elliprc(1,2)*4)
#
# def pi_023():
#     return simplify(elliprg(0,1,1)*4)
#
# def pi_024():
#     return simplify(2*agm(1,S(1)/2)*ellipk(S(3)/4))
#
# def pi_025():
#     return (gamma(S(3)/4)*jtheta(3, 0, exp(-pi)))**4
#
# def pi_026():
#     return root(gamma(S(1)/4)**4*agm(1, sqrt(2))**2/8, 3)

def pi_027():
    return sqrt(6*zeta(2))

def pi_028():
    return sqrt(6*(zeta(2,3)+S(5)/4))

# def pi_029():
#     return simplify(sqrt(zeta(2,(3,4))+8*Catalan))

# def pi_030():
#     return simplify(exp(-2*zeta(x, 1).diff(x).subs(x, 0))/2)

def pi_031():
    return sqrt(12*dirichlet_eta(2))

# SymPy doesn't have dirichlet yet

# def pi_032():
#     return simplify(4*dirichlet(1,[0,1,0,-1]))
#
# def pi_033():
#     return simplify(2*Catalan/dirichlet(-1,[0,1,0,-1],1))
#
# def pi_034():
#     return exp(-dirichlet(0,[0,1,0,-1],1))*gamma(S(1)/4)**2/(2*sqrt(2))

# def pi_035():
#     return simplify(sqrt(7*zeta(3)/(4*diff(lerchphi(-1,x,1), x)).subs(x, -2)))

# SymPy is either wrong here, or there is some difference in definitions

# def pi_036():
#     return simplify(sqrt(-12*polylog(2,-1)))

# def pi_037():
#     simplify(sqrt(6*log(2)**2+12*polylog(2,S(1)/2)))

# def pi_038():
#     return simplify(root(-81*I*(polylog(3,root(1,3,1))+4*zeta(3)/9)/2,3))

# SymPy doesn't have these functions

# def pi_039():
#     return 2*clsin(1,1)+1
#
# def pi_040():
#     return (3+sqrt(3)*sqrt(1+8*clcos(2,1)))/2
#
# def pi_041():
#     return root(2,6)*sqrt(e)/(glaisher**6*barnesg(S(1)/2)**4)

def pi_042():
    return 4*summation((-1)**k/(2*k+1), (k,0,oo))

# def pi_043():
#     return summation((3**k-1)/4**k*zeta(k+1), (k, 1, oo))

# Wrong result

# def pi_044():
#     return sqrt(summation(8/(2*k-1)**2, (k, 1, oo))).replace(exp_polar, exp)

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
    return simplify(integrate(3*sqrt(3)/(2*(1 - x**3)**(S(1)/3)), (x, 0, 1)))

def pi_109():
    return (integrate(exp(-x**2)*cos(2*x), (x, 0, oo))*E*2)**2

def pi_110():
    return integrate(sqrt(2)/(1 + x**4), (x, -oo, oo))

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
