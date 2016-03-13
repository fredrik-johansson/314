from mpmath import *

def pi_000():
    return +pi

def pi_001():
    return 180*degree

def pi_002():
    return 4*atan(1)

def pi_003():
    return 16*acot(5)-4*acot(239)

def pi_004():
    return 48*acot(49)+128*acot(57)-20*acot(239)+48*acot(110443)

def pi_005():
    return chop(2*j*log((1-j)/(1+j)))

def pi_006():
    return chop(-2j*asinh(1j))

def pi_007():
    return chop(ci(-inf)/1j)

def pi_008():
    return gamma(0.5)**2

def pi_009():
    return beta(0.5,0.5)

def pi_010():
    return (2/diff(erf, 0))**2

def pi_011():
    return findroot(sin, 3)

def pi_012():
    return findroot(cos, 1)*2

def pi_013():
    return chop(-2j*lambertw(-pi/2))

def pi_014():
    return besseljzero(0.5,1)

def pi_015():
    return 3*sqrt(3)/2/hyp2f1((-1,3),(1,3),1,1)

def pi_016():
    return 8/(hyp2f1(0.5,0.5,1,0.5)*gamma(0.75)/gamma(1.25))**2

def pi_017():
    return 4*(hyp1f2(1,1.5,1,1) / struvel(-0.5, 2))**2

def pi_018():
    return 1/meijerg([[],[]], [[0],[0.5]], 0)**2

def pi_019():
    return (meijerg([[],[2]], [[1,1.5],[]], 1, 0.5) / erfc(1))**2

def pi_020():
    return (1-e) / meijerg([[1],[0.5]], [[1],[0.5,0]], 1)

def pi_021():
    return sqrt(psi(1,0.25)-8*catalan)

def pi_022():
    return elliprc(1,2)*4

def pi_023():
    return elliprg(0,1,1)*4

def pi_024():
    return 2*agm(1,0.5)*ellipk(0.75)

def pi_025():
    return (gamma(0.75)*jtheta(3,0,exp(-pi)))**4

def pi_026():
    return cbrt(gamma(0.25)**4*agm(1,sqrt(2))**2/8)

def pi_027():
    return sqrt(6*zeta(2))

def pi_028():
    return sqrt(6*(zeta(2,3)+5./4))

def pi_029():
    return sqrt(zeta(2,(3,4))+8*catalan)

def pi_030():
    return exp(-2*zeta(0,1,1))/2

def pi_031():
    return sqrt(12*altzeta(2))

def pi_032():
    return 4*dirichlet(1,[0,1,0,-1])

def pi_033():
    return 2*catalan/dirichlet(-1,[0,1,0,-1],1)

def pi_034():
    return exp(-dirichlet(0,[0,1,0,-1],1))*gamma(0.25)**2/(2*sqrt(2))

def pi_035():
    return sqrt(7*zeta(3)/(4*diff(lerchphi, (-1,-2,1), (0,1,0))))

def pi_036():
    return sqrt(-12*polylog(2,-1))

def pi_037():
    return sqrt(6*log(2)**2+12*polylog(2,0.5))

def pi_038():
    return chop(root(-81j*(polylog(3,root(1,3,1))+4*zeta(3)/9)/2,3))

def pi_039():
    return 2*clsin(1,1)+1

def pi_040():
    return (3+sqrt(3)*sqrt(1+8*clcos(2,1)))/2

def pi_041():
    return root(2,6)*sqrt(e)/(glaisher**6*barnesg(0.5)**4)

def pi_042():
    return nsum(lambda k: 4*(-1)**k/(2*k+1), [0,inf])

def pi_043():
    return nsum(lambda k: (3**k-1)/4**k*zeta(k+1), [1,inf])

def pi_044():
    return nsum(lambda k: 8/(2*k-1)**2, [1,inf])**0.5

def pi_045():
    return nsum(lambda k: 2*fac(k)/fac2(2*k+1), [0,inf])

def pi_046():
    return nsum(lambda k: fac(k)**2/fac(2*k+1), [0,inf])*3*sqrt(3)/2

def pi_047():
    return nsum(lambda k: fac(k)**2/(phi**(2*k+1)*fac(2*k+1)), [0,inf])*(5*sqrt(phi+2))/2

def pi_048():
    return nsum(lambda k: (4/(8*k+1)-2/(8*k+4)-1/(8*k+5)-1/(8*k+6))/16**k, [0,inf])

def pi_049():
    return 2/nsum(lambda k: (-1)**k*(4*k+1)*(fac2(2*k-1)/fac2(2*k))**3, [0,inf])

def pi_050():
    return nsum(lambda k: 72/(k*expm1(k*pi))-96/(k*expm1(2*pi*k))+24/(k*expm1(4*pi*k)), [1,inf])

def pi_051():
    return 1/nsum(lambda k: binomial(2*k,k)**3*(42*k+5)/2**(12*k+4), [0,inf])

def pi_052():
    return 4/nsum(lambda k: (-1)**k*(1123+21460*k)*fac2(2*k-1)*fac2(4*k-1)/(882**(2*k+1)*32**k*fac(k)**3), [0,inf])

def pi_053():
    return 9801/sqrt(8)/nsum(lambda k: fac(4*k)*(1103+26390*k)/(fac(k)**4*396**(4*k)), [0,inf])

def pi_054():
    return 426880*sqrt(10005)/nsum(lambda k: (-1)**k*fac(6*k)*(13591409+545140134*k)/(fac(k)**3*fac(3*k)*(640320**3)**k), [0,inf])

def pi_055():
    return 4/nsum(lambda k: (6*k+1)*rf(0.5,k)**3/(4**k*fac(k)**3), [0,inf])

def pi_056():
    return (ln(8)+sqrt(48*nsum(lambda m,n: (-1)**(m+n)/(m**2+n**2), [1,inf],[1,inf]) + 9*log(2)**2))/2

def pi_057():
    return -nsum(lambda x,y: (-1)**(x+y)/(x**2+y**2), [-inf,inf], [-inf,inf], ignore=True)/ln2

def pi_058():
    return 2*nsum(lambda k: sin(k)/k, [1,inf])+1

def pi_059():
    return quad(lambda x: 2/(x**2+1), [0,inf])

def pi_060():
    return quad(lambda x: exp(-x**2), [-inf,inf])**2

def pi_061():
    return 2*quad(lambda x: sqrt(1-x**2), [-1,1])

def pi_062():
    return chop(quad(lambda z: 1/(2j*z), [1,j,-1,-j,1]))

def pi_063():
    return 3*(4*log(2+sqrt(3))-quad(lambda x,y: 1/sqrt(1+x**2+y**2), [-1,1],[-1,1]))/2

def pi_064():
    return sqrt(8*quad(lambda x,y: 1/(1-(x*y)**2), [0,1],[0,1]))

def pi_065():
    return sqrt(6*quad(lambda x,y: 1/(1-x*y), [0,1],[0,1]))

def pi_066():
    return sqrt(6*quad(lambda x: x/expm1(x), [0,inf]))

def pi_067():
    return quad(lambda x: (16*x-16)/(x**4-2*x**3+4*x-4), [0,1])

def pi_068():
    return quad(lambda x: sqrt(x-x**2), [0,0.25])*24+3*sqrt(3)/4

def pi_069():
    return mpf(22)/7 - quad(lambda x: x**4*(1-x)**4/(1+x**2), [0,1])

def pi_070():
    return mpf(355)/113 - quad(lambda x: x**8*(1-x)**8*(25+816*x**2)/(1+x**2), [0,1])/3164

def pi_071():
    return 2*quadosc(lambda x: sin(x)/x, [0,inf], omega=1)

def pi_072():
    return 40*quadosc(lambda x: sin(x)**6/x**6, [0,inf], omega=1)/11

def pi_073():
    return e*quadosc(lambda x: cos(x)/(1+x**2), [-inf,inf], omega=1)

def pi_074():
    return 8*quadosc(lambda x: cos(x**2), [0,inf], zeros=lambda n: sqrt(n))**2

def pi_075():
    return 2*quadosc(lambda x: sin(exp(x)), [1,inf], zeros=ln)+2*si(e)

def pi_076():
    return exp(2*quad(loggamma, [0,1]))/2

def pi_077():
    return 2*nprod(lambda k: sec(pi/2**k), [2,inf])

def pi_078():
    s=lambda k: sqrt(0.5+s(k-1)/2) if k else 0; return 2/nprod(s, [1,inf])

def pi_079():
    s=lambda k: sqrt(2+s(k-1)) if k else 0; return limit(lambda k: sqrt(2-s(k))*2**(k+1), inf)

def pi_080():
    return 2*nprod(lambda k: (2*k)**2/((2*k-1)*(2*k+1)), [1,inf])

def pi_081():
    return 2*nprod(lambda k: (4*k**2)/(4*k**2-1), [1, inf])

def pi_082():
    return sqrt(6*ln(nprod(lambda k: exp(1/k**2), [1,inf])))

def pi_083():
    return nprod(lambda k: (k**2-1)/(k**2+1), [2,inf])/csch(pi)

def pi_084():
    return nprod(lambda k: (k**2-1)/(k**2+1), [2,inf])*sinh(pi)

def pi_085():
    return nprod(lambda k: (k**4-1)/(k**4+1), [2, inf])*(cosh(sqrt(2)*pi)-cos(sqrt(2)*pi))/sinh(pi)

def pi_086():
    return sinh(pi)/nprod(lambda k: (1-1/k**4), [2, inf])/4

def pi_087():
    return sinh(pi)/nprod(lambda k: (1+1/k**2), [2, inf])/2

def pi_088():
    return (exp(1+euler/2)/nprod(lambda n: (1+1/n)**n * exp(1/(2*n)-1), [1, inf]))**2/2

def pi_089():
    return 3*sqrt(2)*cosh(pi*sqrt(3)/2)**2*csch(pi*sqrt(2))/nprod(lambda k: (1+1/k+1/k**2)**2/(1+2/k+3/k**2), [1, inf])

def pi_090():
    return 2/e*nprod(lambda k: (1+2/k)**((-1)**(k+1)*k), [1,inf])

def pi_091():
    return limit(lambda k: 16**k/(k*binomial(2*k,k)**2), inf)

def pi_092():
    return limit(lambda x: 4*x*hyp1f2(0.5,1.5,1.5,-x**2), inf)

def pi_093():
    return 1/log(limit(lambda n: nprod(lambda k: pi/(2*atan(k)), [n,2*n]), inf),4)

def pi_094():
    return limit(lambda k: 2**(4*k+1)*fac(k)**4/(2*k+1)/fac(2*k)**2, inf)

def pi_095():
    return limit(lambda k: fac(k) / (sqrt(k)*(k/e)**k), inf)**2/2

def pi_096():
    return limit(lambda k: (-(-1)**k*bernoulli(2*k)*2**(2*k-1)/fac(2*k))**(-1/(2*k)), inf)

def pi_097():
    return limit(lambda k: besseljzero(1,k)/k, inf)

def pi_098():
    return 1/limit(lambda x: airyai(x)*2*x**0.25*exp(2*x**1.5/3), inf, exp=True)**2

def pi_099():
    return 1/limit(lambda x: airybi(x)*x**0.25*exp(-2*x**1.5/3), inf, exp=True)**2

def pi_100():
    return sqrt(nsum(lambda k: 1/k**2, [1,inf])*6)

def pi_101():
    return acos(-1)

def pi_102():
    return 4*nsum(lambda k: atan(1/fib(2*k+1)), [1,inf])

def pi_103():
    return quad(lambda x: 3*(x**2+2)/((x**2+1)*(x**2+4)), [0,inf])

def pi_104():
    return chop(log(-1)/j)

def pi_105():
    return extradps(mp.dps)(lambda: quad(lambda x: 1/sqrt(1-x**2),[-1,1]))()

if __name__ == "__main__":
    mp.dps = 50
    mp.pretty = True
    for fn in dir():
        if fn.startswith("pi_"):
            print(fn),
            f = globals()[fn]
            v = f()
            if abs(v-pi) > eps * 100:
                raise ValueError(str(v) + " is not even close to pi!")
            print(v)
