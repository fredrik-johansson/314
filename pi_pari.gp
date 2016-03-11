\p 50

pi_000() = Pi;

pi_002() = 4*atan(1);

pi_003() = 16*atan(1/5)-4*atan(1/239);

pi_004() = 48*atan(1/49)+128*atan(1/57)-20*atan(1/239)+48*atan(1/110443);

pi_005() = 2*I*log((1-I)/(1+I));

pi_006() = -2*I*asinh(I);

pi_008() = gamma(1/2)^2;

pi_011() = solve(x=3,4,sin(x));

pi_012() = solve(x=1,2,cos(x))*2;

pi_027() = sqrt(6*zeta(2));

pi_059() = intnum(x=0,[1],2/(x^2+1));

pi_061() = 2*intnum(x=-1,1,sqrt(1-x^2));

pi_100() = sqrt(sumpos(k=1,1/k^2)*6);

check(s,f) = {
    my (v);
    v = f();
    if (abs(v-Pi) > 1e-45, print(s," ",v," is not even close to pi!"); quit(););
    print(s," ",v);
}

check("pi_000",pi_000);
check("pi_002",pi_002);
check("pi_003",pi_003);
check("pi_004",pi_004);
check("pi_005",pi_005);
check("pi_006",pi_006);
check("pi_008",pi_008);
check("pi_011",pi_011);
check("pi_012",pi_012);
check("pi_027",pi_027);
check("pi_059",pi_059);
check("pi_061",pi_061);
check("pi_100",pi_100);

quit();

