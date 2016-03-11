\p 50

pi_000() = Pi;

pi_002() = 4*atan(1);

pi_003() = 16*atan(1/5)-4*atan(1/239);

pi_005() = 2*I*log((1-I)/(1+I));

pi_008() = gamma(1/2)^2;

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
check("pi_005",pi_005);
check("pi_008",pi_008);
check("pi_100",pi_100);

quit();


