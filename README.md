# 314

The blog post [100 mpmath one-liners for pi](http://fredrikj.net/blog/2011/03/100-mpmath-one-liners-for-pi/),
posted on Pi Day 2011,
showed 100 different ways to compute pi to 50 digits with
the Python library mpmath. Here are four examples:

    16*acot(5)-4*acot(239)
    findroot(sin, 3)
    2*quad(lambda x: sqrt(1-x**2), [-1,1])
    limit(lambda k: fac(k) / (sqrt(k)*(k/e)**k), inf)**2/2

The goal of this repository is to collect such computable pi formulas
in testable form, translated to different math software.

For the numbered formulas in mathematical notation, see formulas.html:
https://rawgit.com/fredrik-johansson/314/master/formulas.html

Numerical software should be able to reproduce the value of pi
with reasonable precision by evaluating the given formula in a
natural way, and symbolic software should
be able to simplify the given symbolic formula all the way
to the atomic symbol pi, perhaps with some explicit guidance.

So far, we have several formulas for the following software:

* [mpmath](https://github.com/fredrik-johansson/mpmath) (numerical)
* [SymPy](https://github.com/sympy/sympy) (symbolic)
* [Pari/GP](http://pari.math.u-bordeaux.fr/) (numerical)

New formulas are welcome. If you have some Russian-made table of integrals, series and products sitting on your bookshelf, now is the time to open it for inspiration! Added pi programs should be short, but don't literally have to be one-liners; a few lines of code are fine, especially in languages where one-liners are impractical.
