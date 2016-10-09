/* numbers.c - standard procedures 6.5 */

#include "sxm.h"
#include "sxwrite.h"
#include "sxread.h"
#include "define.h"
#include "sxmath.h"
#include "os.h"     /* for osrandomize only */

/*  SXM's numerical types: fixnum, flonum
 *  exact    <=>  fixnum
 *  inexact  <=>  flonum
 *  integer  <=>  fixnum | (flonum : round (f) == f)
 *  rational <=>  fixnum | flonum
 *  real     <=>  fixnum | flonum
 *  complex  <=>  fixnum | flonum
 *  number   <=>  fixnum | flonum
 */

/*#| (number? obj) |#*/
/*#| (complex? obj) |#*/
/*#| (real? obj) |#*/
/*#| (rational? obj) |#*/
DEFINE_INITIAL_BINDING("number?", sp_numberp)
DEFINE_INITIAL_BINDING("complex?", sp_numberp)
DEFINE_INITIAL_BINDING("real?", sp_numberp)
DEFINE_INITIAL_BINDING("rational?", sp_numberp)
DEFINE_PROCEDURE(sp_numberp)
{
  SOBJ arg = xlonearg();
  return cvbool(numberp(arg));
}

/*#| (integer? obj) |#*/
DEFINE_INITIAL_BINDING("integer?", sp_integerp)
DEFINE_PROCEDURE(sp_integerp)
{
  SOBJ arg = xlonearg();
  if (fixp(arg))
    return so_true;
  else if (floatp(arg))
    return cvbool(numFloatIsInt(getflonum(arg)));
  else
    return so_false;
}

/*#| (fixnum? obj) |#*/
/*#| (exact-integer? obj) |#*/
DEFINE_INITIAL_BINDING("fixnum?", sp_fixnump)
DEFINE_INITIAL_BINDING("exact-integer?", sp_fixnump)
DEFINE_PROCEDURE(sp_fixnump)
{
  SOBJ arg = xlonearg();
  return cvbool(fixp(arg));
}

/*#| (exact-nonnegative-integer? obj) |#*/
DEFINE_INITIAL_BINDING("exact-nonnegative-integer?", sp_nnegfixnump)
DEFINE_PROCEDURE(sp_nnegfixnump)
{
  SOBJ arg = xlonearg();
  return cvbool(fixp(arg) && getfixnum(arg) >= 0);
}

/*#| (flonum? obj) |#*/
DEFINE_INITIAL_BINDING("flonum?", sp_flonump)
DEFINE_PROCEDURE(sp_flonump)
{
  SOBJ arg = xlonearg();
  return cvbool(floatp(arg));
}

/*#| (exact? z) |#*/
DEFINE_INITIAL_BINDING("exact?", sp_exactp)
DEFINE_PROCEDURE(sp_exactp)
{
  SOBJ arg = xlganumber();
  xllastarg();
  return cvbool(fixp(arg));
}

/*#| (inexact? z) |#*/
DEFINE_INITIAL_BINDING("inexact?", sp_inexactp)
DEFINE_PROCEDURE(sp_inexactp)
{
  SOBJ arg = xlganumber();
  xllastarg();
  return cvbool(floatp(arg));
}


/*#| (most-positive-fixnum) |#*/
DEFINE_INITIAL_BINDING("most-positive-fixnum", sp_mostposfixnum)
DEFINE_PROCEDURE(sp_mostposfixnum)
{
  xllastarg();
  return cvfixnum(FIXMAX);
}

/*#| (most-negative-fixnum) |#*/
DEFINE_INITIAL_BINDING("most-negative-fixnum", sp_mostnegfixnum)
DEFINE_PROCEDURE(sp_mostnegfixnum)
{
  xllastarg();
  return cvfixnum(FIXMIN);
}


/* comparison functions */
/*#| (< x1 x2 x3 ...) |#*/
DEFINE_INITIAL_BINDING("<", sp_lss)
DEFINE_PROCEDURE(sp_lss)
{
  return numOrdered(numLessP);
}

/*#| (<= x1 x2 x3 ...) |#*/
DEFINE_INITIAL_BINDING("<=", sp_leq)
DEFINE_PROCEDURE(sp_leq)
{
  return numOrdered(numLessOrEqualP);
}

/*#| (= x1 x2 x3 ...) |#*/
DEFINE_INITIAL_BINDING("=", sp_eql)
DEFINE_PROCEDURE(sp_eql)
{
  return numOrdered(numEqualP);
}

/*#| (>= x1 x2 x3 ...) |#*/
DEFINE_INITIAL_BINDING(">=", sp_geq)
DEFINE_PROCEDURE(sp_geq)
{
  return numOrdered(numGreaterOrEqualP);
}

/*#| (> x1 x2 x3 ...) |#*/
DEFINE_INITIAL_BINDING(">", sp_gtr)
DEFINE_PROCEDURE(sp_gtr)
{
  return numOrdered(numGreaterP);
}


/*#| (zero? z) |#*/
DEFINE_INITIAL_BINDING("zero?", sp_zerop)
DEFINE_PROCEDURE(sp_zerop)
{
  return numPredicate(numZeroP);
}

/*#| (positive? x) |#*/
DEFINE_INITIAL_BINDING("positive?", sp_positivep)
DEFINE_PROCEDURE(sp_positivep)
{
  return numPredicate(numPositiveP);
}

/*#| (negative? x) |#*/
DEFINE_INITIAL_BINDING("negative?", sp_negativep)
DEFINE_PROCEDURE(sp_negativep)
{
  return numPredicate(numNegativeP);
}

/*#| (nonpositive? x) |#*/
DEFINE_INITIAL_BINDING("nonpositive?", sp_nonpositivep)
DEFINE_PROCEDURE(sp_nonpositivep)
{
  return cvbool(numPredicate(numPositiveP) == so_false);
}

/*#| (nonnegative? x) |#*/
DEFINE_INITIAL_BINDING("nonnegative?", sp_nonnegativep)
DEFINE_PROCEDURE(sp_nonnegativep)
{
  return cvbool(numPredicate(numNegativeP) == so_false);
}


/*#| (odd? n) |#*/
DEFINE_INITIAL_BINDING("odd?", sp_oddp)
DEFINE_PROCEDURE(sp_oddp)
{
  return numPredicate(numOddP);
}

/*#| (even? n) |#*/
DEFINE_INITIAL_BINDING("even?", sp_evenp)
DEFINE_PROCEDURE(sp_evenp)
{
  return numPredicate(numEvenP);
}


/*#| (max x1 x2 ...) |#*/
DEFINE_INITIAL_BINDING("max", sp_max)
DEFINE_PROCEDURE(sp_max)
{
  return numReduce(numMax);
}

/*#| (min x1 x2 ...) |#*/
DEFINE_INITIAL_BINDING("min", sp_min)
DEFINE_PROCEDURE(sp_min)
{
  return numReduce(numMin);
}

/*#| (+ z1 ...) |#*/
DEFINE_INITIAL_BINDING("+", sp_add)
DEFINE_PROCEDURE(sp_add)
{
  return numReduceInit(0, numAdd);
}

/*#| (- z) |#*/
/*#| (- z1 z2 ...) |#*/
DEFINE_INITIAL_BINDING("-", sp_sub)
DEFINE_PROCEDURE(sp_sub)
{
  if (PROC_ARGC() == 1)
    return numUnary(numAddInverse);
  else
    return numReduce(numSubtract);
}

/*#| (1+ z) |#*/
DEFINE_INITIAL_BINDING("1+", sp_add1)
DEFINE_PROCEDURE(sp_add1)
{
  return numUnary(numAdd1);
}

/*#| (-1+ z) |#*/
DEFINE_INITIAL_BINDING("-1+", sp_sub1)
DEFINE_PROCEDURE(sp_sub1)
{
  return numUnary(numSubtract1);
}

/*#| (* z1 ...) |#*/
DEFINE_INITIAL_BINDING("*", sp_mul)
DEFINE_PROCEDURE(sp_mul)
{
  return numReduceInit(1, numMultiply);
}

/*#| (/ z) |#*/
/*#| (/ z1 z2 ...) |#*/
DEFINE_INITIAL_BINDING("/", sp_div)
DEFINE_PROCEDURE(sp_div)
{
  if (PROC_ARGC() == 1)
    return numUnary(numMulInverse);
  else
    return numReduce(numDivide);
}

/*#| (abs x) |#*/
DEFINE_INITIAL_BINDING("abs", sp_abs)
DEFINE_PROCEDURE(sp_abs)
{
  return numUnary(numAbs);
}


/*#| (quotient n1 n2) |#*/
DEFINE_INITIAL_BINDING("quotient", sp_quo)
DEFINE_PROCEDURE(sp_quo)
{
  return numBinary(numQuotient);
}

/*#| (remainder n1 n2) |#*/
DEFINE_INITIAL_BINDING("remainder", sp_rem)
DEFINE_PROCEDURE(sp_rem)
{
  return numBinary(numRemainder);
}

/*#| (divide n1 n2) |#*/
DEFINE_INITIAL_BINDING("divide", sp_idiv)
DEFINE_PROCEDURE(sp_idiv)
{
  number_t x, y;
  SOBJ first = xlgetarg();
  SOBJ second = xlgetarg();
  xllastarg();
  numParseSOBJ(first, &x);
  numParseSOBJ(second, &y);
  numIntegerDivide(&x, &y);
  PUSH_ARG(numMakeSOBJ(&y));
  PUSH_ARG(numMakeSOBJ(&x));
  PROC_RETURN_VALUES(2);
}

/*#| (modulo n1 n2) |#*/
DEFINE_INITIAL_BINDING("modulo", sp_mod)
DEFINE_PROCEDURE(sp_mod)
{
  return numBinary(numModulo);
}


/*#| (gcd n1 ...) |#*/
DEFINE_INITIAL_BINDING("gcd", sp_gcd)
DEFINE_PROCEDURE(sp_gcd)
{
  return numReduceInit(0, numGCD);
}

/*#| (lcm n1 ...) |#*/
DEFINE_INITIAL_BINDING("lcm", sp_lcm)
DEFINE_PROCEDURE(sp_lcm)
{
  return numReduceInit(1, numLCM);
}


/*#| (numerator n) |#*/
DEFINE_INITIAL_BINDING("numerator", sp_numerator)
DEFINE_PROCEDURE(sp_numerator)
{
  SOBJ n = xlganumber();
  xllastarg();
  if (!fixp(n)) sxErr(T("numerator is not supported"), n);
  return n;
}

/*#| (denominator n) |#*/
DEFINE_INITIAL_BINDING("denominator", sp_denominator)
DEFINE_PROCEDURE(sp_denominator)
{
  SOBJ n = xlganumber();
  xllastarg();
  if (!fixp(n)) sxErr(T("denominator is not supported"), n);
  return so_fix1;
}


/*#| (floor x) |#*/
DEFINE_INITIAL_BINDING("floor", sp_floor)
DEFINE_PROCEDURE(sp_floor)
{
  return numUnary(numFloor);
}

/*#| (ceiling x) |#*/
DEFINE_INITIAL_BINDING("ceiling", sp_ceiling)
DEFINE_PROCEDURE(sp_ceiling)
{
  return numUnary(numCeiling);
}

/*#| (truncate x) |#*/
DEFINE_INITIAL_BINDING("truncate", sp_truncate)
DEFINE_PROCEDURE(sp_truncate)
{
  return numUnary(numTruncate);
}

/*#| (round x) |#*/
DEFINE_INITIAL_BINDING("round", sp_round)
DEFINE_PROCEDURE(sp_round)
{
  return numUnary(numRound);
}

/*#| (rationalize n d) |#*/
DEFINE_INITIAL_BINDING("rationalize", sp_rationalize)
DEFINE_PROCEDURE(sp_rationalize)
{
  SOBJ n = xlganumber();
  /* SOBJ d = */ xlganumber();
  xllastarg();
  if (!fixp(n)) sxErr(T("rationalize is not supported"), n);
  return n;
}

/*#| (exp z) |#*/
DEFINE_INITIAL_BINDING("exp", sp_exp)
DEFINE_PROCEDURE(sp_exp)
{
  return numUnary(numExp);
}

/*#| (log z) |#*/
DEFINE_INITIAL_BINDING("log", sp_log)
DEFINE_PROCEDURE(sp_log)
{
  return numUnary(numLog);
}

/*#| (sin z) |#*/
DEFINE_INITIAL_BINDING("sin", sp_sin)
DEFINE_PROCEDURE(sp_sin)
{
  return numUnary(numSin);
}

/*#| (cos z) |#*/
DEFINE_INITIAL_BINDING("cos", sp_cos)
DEFINE_PROCEDURE(sp_cos)
{
  return numUnary(numCos);
}

/*#| (tan z) |#*/
DEFINE_INITIAL_BINDING("tan", sp_tan)
DEFINE_PROCEDURE(sp_tan)
{
  return numUnary(numTan);
}

/*#| (asin z) |#*/
DEFINE_INITIAL_BINDING("asin", sp_asin)
DEFINE_PROCEDURE(sp_asin)
{
  return numUnary(numAsin);
}

/*#| (acos z) |#*/
DEFINE_INITIAL_BINDING("acos", sp_acos)
DEFINE_PROCEDURE(sp_acos)
{
  return numUnary(numAcos);
}

/*#| (atan z) |#*/
/*#| (atan y x) |#*/
DEFINE_INITIAL_BINDING("atan", sp_atan)
DEFINE_PROCEDURE(sp_atan)
{
  if (PROC_ARGC() == 1)
    return numUnary(numAtan);
  else
    return numBinary(numAtan2);
}

/*#| (sinh z) |#*/
DEFINE_INITIAL_BINDING("sinh", sp_sinh)
DEFINE_PROCEDURE(sp_sinh)
{
  return numUnary(numSinh);
}

/*#| (cosh z) |#*/
DEFINE_INITIAL_BINDING("cosh", sp_cosh)
DEFINE_PROCEDURE(sp_cosh)
{
  return numUnary(numCosh);
}

/*#| (tanh z) |#*/
DEFINE_INITIAL_BINDING("tanh", sp_tanh)
DEFINE_PROCEDURE(sp_tanh)
{
  return numUnary(numTanh);
}


/*#| (sqrt z) |#*/
DEFINE_INITIAL_BINDING("sqrt", sp_sqrt)
DEFINE_PROCEDURE(sp_sqrt)
{
  return numUnary(numSqrt);
}

/*#| (expt z1 z2) |#*/
DEFINE_INITIAL_BINDING("expt", sp_expt)
DEFINE_PROCEDURE(sp_expt)
{
  return numBinary(numExpt);
}

/*#| (make-rectangular r i) |#*/
DEFINE_INITIAL_BINDING("make-rectangular", sp_makerect)
DEFINE_PROCEDURE(sp_makerect)
{
  SOBJ r = xlganumber();
  SOBJ i = xlganumber();
  xllastarg();
  if (i == so_fix0) return r;
  if (floatp(i) && getflonum(i) == 0.0) {
    if (floatp(r)) return r;
    if (fixp(r)) return cvflonum((FLOTYPE)getfixnum(r));
  } 
  return sxErr(T("imag part not supported"), i);
}

/*#| (make-polar m a) |#*/
DEFINE_INITIAL_BINDING("make-polar", sp_makepolar)
DEFINE_PROCEDURE(sp_makepolar)
{
  SOBJ m = xlganumber();
  SOBJ a = xlganumber();
  xllastarg();
  if (a == so_fix0) return m;
  if (floatp(a) && getflonum(a) == 0.0) {
    if (floatp(m)) return m;
    if (fixp(m)) return cvflonum((FLOTYPE)getfixnum(m));
  } 
  return sxErr(T("angle not supported"), a);
}

/*#| (real-part x) |#*/
DEFINE_INITIAL_BINDING("real-part", sp_realpart)
DEFINE_PROCEDURE(sp_realpart)
{
  SOBJ x = xlganumber();
  xllastarg();
  if (!fixp(x) && !floatp(x)) sxErr(T("real-part not supported"), x);
  return x;
}

/*#| (imag-part x) |#*/
DEFINE_INITIAL_BINDING("imag-part", sp_imagpart)
DEFINE_PROCEDURE(sp_imagpart)
{
  SOBJ x = xlganumber();
  xllastarg();
  if (!fixp(x) && !floatp(x)) sxErr(T("imag-part not supported"), x);
  if (fixp(x)) return so_fix0;
  return cvflonum(0.0);
}

/*#| (magnitude x) |#*/
DEFINE_INITIAL_BINDING("magnitude", sp_magnitude)
DEFINE_PROCEDURE(sp_magnitude)
{
  SOBJ x = xlganumber();
  xllastarg();
  if (!fixp(x) && !floatp(x)) sxErr(T("magnitude not supported"), x);
  PUSH_ARG(x);
  PROC_GOTO(sp_abs, 1);
}

/*#| (angle x) |#*/
DEFINE_INITIAL_BINDING("angle", sp_angle)
DEFINE_PROCEDURE(sp_angle)
{
  SOBJ x = xlganumber();
  xllastarg();
  if (fixp(x)) {
    FIXTYPE i = getfixnum(x);
    if (i > 0) return so_fix0;
    if (i < 0) return cvflonum(3.141592653589793238462643);
  } else if (floatp(x)) {
    FLOTYPE f = getflonum(x);
    if (f > 0) return cvflonum(0.0);
    if (f < 0) return cvflonum(3.141592653589793238462643);
  }
  return sxErr(T("angle not supported"), x);
}


/*#| (exact->inexact z) |#*/
/*#| (fixnum->flonum n) |#*/
DEFINE_INITIAL_BINDING("exact->inexact", sp_2inexact)
DEFINE_INITIAL_BINDING("fixnum->flonum", sp_2inexact)
DEFINE_PROCEDURE(sp_2inexact)
{
  return numUnary(numToInexact);
}

/*#| (inexact->exact z) |#*/
/*#| (flonum->fixnum f) |#*/
DEFINE_INITIAL_BINDING("inexact->exact", sp_2exact)
DEFINE_INITIAL_BINDING("flonum->fixnum", sp_2exact)
DEFINE_PROCEDURE(sp_2exact)
{
  return numUnary(numToExact);
}

/*#| (fxlogand n1 n2 ...) |#*/
DEFINE_INITIAL_BINDING("fxlogand", sp_fxlogand)
DEFINE_PROCEDURE(sp_fxlogand)
{
  return numReduce(numBitwiseAnd);
}

/*#| (fxlogor n1 n2 ...) |#*/
DEFINE_INITIAL_BINDING("fxlogor", sp_fxlogor)
DEFINE_PROCEDURE(sp_fxlogor)
{
  return numReduce(numBitwiseIor);
}

/*#| (fxlogxor n1 n2 ...) |#*/
DEFINE_INITIAL_BINDING("fxlogxor", sp_fxlogxor)
DEFINE_PROCEDURE(sp_fxlogxor)
{
  return numReduce(numBitwiseXor);
}

/*#| (fxlognot n) |#*/
DEFINE_INITIAL_BINDING("fxlognot", sp_fxlognot)
DEFINE_PROCEDURE(sp_fxlognot)
{
  return numUnary(numBitwiseNot);
}

/*#| (fxsll n m) |#*/
DEFINE_INITIAL_BINDING("fxsll", sp_fxsll)
DEFINE_PROCEDURE(sp_fxsll)
{
  return numBinary(numBitwiseSll);
}

/*#| (fxsrl n m) |#*/
DEFINE_INITIAL_BINDING("fxsrl", sp_fxsrl)
DEFINE_PROCEDURE(sp_fxsrl)
{
  return numBinary(numBitwiseSrl);
}

/*#| (fxsra n m) |#*/
DEFINE_INITIAL_BINDING("fxsra", sp_fxsra)
DEFINE_PROCEDURE(sp_fxsra)
{
  return numBinary(numBitwiseSra);
}

/*#| (random n) |#*/
DEFINE_INITIAL_BINDING("random", sp_random)
DEFINE_PROCEDURE(sp_random)
{
  return numUnary(numRandom);
}

/*#| (randomize) |#*/
DEFINE_INITIAL_BINDING("randomize", sp_randomize)
DEFINE_PROCEDURE(sp_randomize)
{
  osrandomize();
  return so_void;
}

/*#| (string->number string) |#*/
/*#| (string->number string radix) |#*/
DEFINE_INITIAL_BINDING("string->number", sp_str2num)
DEFINE_PROCEDURE(sp_str2num)
{
  SOBJ num; FIXTYPE radix;
  SOBJ str = xlgastring();
  if (optarg()) {
    SOBJ rad = xlgafixnum();
    radix = getfixnum(rad);
    switch (radix) {
      case 2: case 8: case 10: case 16:  break;
      default: sxErr(T("radix must be 2,8,10,16"), rad);
    }
  } else
    radix = 10;
  xllastarg();
  /* call sxParseNumber with negative (implicit) radix */
  return sxParseNumber(getstring(str), &num, -(int)radix) ? num : so_false;
}

/*#| (number->string number) |#*/
/*#| (number->string number radix) |#*/
DEFINE_INITIAL_BINDING("number->string", sp_num2str)
DEFINE_PROCEDURE(sp_num2str)
{
  SOBJ num = xlgetarg();
  SOBJ rad = optarg() ? xlgasfixnum() : cvsfixnum(10);
  int radix = (int)getsfixnum(rad);
  xllastarg();

  if (fixp(num))
    return cvstring(sxUnparseFixnum(getfixnum(num), radix));
  else if (floatp(num)) {
    if (radix != 10) sxErr(T("bad radix for inexact number"), rad);
    return cvstring(sxUnparseFlonum(getflonum(num)));
  } else
    never_returns(xlreqnumber(num));
}

/*#| (valid-index? idx size) |#*/
DEFINE_INITIAL_BINDING("valid-index?", sp_validindexp)
DEFINE_PROCEDURE(sp_validindexp)
{
  SOBJ idx = xlgetarg();
  SOBJ size = xlgafixnum();
  FIXTYPE sz = getfixnum(size);
  xllastarg();
  if (fixp(idx)) {
    FIXTYPE x = getfixnum(idx);
    return cvbool(x >= 0 && x < sz);
  }
  return so_false;
}
