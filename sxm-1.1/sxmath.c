/* sxmath.c - math procedures */

#include <math.h>

#include "sxm.h"
#include "sxmath.h"
#include "os.h"     /* for osrand only */

/* error messages */
static tchar_t* err_reqint = T("not an integer");
static tchar_t* err_reqfix = T("not an exact integer");
static tchar_t* err_div0 = T("division by zero");
static tchar_t* err_sqrtneg = T("SQRT of a negative number");

/* shortcuts for number_t members access */
#define fix xd_num.xn_fix
#define flo xd_num.xn_flo


/* integer float predicates:
 * internal macro and external function
 */

#define fisint(x) ((x) == floor(x))

bool_t numFloatIsInt (FLOTYPE x)
{
  return fisint(x);
}

/************** Numerical Converters **************/

FLOTYPE numGetFloat (number_t* px)
{
  if (px->isfix) return (FLOTYPE)px->fix;
  return px->flo;
}

void numSetFloat (number_t* px, FLOTYPE x)
{
  px->isfix = FALSE;
  px->flo = x;
}

void numSetFix (number_t* px, FIXTYPE x)
{
  px->isfix = TRUE;
  px->fix = x;
}

void numToFloat (number_t* px)
{
  if (px->isfix) {
    px->isfix = FALSE;
    px->flo = (FLOTYPE)px->fix;
  }
}

/************** Numerical Predicates **************/

int numZeroP (number_t* px)
{
  if (px->isfix) return (px->fix == 0);
  return (px->flo == 0.0);
}

int numPositiveP (number_t* px)
{
  if (px->isfix) return (px->fix > 0);
  return (px->flo > 0.0);
}

int numNegativeP (number_t* px)
{
  if (px->isfix) return (px->fix < 0);
  return (px->flo < 0.0);
}

int numOddP (number_t* px)
{
  if (px->isfix) return (px->fix % 2 != 0);
  else {
    FLOTYPE x = px->flo;
    if (!fisint(x)) sxErr(err_reqint, cvflonum(px->flo));
    x /= 2.0;
    return !fisint(x);
  }
}

int numEvenP (number_t* px)
{
  if (px->isfix) return (px->fix % 2 == 0);
  else {
    FLOTYPE x = px->flo;
    if (!fisint(x)) sxErr(err_reqint, cvflonum(px->flo));
    x /= 2.0;
    return fisint(x);
  }
}


/*************** Numerical Compare ****************/

int numLessP (number_t* px, number_t* py)
{
  if (px->isfix) {
    if (py->isfix)  return (px->fix < py->fix);
    else return ((FLOTYPE)px->fix < py->flo);
  } else {
    if (py->isfix)  return (px->flo < (FLOTYPE)py->fix);
    else return (px->flo < py->flo);
  }
}

int numLessOrEqualP (number_t* px, number_t* py)
{
  if (px->isfix) {
    if (py->isfix) return (px->fix <= py->fix);
    else return ((FLOTYPE)px->fix <= py->flo);
  } else {
    if (py->isfix)  return (px->flo <= (FLOTYPE)py->fix);
    else return (px->flo <= py->flo);
  }
}

int numEqualP (number_t* px, number_t* py)
{
  if (px->isfix) {
    if (py->isfix) return (px->fix == py->fix);
    else return ((FLOTYPE)px->fix == py->flo);
  } else {
    if (py->isfix)  return (px->flo == (FLOTYPE)py->fix);
    else return (px->flo == py->flo);
  }
}

int numGreaterOrEqualP (number_t* px, number_t* py)
{
  if (px->isfix) {
    if (py->isfix)  return (px->fix >= py->fix);
    else return ((FLOTYPE)px->fix >= py->flo);
  } else {
    if (py->isfix)  return (px->flo >= (FLOTYPE)py->fix);
    else return (px->flo >= py->flo);
  }
}

int numGreaterP (number_t* px, number_t* py)
{
  if (px->isfix) {
    if (py->isfix)  return (px->fix > py->fix);
    else return ((FLOTYPE)px->fix > py->flo);
  } else {
    if (py->isfix)  return (px->flo > (FLOTYPE)py->fix);
    else return (px->flo > py->flo);
  }
}


/****** Numerical Unary Ops (result in *px) *******/

void numAddInverse (number_t* px)
{
  if (px->isfix) px->fix = - px->fix;
  else px->flo = - px->flo;
}

void numAdd1 (number_t* px)
{
  if (px->isfix) px->fix += 1;
  else px->flo += 1.0;
}

void numSubtract1 (number_t* px)
{
  if (px->isfix) px->fix -= 1;
  else px->flo -= 1.0;
}

void numMulInverse (number_t* px)
{
  FLOTYPE fx;
  if (px->isfix) {
    FIXTYPE xx = px->fix;
    if (xx == 1 || xx == -1) return; /* identity case */
    fx = (FLOTYPE) xx;
    px->isfix = FALSE;
  } else {
    fx = px->flo;
  }
  if (fx == 0.0) sxFail(err_div0);
  px->flo = (1.0 / fx);
}

void numAbs (number_t* px)
{
  if (px->isfix) {
    if (px->fix < 0) px->fix = - px->fix;
  } else
    px->flo = fabs(px->flo);
}

void numFloor (number_t* px)
{
  if (!px->isfix) px->flo = floor(px->flo);
}

void numCeiling (number_t* px)
{
  if (!px->isfix) px->flo = ceil(px->flo);
}

void numTruncate (number_t* px)
{
  if (!px->isfix) {
    double ipart;
    modf(px->flo,  &ipart);
    px->flo = ipart;
  }
}

void numRound (number_t* px)
{
  if (!px->isfix) {
   FLOTYPE x = px->flo;
   FLOTYPE y = floor(x);
   FLOTYPE z = x - y;
   if (z == 0.5) px->flo = (((FIXTYPE)y & 1) != 0) ? y + 1 : y;
   else px->flo = (z < 0.5) ? y : y + 1;
  }
}

void numExp (number_t* px)
{
  FLOTYPE fx;
  if (px->isfix) {
    if (px->fix == 0) { px->fix = 1; return; }
    px->isfix = FALSE;
    fx = (FLOTYPE) px->fix;
  } else {
    fx = px->flo;
  }
  px->flo = exp(fx);
}

void numLog (number_t* px)
{
  FLOTYPE fx;
  if (px->isfix) {
    if (px->fix == 1) { px->fix = 0; return; }
    px->isfix = FALSE;
    fx = (FLOTYPE) px->fix;
  } else {
    fx = px->flo;
  }
  px->flo = log(fx);
}

void numSin (number_t* px)
{
  FLOTYPE fx;
  if (px->isfix) {
    if (px->fix == 0) return;
    px->isfix = FALSE;
    fx = (FLOTYPE) px->fix;
  } else {
    fx = px->flo;
  }
  px->flo = sin(fx);
}

void numCos (number_t* px)
{
  FLOTYPE fx;
  if (px->isfix) {
    if (px->fix == 0) { px->fix = 1; return; }
    px->isfix = FALSE;
    fx = (FLOTYPE) px->fix;
  } else {
    fx = px->flo;
  }
  px->flo = cos(fx);
}

void numTan (number_t* px)
{
  FLOTYPE fx;
  if (px->isfix) {
    if (px->fix == 0) return;
    px->isfix = FALSE;
    fx = (FLOTYPE) px->fix;
  } else {
    fx = px->flo;
  }
  px->flo = tan(fx);
}

void numAsin (number_t* px)
{
  FLOTYPE fx;
  if (px->isfix) {
    if (px->fix == 0) return;
    px->isfix = FALSE;
    fx = (FLOTYPE) px->fix;
  } else {
    fx = px->flo;
  }
  px->flo = asin(fx);
}

void numAcos (number_t* px)
{
  FLOTYPE fx;
  if (px->isfix) {
    if (px->fix == 1) { px->fix = 0; return; }
    px->isfix = FALSE;
    fx = (FLOTYPE) px->fix;
  } else {
    fx = px->flo;
  }
  px->flo = acos(fx);
}

void numAtan (number_t* px)
{
  FLOTYPE fx;
  if (px->isfix) {
    if (px->fix == 0) return;
    px->isfix = FALSE;
    fx = (FLOTYPE) px->fix;
  } else {
    fx = px->flo;
  }
  px->flo = atan(fx);
}

void numSinh (number_t* px)
{
  FLOTYPE fx;
  if (px->isfix) {
    if (px->fix == 0) return;
    px->isfix = FALSE;
    fx = (FLOTYPE) px->fix;
  } else {
    fx = px->flo;
  }
  px->flo = sinh(fx);
}

void numCosh (number_t* px)
{
  FLOTYPE fx;
  if (px->isfix) {
    if (px->fix == 0) { px->fix = 1; return; }
    px->isfix = FALSE;
    fx = (FLOTYPE) px->fix;
  } else {
    fx = px->flo;
  }
  px->flo = cosh(fx);
}

void numTanh (number_t* px)
{
  FLOTYPE fx;
  if (px->isfix) {
    if (px->fix == 0) return;
    px->isfix = FALSE;
    fx = (FLOTYPE) px->fix;
  } else {
    fx = px->flo;
  }
  px->flo = tanh(fx);
}


void numSqrt (number_t* px)
{
  FLOTYPE fx;
  if (px->isfix) {
    FIXTYPE xx = px->fix;
    if (xx < 0) sxErr(err_sqrtneg, cvfixnum(xx));
    fx = sqrt((FLOTYPE)xx);
    xx = (FIXTYPE)fx;
    if (xx * xx == px->fix)
      px->fix = xx; /* exact! */
    else {
      px->isfix = FALSE; px->flo = fx;
    }
  } else {
    fx = px->flo;
    if (fx < 0.0) sxErr(err_sqrtneg, cvflonum(fx));
    px->flo = sqrt(fx);
  }
}


void numBitwiseNot (number_t* px)
{
  if (!px->isfix) sxErr(err_reqfix, cvflonum(px->flo));
  px->fix = ~px->fix;
}

bool_t numTryToExact (number_t* px)
{
  if (!px->isfix) {
    FLOTYPE x = px->flo;
    bool_t ok = fisint(x);
    px->fix = (FIXTYPE)x;
    px->isfix = TRUE;
    return ok;
  }
  return TRUE;
}

bool_t numTryToInexact (number_t* px)
{
  if (px->isfix) {
    px->flo = (FLOTYPE)px->fix;
    px->isfix = FALSE;
  }
  return TRUE;
}

void numToExact (number_t* px)
{
  numTryToExact(px);
}

void numToInexact (number_t* px)
{
  numTryToInexact(px);
}

void numRandom (number_t* px)
{
  if (px->isfix)
    px->fix = (FIXTYPE)osrand((int)(px->fix));
  else
    px->flo *= osrand(30000) / 30000.0;
}


/****** Numerical Binary Ops (result in *px) ******/

void numMin (number_t* px, number_t* py)
{
  if (px->isfix && py->isfix) {
    if (px->fix > py->fix) px->fix = py->fix;
  } else {
    FLOTYPE fx = numGetFloat(px);
    FLOTYPE fy = numGetFloat(py);
    numSetFloat(px, fx < fy ? fx : fy);
  }
}

void numMax (number_t* px, number_t* py)
{
  if (px->isfix && py->isfix) {
    if (px->fix < py->fix) px->fix = py->fix;
  } else {
    FLOTYPE fx = numGetFloat(px);
    FLOTYPE fy = numGetFloat(py);
    numSetFloat(px, fx > fy ? fx : fy);
  }
}

void numAdd (number_t* px, number_t* py)
{
  if (px->isfix && py->isfix) {
    px->fix = px->fix + py->fix;
  } else {
    FLOTYPE fx = numGetFloat(px);
    FLOTYPE fy = numGetFloat(py);
    numSetFloat(px, fx + fy);
  }
}

void numSubtract (number_t* px, number_t* py)
{
  if (px->isfix && py->isfix) {
    px->fix = px->fix - py->fix;
  } else {
    FLOTYPE fx = numGetFloat(px);
    FLOTYPE fy = numGetFloat(py);
    numSetFloat(px, fx - fy);
  }
}

void numMultiply (number_t* px, number_t* py)
{
  if (px->isfix) {
    FIXTYPE xx = px->fix;
    if (xx == 0) return;
    if (py->isfix) { px->fix = xx * py->fix; return; }
    numSetFloat(px, ((FLOTYPE)xx) * py->flo);
  } else if (py->isfix) {
    FIXTYPE xy = py->fix;
    if (xy == 0) { numSetFix(px, 0); return; }
    px->flo *= (FLOTYPE)xy;
  } else {
    px->flo *= py->flo;
  }
}

void numDivide (number_t* px, number_t* py)
{
  if (px->isfix && py->isfix) {
    FIXTYPE xx = px->fix;
    FIXTYPE xy = py->fix;
    if (xy == 0) sxFail(err_div0);
    if (xx % xy == 0) px->fix = xx / xy;
    else numSetFloat(px, (FLOTYPE)xx / (FLOTYPE)xy);
  } else {
    FLOTYPE fx = numGetFloat(px);
    FLOTYPE fy = numGetFloat(py);
    if (fy == 0) sxFail(err_div0);
    numSetFloat(px, fx / fy);
  }
}

/* this function returns quotient in px and remainder in py */
void numIntegerDivide (number_t* px, number_t* py)
{
  FLOTYPE fx, fy;
  if (px->isfix) {
    FIXTYPE xx = px->fix;
    if (xx == 0) {
      px->fix = py->fix = 0;
      return;
    } else if (py->isfix) {
      FIXTYPE xy = py->fix;
      if (xy == 0) sxFail(err_div0);
      px->fix = xx / xy;
      py->fix = xx % xy;
      return;
    }
    fx = (FLOTYPE)xx;
  } else {
    fx = px->flo;
    if (!fisint(fx)) sxErr(err_reqint, cvflonum(fx));
  }
  fy = numGetFloat(py);
  if (!fisint(fy)) sxErr(err_reqint, cvflonum(fy));
  if (fy == 0) sxFail(err_div0);
  { /* both args are integer floats */
    FLOTYPE z = floor(fabs(fx) / fabs(fy));
    if (fx * fy < 0.0) z = -z;
    numSetFloat(px, z);
    numSetFloat(py, fx - fy * z);
  }
}

void numQuotient (number_t* px, number_t* py)
{
  number_t y = *py;
  numIntegerDivide (px, &y);
}

void numRemainder (number_t* px, number_t* py)
{
  number_t y = *py;
  numIntegerDivide (px, &y);
  *px = y;
}

void numModulo (number_t* px, number_t* py)
{
  FLOTYPE fx, fy;
  if (px->isfix) {
    FIXTYPE xx = px->fix;
    if (xx == 0) {
      px->fix = 0;
      return;
    } else if (py->isfix) {
      FIXTYPE xy = py->fix;
      if (xy == 0) sxFail(err_div0);
      px->fix = (xx - xy * (FIXTYPE)floor((FLOTYPE)xx / (FLOTYPE)xy));
      return;
    }
    fx = (FLOTYPE)xx;
  } else {
    fx = px->flo;
    if (!fisint(fx)) sxErr(err_reqint, cvflonum(fx));
  }
  fy = numGetFloat(py);
  if (!fisint(fy)) sxErr(err_reqint, cvflonum(fy));
  if (fy == 0) sxFail(err_div0);
  numSetFloat(px, fx - fy * floor(fx / fy));
}

void numGCD (number_t* px, number_t* py)
{
  FLOTYPE fx, fy;
  if (px->isfix) {
    FIXTYPE xx = px->fix;
    if (xx == 0) { *px = *py; numAbs(px); return; }
    if (xx == 1) return;
    if (xx == -1) { px->fix = 1; return; }
    if (xx < 0) xx = -xx;
    if (py->isfix) {
      FIXTYPE xy = py->fix;
      if (xy == 0) { numAbs(px); return; }
      else if (xy < 0) xy = -xy;
      for (;;) {
        FIXTYPE xr = xx % xy;
        if (xr == 0) break;
        xx = xy;
        xy = xr;
      }
      px->fix = xy;
      return;
    }
    fx = (FLOTYPE)xx;
    fy = fabs(py->flo);
    if (!fisint(fy)) sxErr(err_reqint, cvflonum(fx));
  } else if (py->isfix) {
    FIXTYPE xy = py->fix;
    if (xy == 0) { numAbs(px); return; }
    if (xy == 1 || xy == -1) { numSetFix(px, 1); return; }
    if (xy < 0) xy = -xy;
    fx = fabs(px->flo);
    if (!fisint(fx)) sxErr(err_reqint, cvflonum(fx));
    fy = (FLOTYPE)xy;
  } else {
    fx = fabs(px->flo);
    if (!fisint(fx)) sxErr(err_reqint, cvflonum(fx));
    fy = fabs(py->flo);
    if (!fisint(fy)) sxErr(err_reqint, cvflonum(fx));
  }
  if (fy == 0.0) { numSetFloat(px, fx); return; }
  for (;;) {
    FLOTYPE fr = fx - fy * floor(fx / fy); /* remainder(fx, fy) */
    if (fabs(fr) < 0.1) break;  /* better this way than r == 0.0 */
    fx = fy;
    fy = fr;
  }
  numSetFloat(px, fy);
}

void numLCM (number_t* px, number_t* py)
{
  number_t x = *px;
  number_t y = *py;
  numGCD (px, py); /* GCD in *px */
  if (numZeroP (px)) return; /* gcd(x,y)=0 => x=y=0 => lcm(x,y)=0 */
  numQuotient (&x, px); /* result in x */
  numMultiply (&x, &y); /* result in x */
  numAbs(&x); /* result in x */
  *px = x;
}

void numAtan2 (number_t* py, number_t* px)
{
  FLOTYPE fy, fx;
  if (py->isfix && py->fix == 0) return;
  fy = numGetFloat(py);
  fx = numGetFloat(px);
  if (fx == 0.0) sxFail(err_div0);
  numSetFloat(py, atan2(fy, fx));
}

void numExpt (number_t* px, number_t* py)
{
  FIXTYPE xx = px->fix;
  FIXTYPE xy = py->fix;
  /* special values of y */
  if (py->isfix) {
    if (xy == 0) { numSetFix(px, 1); return; } /* x^0 = 1 */
    if (xy == 1) return; /* x^1 = x */
  }
  /* special values of x */
  if (px->isfix) {
    if (xx == 0) return; /* x!=0: 0^x = 0; */
    if (xx == 1) return; /* 1^x = 1 */
  }
  /* both x & y are fixnums */
  if (py->isfix && px->isfix) {
    FLOTYPE z;
    if (xx == -1 && xy < 0) xy = -xy; /* one more exactness-pres. case */
    z = pow((FLOTYPE)xx, (FLOTYPE)xy);
    if (xy > 0) { /* z should be exact if fits into FIXTYPE */
      xx = (FIXTYPE)z;
      if ((FLOTYPE)xx == z) { px->fix = xx; return; }
    }
    numSetFloat(px, z);
    return;
  }
  { /* regular flonum expt */
    FLOTYPE fx = numGetFloat(px);
    FLOTYPE fy = numGetFloat(py);
    /* pow(0,0) signals domain error! */
    if (fx == 0.0 && fy == 0.0) numSetFloat(px, 1.0);
    else numSetFloat(px, pow(fx, fy));
  }
}

void numBitwiseAnd (number_t* px, number_t* py)
{
  if (!px->isfix) sxErr(err_reqfix, cvflonum(px->flo));
  if (!py->isfix) sxErr(err_reqfix, cvflonum(py->flo));
  px->fix &= py->fix;
}

void numBitwiseIor (number_t* px, number_t* py)
{
  if (!px->isfix) sxErr(err_reqfix, cvflonum(px->flo));
  if (!py->isfix) sxErr(err_reqfix, cvflonum(py->flo));
  px->fix |= py->fix;
}

void numBitwiseXor (number_t* px, number_t* py)
{
  if (!px->isfix) sxErr(err_reqfix, cvflonum(px->flo));
  if (!py->isfix) sxErr(err_reqfix, cvflonum(py->flo));
  px->fix ^= py->fix;
}

/* smal functions for shifts; y is nonnegative */

static FIXTYPE sll(FIXTYPE x, FIXTYPE y)
{ /* << should work just fine */
  return x << y;
}

static FIXTYPE srl(FIXTYPE x, FIXTYPE y)
{ /* >> may not be logical: fix it */
  FIXTYPE z = x >> y;
  if (y && (z & FIXMIN)) { /* sign is set */
    FIXTYPE mask = 0;
    while (y--) mask = (mask >> 1) | FIXMIN; 
    z &= ~mask;
  }
  return z;
}

static FIXTYPE sla(FIXTYPE x, FIXTYPE y)
{ /* << should work just fine */
  return x << y;
}

static FIXTYPE sra(FIXTYPE x, FIXTYPE y)
{ /* >> may not be arithmetical: fix it */
  FIXTYPE z = x >> y;
  if ((x ^ z) & FIXMIN) { /* sign is lost */
    FIXTYPE mask = 0;
    while (y--) mask = (mask >> 1) | FIXMIN; 
    if (x & FIXMIN) z |= mask; else z &= ~mask;
  }
  return z;
}

void numBitwiseSll (number_t* px, number_t* py)
{
  if (!px->isfix) sxErr(err_reqfix, cvflonum(px->flo));
  if (!py->isfix) sxErr(err_reqfix, cvflonum(py->flo));
  if (py->fix > 0) px->fix = sll(px->fix, py->fix); 
  else px->fix = srl(px->fix, - py->fix);
}

void numBitwiseSrl (number_t* px, number_t* py)
{
  if (!px->isfix) sxErr(err_reqfix, cvflonum(px->flo));
  if (!py->isfix) sxErr(err_reqfix, cvflonum(py->flo));
  if (py->fix > 0) px->fix = srl(px->fix, py->fix);  
  else px->fix = sll(px->fix, - py->fix);
}

void numBitwiseSla (number_t* px, number_t* py)
{
  if (!px->isfix) sxErr(err_reqfix, cvflonum(px->flo));
  if (!py->isfix) sxErr(err_reqfix, cvflonum(py->flo));
  if (py->fix > 0) px->fix = sla(px->fix, py->fix); 
  else px->fix = sra(px->fix, - py->fix);
}

void numBitwiseSra (number_t* px, number_t* py)
{
  if (!px->isfix) sxErr(err_reqfix, cvflonum(px->flo));
  if (!py->isfix) sxErr(err_reqfix, cvflonum(py->flo));
  if (py->fix > 0) px->fix = sra(px->fix, py->fix);  
  else px->fix = sla(px->fix, - py->fix);
}

/************* Scheme/Math Converters *************/

void numParseSOBJ (SOBJ obj, number_t* px)
{
  if (fixp(obj)) numSetFix(px, getfixnum(obj));
  else if (floatp(obj)) numSetFloat(px, getflonum(obj));
  else xlreqnumber(obj);
}

SOBJ numMakeSOBJ (number_t* px)
{
  if (px->isfix) return cvfixnum(px->fix);
  else return cvflonum(px->flo);
}


/*****************  Combinators  ******************/

/* numerical predicate test */
SOBJ numPredicateSOBJ (numpred_t numpred, SOBJ sx)
{
  number_t x;
  numParseSOBJ(sx, &x);
  return cvbool(numpred(&x));
}

/* numerical compare test */
SOBJ numCompareSOBJ (numcmp_t numcmp, SOBJ sx, SOBJ sy)
{
  number_t x, y;
  numParseSOBJ(sx, &x);
  numParseSOBJ(sy, &y);
  return cvbool(numcmp(&x, &y));
}

/* unary numerical operation */
SOBJ numUnarySOBJ (numop1_t numop1, SOBJ sx)
{
  number_t x;
  numParseSOBJ(sx, &x);
  numop1(&x);
  return numMakeSOBJ(&x);
}

/* binary numerical operation */
SOBJ numBinarySOBJ (numop2_t numop2, SOBJ sx, SOBJ sy)
{
  number_t x, y;
  numParseSOBJ(sx, &x);
  numParseSOBJ(sy, &y);
  numop2(&x, &y);
  return numMakeSOBJ(&x);
}

/**********  Procedure-Level Combinators **********/

/* reduce left to right with initial fixnum value */
SOBJ numReduceInit (FIXTYPE init, numop2_t numop2)
{
  number_t x, y;
  numSetFix(&x, init);
  while (moreargs()) {
    SOBJ next = nextarg();
    numParseSOBJ(next, &y);
    numop2(&x, &y); /* -> x */
  }
  return numMakeSOBJ(&x);
}

/* reduce left to right */
SOBJ numReduce (numop2_t numop2)
{
  number_t x, y;
  SOBJ first = xlgetarg();
  numParseSOBJ(first, &x);
  while (moreargs()) {
    SOBJ next = nextarg();
    numParseSOBJ(next, &y);
    numop2(&x, &y); /* -> x */
  }
  return numMakeSOBJ(&x);
}

/* numerical predicate test */
SOBJ numPredicate (numpred_t numpred)
{
  number_t x;
  SOBJ first = xlgetarg();
  xllastarg();
  numParseSOBJ(first, &x);
  return cvbool(numpred(&x));
}

/* numerical compare test */
SOBJ numCompare (numcmp_t numcmp)
{
  number_t x, y;
  SOBJ first = xlgetarg();
  SOBJ second = xlgetarg();
  xllastarg();
  numParseSOBJ(first, &x);
  numParseSOBJ(second, &y);
  return cvbool(numcmp(&x, &y));
}

/* numerical order test */
SOBJ numOrdered (numcmp_t numcmp)
{
  number_t x, y;
  SOBJ first = xlgetarg();
  numParseSOBJ(first, &x);
  while (moreargs()) {
    SOBJ next = nextarg();
    numParseSOBJ(next, &y);
    if (!numcmp(&x, &y)) {
      xlpoprest ();
      return so_false;
    }
    x = y;
  }
  return so_true;
}

/* unary numerical operation */
SOBJ numUnary (numop1_t numop1)
{
  number_t x;
  SOBJ first = xlgetarg();
  xllastarg();
  numParseSOBJ(first, &x);
  numop1(&x);
  return numMakeSOBJ(&x);
}

/* binary numerical operation */
SOBJ numBinary (numop2_t numop2)
{
  number_t x, y;
  SOBJ first = xlgetarg();
  SOBJ second = xlgetarg();
  xllastarg();
  numParseSOBJ(first, &x);
  numParseSOBJ(second, &y);
  numop2(&x, &y);
  return numMakeSOBJ(&x);
}
