/* sxmath.c - math procedures */

#include <math.h>

#include "sxm.h"
#include "sxmath.h"
#include "sxmathread.h"

/************** Misc. Utils **************/

/* digit from character */
int chartodigit(tint_t c)
{
  return istdigit(c) 
          ? (int)(c - T('0')) 
          : (istupper(c) ? (int)(c - T('A')) + 10 
                         : (int)(c - T('a')) + 10);
}

/************** Math Grammar Parsers **************/

/* prefix parser */
bool_t numParsePrefix(const tchar_t **ppch, int *pradix, int *pe0i)
{
  const tchar_t *str = *ppch;
  for (;;) { /* loop until there's no more prefixes (at most twice!) */
    tint_t ch = *str++;
    if (ch == T('#')) { /* prefix? */
      ch = *str++;
      if (!ch) return FALSE; /* illegal prefix */
      /* ToDo: check symbols2lc? */ 
      ch = totlower(ch);
      switch (ch) {
        int ne0i, nradix;
        case T('e'): ne0i = -1; goto new_e0i;
        case T('i'): ne0i = 1; goto new_e0i;
        new_e0i: 
          if (*pe0i) return FALSE; /* duplicate exactness prefix */
          *pe0i = ne0i;
          break;
        case T('b'): nradix = 2; goto new_radix;
        case T('o'): nradix = 8; goto new_radix;
        case T('d'): nradix = 10; goto new_radix;
        case T('x'): nradix = 16; goto new_radix;
        new_radix:
          if (*pradix > 0) return FALSE; /* duplicate radix prefix */
          *pradix = nradix;
          break;
        default: /* illegal number prefix */
          return FALSE;
      }
    } else { /* no (more) prefixes */
      --str; /* unroll to point to the break char */
      break;
    }
  }
  /* leave *ppch pointing to the break char */
  *ppch = str;
  return TRUE;
}

/* postfactum: override exactness as specified by prefix */
bool_t numApplyExactnessPrefix(number_t* px, int e0i)
{
  switch (e0i) {
    case -1: return numTryToExact(px);
    case 0: return TRUE; /* leave as is */
    case 1: return numTryToInexact(px);
  }
  assert(FALSE);
  never_returns(FALSE);
}

int numParseSign(const tchar_t **ppch)
{
  tint_t ch = **ppch;
  int sign = 1;
  switch (ch) {
    case T('-'): sign = -1; /* fall thru */
    case T('+'): (*ppch)++;
  }
  return sign;
}

/************** Number Parsers **************/

bool_t numParseUInteger(const tchar_t **ppch, number_t *px, int radix)
{
  FIXTYPE num;
  FLOTYPE fnum;
  int digit;
  bool_t overflow = FALSE;
  bool_t gotdigit = FALSE;
  bool_t gothash = FALSE;
  const tchar_t *str;
  FIXTYPE maxfix = (FIXTYPE)(((unsigned FIXTYPE)-1)>>1);

  /* initialize */
  if (!radix) radix = 10; else if (radix < 0) radix = -radix;
  str = *ppch; num = 0; fnum = 0.0;

  for (;;) {
    tint_t ch = *str++;
    if (!ch) break;
    if (ch == T('#')) { /* precision is lost */
      if (!gotdigit) return FALSE;
      digit = 0; gothash = TRUE;
    } else {
      digit = chartodigit(ch);
      if (digit >= radix || digit < 0) break; /* wasn't a digit */
      if (gothash) return FALSE; /* digits after hash are not legal */
      gotdigit = TRUE;
    }
    if (!overflow) {
      FIXTYPE n = num * radix + digit;
      if (num <= (maxfix-digit)/radix) /* still doing ok */
        num = n;
      else { /* num overflows */
        fnum = ((FLOTYPE)num) * radix + digit;
        overflow = TRUE;
      }
    } else {
      fnum = fnum * radix + digit;
    }
  }
  /* no (more) digits */
  --str; /* unroll to point to the break char */
  /* calc result */
  if (overflow) /* fixnum overflow: make inexact */
    numSetFloat(px, fnum);
  else if (gothash) /* fixnum, but must be made inexact */
    numSetFloat(px, (FLOTYPE)num);
  else /* genuine exact fixnum */
    numSetFix(px, num);
  /* leave *ppch pointing to the break char */
  *ppch = str;
  return TRUE;
}

/* pow10 is nonstandard!! conditional compilation here??? */
#define pow10(x) pow(10,x)

bool_t numParseDecimal(const tchar_t **ppch, number_t *px, bool_t *pisint)
{
  int i, dl, dot, dr;
  FIXTYPE iipart = 0;
  FLOTYPE fipart = 0;
  FLOTYPE ffpart = 0;
  int iexpt = 0;
  bool_t gotdigit = FALSE;
  bool_t gothash = FALSE;
  const tchar_t *str;
  FIXTYPE maxfix = (FIXTYPE)(((unsigned FIXTYPE)-1)>>1);

  /* initialize */
  dl = dot = dr = 0;
  str = *ppch;
  *pisint = TRUE; /* optimistic hope that we got plain UInteger */

  /* skip leading zeros */
  while (*str == T('0')) {
    ++str;
    dl = 1;
    gotdigit = TRUE;
  }
  /* check for a string of digits */
  for (;; ++str, ++dl) {
    if (*str == T('#')) {
      if (!gotdigit) return FALSE;
      i = 0; 
      gothash = TRUE;
    } else {
      if (!istdigit(*str)) break;
      if (gothash) return FALSE;
      i = (*str - T('0'));
      gotdigit = TRUE;
    }
    if (dl < 1000) { /* inc fixtype with overflow check */
      FIXTYPE iip = iipart * 10 + i;
      if (iipart <= (maxfix-i)/10) /* still doing ok */
        iipart = iip;
      else { /* iipart overflows */
        fipart = ((FLOTYPE)iipart) * 10.0 + i;
        dl = 1000;
        iipart = 0;
      }
    } else { /* int overflows */
      fipart = fipart * 10.0 + i;
    }
  }
  if (dl < 1000) {
    fipart = 0;
  } else {
    dot = 1; iipart = 0;
  }

  /* check for a decimal point */
  if (*str == T('.')) {
    str++; dot = 1; *pisint = FALSE;
    for (i = -1 ; ; i--, str++, dr++) {
      if (*str == T('#')) {
        if (!gotdigit) return FALSE;
        gothash = TRUE;
      } else {
        if (!istdigit(*str)) break;
        if (gothash) return FALSE;
        ffpart += pow10(i) * (*str - T('0'));
        gotdigit = TRUE;
      }
    }
  }
  /* check for the exponent suffix */
  if ((dl || dr) && *str && tcschr(T("esfdlESFDL"), *str) != NULL) {
    int esign;
    str++; dot = 1; *pisint = FALSE;
    /* check for an exp sign */
    if ((esign = (*str == T('-'))) != 0 || *str == T('+'))
      ++str;
    /* check for a string of digits */
    while (istdigit(*str)) {
      iexpt = iexpt * 10 + (*str - T('0'));
      str++;
      dr++;
    }
    if (esign)
      iexpt = -iexpt;
  }
  /* make sure there was at least one digit */
  if (dl == 0 && dr == 0) return FALSE;
  /* calc result */
  if (dot) /* dot or exponent or fixnum overflow */
    numSetFloat(px, (iipart + fipart + ffpart) * pow10(iexpt));
  else if (gothash) /* fixnum, but must be made inexact */
    numSetFloat(px, (FLOTYPE)iipart);
  else /* exact fixnum */
    numSetFix(px, iipart);
  /* leave *ppch pointing to the break char */
  *ppch = str;
  return TRUE;
}

bool_t numParseUReal(const tchar_t **ppch, number_t *px, int radix)
{
  bool_t isint = TRUE;
  /* if we still don't have explicit radix, or have {implicit} radix 10... */
  if (!radix || abs(radix) == 10) { /* parse extended decimal syntax */
    if (!numParseDecimal(ppch, px, &isint)) return FALSE;
  } else { /* non-decimal radix: integers only */
    if (!numParseUInteger(ppch, px, radix)) return FALSE;
  }
  /* we got our number in *px; if we had integer syntax,
     it could be numerator followed by / and denominator */
  if (isint && **ppch == T('/')) {
    number_t den;
    (*ppch)++;
    /* denominator should be a nonzero uint */
    if (!numParseUInteger(ppch, &den, radix)) return FALSE;
    if (numZeroP(&den)) return FALSE;
    numDivide(px, &den);
  }
  return TRUE;
}

bool_t numParseReal(const tchar_t **ppch, number_t *px, int radix)
{
  int sign = numParseSign(ppch);
  if (!numParseUReal(ppch, px, radix)) return FALSE;
  /* ToDo: this might not allow us to represent LONG_MIN??? */
  if (sign < 0) numAddInverse(px);
  return TRUE;
}

bool_t numParseComplex(const tchar_t **ppch, number_t *px, int radix)
{
  /* this one obviosly cannot parse general complex numbers */
  if (!numParseReal(ppch, px, radix)) return FALSE;
  /* ToDo: here we could look for following @,+,-,i later... */
  return TRUE;
}

/************** Expression Parsers **************/

/*
 * numParseNumber - try to convert a string to a number for numbers that out
 * of FIXNUM range should return FLONUM R4RS:7.1.1 [#b | #o | #d | #x] [#e | #i]
 * [+|-] DDD.DDD {[s|f|d|l][+|-]DDD}; 'Numbers containing decimal points or
 * exponents must be in decimal radix.'
 */

bool_t numParseNumber(const tchar_t **ppch, number_t *px, int radix, int e0i)
{
  /* if we still have some prefixes left, process them first */ 
  if (!numParsePrefix(ppch, &radix, &e0i)) return FALSE;
  /* after prefix we expect complex number */
  if (!numParseComplex(ppch, px, radix)) return FALSE;
  /* apply explicit exactness prefix, if any */
  if (!numApplyExactnessPrefix(px, e0i)) return FALSE; /* coercion error */
  return TRUE;
}

bool_t numParseInteger(const tchar_t **ppch, number_t *px, int radix, int e0i)
{
  int sign;
  /* if we still have some prefixes left, process them first */ 
  if (!numParsePrefix(ppch, &radix, &e0i)) return FALSE;
  /* after prefix we expect optional sign and uinteger */
  sign = numParseSign(ppch);
  if (!numParseUInteger(ppch, px, radix)) return FALSE;
  /* ToDo: this might not allow us to represent LONG_MIN??? */
  if (sign < 0) numAddInverse(px);
  /* apply explicit exactness prefix, if any */
  if (!numApplyExactnessPrefix(px, e0i)) return FALSE; /* coercion error */
  return TRUE;
}


/**********  hi-level number parsers ************/

bool_t sxParseInteger(const tchar_t *str, SOBJ *pval, int radix)
{
  number_t num; 
  int e0i = 0; /* start with no explicit exactness prefix */
  assert(str != NULL); assert(pval != NULL);
  /* parse integer of given radix */
  if (!numParseInteger(&str, &num, radix, e0i)) return FALSE;
  /* check that there's nothing left in str */
  if (*str != 0) return FALSE; /* parsing was not completed */
  /* make and return SOBJ */
  *pval = numMakeSOBJ(&num);
  return TRUE;
}

bool_t sxParseNumber(const tchar_t *str, SOBJ *pval, int radix)
{
  number_t num;
  int e0i = 0; /* start with no explicit exactness prefix */
  assert(str != NULL); assert(pval != NULL);
  /* parse number in given radix */
  if (!numParseNumber(&str, &num, radix, e0i)) return FALSE;
  /* check that there's nothing left in str */
  if (*str != 0) return FALSE; /* parsing was not completed */
  /* make and return SOBJ */
  *pval = numMakeSOBJ(&num);
  return TRUE;
}
