/* sxmath.h - numerical utils */

#ifndef __SXMATHREAD_H
#define __SXMATHREAD_H

#include "sxm.h"
#include "sxmath.h"

/************** Math Grammar Parsers **************/
extern bool_t numParsePrefix(const tchar_t **ppch, int *pradix, int *pe0i);
extern bool_t numApplyExactnessPrefix(number_t* px, int e0i);
extern int numParseSign(const tchar_t **ppch);

/************** Number Parsers **************/
extern bool_t numParseUInteger(const tchar_t **ppch, number_t *px, int radix);
extern bool_t numParseDecimal(const tchar_t **ppch, number_t *px, bool_t *pisint);
extern bool_t numParseUReal(const tchar_t **ppch, number_t *px, int radix);
extern bool_t numParseReal(const tchar_t **ppch, number_t *px, int radix);
extern bool_t numParseComplex(const tchar_t **ppch, number_t *px, int radix);

/************** Expression Parsers **************/
extern bool_t numParseNumber(const tchar_t **ppch, number_t *px, int radix, int e0i);
extern bool_t numParseInteger(const tchar_t **ppch, number_t *px, int radix, int e0i);

/**********  hi-level number parsers ************/
extern bool_t sxParseInteger(const tchar_t *str, SOBJ *pval, int radix);
extern bool_t sxParseNumber(const tchar_t *str, SOBJ *pval, int radix);

#endif /* ndef __SXMATHREAD_H */

