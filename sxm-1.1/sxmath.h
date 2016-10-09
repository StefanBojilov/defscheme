/* sxmath.h - numerical utils */

#ifndef __SXMATH_H
#define __SXMATH_H

#include "sxm.h"

/* special check for integer flonums */
extern bool_t numFloatIsInt (FLOTYPE x);

/******** SxMath Numbers Representation **********/
typedef struct number_s {
  int isfix;
  union numdata_s xd_num;
} number_t;
typedef int  (*numpred_t) (number_t* px);
typedef int  (*numcmp_t)  (number_t* px, number_t* py);
typedef void (*numop1_t)  (number_t* px);
typedef void (*numop2_t)  (number_t* px, number_t* py);

/************** Numerical Converters **************/
extern FLOTYPE numGetFloat (number_t* px);
extern void    numSetFloat (number_t* px, FLOTYPE x);
extern void    numSetFix   (number_t* px, FIXTYPE x);
extern void    numToFloat  (number_t* px);
extern void    numToExact  (number_t* px);
extern void    numToInexact (number_t* px);
extern bool_t  numTryToExact  (number_t* px);
extern bool_t  numTryToInexact (number_t* px);

/************** Numerical Predicates **************/
extern int numZeroP     (number_t* px);
extern int numPositiveP (number_t* px);
extern int numNegativeP (number_t* px);
extern int numOddP      (number_t* px);
extern int numEvenP     (number_t* px);

/*************** Numerical Compare ****************/
extern int numLessP           (number_t* px, number_t* py);
extern int numLessOrEqualP    (number_t* px, number_t* py);
extern int numEqualP          (number_t* px, number_t* py);
extern int numGreaterOrEqualP (number_t* px, number_t* py);
extern int numGreaterP        (number_t* px, number_t* py);

/******* Numerical Unary Ops (result -> x) ********/
extern void numAddInverse    (number_t* px);
extern void numMulInverse    (number_t* px);
extern void numAdd1          (number_t* px);
extern void numSubtract1     (number_t* px);
extern void numAbs           (number_t* px);
extern void numFloor         (number_t* px);
extern void numCeiling       (number_t* px);
extern void numTruncate      (number_t* px);
extern void numRound         (number_t* px);
extern void numExp           (number_t* px);
extern void numLog           (number_t* px);
extern void numSin           (number_t* px);
extern void numCos           (number_t* px);
extern void numTan           (number_t* px);
extern void numAsin          (number_t* px);
extern void numAcos          (number_t* px);
extern void numAtan          (number_t* px);
extern void numSinh          (number_t* px);
extern void numCosh          (number_t* px);
extern void numTanh          (number_t* px);
extern void numSqrt          (number_t* px);
extern void numBitwiseNot    (number_t* px);
extern void numRandom        (number_t* px);

/******* Numerical Binary Ops (result -> x) *******/
extern void numMin           (number_t* px, number_t* py);
extern void numMax           (number_t* px, number_t* py);
extern void numAdd           (number_t* px, number_t* py);
extern void numSubtract      (number_t* px, number_t* py);
extern void numMultiply      (number_t* px, number_t* py);
extern void numDivide        (number_t* px, number_t* py);
extern void numIntegerDivide (number_t* px, number_t* py); /* -> x,y */
extern void numQuotient      (number_t* px, number_t* py);
extern void numRemainder     (number_t* px, number_t* py);
extern void numModulo        (number_t* px, number_t* py);
extern void numGCD           (number_t* px, number_t* py);
extern void numLCM           (number_t* px, number_t* py);
extern void numAtan2         (number_t* py, number_t* px); /* -> y */
extern void numExpt          (number_t* px, number_t* py);
extern void numBitwiseAnd    (number_t* px, number_t* py);
extern void numBitwiseIor    (number_t* px, number_t* py);
extern void numBitwiseXor    (number_t* px, number_t* py);
extern void numBitwiseSll    (number_t* px, number_t* py);
extern void numBitwiseSrl    (number_t* px, number_t* py);
extern void numBitwiseSla    (number_t* px, number_t* py);
extern void numBitwiseSra    (number_t* px, number_t* py);

/************ Scheme/sxMath Converters ************/
extern void numParseSOBJ (SOBJ obj, number_t* px);
extern SOBJ numMakeSOBJ  (number_t* px);

/******************  Combinators  *****************/
extern SOBJ numPredicateSOBJ (numpred_t numpred, SOBJ sx);
extern SOBJ numCompareSOBJ   (numcmp_t numcmp, SOBJ sx, SOBJ sy);
extern SOBJ numUnarySOBJ     (numop1_t numop1, SOBJ sx);
extern SOBJ numBinarySOBJ    (numop2_t numop2, SOBJ sx, SOBJ sy);

/**********  Procedure-Level Combinators  *********/
extern SOBJ numReduceInit (FIXTYPE init, numop2_t numop2);
extern SOBJ numReduce     (numop2_t numop2);    /* argc>0 */
extern SOBJ numPredicate  (numpred_t numpred);  /* argc=1 */
extern SOBJ numCompare    (numcmp_t numcmp);    /* argc=2 */
extern SOBJ numOrdered    (numcmp_t numcmp);    /* argc>0 */
extern SOBJ numUnary      (numop1_t numop1);    /* argc=1 */
extern SOBJ numBinary     (numop2_t numop2);    /* argc=2 */

#endif /* ndef __SXMATH_H */

