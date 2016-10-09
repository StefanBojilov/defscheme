/* ntag.t - main Sxeme node tags table */

/* node tags & types */
NTAG(FREE,      0,   FREE)     /* free node */
NTAG(CONS,      1,   CONS)     /* cons (pair) of objects */
NTAG(SYMBOL,    2,   SYMBOL)   /* symbol */
NTAG(FIXNUM,    3,   FIXNUM)   /* small exact integer */
NTAG(FLONUM,    4,   FLONUM)   /* inexact floating-point number */
NTAG(STRING,    5,   STRING)   /* ANSI/UNICODE string */
NTAG(PORT,      6,   PORT)     /* port (input, output, io) */
NTAG(VECTOR,    7,   VECTOR)   /* vector */
NTAG(CLOSURE,   8,   PAIR)     /* closure (pair of code and environment) */
NTAG(CODE,      9,   VECTOR)   /* code (vector of bytecode string & literals) */
NTAG(SUBR,      10,  SUBR)     /* built-in procedure */
/*              11                reserved */
NTAG(CSUBR,     12,  SUBR)     /* built-in continuation procedure */
/*              13                reserved */
NTAG(CHAR,      14,  FROB)     /* ANSI/UNICODE character */
NTAG(BYTEVEC,   15,  BYTEVEC)  /* byte vector */
NTAG(PROMISE,   16,  PAIR)     /* delayed thunk */
NTAG(HASHTABLE, 17,  VECTOR)   /* hash table (vector of alists) */
NTAG(WEAKPAIR,  18,  WEAKPAIR) /* pair with weak car field */
NTAG(KEYWORD,   19,  KEYWORD)  /* special kind of symbol */
NTAG(BOX,       20,  PAIR)     /* box, containing another object */
NTAG(RECORD,    21,  VECTOR)   /* record (vector with rtd in v[0]) */
/*              22                reserved */
/*              23                reserved */
/*              24                reserved */
NTAG(HANDLE,    25,  HANDLE)   /* OS-specific handle */
NTAG(BOOLEAN,   26,  FROB)     /* true and false objects */
NTAG(FROB,      27,  FROB)     /* special atomic objects */
NTAG(GCELL,     28,  PAIR)     /* global value cell */

