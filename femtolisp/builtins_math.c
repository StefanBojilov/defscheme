/*
  Extra femtoLisp builtin functions
*/

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>
#include <setjmp.h>
#include <stdarg.h>
#include <assert.h>
#include <ctype.h>
#include <sys/types.h>
#include <sys/time.h>
#include <sys/stat.h>
#include <errno.h>
#include "llt.h"
#include "flisp.h"
#include "random.h"
#include "cvalues.h"

value_t fl_math_sin(value_t *args, u_int32_t nargs)
{
    double da;
    int_t ai;
    numerictype_t ta;
    void *aptr;

    argcount("length", nargs, 1);
    value_t a = args[0];
//   if( !fl_isnumber(a) )  // redundant
//       type_error("sine", "number", args[0]);
    if (!num_to_ptr(a, &ai, &ta, &aptr))
        type_error("math.sin", "number", a);
    da = conv_to_double(aptr, ta);
    da = sin(da);
    return mk_double( da );
}

value_t fl_math_tan(value_t *args, u_int32_t nargs)
{
    double da;
    int_t ai;
    numerictype_t ta;
    void *aptr;

    argcount("length", nargs, 1);
    value_t a = args[0];
    if (!num_to_ptr(a, &ai, &ta, &aptr))
        type_error("math.math.cosine", "number", a);
    da = conv_to_double(aptr, ta);
    da = tan(da);
    return mk_double( da );
}
value_t fl_math_asin(value_t *args, u_int32_t nargs)
{
    double da;
    int_t ai;
    numerictype_t ta;
    void *aptr;

    argcount("length", nargs, 1);
    value_t a = args[0];
    if (!num_to_ptr(a, &ai, &ta, &aptr))
        type_error("math.asin", "number", a);
    da = conv_to_double(aptr, ta);
    da = asin(da);
    return mk_double( da );
}

value_t fl_math_atan(value_t *args, u_int32_t nargs)
{
    double da;
    int_t ai;
    numerictype_t ta;
    void *aptr;

    argcount("length", nargs, 1);
    value_t a = args[0];
    if (!num_to_ptr(a, &ai, &ta, &aptr))
        type_error("math.atan", "number", a);
    da = conv_to_double(aptr, ta);
    da = atan(da);
    return mk_double( da );
}
value_t fl_math_sqrt(value_t *args, u_int32_t nargs)
{
    double da;
    int_t ai;
    numerictype_t ta;
    void *aptr;

    argcount("length", nargs, 1);
    value_t a = args[0];
    if (!num_to_ptr(a, &ai, &ta, &aptr))
        type_error("math.sqrt", "number", a);
    da = conv_to_double(aptr, ta);
    da = sqrt(da);
    return mk_double( da );
}
value_t fl_math_log(value_t *args, u_int32_t nargs)
{
    double da;
    int_t ai;
    numerictype_t ta;
    void *aptr;

    argcount("length", nargs, 1);
    value_t a = args[0];
    if (!num_to_ptr(a, &ai, &ta, &aptr))
        type_error("math.log", "number", a);
    da = conv_to_double(aptr, ta);
    da = log(da);
    return mk_double( da );
}
value_t fl_math_pow(value_t *args, u_int32_t nargs) /// scheme's expt 
{
    double da,db;
    int_t ai,bi;
    numerictype_t ta,tb;
    void *aptr, *bptr;

    argcount("length", nargs, 2);
    value_t a = args[0];
    if (!num_to_ptr(a, &ai, &ta, &aptr))
        type_error("math.pow : arg 1", "number", a);
    value_t b = args[1];
    if (!num_to_ptr(b, &bi, &tb, &bptr))
        type_error("math.pow : arg 2", "number", b);
    da = conv_to_double(aptr, ta);
    db = conv_to_double(bptr, tb);
    da = pow(da,db);
    return mk_double( da );
}

