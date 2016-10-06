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

value_t fl_math_cosine(value_t *args, u_int32_t nargs)
{
    double da;
    int_t ai;
    numerictype_t ta;
    void *aptr;

    argcount("length", nargs, 1);
    value_t a = args[0];
    if( !fl_isnumber(a) )
       type_error("cosine", "number", args[0]);
    if (!num_to_ptr(a, &ai, &ta, &aptr)) // redundant
        type_error("cosine", "number", a);
    da = conv_to_double(aptr, ta);
    da = cos(da);
    return mk_double( da );
}

value_t fl_math_sine(value_t *args, u_int32_t nargs)
{
    double da;
    int_t ai;
    numerictype_t ta;
    void *aptr;

    argcount("length", nargs, 1);
    value_t a = args[0];
    if( !fl_isnumber(a) )
       type_error("sine", "number", args[0]);
    if (!num_to_ptr(a, &ai, &ta, &aptr)) // redundant
        type_error("sine", "number", a);
    da = conv_to_double(aptr, ta);
    da = sin(da);
    return mk_double( da );
}
