/*
  Builtin femtoLisp math functions
*/

#ifndef   __BUILTINS_MATH_H__
#define  __BUILTINS_MATH_H__

// int num_to_ptr(value_t a, fixnum_t *pi, numerictype_t *pt, void **pp)
// value_t fl_div2(value_t a, value_t b)
value_t fl_math_sin( value_t *args, u_int32_t nargs);
value_t fl_math_tan( value_t *args, u_int32_t nargs);
value_t fl_math_asin(value_t *args, u_int32_t nargs);
value_t fl_math_atan(value_t *args, u_int32_t nargs);
value_t fl_math_sqrt(value_t *args, u_int32_t nargs);
value_t fl_math_log( value_t *args, u_int32_t nargs);
value_t fl_math_pow( value_t *args, u_int32_t nargs); /// scheme's expt 

#endif
