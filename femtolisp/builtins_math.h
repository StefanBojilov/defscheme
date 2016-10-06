/*
  Builtin femtoLisp math functions
*/

#ifndef   __BUILTINS_MATH_H__
#define  __BUILTINS_MATH_H__

// int num_to_ptr(value_t a, fixnum_t *pi, numerictype_t *pt, void **pp)
// value_t fl_div2(value_t a, value_t b)
value_t fl_math_cosine(value_t *args, u_int32_t nargs);
value_t fl_math_sine(value_t *args, u_int32_t nargs);

#endif
