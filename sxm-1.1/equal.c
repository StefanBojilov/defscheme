/* equal.c - standard procedures 6.2 */

#include "sxm.h"
#include "define.h"

/* eqtest - common routine for eq?/eqv?/equal? */
static SOBJ eqtest(sobjcmpfun_t eqfcn)
{
  SOBJ arg1 = xlgetarg();
  SOBJ arg2 = xlgetarg();
  xllastarg();
  return cvbool((*eqfcn)(arg1, arg2));
}

/*#| (equal? obj1 obj2) |#*/
DEFINE_INITIAL_BINDING("equal?", sp_equal)
DEFINE_PROCEDURE(sp_equal)
{
  return eqtest(equal);
}

/*#| (eqv? obj1 obj2) |#*/
DEFINE_INITIAL_BINDING("eqv?", sp_eqv)
DEFINE_PROCEDURE(sp_eqv)
{
  return eqtest(eqv);
}

/*#| (eq? obj1 obj2) |#*/
DEFINE_INITIAL_BINDING("eq?", sp_eq)
DEFINE_PROCEDURE(sp_eq)
{
  return eqtest(eq);
}

