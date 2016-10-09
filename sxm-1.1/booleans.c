/* booleans.c - standard procedures 6.1 */

#include "sxm.h"
#include "define.h"

/*#| (not obj) |#*/
DEFINE_INITIAL_BINDING("not", sp_falsep)
/*#| (false? obj) |#*/
DEFINE_INITIAL_BINDING("false?", sp_falsep)
DEFINE_PROCEDURE(sp_falsep)
{
  SOBJ arg = xlonearg();
  return cvbool(falsep(arg));
}

/*#| (true? obj) |#*/
DEFINE_INITIAL_BINDING("true?", sp_truep)
DEFINE_PROCEDURE(sp_truep)
{
  SOBJ arg = xlonearg();
  return cvbool(truep(arg));
}

/*#| (boolean? obj) |#*/
DEFINE_INITIAL_BINDING("boolean?", sp_booleanp)
DEFINE_PROCEDURE(sp_booleanp)
{
  SOBJ arg = xlonearg();
  return cvbool(booleanp(arg));
}
