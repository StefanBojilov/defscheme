/* hash.c - hash procedures */

#include "sxm.h"
#include "sxhash.h"
#include "define.h"

/* calchash - common routine for hashq/hashv/... */
static SOBJ calchash(int nt, sobjhashfun_t hashfcn)
{
  /* get object and len */
  SOBJ obj = xlgetarg();
  SOBJ arg = xlgasfixnum();
  FIXTYPE len = getsfixnum(arg);
  xllastarg();

  /* typecheck the arg */
  if (nt != NT__ANY && xntype(obj) != nt) sxae_type(obj, nt);

  /* range check the length */
  if (len <= 0 || len > SX_MAX_VECTOR_ELTS)
    sxae_range(arg, cvfixnum(SX_MAX_VECTOR_ELTS));

  /* hash using hashfcn */
  return cvfixnum((*hashfcn)(obj, (sv_size_t)len));
}


/*#| (hashq obj htsize) |#*/
DEFINE_INITIAL_BINDING("hashq", sp_hashq)
DEFINE_PROCEDURE(sp_hashq)
{
  return calchash(NT__ANY, hashq);
}

/*#| (hashv obj htsize) |#*/
DEFINE_INITIAL_BINDING("hashv", sp_hashv)
DEFINE_PROCEDURE(sp_hashv)
{
  return calchash(NT__ANY, hashv);
}

/*#| (hash-char char k) |#*/
DEFINE_INITIAL_BINDING("hash-char", sp_hashchar)
DEFINE_PROCEDURE(sp_hashchar)
{
  return calchash(NT_CHAR, hash_char);
}

/*#| (hash-char-ci char k) |#*/
DEFINE_INITIAL_BINDING("hash-char-ci", sp_hashchari)
DEFINE_PROCEDURE(sp_hashchari)
{
  return calchash(NT_CHAR, hash_char_ci);
}

/*#| (hash-string string k) |#*/
DEFINE_INITIAL_BINDING("hash-string", sp_hashstr)
DEFINE_PROCEDURE(sp_hashstr)
{
  return calchash(NT_STRING, hash_string);
}

/*#| (hash-string-ci str k) |#*/
DEFINE_INITIAL_BINDING("hash-string-ci", sp_hashstri)
DEFINE_PROCEDURE(sp_hashstri)
{
  return calchash(NT_STRING, hash_string_ci);
}


