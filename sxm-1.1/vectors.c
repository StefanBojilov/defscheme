/* vectors.c - standard procedures 6.8 */

#include "sxm.h"
#include "define.h"

/*#| (vector? obj) |#*/
DEFINE_INITIAL_BINDING("vector?", sp_vectorp)
DEFINE_PROCEDURE(sp_vectorp)
{
  SOBJ arg = xlonearg();
  return cvbool(vectorp(arg));
}

/* common vector reference routine */
static SOBJ vrefset(SOBJ vector, bool_t setp)
{
  SOBJ index = xlgafixnum();
  FIXTYPE i = getfixnum(index);
  if (i < 0 || i >= (FIXTYPE)getvsize(vector))
    sxae_range(index, cvsfixnum(getvsize(vector)));
  if (setp) {
    SOBJ val = xlgetarg();
    xllastarg();
    getvdata(vector)[(sv_size_t)i] = val;
    return so_void;
  } else {
    xllastarg();
    return getvdata(vector)[(sv_size_t)i];
  }
}

/* common vector->list converter */
static SOBJ vlist(SOBJ vector)
{
  SOBJ val;
  sv_size_t size;
  xllastarg();
  /* make a list from the vector */
  gcLock(vector);
  size = getvsize(vector);
  /* GC NOTE: vector data can be moved, don't use pointers! */
  for (val = so_nil; size != 0;)
    val = cons(getelement(vector, --size), val);
  gcUnlock(1);
  return val;
}

/*#| (make-vector k) |#*/
/*#| (make-vector k fill) |#*/
DEFINE_INITIAL_BINDING("make-vector", sp_makevector)
DEFINE_PROCEDURE(sp_makevector)
{
  /* get the vector size and fill value */
  SOBJ vlen = xlgasfixnum();
  FIXTYPE len = getsfixnum(vlen);
  SOBJ fill = optarg() ? nextarg() : so_nil;
  xllastarg();
  if (len < 0 || len > SX_MAX_VECTOR_ELTS)
    sxErr(T("illegal vector length"), vlen);
  /* create and initialize the vector */
  return newvector((sv_size_t)len, fill);
}

/*#| (vector obj ...) |#*/
DEFINE_INITIAL_BINDING("vector", sp_vector)
DEFINE_PROCEDURE(sp_vector)
{
  SOBJ vect;
  if (vmargc < 0 || vmargc > SX_MAX_VECTOR_ELTS)
    sxErr(T("illegal vector length"), cvfixnum(vmargc));
  vect = newvector(vmargc, NV_NO_INIT);
  sobjcpy(getvdata(vect), vmsp, vmargc);
  xlpoprest();
  return vect;
}

/*#| (vector-length vector) |#*/
DEFINE_INITIAL_BINDING("vector-length", sp_veclen)
DEFINE_PROCEDURE(sp_veclen)
{
  SOBJ arg = xlgavector();
  xllastarg();
  return cvfixnum((FIXTYPE)getvsize(arg));
}

/*#| (%vector-length vector-like) |#*/
DEFINE_INITIAL_BINDING("%vector-length", sp_iveclen)
DEFINE_PROCEDURE(sp_iveclen)
{
  SOBJ arg = xlonearg();
  return cvfixnum((FIXTYPE)getvsize(arg));
}

/*#| (vector-ref vector k) |#*/
DEFINE_INITIAL_BINDING("vector-ref", sp_vecref)
DEFINE_PROCEDURE(sp_vecref)
{
  return vrefset(xlgavector(), FALSE);
}

/*#| (%vector-ref vector-like k) |#*/
DEFINE_INITIAL_BINDING("%vector-ref", sp_ivecref)
DEFINE_PROCEDURE(sp_ivecref)
{
  return vrefset(xlgetarg(), FALSE);
}

/*#| (vector-set! vector k obj) |#*/
DEFINE_INITIAL_BINDING("vector-set!", sp_vecset)
DEFINE_PROCEDURE(sp_vecset)
{
  SOBJ vect = xlgavector();
  return vrefset(testmutable(vect), TRUE);
}

/*#| (%vector-set! vector-like k obj) |#*/
DEFINE_INITIAL_BINDING("%vector-set!", sp_ivecset)
DEFINE_PROCEDURE(sp_ivecset)
{
  SOBJ vect = xlgetarg();
  return vrefset(testmutable(vect), TRUE);
}

/*#| (vector->list vector) |#*/
DEFINE_INITIAL_BINDING("vector->list", sp_vec2list)
DEFINE_PROCEDURE(sp_vec2list)
{
  return vlist(xlgavector());
}

/*#| (%vector->list vector-like) |#*/
DEFINE_INITIAL_BINDING("%vector->list", sp_ivec2list)
DEFINE_PROCEDURE(sp_ivec2list)
{
  return vlist(xlgetarg());
}

/*#| (list->vector list) |#*/
DEFINE_INITIAL_BINDING("list->vector", sp_list2vec)
DEFINE_PROCEDURE(sp_list2vec)
{
  SOBJ* p;
  SOBJ vect;
  sv_size_t size;
  SOBJ lst = xlgalist();
  xllastarg();
  /* make a vector from the list */
  size = length(lst);
  gcLock(lst);
  vect = newvector(size, NV_NO_INIT);
  gcUnlock(1);
  for (p = getvdata(vect); size--; lst = cdr(lst))
    *p++ = car(lst);
  return vect;
}

/*#| (vector-fill! vector obj) |#*/
DEFINE_INITIAL_BINDING("vector-fill!", sp_vecfill)
DEFINE_PROCEDURE(sp_vecfill)
{
  SOBJ* p;
  sv_size_t size;
  SOBJ vect = xlgavector();
  SOBJ fill = xlgetarg();
  xllastarg();
  size = getvsize(testmutable(vect));
  for (p = getvdata(vect); size != 0; --size) *p++ = fill;
  return vect;
}

/*#| (vector-copy vector) |#*/
DEFINE_INITIAL_BINDING("vector-copy", sp_veccopy)
DEFINE_PROCEDURE(sp_veccopy)
{
  SOBJ vect;
  sv_size_t size;
  SOBJ arg = xlgavector();
  xllastarg();
  size = getvsize(arg);
  gcLock(arg);
  vect = newvector(size, NV_NO_INIT);
  sobjcpy(getvdata(vect), getvdata(arg), size);
  gcUnlock(1);
  return vect;
}

