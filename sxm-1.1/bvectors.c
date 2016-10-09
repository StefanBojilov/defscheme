/* bvectors.c - byte vectors (with SRFI4-like interface) */

#include "sxm.h"
#include "define.h"

/*#| (u8vector? obj) |#*/
DEFINE_INITIAL_BINDING("u8vector?", sp_u8vectorp)
DEFINE_PROCEDURE(sp_u8vectorp)
{
  SOBJ arg = xlonearg();
  return cvbool(bvectorp(arg));
}

/*#| (u8vector-length u8vec) |#*/
DEFINE_INITIAL_BINDING("u8vector-length", sp_u8veclen)
DEFINE_PROCEDURE(sp_u8veclen)
{
  SOBJ bvec = xlgabvector();
  xllastarg();
  return cvsfixnum(getbcount(bvec));
}


/*#| (u8vector u8int ...) |#*/
DEFINE_INITIAL_BINDING("u8vector", sp_u8vector)
DEFINE_PROCEDURE(sp_u8vector)
{
  byte_t* bp;
  SOBJ bvec = newbytevec(vmargc);
  for (bp = getbytes(bvec); moreargs();)
    *bp++ = (byte_t)getsfixnum(xlgabyte());
  return bvec;
}

/*#| (make-u8vector k) |#*/
/*#| (make-u8vector k u8int) |#*/
DEFINE_INITIAL_BINDING("make-u8vector", sp_makeu8vec)
DEFINE_PROCEDURE(sp_makeu8vec)
{
  SOBJ k = xlgasfixnum();
  FIXTYPE flen = getsfixnum(k);
  int b = (int)(optarg() ? getsfixnum(xlgabyte()) : -1);
  xllastarg();
  if (flen < 0 || flen > SX_MAX_BVECTOR_BYTES) {
    sxae_range(k, cvfixnum(SX_MAX_BVECTOR_BYTES));
    return so_false; /* never returns */
  } else {
    ss_size_t len = (ss_size_t)flen;
    SOBJ bvec = newbytevec(len);
    /* initialize the vector */
    if (b != -1) memset(getbytes(bvec), b, len);
    /* return the new vector */
    return bvec;
  }
}


/*#| (u8vector-ref u8vec k) |#*/
DEFINE_INITIAL_BINDING("u8vector-ref", sp_u8vecref)
DEFINE_PROCEDURE(sp_u8vecref)
{
  /* get the vector and the index */
  SOBJ bvec = xlgabvector();
  SOBJ index = xlgasfixnum();
  FIXTYPE i = getsfixnum(index);
  xllastarg();
  if (i < 0 || i >= (FIXTYPE)getbcount(bvec))
    sxae_range(index, cvsfixnum(getbcount(bvec)));
  return cvsfixnum(getbytes(bvec)[(ss_size_t)i]);
}


/*#| (u8vector-set! u8vec k u8int) |#*/
DEFINE_INITIAL_BINDING("u8vector-set!", sp_u8vecset)
DEFINE_PROCEDURE(sp_u8vecset)
{
  SOBJ bvec = xlgabvector();
  SOBJ index = xlgasfixnum();
  FIXTYPE i = getsfixnum(index);
  int b = (int)(getsfixnum(xlgabyte()));
  xllastarg();
  if (i < 0 || i >= (FIXTYPE)getbcount(bvec))
    sxae_range(index, cvsfixnum(getbcount(bvec)));
  getbytes(testmutable(bvec))[(ss_size_t)i] = b;
  return so_void;
}


/*#| (u8vector->list u8vec) |#*/
DEFINE_INITIAL_BINDING("u8vector->list", sp_u8vec2list)
DEFINE_PROCEDURE(sp_u8vec2list)
{
  SOBJ bvec = xlgabvector();
  ss_size_t size = getbcount(bvec);
  SOBJ lst;
  xllastarg();
  /* GC NOTE: bytes can be moved, don't use pointers! */
  gcLock(bvec);
  for (lst = so_nil; size > 0; size--) {
    lst = cons(cvsfixnum(getbytes(bvec)[size-1]), lst);
  }
  gcUnlock(1);
  return lst;
}

/*#| (list->u8vector list) |#*/
DEFINE_INITIAL_BINDING("list->u8vector", sp_list2u8vec)
DEFINE_PROCEDURE(sp_list2u8vec)
{
  SOBJ lst = xlgalist();
  ss_size_t size = length(lst);
  SOBJ bvec; byte_t *p;
  xllastarg();
  gcLock(lst);
  bvec = newbytevec(size);
  gcUnlock(1);
  for (p = getbytes(bvec); size; --size, lst = cdr(lst))
   if (bytep(car(lst)))
     *p++ = (byte_t)getsfixnum(car(lst));
   else
     xlreqbyte(car(lst));
  return bvec;
}

