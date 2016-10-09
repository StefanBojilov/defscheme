/* locales.c - gcell/locale procedures */

#include "sxm.h"
#include "sxintern.h"
#include "sxlocale.h"
#include "define.h"
#include "extern.h"

EXTERN_VARIABLE(sv_curloc)

/*#| (current-locale [locale]) |#*/
DEFINE_INITIAL_BINDING("current-locale", sp_curloc)
DEFINE_PROCEDURE_VARACCESS(sp_curloc, sv_curloc, NT_VECTOR)


/*#| (with-id-interpretation-in-locale locale thunk) |#*/
DEFINE_INITIAL_BINDING("with-id-interpretation-in-locale", sp_with_loc)
DEFINE_PROCEDURE(sp_with_loc)
{
  SOBJ newloc = xlgavector();
  SOBJ thunk = xlgetarg();
  xllastarg();
  /* call thunk in the new dynamic environment */
  PROC_FLUID_BIND(sp_curloc, newloc, thunk, sv_curloc);
}

/*#| (make-locale name [size]) |#*/
DEFINE_INITIAL_BINDING("make-locale", sp_makelocale)
DEFINE_PROCEDURE(sp_makelocale)
{
  SOBJ loc;
  SOBJ sym = xlgasymbol();
  FIXTYPE size = optarg() ? getsfixnum(xlgasfixnum()) : 37;
  xllastarg();
  if (size < 0 || size > SX_MAX_VECTOR_ELTS - FIRSTLENTRY)
    sxErr(T("illegal locale length"), cvfixnum(size));
  /* create and initialize the locale vector */
  gcLock(sym);
  loc = newlocale((sv_size_t)size);
  gcUnlock(1);
  setlname(loc, sym);
  return loc;
}

/*#| (locale-name locale) |#*/
DEFINE_INITIAL_BINDING("locale-name", sp_localename)
DEFINE_PROCEDURE(sp_localename)
{
  SOBJ loc = xlgavector();
  xllastarg();
  if (getvsize(loc) < 3)
    sxErr(T("illegal locale format"), loc);
  return getlname(loc);
}

