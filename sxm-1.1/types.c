/* types.c - built-in predicates */

#include "sxm.h"
#include "define.h"
#include "extern.h"

/*#| (%tag obj) |#*/
DEFINE_INITIAL_BINDING("%tag", sp_igettag)
DEFINE_PROCEDURE(sp_igettag)
{
  SOBJ obj = xlonearg();
  return cvsfixnum(xntype(obj));
}

/*#| (%set-tag! obj itag) |#*/
DEFINE_INITIAL_BINDING("%set-tag!", sp_isettag)
DEFINE_PROCEDURE(sp_isettag)
{
  SOBJ obj = xlgetarg();
  SOBJ newtag = xlgasfixnum();
  xllastarg();
  if (!nodep(obj)) sxErr(T("immediate object"), obj);
  obj->n_type = (int)getsfixnum(newtag);
  return obj;
}

/*#| (%mutability obj) |#*/
DEFINE_INITIAL_BINDING("%mutability", sp_igetmut)
DEFINE_PROCEDURE(sp_igetmut)
{
  SOBJ obj = xlonearg();
  return nodep(obj) ? cvbool(ismutable(obj)) : so_false;
}

/*#| (%set-mutability! obj boolean) |#*/
DEFINE_INITIAL_BINDING("%set-mutability!", sp_isetmut)
DEFINE_PROCEDURE(sp_isetmut)
{
  SOBJ obj = xlgetarg();
  SOBJ newmut = xlgetarg();
  xllastarg();
  if (!nodep(obj)) sxErr(T("immediate object"), obj);
  if (falsep(newmut)) protect(obj); else unprotect(obj);
  return obj;
}

/*#| (%at num) |#*/
DEFINE_INITIAL_BINDING("%at", sp_iat)
DEFINE_PROCEDURE(sp_iat)
{
  SOBJ addr = xlgafixnum();
  xllastarg();
  return (SOBJ)getfixnum(addr);
}

/*#| (%handle-value handle) |#*/
DEFINE_INITIAL_BINDING("%handle-value", sp_ihndlval)
DEFINE_PROCEDURE(sp_ihndlval)
{
  SOBJ obj = xlgahandle();
  xllastarg();
  return cvfixnum((FIXTYPE)gethandle(obj));
}

/*#| (%handle-tag handle) |#*/
DEFINE_INITIAL_BINDING("%handle-tag", sp_ihndltag)
DEFINE_PROCEDURE(sp_ihndltag)
{
  SOBJ obj = xlgahandle();
  xllastarg();
  return cvfixnum((FIXTYPE)gethtype(obj));
}

/*#| (%make-handle h type) |#*/
DEFINE_INITIAL_BINDING("%make-handle", sp_imakehndl)
DEFINE_PROCEDURE(sp_imakehndl)
{
  /* get the handle value & type */
  SOBJ h = xlgafixnum();
  SOBJ ht = xlgafixnum();
  xllastarg();
  return cvhandle((OSHANDLE)getfixnum(h), (int)getfixnum(ht));
}

/*#| (built-in-procedure? obj) |#*/
DEFINE_INITIAL_BINDING("built-in-procedure?", sp_builtinp)
DEFINE_PROCEDURE(sp_builtinp)
{
  SOBJ arg = xlonearg();
  return cvbool(xntype(arg) == NT_SUBR);
}

/*#| (closure? obj) |#*/
DEFINE_INITIAL_BINDING("closure?", sp_closurep)
DEFINE_PROCEDURE(sp_closurep)
{
  SOBJ arg = xlonearg();
  return cvbool(closurep(arg));
}

/*#| (weak-pair? obj) |#*/
DEFINE_INITIAL_BINDING("weak-pair?", sp_weakpairp)
DEFINE_PROCEDURE(sp_weakpairp)
{
  SOBJ arg = xlonearg();
  return cvbool(weakpairp(arg));
}

/*#| (code? obj) |#*/
DEFINE_INITIAL_BINDING("code?", sp_codep)
DEFINE_PROCEDURE(sp_codep)
{
  SOBJ arg = xlonearg();
  return cvbool(codep(arg));
}

/*#| (promise? obj) |#*/
DEFINE_INITIAL_BINDING("promise?", sp_promisep)
DEFINE_PROCEDURE(sp_promisep)
{
  SOBJ arg = xlonearg();
  return cvbool(promisep(arg));
}

/*#| (handle? obj) |#*/
DEFINE_INITIAL_BINDING("handle?", sp_handlep)
DEFINE_PROCEDURE(sp_handlep)
{
  SOBJ arg = xlonearg();
  return cvbool(handlep(arg));
}

