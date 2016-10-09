/* gcells.c - gcell/locale procedures */

#include "sxm.h"
#include "sxintern.h"
#include "sxlocale.h"
#include "define.h"
#include "extern.h"

/*#| (gcell? obj) |#*/
DEFINE_INITIAL_BINDING("gcell?", sp_gcellp)
DEFINE_PROCEDURE(sp_gcellp)
{
  SOBJ arg = xlonearg();
  return cvbool(gcellp(arg));
}

/*#| (make-gcell symbol locale [value]) |#*/
DEFINE_INITIAL_BINDING("make-gcell", sp_makegcell)
DEFINE_PROCEDURE(sp_makegcell)
{
  SOBJ sym = xlgasymbol();
  SOBJ loc = xlgavector();
  SOBJ val = optarg() ? xlgetarg() : so_unbound;
  xllastarg();
  return cvgcell(val, sym, loc);
}

EXTERN_VARIABLE(sv_curloc)

/*#| (find-gcell symbol [locale]) |#*/
DEFINE_INITIAL_BINDING("find-gcell", sp_findgcell)
DEFINE_PROCEDURE(sp_findgcell)
{
  SOBJ sym = xlgasymbol();
  SOBJ loc = optarg() ? xlgavector() : sv_curloc;
  xllastarg();
  return sxLocFind(loc, sym, FALSE);
}

/*#| (find-public-gcell sym [locale]) |#*/
DEFINE_INITIAL_BINDING("find-public-gcell", sp_findpubgcell)
DEFINE_PROCEDURE(sp_findpubgcell)
{
  SOBJ sym = xlgasymbol();
  SOBJ loc = optarg() ? xlgavector() : sv_curloc;
  xllastarg();
  return sxLocFind(loc, sym, TRUE);
}

/*#| (symbol->gcell sym [locale]) |#*/
DEFINE_INITIAL_BINDING("symbol->gcell", sp_sym2gcell)
DEFINE_PROCEDURE(sp_sym2gcell)
{
  SOBJ sym = xlgasymbol();
  SOBJ loc = optarg() ? xlgavector() : sv_curloc;
  xllastarg();
  return sxLocEnter(loc, sym);
}

/*#| (gcell->symbol gcell) |#*/
/*#| (gcell-name gcell) |#*/
DEFINE_INITIAL_BINDING("gcell->symbol", sp_gcell2sym)
DEFINE_INITIAL_BINDING("gcell-name", sp_gcell2sym)
DEFINE_PROCEDURE(sp_gcell2sym)
{
  SOBJ gcl = xlgagcell();
  xllastarg();
  return getgcellname(gcl);
}

/*#| (gcell-home-locale gcell) |#*/
DEFINE_INITIAL_BINDING("gcell-home-locale", sp_gcellhome)
DEFINE_PROCEDURE(sp_gcellhome)
{
  SOBJ gcl = xlgagcell();
  xllastarg();
  return getgcellhome(gcl);
}

/*#| (find-gcell&flags sym [locale]) |#*/
DEFINE_INITIAL_BINDING("find-gcell&flags", sp_findgcellwflags)
DEFINE_PROCEDURE(sp_findgcellwflags)
{
  bool_t fpublic = FALSE;
  bool_t fown = FALSE;
  SOBJ gcell;

  /* get arguments */
  SOBJ sym = xlgasymbol();
  SOBJ loc = optarg() ? xlgavector() : sv_curloc;
  xllastarg();

  /* find entry in locale  */
  gcell = sxLocFindEx(loc, sym, NULL, &fpublic, &fown);

  /* return multiple values: gcell, public?, own? */
  PUSH_ARG(cvbool(fown));
  PUSH_ARG(cvbool(fpublic));
  PUSH_ARG(gcell);
  PROC_RETURN_VALUES(3);
}

/*#| (replace-gcell! sym gcell [locale]) |#*/
DEFINE_INITIAL_BINDING("replace-gcell!", sp_rplacgcell)
DEFINE_PROCEDURE(sp_rplacgcell)
{
  SOBJ oldgcell, entry = so_false;
  SOBJ sym = xlgasymbol();
  SOBJ gcell = xlgagcell();
  SOBJ loc = optarg() ? xlgavector() : sv_curloc;
  xllastarg();
  oldgcell = sxLocFindEx(loc, sym, &entry, NULL, NULL);
  if (falsep(oldgcell)) return so_false;
  if (!consp(entry)) sxErr(T("bad entry"), entry);
  setcar(entry, gcell);
  return oldgcell;
}

/*#| (enter-private-gcell! gcell sym [locale]) |#*/
DEFINE_INITIAL_BINDING("enter-private-gcell!", sp_entprivgcell)
DEFINE_PROCEDURE(sp_entprivgcell)
{
  SOBJ gcell = xlgagcell();
  SOBJ sym = xlgasymbol();
  SOBJ loc = optarg() ? xlgavector() : sv_curloc;
  xllastarg();
  return sxLocImport(loc, sym, gcell, FALSE) ? gcell : so_false;
}

/*#| (enter-public-gcell! gcell sym [locale]) |#*/
DEFINE_INITIAL_BINDING("enter-public-gcell!", sp_entpubgcell)
DEFINE_PROCEDURE(sp_entpubgcell)
{
  SOBJ gcell = xlgagcell();
  SOBJ sym = xlgasymbol();
  SOBJ loc = optarg() ? xlgavector() : sv_curloc;
  xllastarg();
  return sxLocImport(loc, sym, gcell, TRUE) ? gcell : so_false;
}

/*#| (gcell-bound? gcell) |#*/
DEFINE_INITIAL_BINDING("gcell-bound?", sp_gcellboundp)
DEFINE_PROCEDURE(sp_gcellboundp)
{
  SOBJ gcl = xlgagcell();
  xllastarg();
  return cvbool(gcellboundp(gcl));
}

/*#| (gcell-value gcell) |#*/
DEFINE_INITIAL_BINDING("gcell-value", sp_gcellval)
DEFINE_PROCEDURE(sp_gcellval)
{
  SOBJ gcell = xlgagcell();
  xllastarg();
  return getgcellval(gcell);
}

/*#| (set-gcell-value! gcell obj) |#*/
DEFINE_INITIAL_BINDING("set-gcell-value!", sp_setgcellval)
DEFINE_PROCEDURE(sp_setgcellval)
{
  SOBJ gcell = xlgagcell();
  SOBJ val = xlgetarg();
  xllastarg();
  setgcellval(testmutable(gcell), val);
  return so_void;
}

/*#| (top-level-bound? symbol [locale]) |#*/
DEFINE_INITIAL_BINDING("top-level-bound?", sp_topboundp)
DEFINE_PROCEDURE(sp_topboundp)
{
  SOBJ gcell;
  SOBJ sym = xlgasymbol();
  SOBJ loc = optarg() ? xlgavector() : sv_curloc;
  xllastarg();
  gcell = sxLocFind(loc, sym, FALSE);
  if (falsep(gcell)) return so_false;
  return cvbool(gcellboundp(gcell));
}

/*#| (top-level-value symbol [locale]) |#*/
DEFINE_INITIAL_BINDING("top-level-value", sp_gettopval)
DEFINE_PROCEDURE(sp_gettopval)
{
  SOBJ gcell;
  SOBJ sym = xlgasymbol();
  SOBJ loc = optarg() ? xlgavector() : sv_curloc;
  xllastarg();
  gcell = sxLocFind(loc, sym, FALSE);
  if (falsep(gcell)) return so_unbound;
  return getgcellval(gcell);
}

/*#| (set-top-level-value! symbol obj [locale]) |#*/
DEFINE_INITIAL_BINDING("set-top-level-value!", sp_settopval)
DEFINE_PROCEDURE(sp_settopval)
{
  /* get the symbol, value, and optional locale */
  SOBJ sym = xlgasymbol();
  SOBJ val = xlgetarg();
  SOBJ loc = optarg() ? xlgavector() : sv_curloc;
  xllastarg();

  /* set the global value */
  gcLock(sym);
  gcLock(val);
  setgcellval(testmutable(sxLocEnter(loc, sym)), val);
  gcUnlock(2);
  return val;
}

