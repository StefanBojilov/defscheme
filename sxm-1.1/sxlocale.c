/* sxlocale.c - locale and gcell functions */

#include "sxm.h"
#include "sxhash.h"
#include "sxintern.h"
#include "sxlocale.h"
#include "define.h"

DEFINE_VARIABLE(sv_curloc) /* initialized in sxinit.c */

/* sxLocFindEx - find gcell in locale */
SOBJ sxLocFindEx(SOBJ loc, SOBJ sym, SOBJ *pentry, bool_t *pfpublic, bool_t *pfown)
{
  int i;
  SOBJ p;

  if (!vectorp(loc)) sxErr(T("invalid locale"), loc);
  i = FIRSTLENTRY + sxHashq(sym, getvsize(loc) - FIRSTLENTRY);

  /* new format of locale entry: (sym . gcell) */
  for (p = getelement(loc, i); consp(p); p = cdr(p)) {
    SOBJ entry = car(p);

    if (!consp(entry) || !gcellp(cdr(entry)))
      sxErr(T("bad locale entry"), entry);
    if (car(entry) == sym) {
      SOBJ gcell = cdr(entry);

      if (pentry != NULL)
        *pentry = entry;
      if (pfpublic != NULL)
        *pfpublic = isimmutable(entry);
      if (pfown != NULL)
        *pfown = (loc == getgcellhome(gcell));
      return gcell;
    }
  }
  return so_false;
}

/* sxLocFind - find gcell into locale, (do not enter it if absent) */
SOBJ sxLocFind(SOBJ loc, SOBJ sym, bool_t public_only)
{
  bool_t fpublic;
  SOBJ gcell = sxLocFindEx(loc, sym, NULL, &fpublic, NULL);
  if (public_only && !falsep(gcell))
    return fpublic ? gcell : so_false;
  return gcell;
}

static void testmutableloc (SOBJ loc)
{
  if (isimmutable(loc)) /* esl: specific error */
    sxErr(T("cannot create new bindings in read-only locale"), getlname(loc));
}

/* sxLocEnter - enter new gcell for sym into locale or find old one */
SOBJ sxLocEnter(SOBJ loc, SOBJ sym)
{
  int i = FIRSTLENTRY + sxHashq(sym, getvsize(loc) - FIRSTLENTRY);
  SOBJ p;

  if (!vectorp(loc))
    sxFail(T("nonvector locale"));
  /* new format of locale entry: (sym . gcell) */
  for (p = getelement(loc, i); consp(p); p = cdr(p)) {
    if (!consp(car(p)))
      sxFail(T("bad locale entry"));
    if (car(car(p)) == sym)
      return cdr(car(p));
  }
  testmutableloc(loc);
  gcLock(sym);
  gcLock(loc);
  p = cvgcell(so_unbound, sym, loc);
  setelement(loc, i, cons(cons(sym, p), getelement(loc, i)));
  gcUnlock(2);
  return p;
}

/* sxCurrentLocEnter - enter gcell into the current locale */
SOBJ sxCurrentLocEnter(SOBJ sym)
{
  return sxLocEnter(sv_curloc, sym);
}

/* sxLocImport - import gcell from another locale. Returns FALSE
   if sym is already bound in dest locale; TRUE otherwise */
bool_t sxLocImport(SOBJ loc, SOBJ sym, SOBJ gcell, bool_t fpublish)
{
  int i = FIRSTLENTRY + sxHashq(sym, getvsize(loc) - FIRSTLENTRY);
  SOBJ p;
  if (!vectorp(loc)) sxFail(T("nonvector locale"));
  /* new format of locale entry: (sym . gcell) */
  for (p = getelement(loc, i); consp(p); p = cdr(p)) {
    if (!consp(car(p)))
      sxFail(T("bad locale entry"));
    if (car(car(p)) == sym)
      return FALSE; /* sym is already here */
  }
  testmutableloc(loc);
  gcLock(sym);
  gcLock(loc);
  p = cons(sym, gcell);
  if (fpublish) protect(p);  /* set 'public' flag */
  setelement(loc, i, cons(p, getelement(loc, i)));
  gcUnlock(2);
  return TRUE;
}
