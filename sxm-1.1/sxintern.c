/* sxintern.c - symbol/keyword table routines */

#include "sxm.h"
#include "define.h"

/* tables are initialized in sxinit.c */
DEFINE_VARIABLE(sv_symtable)
DEFINE_VARIABLE(sv_keytable)

/* hashstring - hash a string (case-sensitive) */
static size_t hashstring(const tchar_t* str, size_t len, size_t hsize)
{
  size_t z = len & 1 ? str[len-1] + NT_STRING : NT_STRING;
  for (len /= 2; len; len--, str += 2) {
    z = (z >> 8) | (z << 8);
    z += str[0];
    z += str[1] << 4;
  }
  return z % hsize;
}

/* lookup - returns weak pair or so_nil if allocation is prohibited */
/* GC note: can invoke GC if allocp but _after_ use of name */
/* GC note: automatic GC does not remove dead weak pairs from lists! */
static SOBJ lookup(const tchar_t *name, SOBJ table, bool_t allocp)
{
  ss_size_t nlen = tcslen(name);
  sv_size_t i = hashstring(name, nlen, getvsize(table));
  SOBJ wlst, sym;
  SOBJ dead = so_nil; /* points to reclaimed entry, if any */
  /* walk thru weak list */
  for (wlst = getelement(table, i); weakpairp(wlst); wlst = cdr(wlst)) {
    sym = car(wlst);
    if (bwpp(sym)) dead = wlst; /* remember reclaimed entry */
    else if (nlen == getpslength(sym) - 1 &&
             tmemcmp(name, getpstring(sym), nlen) == 0)
      /* name is found */
      return wlst;
  }
  /* name is not found */
  /* if allocation is prohibited, return nil */
  if (!allocp) return so_nil;
  /* return or allocate and return new weak pair */
  /* GC note: this part may invalidate name but we are not using it here */
  if (weakpairp(dead)) return dead;
  wlst = consa(so_bwp, getelement(table, i), NT_WEAKPAIR);
  setelement(table, i, wlst);
  return wlst;
}

/* sxSymbolTableIntern - intern string into a table */
SOBJ sxSymbolTableIntern(const tchar_t *name, SOBJ table, vmtag_t ntype)
{
  /* lookup and allocate entry if needed */
  SOBJ wpair = lookup(name, table, TRUE);
  /* if symbol/keyword is found, return it */
  SOBJ sym = car(wpair);
  if (!bwpp(sym)) return sym;
  /* create new symbol/keyword and put it into table */
  sym = mksym(name, ntype);
  setcar(wpair, sym);
  return sym;
}

/* sxSymbolTableLookup - check if string is in the table */
SOBJ sxSymbolTableLookup(const tchar_t *name, SOBJ table)
{
  /* lookup but do not allocate entry if absent */
  SOBJ wpair = lookup(name, table, FALSE);
  if (wpair == so_nil) return so_false; /* no entry */
  else { /* car contains the symbol object or bwp */
    SOBJ sym = car(wpair);
    return bwpp(sym) ? so_false : sym;
  }
}

/* sxCompactSymbolTable - remove dead entries from the table */
void sxCompactSymbolTable(SOBJ table)
{
  SOBJ wlst;
  sv_size_t n;
  if (!vectorp(table)) return;
  for (n = getvsize(table); n--;) {
     SOBJ *pos = getvdata(table) + n;
     for (wlst = *pos; !nullp(wlst); wlst = cdr(wlst)) {
       if (bwpp(car(wlst)))
         *pos = cdr(wlst);
       else
         pos = &(wlst->n_cdr);
     }
  }
}

/* sxInternString - intern static c string as symbol/keyword */
SOBJ sxInternString(const tchar_t *name, vmtag_t ntype)
{
  SOBJ table = (ntype == NT_KEYWORD) ? sv_keytable : sv_symtable;
  return sxSymbolTableIntern(name, table, ntype);
}

/* sxIntern - intern a string-like object as symbol/keyword */
SOBJ sxIntern(SOBJ src, vmtag_t ntype)
{
  SOBJ table = (ntype == NT_KEYWORD) ? sv_keytable : sv_symtable;
  SOBJ sym;
  gcLock(src);
  { /* can't use sxSymbolTableIntern: src is not a static string */
    /* lookup and allocate entry if needed */
    SOBJ wpair = lookup(getstring(src), table, TRUE);
    /* if symbol/keyword is found, use it */
    sym = car(wpair);
    if (bwpp(sym)) {
      /* create new symbol/keyword and put it into table */
      sym = cvsym(src, ntype);
      setcar(wpair, sym);
    }
  }
  gcUnlock(1);
  return sym;
}

/* sxIsInterned - check if a symbol/keyword is interned */
bool_t sxIsInterned(SOBJ src, vmtag_t ntype)
{
  SOBJ table = (ntype == NT_KEYWORD) ? sv_keytable : sv_symtable;
  SOBJ sym;
  gcLock(src);
  sym = sxSymbolTableLookup(getstring(src), table);
  gcUnlock(1);
  return sym == src;
}

