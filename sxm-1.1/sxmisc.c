/* sxmisc.c - miscellaneous commonly-used functions */

#include <math.h>
#include "sxm.h"

/* listp - list? predicate, safe for self-reference */
bool_t listp(SOBJ list)
{
  SOBJ slow = list;
  for (;;) {
    if (nullp(list)) return TRUE;
    if (!consp(list)) return FALSE;
    if ((list = cdr(list)) == slow) return FALSE;
    if (nullp(list)) return TRUE;
    if (!consp(list)) return FALSE;
    if ((list = cdr(list)) == slow) return FALSE;
    slow = cdr(slow);
  }
}

/* length - find the length of a list */
size_t length(SOBJ list)
{
  size_t len;
  for (len = 0; consp(list); list = cdr(list)) ++len;
  return len;
}

/* eq - 'eq?' predicate */
/* includes sfixnums, symbols & frobs ... */
bool_t eq(SOBJ arg1, SOBJ arg2)
{
  return arg1 == arg2;
}

/* eqv - 'eqv?' predicate */
bool_t eqv(SOBJ arg1, SOBJ arg2)
{
  /* try the eq test first */
  if (arg1 == arg2) return TRUE;
  /* one or both - SFIXNUMS? */
  if (!node2p(arg1, arg2)) {
    return sfixp(arg1)
      ? (sfixp(arg2) ? FALSE : (_ntype(arg2)==NT_FIXNUM && getsfixnum(arg1) == arg2->n_int))
      : (sfixp(arg2) ? (_ntype(arg1)==NT_FIXNUM && getsfixnum(arg2) == arg1->n_int) : FALSE);
  }
  /* both arguments are nodes here */
  if (_ntype(arg1) != _ntype(arg2)) return FALSE;
  /* compare fixnums, flonums, characters, and handles */
  switch (_ntype(arg1)) {
    case NT_FIXNUM:
      return arg1->n_int == arg2->n_int;
    case NT_FLONUM:
      return arg1->n_flonum == arg2->n_flonum;
    case NT_HANDLE:
      return (handlep(arg2) && gethandle(arg1) == gethandle(arg2)
              && gethtype(arg1) == gethtype(arg2));
    case NT_CHAR:
      return getchcode(arg1) == getchcode(arg2);
  }
  return FALSE;
}

/* vectorequal - compare two vectors */
static bool_t vectorequal(SOBJ v1, SOBJ v2)
{
  sv_size_t i;
  if ((i = getvsize(v1)) != getvsize(v2)) return FALSE;
  while (i-- != 0)
    if (!equal(getelement(v1, i), getelement(v2, i))) return FALSE;
  return TRUE;
}

/* recordequal - compare two records */
static bool_t recordequal(SOBJ r1, SOBJ r2)
{
  sv_size_t i;
  if ((i = getrsize(r1)) != getrsize(r2)) return FALSE;
  if (getrelement(r1, 0) != getrelement(r2, 0)) return FALSE;
  while (i-- != 1)
    if (!equal(getrelement(r1, i), getrelement(r2, i))) return FALSE;
  return TRUE;
}

/* equal - 'equal?' predicate - NOT SAFE, can overflow run-time stack */
bool_t equal(SOBJ arg1, SOBJ arg2)
{
  for (;;) { /* car-recursive! */
    /* try the eq? and eqv? test first */
    if (arg1 == arg2)  return TRUE;
    if (!node2p(arg1, arg2)) { /* one ore both - SFIXNUMS */
      return sfixp(arg1)
        ? (sfixp(arg2) ? FALSE : (_ntype(arg2)==NT_FIXNUM && getsfixnum(arg1) == arg2->n_int))
        : (sfixp(arg2) ? (_ntype(arg1)==NT_FIXNUM && getsfixnum(arg2) == arg1->n_int) : FALSE);
    }
    if (_ntype(arg1) != _ntype(arg2)) return FALSE;
    /* compare structured objects */
    switch (_ntype(arg1)) { /* all eqv? tags should be here! */
      case NT_STRING: /* compare (non-hashed) string-like nodes */
        return getslength(arg1) == getslength(arg2)
            && !tmemcmp(getstring(arg2), getstring(arg1), getslength(arg1));
      case NT_CONS: /* compare cons-like nodes */
      case NT_BOX:
      case NT_CLOSURE:
      case NT_PROMISE:
        if (!equal(car(arg1), car(arg2)))
          return FALSE;
        arg1 = cdr(arg1);
        arg2 = cdr(arg2);
        continue;
      case NT_RECORD: /* compare rdts by eq?, fields by equal? */
        return recordequal(arg1, arg2);
      case NT_VECTOR: /* compare vector-like nodes */
      case NT_CODE:
      case NT_HASHTABLE:
        return vectorequal(arg1, arg2);
        /* copy eqv? comparisons for efficiency */
      case NT_CHAR:
        return getchcode(arg1) == getchcode(arg2);
      case NT_FIXNUM:
        return arg1->n_int == arg2->n_int;
      case NT_FLONUM:
        return arg1->n_flonum == arg2->n_flonum;
      case NT_HANDLE:
        return eqv(arg1, arg2);
      default:
        return FALSE;
    }
  }
}

/* char_eq - 'char=?' predicate (type-safe) */
bool_t char_eq(SOBJ arg1, SOBJ arg2)
{
  if (!charp(arg1) || !charp(arg2)) return FALSE;
  return getchcode(arg1) == getchcode(arg2);
}

/* char_ci_eq - 'char-ci=?' predicate (type-safe) */
bool_t char_ci_eq(SOBJ arg1, SOBJ arg2)
{
  if (!charp(arg1) || !charp(arg2)) return FALSE;
  return totlower(getchcode(arg1)) == totlower(getchcode(arg2));
}

/* string_eq - 'string=?' predicate (type-safe) */
bool_t string_eq(SOBJ arg1, SOBJ arg2)
{
  if (stringp(arg1) && stringp(arg2)) {
    ss_size_t start = 0;
    ss_size_t end = getslength(arg1);
    if (getslength(arg2) != end) return FALSE;
    else {
      tchar_t *p1 = getstring(arg1);
      tchar_t *p2 = getstring(arg2);
      for (; start < end; start++) {
        tint_t ch1 = (*p1++) & (tchar_t)-1;
        tint_t ch2 = (*p2++) & (tchar_t)-1;
        if (ch1 != ch2) return FALSE;
      }
      return TRUE;
    }
  } else return FALSE;
}

/* string_ci_eq - 'string-ci=?' predicate (type-safe) */
bool_t string_ci_eq(SOBJ arg1, SOBJ arg2)
{
  if (stringp(arg1) && stringp(arg2)) {
    ss_size_t start = 0;
    ss_size_t end = getslength(arg1);
    if (getslength(arg2) != end) return FALSE;
    else {
      tchar_t *p1 = getstring(arg1);
      tchar_t *p2 = getstring(arg2);
      for (; start < end; start++) {
        tint_t ch1 = (*p1++) & (tchar_t)-1;
        tint_t ch2 = (*p2++) & (tchar_t)-1;
        if (istupper(ch1)) ch1 = totlower(ch1);
        if (istupper(ch2)) ch2 = totlower(ch2);
        if (ch1 != ch2) return FALSE;
      }
      return TRUE;
    }
  } else return FALSE;
}

bool_t wildicmp(const tchar_t *src, const tchar_t *model)
{
  for (; *src; ++src, ++model) {
    switch (*model) {
      case T('\0'):
        return FALSE;
      case T('?'):
        break;
      case T('*'):
        for (++model; *model == T('?') || *model == T('*'); ++model);
          if (*model == T('\0'))
            return TRUE;
        for (; *src; ++src)
          if (totupper(*src) == totupper(*model) &&
              wildicmp(src + 1, model + 1))
            return TRUE;
        return FALSE;
      case T('\\'):
        if (model[1] == T('*') || model[1] == T('?') || model[1] == T('\\'))
          ++model;
      default:
        if (totupper(*src) != totupper(*model))
          return FALSE;
        break;
    }
  }
  return *model == 0 || (model[0] == T('*') && model[1] == 0);
}

