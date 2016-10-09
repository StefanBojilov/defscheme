/* sxhtab.c - hash table functions */

#include "sxm.h"
#include "sxhash.h"
#include "sxhtab.h"


/* gethttype - define hash table type */
static void gethttype(SOBJ ht, sobjcmpfun_t *peqfcn, sobjhashfun_t *phfcn, 
                      int *poff, SOBJ *pdef)
{
  SOBJ e0 = gethelement(ht, 0);
  /* in plain eq? table all slots are used for overflow alists */
  int htt = HTT_EQ; *poff = 0; *pdef = so_false;
  /* in non-plain table 1st slot holds table type, 2nd holds default */
  if (sfixp(e0)) { htt = (int)(getsfixnum(e0)); *poff = 2; *pdef = gethelement(ht, 1); }
  /* htt -> procedures */
  switch (htt) {
    default:
    case HTT_EQ:        *peqfcn = eq; *phfcn = hashq; break;
    case HTT_EQV:       *peqfcn = eqv; *phfcn = hashv; break;
    case HTT_EQUAL:     *peqfcn = equal; *phfcn = hash; break;
    case HTT_NUMBER:    *peqfcn = eqv; *phfcn = hashv; break;
    case HTT_CHAR:      *peqfcn = char_eq; *phfcn = hash_char; break;
    case HTT_CHAR_CI:   *peqfcn = char_ci_eq; *phfcn = hash_char_ci; break;
    case HTT_STRING:    *peqfcn = string_eq; *phfcn = hash_string; break;
    case HTT_STRING_CI: *peqfcn = string_ci_eq; *phfcn = hash_string_ci; break;
  }
} 

/* findprop - find a key/value pair */
static SOBJ findprop(SOBJ p, SOBJ key, sobjcmpfun_t eqfcn)
{
  for (; consp(p) && consp(cdr(p)); p = cdr(cdr(p)))
    if ((eqfcn)(car(p), key)) return cdr(p);
  return so_nil;
}

/* sxMakeHashTable - make a generic hash table */
SOBJ sxMakeHashTable(int htt, sv_size_t hsize, SOBJ def)
{
  if (htt == HTT_EQ && def == so_false) {
    /* plain eq? hash table */
    return newhashtable(hsize); /* inited by so_nil */
  } else {
    /* non-plain hash table: 2 reserved slots for htt and def */
    SOBJ ht;
    gcLock(def);
    ht = newhashtable(hsize + 2); /* inited by so_nil */
    gcUnlock(1);
    sethelement(ht, 0, cvsfixnum(htt));
    sethelement(ht, 1, def);
    return ht;
  }
}

/* sxHashTableGet - lookup the value of a key */
SOBJ sxHashTableGet(SOBJ ht, SOBJ key, bool_t *pfound)
{
  int i; SOBJ p;
  sobjcmpfun_t eqfcn; sobjhashfun_t hfcn; int off; SOBJ def;
  gethttype(ht, &eqfcn, &hfcn, &off, &def);
  i = (*hfcn)(key, gethsize(ht) - off) + off;
  p = findprop(gethelement(ht, i), key, eqfcn);
  if (pfound != NULL) *pfound = !nullp(p);
  return nullp(p) ? def : car(p);
}

/* sxHashTablePut - put a value for a key */
bool_t sxHashTablePut(SOBJ ht, SOBJ key, SOBJ val, bool_t frepl)
{
  int i; SOBJ pair;
  sobjcmpfun_t eqfcn; sobjhashfun_t hfcn; int off; SOBJ def;
  gethttype(ht, &eqfcn, &hfcn, &off, &def);
  i = (*hfcn)(key, gethsize(ht) - off) + off;
  pair = findprop(gethelement(ht, i), key, eqfcn);
  if (!nullp(pair)) {
    if (!frepl) return FALSE;
    setcar(pair, val);
  } else { /* no replace needed */
    sethelement(ht, i, cons(key, cons(val, gethelement(ht, i))));
  }
  return TRUE;
}

/* sxHashTableRemove - remove a property from the hash table */
SOBJ sxHashTableRemove(SOBJ ht, SOBJ key)
{
  int i; SOBJ p, q;
  sobjcmpfun_t eqfcn; sobjhashfun_t hfcn; int off; SOBJ def;
  gethttype(ht, &eqfcn, &hfcn, &off, &def);
  i = (*hfcn)(key, gethsize(ht) - off) + off;
  for (p = gethelement(ht, i), q = so_nil;
       consp(p) && consp(cdr(p));
       q = cdr(p), p = cdr(q)) {
    if ((eqfcn)(car(p), key)) {
      if (!nullp(q))
        setcdr(q, cdr(cdr(p)));
      else
        sethelement(ht, i, cdr(cdr(p)));
      return car(cdr(p));
    }
  }
  return def;
}

