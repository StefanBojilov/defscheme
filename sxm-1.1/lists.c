/* lists.c - standard procedures 6.3 p.2 */

#include "sxm.h"
#include "define.h"

typedef bool_t (*binfunc_t)(SOBJ, SOBJ);

/*#| (null? obj) |#*/
DEFINE_INITIAL_BINDING("null?", sp_null)
DEFINE_PROCEDURE(sp_null)
{
  SOBJ arg = xlonearg();
  return cvbool(nullp(arg));
}

/*#| (list? obj) |#*/
DEFINE_INITIAL_BINDING("list?", sp_listp)
DEFINE_PROCEDURE(sp_listp)
{
  SOBJ list = xlonearg();
  return cvbool(listp(list));
}

/*#| (list obj ...) |#*/
DEFINE_INITIAL_BINDING("list", sp_list)
DEFINE_PROCEDURE(sp_list)
{
  SOBJ val = cvlist(vmsp, vmargc, so_nil);
  xlpoprest();
  return val;
}

/*#| (list* obj ... tail) |#*/
DEFINE_INITIAL_BINDING("list*", sp_list1)
DEFINE_PROCEDURE(sp_list1)
{
  SOBJ val;
  switch (vmargc) {
    case 0:  val = so_nil; break;
    case 1:  val = top(); break;
    default: val = cvlist(vmsp, vmargc-1, vmsp[vmargc-1]); break;
  }
  xlpoprest();
  return val;
}

/*#| (length list) |#*/
DEFINE_INITIAL_BINDING("length", sp_length)
DEFINE_PROCEDURE(sp_length)
{
  SOBJ lst = xlgalist();
  xllastarg();
  return cvfixnum(length(lst));
}

/*#| (append list ...) |#*/
DEFINE_INITIAL_BINDING("append", sp_append)
DEFINE_PROCEDURE(sp_append)
{
  SOBJ val, last;

  /* append each argument */
  for (val = last = so_nil; vmargc > 1; ) {
    /* append each element of next list to the result list */
    SOBJ next = nextarg();
    if (nullp(next))  continue;
    if (!consp(next)) xlreqlist(next);
    while (consp(next)) {
      SOBJ tnew = cons(car(next),next);
      next = cdr(next);
      if (nullp(last)) { val = tnew; gcLock(val); }
      else setcdr(last, tnew);
      last = tnew;
    }
  }

  /* tack on the last argument */
  if (moreargs()) {
    if (nullp(last)) val = nextarg();
    else { setcdr(last, nextarg()); gcUnlock(1); }
  }

  return val;
}

/* apprev - common routine for reverse and append-reverse [?] */
static SOBJ apprev(SOBJ lst, SOBJ val)
{
  /* append each element of this list to the result list */
  while (consp(lst)) {
    SOBJ tmp = car(lst);
    lst = cdr(lst);
    gcLock(lst);
    val = cons(tmp, val);
    gcUnlock(1);
  }
  /* return the list */
  return val;
}

/*#| (reverse list) |#*/
DEFINE_INITIAL_BINDING("reverse", sp_reverse)
DEFINE_PROCEDURE(sp_reverse)
{
  SOBJ lst = xlgalist();
  xllastarg();
  return apprev(lst, so_nil);
}

/*#| (append-reverse list to) |#*/
DEFINE_INITIAL_BINDING("append-reverse", sp_apprev)
DEFINE_PROCEDURE(sp_apprev)
{
  SOBJ lst = xlgalist();
  SOBJ to = xlgetarg();
  xllastarg();
  return apprev(lst, to);
}

/* apprevi - common routine for reverse! and append-reverse! [?] */
static SOBJ apprevi(SOBJ lst, SOBJ val)
{
  /* reverse! the list in place */
  while (consp(lst)) {
    SOBJ tmp = cdr(lst);
    setcdr(testmutable(lst), val);
    val = lst;
    lst = tmp;
  }
  /* return the result */
  return val;
}

/*#| (reverse! list) |#*/
DEFINE_INITIAL_BINDING("reverse!", sp_reversei)
DEFINE_PROCEDURE(sp_reversei)
{
  SOBJ lst = xlgalist();
  xllastarg();
  return apprevi(lst, so_nil);
}

/*#| (append-reverse! list to) |#*/
DEFINE_INITIAL_BINDING("append-reverse!", sp_apprevi)
DEFINE_PROCEDURE(sp_apprevi)
{
  SOBJ lst = xlgalist();
  SOBJ to = xlgetarg();
  xllastarg();
  return apprevi(lst, to);
}

/* nth - - common routine for list-ref/list-tail */
static SOBJ nth(int carflag)
{
  FIXTYPE n;

  /* get n and the list */
  SOBJ list = xlgalist();
  SOBJ arg = xlgasfixnum();
  xllastarg();

  /* range check the index */
  if ((n = getsfixnum(arg)) < 0)
    sxErr(T("index out of range"),arg);

  /* find the nth cdr */
  for (; consp(list) && n; n--) list = cdr(list);

  /* make sure the list was long enough */
  if (n || (carflag && nullp(list)))
    sxErr(T("index out of range"), arg);

  /* return the list beginning at the nth element */
  return carflag ? car(list) : list;
}

/*#| (list-ref list n) |#*/
DEFINE_INITIAL_BINDING("list-ref", sp_listref)
DEFINE_PROCEDURE(sp_listref)
{
  return nth(TRUE);
}

/*#| (list-tail list n) |#*/
DEFINE_INITIAL_BINDING("list-tail", sp_listtail)
DEFINE_PROCEDURE(sp_listtail)
{
  return nth(FALSE);
}


/*#| (last-pair list k) |#*/
DEFINE_INITIAL_BINDING("last-pair", sp_lastpair)
DEFINE_PROCEDURE(sp_lastpair)
{
  SOBJ lst = xlgacons();
  xllastarg();
  while (consp(cdr(lst))) lst = cdr(lst);
  return lst;
}


/* member - common routine for member/memv/memq */
static SOBJ member(binfunc_t eqtest)
{
  SOBJ x = xlgetarg();
  SOBJ lst = xlgetarg();
  xllastarg();
  for (; consp(lst); lst = cdr(lst))
    if ((*eqtest)(x, car(lst))) return lst;
  return so_false;
}

/*#| (member obj list) |#*/
DEFINE_INITIAL_BINDING("member", sp_member)
DEFINE_PROCEDURE(sp_member)
{
  return member(equal);
}

/*#| (memv obj list) |#*/
DEFINE_INITIAL_BINDING("memv", sp_memv)
DEFINE_PROCEDURE(sp_memv)
{
  return member(eqv);
}

/*#| (memq obj list) |#*/
DEFINE_INITIAL_BINDING("memq", sp_memq)
DEFINE_PROCEDURE(sp_memq)
{
  return member(eq);
}


/* assoc - common routine for assoc/assv/assq */
static SOBJ assoc(binfunc_t eqtest)
{
  SOBJ x = xlgetarg();
  SOBJ alst = xlgetarg();
  xllastarg();
  for (; consp(alst); alst = cdr(alst)) {
    SOBJ pair = car(alst);
    if (consp(pair) && (*eqtest)(x, car(pair))) return pair;
  }
  return so_false;
}

/*#| (assoc obj alist) |#*/
DEFINE_INITIAL_BINDING("assoc", sp_assoc)
DEFINE_PROCEDURE(sp_assoc)
{
  return assoc(equal);
}

/*#| (assv obj alist) |#*/
DEFINE_INITIAL_BINDING("assv", sp_assv)
DEFINE_PROCEDURE(sp_assv)
{
  return assoc(eqv);
}

/*#| (assq obj alist) |#*/
DEFINE_INITIAL_BINDING("assq", sp_assq)
DEFINE_PROCEDURE(sp_assq)
{
  return assoc(eq);
}


/* nremove - common routine for remove!/remv!/remq! */
static SOBJ nremove(binfunc_t eqtest)
{
  SOBJ prev,curr;
  SOBJ x = xlgetarg();
  SOBJ lst = xlgalist();
  xllastarg();
  for (prev = so_nil, curr = lst; consp(curr); curr = cdr(curr)) {
    if ((*eqtest)(x, car(curr))) {
      if (nullp(prev)) lst = cdr(lst);
      else setcdr(testmutable(prev), cdr(curr));
    } else
      prev = curr;
  }
  return lst;
}

/*#| (remove! obj list) |#*/
DEFINE_INITIAL_BINDING("remove!", sp_nremove)
DEFINE_PROCEDURE(sp_nremove)
{
  return nremove(equal);
}

/*#| (remv! obj list) |#*/
DEFINE_INITIAL_BINDING("remv!", sp_nremv)
DEFINE_PROCEDURE(sp_nremv)
{
  return nremove(eqv);
}

/*#| (remq! obj list) |#*/
DEFINE_INITIAL_BINDING("remq!", sp_nremq)
DEFINE_PROCEDURE(sp_nremq)
{
  return nremove(eq);
}


/* position - common routine for position/posv/posq */
static SOBJ position(binfunc_t eqtest)
{
  SOBJ x = xlgetarg();
  SOBJ lst = xlgetarg();
  FIXTYPE p;
  xllastarg();
  for (p = 0; consp(lst); ++p, lst = cdr(lst))
    if ((*eqtest)(x, car(lst))) return cvfixnum(p);
  return so_false;
}

/*#| (position obj list) |#*/
DEFINE_INITIAL_BINDING("position", sp_position)
DEFINE_PROCEDURE(sp_position)
{
  return position(equal);
}

/*#| (posv obj list) |#*/
DEFINE_INITIAL_BINDING("posv", sp_posv)
DEFINE_PROCEDURE(sp_posv)
{
  return position(eqv);
}

/*#| (posq obj list) |#*/
DEFINE_INITIAL_BINDING("posq", sp_posq)
DEFINE_PROCEDURE(sp_posq)
{
  return position(eq);
}


/* substitution procedure: recursive by car */
static void do_nsubst(binfunc_t eqtest, SOBJ nx, SOBJ ox, SOBJ tree)
{
  for (;consp(tree); tree = cdr(tree)) {
    /* process car */
    if ((*eqtest)(ox, car(tree)))
      setcar(testmutable(tree), nx);
    else if (consp(car(tree)))
      do_nsubst(eqtest, nx, ox, car(tree));
    /* process cdr */
    if ((*eqtest)(ox, cdr(tree))) {
      setcdr(testmutable(tree), nx);
      break; /* end of rest loop */
    }
  }
}

/* nsubst - common routine for subst!/substv!/substq! */
static SOBJ nsubst(binfunc_t eqtest)
{
  /* get the expressions and the list */
  SOBJ nx = xlgetarg();
  SOBJ ox = xlgetarg();
  SOBJ tree = xlgalist(); /* so_nil or cons */
  xllastarg();

  /* do substitutions destructively */
  do_nsubst(eqtest, nx, ox, tree);

  return tree;
}

/*#| (subst! new old tree) |#*/
DEFINE_INITIAL_BINDING("subst!", sp_nsubst)
DEFINE_PROCEDURE(sp_nsubst)
{
  return nsubst(equal);
}

/*#| (substv! new old tree) |#*/
DEFINE_INITIAL_BINDING("substv!", sp_nsubstv)
DEFINE_PROCEDURE(sp_nsubstv)
{
  return nsubst(eqv);
}

/*#| (substq! new old tree) |#*/
DEFINE_INITIAL_BINDING("substq!", sp_nsubstq)
DEFINE_PROCEDURE(sp_nsubstq)
{
  return nsubst(eq);
}

