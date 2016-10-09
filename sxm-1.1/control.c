/* control.c - standard procedures 6.9 */

#include "sxm.h"
#include "define.h"
#include "extern.h"

/************ Basic Procedures *************/

/*#| (procedure? obj) |#*/
DEFINE_INITIAL_BINDING("procedure?", sp_procp)
DEFINE_PROCEDURE(sp_procp)
{
  SOBJ arg = xlonearg();
  return cvbool(procedurep(arg));
}

/*#| (apply proc obj1 ... args) |#*/
DEFINE_INITIAL_BINDING("apply", sp_apply)
DEFINE_PROCEDURE(sp_apply)
{
  SOBJ proc = xlgetarg();    /* get the procedure */
  PROC_GOTO(proc, vm_flatten_star_args(vmargc));
}


/************ Call/CC Procedure *************/

/* code for continuation closure */
EXTERN_DATUM(sd_brestore)

/*#| (call/esc proc) |#*/
DEFINE_INITIAL_BINDING("call/esc", sp_callesc)
DEFINE_PROCEDURE(sp_callesc)
{
  SOBJ proc = xlonearg();
  SOBJ cont;
  gcLock(proc); /* should not be in the STACK */

  /* create a "light" continuation closure */
  cont = cvclosure(sd_brestore, cvsfixnum(vmstktop-vmsp));
  gcUnlock(1);

  /* setup the argument list and apply the function*/
  PUSH_ARG(cont);
  PROC_GOTO(proc, 1);
}

/*#| (call-with-current-continuation proc) |#*/
/*#| (call/cc proc) |#*/
DEFINE_INITIAL_BINDING("call-with-current-continuation", sp_callcc)
DEFINE_INITIAL_BINDING("call/cc", sp_callcc)
DEFINE_PROCEDURE(sp_callcc)
{
  SOBJ proc = xlonearg();
  SOBJ cont;

  gcLock(proc);  /* should NOT be in the stack */
  /* create a real continuation closure */
  cont = cvclosure(sd_brestore, newcontinuation());
  gcUnlock(1);

  /* setup the argument list and apply the function*/
  PUSH_ARG(cont);
  PROC_GOTO(proc, 1);
}

/********** Multiple Value Procedures ***********/

/*#| (call-with-values thunk receiver) |#*/
/*#| (call/mv thunk receiver) |#*/
DEFINE_INITIAL_BINDING("call-with-values", sp_callmv)
DEFINE_INITIAL_BINDING("call/mv", sp_callmv)
DEFINE_PROCEDURE(sp_callmv)
{
  /* get the thunk and the procedure */
  SOBJ thunk    = xlgetarg();
  SOBJ receiver = xlgetarg();
  xllastarg();

  /* push CALL-WITH-VALUES continuation */
  PUSH_CALLMV_CONT(receiver);
  PROC_GOTO(thunk, 0);
}

/*#| (values ob11 ...) |#*/
DEFINE_INITIAL_BINDING("values", sp_values)
DEFINE_PROCEDURE(sp_values)
{
  PROC_RETURN_VALUES(PROC_ARGC());
}

/*#| (values* obj1 ... args) |#*/
DEFINE_INITIAL_BINDING("values*", sp_valuesstar)
DEFINE_PROCEDURE(sp_valuesstar)
{
  PROC_RETURN_VALUES(vm_flatten_star_args(PROC_ARGC()));
}

/*#| (make-values n) |#*/
/*#| (make-values n fill) |#*/
DEFINE_INITIAL_BINDING("make-values", sp_makevalues)
DEFINE_PROCEDURE(sp_makevalues)
{
  /* get the counter and default value */
  SOBJ cnt = xlgasfixnum();
  int n = (int)getsfixnum(cnt);
  SOBJ def = optarg() ? nextarg() : so_void;
  int i;
  xllastarg();
  if (n < 0) sxae_range(cnt, so_nil);
  for (i = n; i > 0; i--)
    PUSH_ARG(def);
  PROC_RETURN_VALUES(n);
}

/* sp_callwcln_aux - perform cleanup and return args to continuation */
DEFINE_PROCEDURE(sp_callwcln_aux)
{
  SOBJ cleanup = xlgetarg();
  /* push continuation that will return other values */
  PUSH_VALUES_CONT(vmargc);
  /* do cleanup */
  PROC_GOTO(cleanup, 0);
}

/*#| (call-with-cleanup-thunk proc cleanup) |#*/
DEFINE_INITIAL_BINDING("call-with-cleanup-thunk", sp_callwcleanup)
DEFINE_PROCEDURE(sp_callwcleanup)
{
  /* get the thunk and the procedure */
  SOBJ proc = xlgetarg();
  SOBJ cleanup = xlgetarg();
  xllastarg();
  /* push cleanup mv continuation and call proc */
  PUSH_ARG(cleanup); /* extra arg for sp_callwcln_aux */
  cleanup = CURRY(sp_callwcln_aux, 1);  /* see above */
  PUSH_CALLMV_CONT(cleanup);
  PROC_GOTO(proc, 0);
}

/********** Dynamic Wind Procedures ************/

/*#| (%current-dynamic-state [newds]) |#*/
DEFINE_INITIAL_BINDING("%current-dynamic-state", sp_curdstate)
DEFINE_PROCEDURE(sp_curdstate)
{
  if (moreargs()) { /* "set" parameter (does re-rooting!) */
    SOBJ newds = xlgacons();
    xllastarg();
    if (!consp(vmdstate) || car(vmdstate) != so_false)
      sxErr(T("bad dstate"), vmdstate);
    return vm_reroot(newds);
  } else { /* get parameter */
    xllastarg();
    return vmdstate;
  }
}

/*#| (dynamic-wind before during after) |#*/
DEFINE_INITIAL_BINDING("dynamic-wind", sp_dynwind)
DEFINE_PROCEDURE(sp_dynwind)
{
  SOBJ before = xlgetarg();
  SOBJ during = xlgetarg();
  SOBJ after = xlgetarg();
  xllastarg();
  PROC_DYNAMIC_WIND(before, during, after);
}

/******* Exception System Procedures ********/

/*#| (raise exception) |#*/
DEFINE_INITIAL_BINDING("raise", sp_raise)
DEFINE_PROCEDURE(sp_raise)
{
  SOBJ exn = xlonearg();
  PROC_RAISE(exn);
}

/*#| (current-exception-handler [handler]) |#*/
DEFINE_INITIAL_BINDING("current-exception-handler", sp_curexnhandler)
DEFINE_PROCEDURE(sp_stdexnhandler)
{
  SOBJ exn = xlgetarg();
  xllastarg();
  sxErr(T("exception not handled"), exn);
  never_returns(so_void);
}
DEFINE_VARIABLE_VARINIT(sv_curexnhandler, sp_stdexnhandler)
DEFINE_PROCEDURE_VARACCESS(sp_curexnhandler, sv_curexnhandler, NT__PROC)

/*#| (call-with-handler handler thunk) |#*/
DEFINE_INITIAL_BINDING("call-with-handler", sp_with_handler)
/*#| (with-exception-handler handler thunk) |#*/
DEFINE_INITIAL_BINDING("with-exception-handler", sp_with_handler)
DEFINE_PROCEDURE(sp_with_handler)
{
  SOBJ newhandler = xlgaprocedure();
  SOBJ thunk = xlgaprocedure();
  xllastarg();
  /* call thunk in the new dynamic environment */
  PROC_FLUID_BIND(sp_curexnhandler, newhandler, thunk, sv_curexnhandler);
}


/************** Curry Procedure *************/

/* curry - create simple closure ((curry f a b) c d) == (f a b c d) */
/*#| (curry proc arg1 ...) |#*/
DEFINE_INITIAL_BINDING("curry", sp_curry)
DEFINE_PROCEDURE(sp_curry)
{
  SOBJ proc = xlgetarg();
  return CURRY(proc, vmargc);
}

/*************** Map Procedures ***************/

/* do_maploop - setup for the next application */
static int do_maploop(SOBJ* sp, int argc, SOBJ cont)
{
  SOBJ* p;
  int cnt;

  /* get a pointer to the end of the argument list */
  p = sp + argc;

  push(cont);
  push(vmenv);

  /* build the argument list for the next application */
  for (cnt = argc; --cnt >= 1;) {
    SOBJ x = *--p;
    if (consp(x)) {
      push(car(x));
      *p = cdr(x);
    } else if (nullp(x)) {
      vmsp = sp + argc;
      return FALSE;
    } else
      sxErr(T("arguments must be lists"), x);
  }
  return TRUE;
}

/* continuation for map */
DEFINE_CONTINUATION(sk_map, result)
{
  SOBJ* sp = vmsp;
  SOBJ val;
  int argc;

  /* add result to the end of the list */
  if (consp(sp[1])) { /* continue list */
    result = cons(result, so_nil); /* make a list from a result */
    setcdr(sp[1], result);         /* add the new value to the tail */
    sp[1] = result;                /* remember the new tail */
  } else /* first result: build the initial list */
    sp[1] = sp[2] = cons(result, so_nil);

  /* convert the argument count and loop */
  argc = (int)getsfixnum(sp[0]);
  val = sp[2];
  if (do_maploop(sp+3, argc, sk_map))
    CONT_GOTO(sp[3], argc-1);
  else
    CONT_RETURN(val);
}

/*#| (map proc list1 list2 ...) |#*/
DEFINE_INITIAL_BINDING("map", sp_map)
DEFINE_PROCEDURE(sp_map)
{
  SOBJ* sp = vmsp;
  if (vmargc < 2) sxae_toofew();
  check(vmargc+3+2);
  /* save a continuation */
  push(so_nil);  /* head */
  push(so_nil);  /* tail */
  push(cvfixnum((FIXTYPE)vmargc));
  if (do_maploop(sp, vmargc, sk_map))
    PROC_GOTO(*sp, vmargc-1);
  else
    PROC_RETURN(so_nil);
}

static SOBJ do_xmap(SOBJ cont, SOBJ defret)
{
  SOBJ* sp = vmsp;
  if (vmargc < 2) sxae_toofew();
  check(vmargc+1+2);
  /* save a continuation */
  push(cvfixnum((FIXTYPE)vmargc));
  if (!do_maploop(sp, vmargc, cont))
    PROC_RETURN(defret);
  else
    PROC_GOTO(*sp, vmargc-1);
}

DEFINE_CONTINUATION(sk_foreach, result)
{
  SOBJ* sp = vmsp;
  int argc;
  /* convert the argument count and loop */
  result = sp[0]; /* use result to make compiler happy! */
  argc = (int)getsfixnum(result);
  if (do_maploop(sp+1, argc, sk_foreach))
    CONT_GOTO(sp[1], argc-1);
  else
    CONT_RETURN(so_void);
}

/*#| (for-each func list1 list2 ...) |#*/
DEFINE_INITIAL_BINDING("for-each", sp_foreach)
DEFINE_PROCEDURE(sp_foreach)
{
  return do_xmap(sk_foreach, so_void);
}

DEFINE_CONTINUATION(sk_andmap, result)
{
  SOBJ* sp = vmsp;
  int argc;
  /* convert the argument count and loop */
  argc = (int)getsfixnum(sp[0]);
  if (falsep(result)) {
    vmsp = sp + argc + 1;
    CONT_RETURN(result);
  }
  if (do_maploop(sp+1, argc, sk_andmap))
    CONT_GOTO(sp[1], argc-1);
  else
    CONT_RETURN(result);
}

/*#| (andmap func list1 list2 ...) |#*/
DEFINE_INITIAL_BINDING("andmap", sp_andmap)
DEFINE_PROCEDURE(sp_andmap)
{
  return do_xmap(sk_andmap, so_true);
}

DEFINE_CONTINUATION(sk_ormap, result)
{
  SOBJ* sp = vmsp;
  int argc;
  /* convert the argument count and loop */
  argc = (int)getsfixnum(sp[0]);
  if (truep(result)) {
    vmsp = sp + argc + 1;
    CONT_RETURN(result);
  }
  if (do_maploop(sp+1, argc, sk_ormap))
    CONT_GOTO(sp[1], argc-1);
  else
    CONT_RETURN(result);
}

/*#| (ormap func list1 list2 ...) |#*/
DEFINE_INITIAL_BINDING("ormap", sp_ormap)
DEFINE_PROCEDURE(sp_ormap)
{
  return do_xmap(sk_ormap, so_false);
}


/******** Lazy Evaluation Procedures ********/

/*#| (make-promise thunk) |#*/
DEFINE_INITIAL_BINDING("make-promise", sp_mkpromise)
DEFINE_PROCEDURE(sp_mkpromise)
{
  SOBJ arg = xlgaclosure();
  xllastarg();
  return cvpromise(arg);
}

/* continuation for force */
DEFINE_CONTINUATION(sk_force, result)
{
  SOBJ promise = CONT_ENV();
  /* to be conformant with R4RS: promise thunk may be recursive! */
  if (nullp(getpproc(promise)))
    CONT_RETURN(getpvalue(promise));
  setpvalue(promise, result);
  setpproc(promise, so_nil);
  CONT_RETURN(result);
}

/*#| (force promise) |#*/
DEFINE_INITIAL_BINDING("force", sp_force)
DEFINE_PROCEDURE(sp_force)
{
  SOBJ promise = xlonearg();
  if (promisep(promise)) {
    SOBJ thunk = getpproc(promise);
    /* force the promise the first time */
    if (!nullp(thunk)) {
      PUSH_CONT(sk_force, promise);
      PROC_GOTO(thunk, 0);
    } else {
      /* return the saved value if the promise has already been forced */
      PROC_RETURN(getpvalue(promise));
    }
  } else /* otherwise, just return the argument */
    PROC_RETURN(promise);
}


/********** Miscellaneous Procedures *********/

/*#| (identity-procedure obj) |#*/
/*#| (identity obj) |#*/
DEFINE_INITIAL_BINDING("identity-procedure", sp_identity)
DEFINE_INITIAL_BINDING("identity", sp_identity)
DEFINE_PROCEDURE(sp_identity)
{
  return xlonearg();
}

/*#| (void) |#*/
DEFINE_INITIAL_BINDING("void", sp_void)
DEFINE_PROCEDURE(sp_void)
{
  xllastarg();
  return so_void;
}

/*#| (default-object? obj) |#*/
DEFINE_INITIAL_BINDING("default-object?", sp_defobjp)
DEFINE_PROCEDURE(sp_defobjp)
{
  SOBJ arg = xlonearg();
  return cvbool(arg == so_default);
}

