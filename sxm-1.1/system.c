/* system.c - system functions */

#include "sxm.h"
#include "os.h"
#include "sxio.h"
#include "sxdisasm.h"
#include "sxlocale.h"
#include "sxintern.h"
#include "sxwrite.h"
#include "sxread.h"
#include "sximage.h"
#include "define.h"
#include "extern.h"
#include "mem.h"


EXTERN_VARIABLE(sv_conin)
EXTERN_VARIABLE(sv_conout)
EXTERN_VARIABLE(sv_curout)
EXTERN_VARIABLE(sv_curstart) /* defined below */
EXTERN_VARIABLE(sv_curloc)
EXTERN_VARIABLE(sv_curcasesensv)
EXTERN_VARIABLE(sv_symtable)
EXTERN_VARIABLE(sv_arglist)
EXTERN_PROCEDURE(sp_stdrepl)
EXTERN_PROCEDURE(sp_opensrcfile)
EXTERN_PROCEDURE(sp_foreach)
EXTERN_PROCEDURE(sp_values)
EXTERN_DATUM(sd_brestore)
EXTERN_PORT_CLASS(xp_isrc)

/*#| (make-parameter initval [guard-proc]) |#*/
DEFINE_INITIAL_BINDING("make-parameter", sp_makeparam)
DEFINE_CONTINUATION(sk_param, result)
{
  SOBJ valbox = CONT_ENV();
  setboxval(valbox, result);
  CONT_RETURN(so_void);
}
DEFINE_PROCEDURE(sp_param)
{
  SOBJ guard = xlgetarg();   /* curried */
  SOBJ valbox = xlgetarg();    /* curried */
  if (moreargs()) { /* assign nev value */
    SOBJ newval = xlgetarg();
    xllastarg();
    if (guard == so_false) {
      setboxval(valbox, newval);
      return so_void;
    } else {
      PUSH_CONT(sk_param, valbox);
      PUSH_ARG(newval);
      PROC_GOTO(guard, 1);
    }
  } else { /* get current value */
    xllastarg();
    return getboxval(valbox);
  }
}
DEFINE_PROCEDURE(sp_makeparam)
{
  SOBJ init = xlgetarg();
  SOBJ guard = optarg() ? xlgaprocedure() : so_false;
  xllastarg();
  PUSH_ARG(cvbox(init, so_nil));
  PUSH_ARG(guard);
  return CURRY(sp_param, 2);
}


/*#| (free-handle! handle) |#*/
DEFINE_INITIAL_BINDING("free-handle!", sp_ihfree)
DEFINE_PROCEDURE(sp_ihfree)
{
  SOBJ h = xlgahandle();
  xllastarg();
  osfreehandle(gethandle(h), gethtype(h));
  return so_void;
}

/*#| (display-apropos name [oport]) |#*/
DEFINE_INITIAL_BINDING("display-apropos", sp_apropos)
DEFINE_PROCEDURE(sp_apropos)
{
  SOBJ table; tchar_t* cp = NULL; int i;

  /* get the substring/symbol to find and output port */
  SOBJ p = xlgetarg();
  SOBJ port = optarg() ? xlgaoport() : sv_curout;
  xllastarg();

  if (symbolp(p)) cp = getpstring(p);
  else if (stringp(p)) cp = getstring(p);
  else xlreqother(p);

  /* \0 terminates substring to search */

  /* browse through symbol table */
  for (table = sv_symtable, i = 0; i < SX_STSIZE; i++) {
    /* walk thru overflow weak list */
    for (p = getelement(table, i); weakpairp(p); p = cdr(p)) {
      if (!bwpp(car(p)) && tcsstr(getpstring(car(p)), cp) != 0) {
        sxDisplay(car(p), port);
        sxWriteChar(T(' '), port);
      }
    }
  }
  sxNewline(port);
  return so_void;
}

/*#| (oblist) |#*/
DEFINE_INITIAL_BINDING("oblist", sp_oblist)
DEFINE_PROCEDURE(sp_oblist)
{
  bool_t compactp = optarg() ? !falsep(nextarg()) : FALSE;
  xllastarg();
  if (compactp) sxCompactSymbolTable(sv_symtable);
  return sv_symtable;
}


static tchar_t *default_header = T("#!")T(SXMPATH)T(" -h");

/*#| (sxm-engine-path) |#*/
DEFINE_INITIAL_BINDING("sxm-engine-path", sp_sxengpath)
DEFINE_PROCEDURE(sp_sxengpath)
{
  xllastarg();
  return cvstring(T(SXMPATH));
}

/*#| (save-image filename [thunk] [strip] [#!header]) |#*/
DEFINE_INITIAL_BINDING("save-image", sp_saveimage)
DEFINE_PROCEDURE(sp_saveimage)
{
  /* get the file name */
  tchar_t *name = getstring(xlgastring());

  /* get the optional restore proc & exit flag */
  SOBJ rproc = optarg() ? nextarg() : sv_curstart;
  bool_t exitp = optarg() ? !falsep(nextarg()) : FALSE;

  /* get the header line */
  tchar_t *header = optarg() ? getstring(xlgastring()) : default_header;
  bool_t res;
  xllastarg();

  /* check the thunk procedure */
  if (!procedurep(rproc)) xlreqproc(rproc);

  res = sxImgSave(name, rproc, exitp, header);
  if (exitp && res) vm_exit(so_fix0);  /* can't proceed with shaked image */
  return cvbool(res);
}



/*#| (restore-image filename) |#*/
DEFINE_INITIAL_BINDING("restore-image", sp_restoreimage)
DEFINE_PROCEDURE(sp_restoreimage)
{
  /* get the file name, verbose flag and print flag */
  tchar_t* name = getstring(xlgastring());
  SOBJ thunk;
  xllastarg();

  /* restore the saved memory image */
  thunk = sxImgRestore(name);
  if (nullp(thunk)) return so_false;

  /* saved memory image is reinstalled; restart */
  vm_restart(thunk);
  never_returns(so_false);
}


/*#| (collect) |#*/
DEFINE_INITIAL_BINDING("collect", sp_gc)
DEFINE_PROCEDURE(sp_gc)
{
  /* check the argument list and call the garbage collector */
  xllastarg();
  sxMemGC();

  /* return (gccalls nnodes nfree nscount vscount total) */
  PUSH_ARG(cvfixnum(vgccalls));
  PUSH_ARG(cvfixnum(total));
  PUSH_ARG(cvfixnum((FIXTYPE)vscount));
  PUSH_ARG(cvfixnum((FIXTYPE)nscount));
  PUSH_ARG(cvfixnum(nfree));
  PUSH_ARG(cvfixnum(nnodes));
  PUSH_ARG(cvfixnum(gccalls));
  PROC_RETURN_VALUES(7);
}

/*#| (bytes-allocated) |#*/
DEFINE_INITIAL_BINDING("bytes-allocated", sp_bytesalld)
DEFINE_PROCEDURE(sp_bytesalld)
{
  xllastarg();
  return cvfixnum(total);
}

/*#| (display-statistics [oport]) |#*/
DEFINE_INITIAL_BINDING("display-statistics", sp_dispstat)
DEFINE_PROCEDURE(sp_dispstat)
{
  tchar_t buf[120];
  SOBJ port = optarg() ? xlgaoport() : sv_curout;
  xllastarg();
  if (gccalls == 0) stprintf(buf, T("    no collections"));
  else if (gccalls == 1) stprintf(buf, T("    1 collection"));
  else stprintf(buf, T("    %ld collections"), (long)gccalls);
  sxWriteString(buf, port); sxNewline(port);
  stprintf(buf, T("    %ld nodes allocated, %ld of them free"), (long)nnodes, (long)nfree);
  sxWriteString(buf, port); sxNewline(port);
  stprintf(buf, T("    %ld node segments"), (long)nscount);
  sxWriteString(buf, port); sxNewline(port);
  stprintf(buf, T("    %ld vector segments"), (long)vscount);
  sxWriteString(buf, port); sxNewline(port);
  stprintf(buf, T("    %ld bytes allocated"), (long)total);
  sxWriteString(buf, port); sxNewline(port);
  return so_void;
}

EXTERN_VARIABLE(sv_curprlen)
EXTERN_VARIABLE(sv_curprlevel)

/* common message formatter (pops args from stack) */
static void xlconmsg(const tchar_t* what)
{
  if (moreargs()) {
    SOBJ where = xlgetarg();
    sxNewline(sv_conout);
    sxWriteString(what, sv_conout);
    if (where != so_false) 
      sxFormat(T(" in ~a"), sv_conout, 1, &where);
    if (moreargs()) {
      SOBJ msg  = xlgetarg();
      sxWriteString(T(": "), sv_conout);
      if (stringp(msg)) {
        SOBJ orgprlevel = sv_curprlevel, orgprlen = sv_curprlen;
        sv_curprlevel = cvsfixnum(3); sv_curprlen = cvsfixnum(6);
        sxFormat(getstring(msg), sv_conout, vmargc, vmsp);
        sv_curprlevel = orgprlevel; sv_curprlen = orgprlen;
      }
      drop(vmargc); vmargc = 0;
    }
    sxWriteString(T("."), sv_conout);
    sxNewline(sv_conout);
  }
} 


/*#| (internal-exit [integer]) |#*/
DEFINE_INITIAL_BINDING("internal-exit", sp_stdexit)
DEFINE_PROCEDURE(sp_stdexit)
{
  SOBJ retcode = so_fix0;
  if (moreargs()) retcode = xlgafixnum();
  xllastarg();
  vm_exit(retcode); /* correct exit to OS with retcode */
  never_returns(so_false);
}

/*#| (exit-handler [proc]) |#*/
DEFINE_INITIAL_BINDING("exit-handler", sp_curexithandler)
DEFINE_VARIABLE_VARINIT(sv_curexithandler, sp_stdexit)
DEFINE_PROCEDURE_VARACCESS(sp_curexithandler, sv_curexithandler, NT__PROC)

/*#| (exit [integer]) |#*/
DEFINE_INITIAL_BINDING("exit", sp_exit)
DEFINE_PROCEDURE(sp_exit)
{
  PROC_GOTO(sv_curexithandler, PROC_ARGC());
}


/*#| (abort-handler [proc]) |#*/
DEFINE_INITIAL_BINDING("abort-handler", sp_curaborthandler)
DEFINE_PROCEDURE(sp_stdabort)
{
  xllastarg();
  vm_exit(so_fix1); /* exit to OS with retcode 1 */
  never_returns(so_false);
}
DEFINE_VARIABLE_VARINIT(sv_curaborthandler, sp_stdabort)
DEFINE_PROCEDURE_VARACCESS(sp_curaborthandler, sv_curaborthandler, NT__PROC)

/*#| (abort) |#*/
DEFINE_INITIAL_BINDING("abort", sp_abort)
DEFINE_PROCEDURE(sp_abort)
{
  xllastarg();
  PROC_GOTO(sv_curaborthandler, 0);
}


/*#| (reset-handler [proc]) |#*/
DEFINE_INITIAL_BINDING("reset-handler", sp_curresethandler)
DEFINE_PROCEDURE(sp_stdreset)
{
  xllastarg();
  vm_exit(so_fix2); /* exit to OS with retcode 2 */
  never_returns(so_false);
}
DEFINE_VARIABLE_VARINIT(sv_curresethandler, sp_stdreset)
DEFINE_PROCEDURE_VARACCESS(sp_curresethandler, sv_curresethandler, NT__PROC)

/*#| (reset) |#*/
DEFINE_INITIAL_BINDING("reset", sp_reset)
DEFINE_PROCEDURE(sp_reset)
{
  xllastarg();
  PROC_GOTO(sv_curresethandler, 0);
}


/*#| (debug-handler [proc]) |#*/
DEFINE_INITIAL_BINDING("debug-handler", sp_curdebughandler)
DEFINE_PROCEDURE(sp_stddebug)
{
  xllastarg();
  sxWriteString(T("No debugger loaded."), sv_conout);
  sxNewline(sv_conout);
  return so_void;
}
DEFINE_VARIABLE_VARINIT(sv_curdebughandler, sp_stddebug)
DEFINE_PROCEDURE_VARACCESS(sp_curdebughandler, sv_curdebughandler, NT__PROC)


/*#| (break-handler [proc]) |#*/
DEFINE_INITIAL_BINDING("break-handler", sp_curbreakhandler)
DEFINE_PROCEDURE(sp_stdbreak)
{
  xlconmsg(T("Break"));
  PROC_GOTO(sv_curdebughandler, 0);
}
DEFINE_VARIABLE_VARINIT(sv_curbreakhandler, sp_stdbreak)
DEFINE_PROCEDURE_VARACCESS(sp_curbreakhandler, sv_curbreakhandler, NT__PROC)

/*#| (break [where [format arg ...]]) |#*/
DEFINE_INITIAL_BINDING("break", sp_break)
DEFINE_PROCEDURE(sp_break)
{
  PROC_GOTO(sv_curbreakhandler, vmargc);
}


/* sv_curerrorcont is used to store error continuation
 * with strategically placed debugger call on top. It is
 * initialized with a "nothing to debug" warning proc and
 * modified each time the standard error handler is invoked.
 */
DEFINE_PROCEDURE(sp_stderrorcont)
{
  /* ignore "return" value to conform to std cont protocol */
  xlgetarg();
  xllastarg();
  sxNewline(sv_conout);
  sxWriteString(T("Warning in debug: nothing to debug."), sv_conout);
  sxNewline(sv_conout);
  return so_void;
}
DEFINE_VARIABLE_VARINIT(sv_curerrorcont, sp_stderrorcont)

/*#| (debug) |#*/
DEFINE_INITIAL_BINDING("debug", sp_debug)
DEFINE_PROCEDURE(sp_debug)
{
  xllastarg();
  /* we need to pass one value to conform to std cont protocol */
  PUSH_ARG(so_void);
  /* reinstall error cont with debugger call on top */
  PROC_GOTO(sv_curerrorcont, 1);
}


/*#| (internal-error where fmtstr arg ...) |#*/
DEFINE_INITIAL_BINDING("internal-error", sp_stderror)
DEFINE_CONTINUATION(sk_stderror, ignored)
{
  /* (debug) call reinstalls this error continuation */
  CONT_GOTO(sv_curdebughandler, 0);
}
DEFINE_PROCEDURE(sp_stderror)
{
  /* print the location and the error message */
  xlconmsg(T("Error"));

  /* when saved cont is invoked, call (debug-handler) */
  PUSH_CONT(sk_stderror, so_void);

  /* store the error/debug continuation for (debug) call */
  sv_curerrorcont = cvclosure(sd_brestore, newcontinuation()); 

  /* tell user about new possibilities */
  sxWriteString(T("Type (debug) to enter the debugger."), sv_conout);
  sxNewline(sv_conout);

  /* reset to current cafe */
  PROC_GOTO(sp_reset, 0);
}

/*#| (error-handler [proc]) |#*/
DEFINE_INITIAL_BINDING("error-handler", sp_curerrorhandler)
DEFINE_VARIABLE_VARINIT(sv_curerrorhandler, sp_stderror)
DEFINE_PROCEDURE_VARACCESS(sp_curerrorhandler, sv_curerrorhandler, NT__PROC)

/*#| (error where format arg ...) |#*/
DEFINE_INITIAL_BINDING("error", sp_error)
DEFINE_CONTINUATION(sk_error, ignored)
{
  /* return from error handler always resets */
  CONT_GOTO(sv_curresethandler, 0);
}
DEFINE_PROCEDURE(sp_error)
{
  /* insert reset continuation sk_error under args */
  INSERT_CONT(sk_error, so_void);
  /* now if sv_curerrorhandler returns, it will reset */
  PROC_GOTO(sv_curerrorhandler, vmargc);
}


/*#| (warning-handler [proc]) |#*/
DEFINE_INITIAL_BINDING("warning-handler", sp_curwarninghandler)
DEFINE_PROCEDURE(sp_stdwarning)
{
  /* print the location and the warning message */
  xlconmsg(T("Warning"));
  return so_void;
}
DEFINE_VARIABLE_VARINIT(sv_curwarninghandler, sp_stdwarning)
DEFINE_PROCEDURE_VARACCESS(sp_curwarninghandler, sv_curwarninghandler, NT__PROC)

/*#| (warning where format arg ...) |#*/
DEFINE_INITIAL_BINDING("warning", sp_warning)
DEFINE_PROCEDURE(sp_warning)
{
  PROC_GOTO(sv_curwarninghandler, vmargc);
}


/*#| (save-program filename thunk [#!header]) |#*/
DEFINE_INITIAL_BINDING("save-program", sp_saveprog)
DEFINE_PROCEDURE(sp_saveprog)
{
  SOBJ filename = xlgastring();
  SOBJ thunk = xlgetarg();
  SOBJ header = optarg() ? xlgastring() : so_default;
  xllastarg();
  /* (save-image filename thunk #t header) */
  PUSH_ARG(header);
  PUSH_ARG(so_true); /* strip image before saving */
  PUSH_ARG(thunk);
  PUSH_ARG(filename);
  PROC_GOTO(sp_saveimage, 4);
}

/*#| (keyboard-interrupt-handler [proc]) |#*/
DEFINE_INITIAL_BINDING("keyboard-interrupt-handler", sp_curkbinthandler)
DEFINE_PROCEDURE(sp_stdkbint)
{
  xllastarg();
  PROC_GOTO(sv_curbreakhandler, 0);
}
DEFINE_VARIABLE_VARINIT(sv_curkbinthandler, sp_stdkbint)
DEFINE_PROCEDURE_VARACCESS(sp_curkbinthandler, sv_curkbinthandler, NT__PROC)


/*#| (timer-interrupt-handler [proc]) |#*/
DEFINE_INITIAL_BINDING("timer-interrupt-handler", sp_curtimhandler)
DEFINE_PROCEDURE(sp_stdtimer)
{
  xllastarg();
  return sxFail(T("timer interrupt occurred with no handler defined"));
}
DEFINE_VARIABLE_VARINIT(sv_curtimhandler, sp_stdtimer)
DEFINE_PROCEDURE_VARACCESS(sp_curtimhandler, sv_curtimhandler, NT__PROC)

/*#| (set-timer [nticks]) |#*/
DEFINE_INITIAL_BINDING("set-timer", sp_settimer)
DEFINE_PROCEDURE(sp_settimer)
{
  SOBJ arg; FIXTYPE oldval,newval;
  oldval = vm_gettimer();

  /* get the optional argument */
  if (moreargs()) {
    arg = xlgafixnum(); newval = getfixnum(arg);
    if (newval < 0) sxErr(T("negative ticks counter"),arg);
    vm_settimer((long)newval);
  }
  xllastarg();

  /* return the old value of the counter */
  return cvfixnum((FIXTYPE)oldval);
}


/*#| (program-arguments) |#*/
DEFINE_INITIAL_BINDING("program-arguments", sp_progargs)
DEFINE_PROCEDURE(sp_progargs)
{
  xllastarg();
  return sv_arglist;
}


/*#| (expand-memory ns vs) |#*/
DEFINE_INITIAL_BINDING("expand-memory", sp_expandmem)
DEFINE_PROCEDURE(sp_expandmem)
{
  SOBJ ns = xlgasfixnum();
  SOBJ vs = xlgasfixnum();
  xllastarg();
  sxMemExpand((int)getsfixnum(ns), (int)getsfixnum(vs));
  return so_void;
}

/*#| (getenv envvar) |#*/
DEFINE_INITIAL_BINDING("getenv", sp_getenv)
DEFINE_PROCEDURE(sp_getenv)
{
  tchar_t* es = getpstring(xlgastring());
  xllastarg();
  es = osgetenv(es);
  return (es == NULL) ? so_false : cvstring(es);
}

/*#| (system [command]) |#*/
DEFINE_INITIAL_BINDING("system", sp_system)
DEFINE_PROCEDURE(sp_system)
{
  SOBJ arg = optarg() ? xlgastring() : so_nil;
  xllastarg();
  gcLock(arg);
  sxMemGC(); /* get rid of garbage ports, handles etc... */
  gcUnlock(1);
  return cvfixnum((FIXTYPE)ossystem(nullp(arg) ? NULL : getstring(arg)));
}

/*#| (file-exists? filename) |#*/
DEFINE_INITIAL_BINDING("file-exists?", sp_fexistsp)
DEFINE_PROCEDURE(sp_fexistsp)
{
  tchar_t* fn = getstring(xlgastring());
  xllastarg();
  return cvbool(osfprobe(fn,QF_EXIST) != NULL);
}

/*#| (delete-file filename) |#*/
DEFINE_INITIAL_BINDING("delete-file", sp_delfile)
DEFINE_PROCEDURE(sp_delfile)
{
  SOBJ fn = xlgastring();
  xllastarg();
  return !osunlink(getstring(fn)) ? so_true : so_false;
}

/*#| (rename-file oldname newname) |#*/
DEFINE_INITIAL_BINDING("rename-file", sp_renfile)
DEFINE_PROCEDURE(sp_renfile)
{
  SOBJ oldfn = xlgastring();
  SOBJ newfn = xlgastring();
  xllastarg();
  return !osrename(getstring(oldfn), getstring(newfn)) ? so_true : so_false;
}

/*#| (temporary-filename) |#*/
DEFINE_INITIAL_BINDING("temporary-filename", sp_tmpfnam)
DEFINE_PROCEDURE(sp_tmpfnam)
{
  tchar_t *fn;
  xllastarg();
  fn = ostmpnam();
  return (fn == NULL) ? so_false : cvstring(fn);
}

/*#| (get-internal-run-time) |#*/
DEFINE_INITIAL_BINDING("get-internal-run-time", sp_runtime)
DEFINE_PROCEDURE(sp_runtime)
{
  xllastarg();
  return cvfixnum((FIXTYPE)osruntime());
}

/*#| (get-internal-gc-run-time) |#*/
DEFINE_INITIAL_BINDING("get-internal-gc-run-time", sp_gcruntime)
DEFINE_PROCEDURE(sp_gcruntime)
{
  xllastarg();
  return cvfixnum((FIXTYPE)osgctime());
}

/*#| (internal-time-units-per-second) |#*/
DEFINE_INITIAL_BINDING("internal-time-units-per-second", sp_runtimeunits)
DEFINE_PROCEDURE(sp_runtimeunits)
{
  xllastarg();
  return cvflonum((FLOTYPE)osrtunits());
}

/*#| (software-type) |#*/
DEFINE_INITIAL_BINDING("software-type", sp_softwaretype)
DEFINE_PROCEDURE(sp_softwaretype)
{
  xllastarg();
  return sxSymEnter(ostype());
}


/* #| (disassemble closure [pc [port]]) |# */
DEFINE_INITIAL_BINDING("disassemble", sp_disassemble)
DEFINE_PROCEDURE(sp_disassemble)
{
  int off;

  /* get the closure (or code), offset and file pointer */
  SOBJ fun = xlgetarg();
  SOBJ v = optarg() ? xlgasfixnum() : so_nil;
  SOBJ port = optarg() ? xlgaoport() : sv_curout;

  xllastarg();

  off = (nullp(v) ? -1 : (int) getsfixnum(v));

  /* make sure we got either a closure or a code object */
  if (!closurep(fun) && !codep(fun))
    xlreqother(fun);

  /* disassemble the procedure */
  sxDisProc(port, fun, off);
  return so_void;
}


/* evaluator components */


/*#| (current-expand [proc]) |#*/
DEFINE_INITIAL_BINDING("current-expand", sp_curexpand)
DEFINE_PROCEDURE(sp_stdexpand)
{
  SOBJ expr = xlgetarg();
  SOBJ loc = optarg() ? xlgavector() : sv_curloc;
  xllastarg();
  loc = loc; /* "use" it to shut gcc up */
  return expr; /* no expansion is needed in core mode */
}
DEFINE_VARIABLE_VARINIT(sv_curexpand, sp_stdexpand)
DEFINE_PROCEDURE_VARACCESS(sp_curexpand, sv_curexpand, NT__PROC)

/*#| (expand expr [locale]) |#*/
DEFINE_INITIAL_BINDING("expand", sp_expand)
DEFINE_PROCEDURE(sp_expand)
{
  SOBJ expr = xlgetarg();
  SOBJ loc = optarg() ? xlgavector() : sv_curloc;
  xllastarg();
  PUSH_ARG(loc);
  PUSH_ARG(expr);
  PROC_GOTO(sv_curexpand, 2);
}


/*#| (compile-core expr [locale]) |#*/
DEFINE_INITIAL_BINDING("compile-core", sp_compilecore)
DEFINE_PROCEDURE(sp_compilecore)
{
  /* get the expression to compile and the environment */
  SOBJ expr = xlgetarg();
  SOBJ loc = optarg() ? xlgavector() : sv_curloc;
  xllastarg();
  return sxCompile(expr, loc);
}


/*#| (current-eval [proc]) |#*/
DEFINE_INITIAL_BINDING("current-eval", sp_cureval)
DEFINE_PROCEDURE(sp_stdeval)
{
  SOBJ expr = xlgetarg();
  SOBJ loc = optarg() ? xlgavector() : sv_curloc;
  xllastarg();
  /* if expr is (#<closure>), call it directly */
  if (consp(expr) && nullp(cdr(expr)) && (xntype(car(expr)) == NT_CLOSURE))
    PROC_GOTO(car(expr), 0);
  /* compile it and call result */
  expr = sxCompile(expr, loc);
  PROC_GOTO(expr, 0);
}
DEFINE_VARIABLE_VARINIT(sv_cureval, sp_stdeval)
DEFINE_PROCEDURE_VARACCESS(sp_cureval, sv_cureval, NT__PROC)

/*#| (eval expr [locale]) |#*/
DEFINE_INITIAL_BINDING("eval", sp_eval)
DEFINE_PROCEDURE(sp_eval)
{
  SOBJ expr = xlgetarg();
  SOBJ loc = optarg() ? xlgavector() : sv_curloc;
  xllastarg();
  PUSH_ARG(loc);
  PUSH_ARG(expr);
  PROC_GOTO(sv_cureval, 2);
}


/*#| (load filename [eval-proc]) |#*/
DEFINE_INITIAL_BINDING("load", sp_load)
DEFINE_CONTINUATION(sk_ldloop, result)
{
  SOBJ eval = CONT_ENV();
  SOBJ port = top();
  SOBJ obj;
  if (!sxRead(port, &obj)) {
    drop(1); /* port arg */
    /* do not close, signal error condition */
    CONT_RAISE(sxGetLastReadError());
  } else if (obj == so_eof) {
    drop(1); /* port arg */
    sxPortClose(port);
    CONT_RETURN(so_void);
  } else {
    /* port is still on stack */
    PUSH_CONT(sk_ldloop, eval);
    PUSH_ARG(obj);
    CONT_GOTO((eval == so_default) ? sv_cureval : eval, 1);
  }
}
DEFINE_CONTINUATION(sk_load, port)
{
  SOBJ eval = CONT_ENV();
  SOBJ obj;
  if (!sxRead(port, &obj)) {
    /* do not close, signal error condition */
    CONT_RAISE(sxGetLastReadError());
  } else if (obj == so_eof) {
    sxPortClose(port);
    CONT_RETURN(so_void);
  } else {
    PUSH_ARG(port);
    PUSH_CONT(sk_ldloop, eval);
    PUSH_ARG(obj);
    CONT_GOTO((eval == so_default) ? sv_cureval : eval, 1);
  }
}
DEFINE_PROCEDURE(sp_load)
{
  SOBJ filename = xlgastring();
  SOBJ eval = optarg() ? xlgaprocedure() : so_default;
  xllastarg();
  PUSH_CONT(sk_load, eval);
  PUSH_ARG(filename);
  PROC_GOTO(sp_opensrcfile, 1);
}


/* standard startup sequence */

/*#| (suppress-greeting [bool]) |#*/
DEFINE_INITIAL_BINDING("suppress-greeting", sp_curnogreeting)
DEFINE_VARIABLE_VARINIT(sv_curnogreeting, so_false)
DEFINE_PROCEDURE_VARACCESS(sp_curnogreeting, sv_curnogreeting, NT__ANY)


DEFINE_CONTINUATION(sk_stdrepl, result)
{
  if (result != so_void) {
    sxWrite(result, sv_conout);
    sxNewline(sv_conout);
  }
  CONT_GOTO(sp_stdrepl, 0);
}

DEFINE_PROCEDURE(sp_stdrepl)
{
  SOBJ expr;
  xllastarg();
  sxWriteString(T("sxm> "), sv_conout);
  sxFlushOutput(sv_conout);
  if (!sxRead(sv_conin, &expr)) {
    PROC_RAISE(sxGetLastReadError());
  } else if (expr == so_eof) {
    PROC_RETURN(so_void);
  } else {
    PUSH_CONT(sk_stdrepl, so_nil);
    PUSH_ARG(expr);
    PROC_GOTO(sv_cureval, 1);
  }
}

/* (sp_foreach sp_load (sp_progargs)) */

/*#| (scheme-start [mainproc]) |#*/
DEFINE_INITIAL_BINDING("scheme-start", sp_curstart)
DEFINE_CONTINUATION(sk_stdstart, ignored)
{
  /* files are loaded; time to start REPL */
  PUSH_CALLMV_CONT(sp_stdrepl);

  /* remember the "reset" continuation */
  sv_curresethandler = cvclosure(sd_brestore, newcontinuation()); 

  /* go to sp_stdrepl by manual "reset" */
  CONT_GOTO(sp_values, 0);
}
DEFINE_PROCEDURE(sp_stdstart)
{
  /* collect command-line args into a list */
  SOBJ args = cvlist(vmsp, vmargc, so_nil);
  xlpoprest();

  if (!sx_exit_after_load) {
    /* arrange future return to sk_stdstart */
    PUSH_CONT(sk_stdstart, so_void);
  }

  /* go load the files */
  PUSH_ARG(args);
  PUSH_ARG(sp_load);
  PROC_GOTO(sp_foreach, 2);
}
DEFINE_VARIABLE_VARINIT(sv_curstart, sp_stdstart)
DEFINE_PROCEDURE_VARACCESS(sp_curstart, sv_curstart, NT__PROC)


