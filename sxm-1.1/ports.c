/* ports.c - standard procedures 6.10.1 */

#include "sxm.h"
#include "sxio.h"
#include "sxintern.h"
#include "define.h"
#include "extern.h"

EXTERN_VARIABLE(sv_conin)
EXTERN_VARIABLE(sv_conout)
EXTERN_VARIABLE(sv_stdin)
EXTERN_VARIABLE(sv_stdout)
EXTERN_VARIABLE(sv_stderr)

EXTERN_PORT_CLASS(xp_closed)
EXTERN_PORT_CLASS(xp_itext)
EXTERN_PORT_CLASS(xp_otext)
EXTERN_PORT_CLASS(xp_istr)
EXTERN_PORT_CLASS(xp_ostr)
EXTERN_PORT_CLASS(xp_null)

EXTERN_PROCEDURE(sp_openinfile)
EXTERN_PROCEDURE(sp_openoutfile)
EXTERN_PROCEDURE(sp_openinstring)
EXTERN_PROCEDURE(sp_openoutstring)
EXTERN_PROCEDURE(sp_getoutstring)
EXTERN_PROCEDURE(sp_openoutcnt)
EXTERN_PROCEDURE(sp_charswritten)

/************* Port Predicates *************/

/*#| (port? obj) |#*/
DEFINE_INITIAL_BINDING("port?", sp_portp)
DEFINE_PROCEDURE(sp_portp)
{
  SOBJ arg = xlonearg();
  return cvbool(portp(arg));
}

/*#| (input-port? obj) |#*/
DEFINE_INITIAL_BINDING("input-port?", sp_inportp)
DEFINE_PROCEDURE(sp_inportp)
{
  SOBJ arg = xlonearg();
  return cvbool(iportp(arg));
}

/*#| (output-port? obj) |#*/
DEFINE_INITIAL_BINDING("output-port?", sp_outportp)
DEFINE_PROCEDURE(sp_outportp)
{
  SOBJ arg = xlonearg();
  return cvbool(oportp(arg));
}


/************ Open Port Exceptions ************/

DEFINE_DATUM_INIT(sv_open_exc)
{
  SOBJ cond;
  SOBJ tag = sxKeyEnter(T("open-error"));
  gcLock(tag);
  cond = newvector(3, so_false);
  setelement(cond, 0, tag);
  gcUnlock(1);
  return cond;
}

SOBJ sxSignalOpenError(const tchar_t *typestr, SOBJ filename)
{
  setelement(sv_open_exc, 1, sxSymEnter(typestr));
  setelement(sv_open_exc, 2, filename);
  PROC_RAISE(sv_open_exc);
}

/*#| (open-error? obj) |#*/
DEFINE_INITIAL_BINDING("open-error?", sp_openerrp)
DEFINE_PROCEDURE(sp_openerrp)
{
  SOBJ obj = xlonearg();
  return cvbool(obj == sv_open_exc);
}

/*#| (open-error-filename obj) |#*/
DEFINE_INITIAL_BINDING("open-error-filename", sp_openerr_fname)
DEFINE_PROCEDURE(sp_openerr_fname)
{
  SOBJ obj = xlonearg();
  if (obj != sv_open_exc) sxErr(T("not an open error exception"), obj);
  return getelement(sv_open_exc, 2);
}


/************ Close Port Procedures ************/

/*#| (close-port port) |#*/
DEFINE_INITIAL_BINDING("close-port", sp_closeport)
DEFINE_PROCEDURE(sp_closeport)
{
  SOBJ port = xlgaport();
  xllastarg();
  sxPortClose(port);
  return so_void;
}

/*#| (close-output-port port) |#*/
DEFINE_INITIAL_BINDING("close-output-port", sp_closeoutport)
DEFINE_PROCEDURE(sp_closeoutport)
{
  SOBJ port = xlgaoport();
  xllastarg();
  sxPortClose(port);
  return so_void;
}

/*#| (close-input-port port) |#*/
DEFINE_INITIAL_BINDING("close-input-port", sp_closeinport)
DEFINE_PROCEDURE(sp_closeinport)
{
  SOBJ port = xlgaiport();
  xllastarg();
  sxPortClose(port);
  return so_void;
}


/********** Access to internal ports ***********/

/*#| (standard-port k) |#*/
DEFINE_INITIAL_BINDING("standard-port", sp_stdport)
DEFINE_PROCEDURE(sp_stdport)
{
  FIXTYPE x = getsfixnum(xlgasfixnum());
  xllastarg();
  switch (x) {
    case 0: return sv_stdin;
    case 1: return sv_stdout;
    case 2: return sv_stderr;
    default: /* Unix fdes? */;
  }
  return so_false;
}

/*#| (current-input-port [iport]) |#*/
DEFINE_INITIAL_BINDING("current-input-port", sp_curin)
DEFINE_VARIABLE_VARINIT(sv_curin, sv_conin)
DEFINE_PROCEDURE_VARACCESS(sp_curin, sv_curin, NT__IPORT)

/*#| (current-output-port [oport]) |#*/
DEFINE_INITIAL_BINDING("current-output-port", sp_curout)
DEFINE_VARIABLE_VARINIT(sv_curout, sv_conout)
DEFINE_PROCEDURE_VARACCESS(sp_curout, sv_curout, NT__OPORT)

/*#| (console-input-port) |#*/
DEFINE_INITIAL_BINDING("console-input-port", sp_conin)
DEFINE_PROCEDURE(sp_conin)
{
  xllastarg();
  return sv_conin;
}

/*#| (console-output-port) |#*/
DEFINE_INITIAL_BINDING("console-output-port", sp_conout)
DEFINE_PROCEDURE(sp_conout)
{
  xllastarg();
  return sv_conout;
}


/******** Call-with-?-port Procedures *********/

/* receiver for call-with-xx-file proc values */
DEFINE_PROCEDURE(sp_callwfile_aux)
{
  /* close port */
  SOBJ port = xlgetarg();
  sxPortClose(port);
  /* return other values */
  PROC_RETURN_VALUES(vmargc);
}

DEFINE_CONTINUATION(sk_callwfile, port)
{
  SOBJ receiver = CONT_ENV();
  SOBJ cleanup;
  /* port is open : push cleanup mv continuation and call proc */
  PUSH_ARG(port); /* extra arg for sp_callwfile_aux */
  gcLock(receiver);
  cleanup = CURRY(sp_callwfile_aux, 1);  /* see above */
  gcUnlock(1);
  PUSH_CALLMV_CONT(cleanup);
  /* ... and call procedure with port */
  PUSH_ARG(port);
  CONT_GOTO(receiver, 1);
}

static SOBJ do_call_with_file(SOBJ openproc)
{
  SOBJ str = xlgastring();
  SOBJ receiver = xlgetarg();
  xllastarg();
  /* prepare for openproc call */
  PUSH_CONT(sk_callwfile, receiver);
  PUSH_ARG(str);
  PROC_GOTO(openproc, 1);
}


/*#| (call-with-input-file string proc) |#*/
DEFINE_INITIAL_BINDING("call-with-input-file", sp_call_with_ifile)
DEFINE_PROCEDURE(sp_call_with_ifile)
{
  return do_call_with_file(sp_openinfile);
}

/*#| (call-with-output-file string proc) |#*/
DEFINE_INITIAL_BINDING("call-with-output-file", sp_call_with_ofile)
DEFINE_PROCEDURE(sp_call_with_ofile)
{
  return do_call_with_file(sp_openoutfile);
}

/*#| (call-with-input-string string proc) |#*/
DEFINE_INITIAL_BINDING("call-with-input-string", sp_call_with_istr)
DEFINE_PROCEDURE(sp_call_with_istr)
{
  return do_call_with_file(sp_openinstring);
}

/******** With-?-port Procedures *********/


static SOBJ do_with_port(SOBJ parmproc, SOBJ oldport)
{
  SOBJ newport = xlgaport();
  SOBJ thunk = xlgetarg();
  xllastarg();
  /* call thunk in the new dynamic environment */
  PROC_FLUID_BIND(parmproc, newport, thunk, oldport);
}

/*#| (with-input-from-port iport thunk) |#*/
DEFINE_INITIAL_BINDING("with-input-from-port", sp_with_iport)
DEFINE_PROCEDURE(sp_with_iport)
{
  return do_with_port(sp_curin, sv_curin);
}

/*#| (with-output-to-port oport thunk) |#*/
DEFINE_INITIAL_BINDING("with-output-to-port", sp_with_oport)
DEFINE_PROCEDURE(sp_with_oport)
{
  return do_with_port(sp_curout, sv_curout);
}

/* special callback procedure for with-?-file */
DEFINE_PROCEDURE(sp_with_file_aux)
{
  SOBJ parmproc, oldport;
  SOBJ inputp = xlgetarg();   /* curried */
  SOBJ thunk = xlgetarg();    /* curried */
  SOBJ newport = xlgaport();  /* from call-with-?-file */
  xllastarg();
  parmproc = truep(inputp) ? sp_curin : sp_curout;
  oldport = truep(inputp) ? sv_curin : sv_curout;
  /* call thunk in the new dynamic environment */
  PROC_FLUID_BIND(parmproc, newport, thunk, oldport);
}

static SOBJ do_with_port_string(SOBJ callwf, SOBJ inputp)
{
  SOBJ receiver;
  SOBJ string = xlgastring();
  SOBJ thunk = xlgetarg();
  xllastarg();
  /* construct port receiver */
  gcLock(string); gcLock(callwf);
  PUSH_ARG(thunk);
  PUSH_ARG(inputp);
  receiver = CURRY(sp_with_file_aux, 2);
  gcUnlock(2);
  /* call call-with-?-file with string & receiver */
  PUSH_ARG(receiver);
  PUSH_ARG(string);
  PROC_GOTO(callwf, 2);
}

/*#| (with-input-from-file string thunk) |#*/
DEFINE_INITIAL_BINDING("with-input-from-file", sp_with_ifile)
DEFINE_PROCEDURE(sp_with_ifile)
{
  return do_with_port_string(sp_call_with_ifile, so_true);
}

/*#| (with-output-to-file oport thunk) |#*/
DEFINE_INITIAL_BINDING("with-output-to-file", sp_with_ofile)
DEFINE_PROCEDURE(sp_with_ofile)
{
  return do_with_port_string(sp_call_with_ofile, so_false);
}

/*#| (with-input-from-string string thunk) |#*/
DEFINE_INITIAL_BINDING("with-input-from-string", sp_with_istr)
DEFINE_PROCEDURE(sp_with_istr)
{
  return do_with_port_string(sp_call_with_istr, so_true);
}



/* receiver for with-output-to-string thunk values */
DEFINE_PROCEDURE(sp_with_ostr_aux)
{
  /* get port */
  SOBJ port = xlgaport();
  /* ignore other values */
  xlpoprest();
  /* extract string from port */
  PUSH_ARG(port);
  PROC_GOTO(sp_getoutstring, 1); /* string port is dropped */
}

DEFINE_CONTINUATION(sk_with_ostr, port)
{
  SOBJ thunk = CONT_ENV();
  /* push cleanup continuation */
  SOBJ cleanup;
  PUSH_ARG(port); /* extra arg for sp_with_ostr_aux */
  gcLock(thunk);
  cleanup = CURRY(sp_with_ostr_aux, 1);  /* see above */
  gcUnlock(1);
  PUSH_CALLMV_CONT(cleanup);
  /* call with-output-to-port */
  PUSH_ARG(thunk);
  PUSH_ARG(port);
  CONT_GOTO(sp_with_oport, 2);
}

/*#| (with-output-to-string thunk) |#*/
DEFINE_INITIAL_BINDING("with-output-to-string", sp_with_ostr)
DEFINE_PROCEDURE(sp_with_ostr)
{
  SOBJ thunk = xlonearg();
  /* open string port */
  PUSH_CONT(sk_with_ostr, thunk);
  PROC_GOTO(sp_openoutstring, 0);
}


/* receiver for with-output-to-counter thunk values */
DEFINE_PROCEDURE(sp_with_ocnt_aux)
{
  /* get port */
  SOBJ port = xlgaport();
  /* ignore other values */
  xlpoprest();
  /* extract counter from port */
  PUSH_ARG(port);
  PROC_GOTO(sp_charswritten, 1); /* counter port is dropped */
}

DEFINE_CONTINUATION(sk_with_ocnt, port)
{
  SOBJ thunk = CONT_ENV();
  /* push cleanup continuation */
  SOBJ cleanup;
  PUSH_ARG(port); /* extra arg for sp_with_ocnt_aux */
  gcLock(thunk);
  cleanup = CURRY(sp_with_ocnt_aux, 1);  /* see above */
  gcUnlock(1);
  PUSH_CALLMV_CONT(cleanup);
  /* call with-output-to-port */
  PUSH_ARG(thunk);
  PUSH_ARG(port);
  CONT_GOTO(sp_with_oport, 2);
}

/*#| (with-output-to-counter thunk) |#*/
DEFINE_INITIAL_BINDING("with-output-to-counter", sp_with_ocnt)
DEFINE_PROCEDURE(sp_with_ocnt)
{
  SOBJ thunk = xlonearg();
  /* open counter port */
  PUSH_CONT(sk_with_ocnt, thunk);
  PROC_GOTO(sp_openoutcnt, 0);
}
