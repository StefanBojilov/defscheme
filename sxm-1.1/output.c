/* output.c - standard procedures 6.10.3 */

#include "sxm.h"
#include "sxintern.h"
#include "sxlocale.h"
#include "sxio.h"
#include "sxread.h"
#include "sxwrite.h"
#include "define.h"
#include "extern.h"

EXTERN_VARIABLE(sv_curout)
EXTERN_VARIABLE(sv_conout)
EXTERN_PROCEDURE(sp_with_ocnt)

/*#| (write obj [oport]) |#*/
DEFINE_INITIAL_BINDING("write", sp_write)
DEFINE_PROCEDURE(sp_write)
{
  SOBJ val = xlgetarg();
  SOBJ port = optarg() ? xlgaoport() : sv_curout;
  xllastarg();
  sxWrite(val, port);
  return so_void;
}

/*#| (write-char char [oport]) |#*/
DEFINE_INITIAL_BINDING("write-char", sp_writechar)
DEFINE_PROCEDURE(sp_writechar)
{
  tchar_t ch = getchcode(xlgachar());
  SOBJ port = optarg() ? xlgaoport() : sv_curout;
  xllastarg();
  sxWriteChar(ch, port);
  return so_void;
}

/*#| (display obj [oport]) |#*/
DEFINE_INITIAL_BINDING("display", sp_display)
DEFINE_PROCEDURE(sp_display)
{
  SOBJ val = xlgetarg();
  SOBJ port = optarg() ? xlgaoport() : sv_curout;
  xllastarg();
  sxDisplay(val, port);
  return so_void;
}

/*#| (newline [oport]) |#*/
DEFINE_INITIAL_BINDING("newline", sp_newline)
DEFINE_PROCEDURE(sp_newline)
{
  SOBJ port = optarg() ? xlgaoport() : sv_curout;
  xllastarg();
  sxNewline(port);
  return so_void;
}

/*#| (force-output [oport]) |#*/
DEFINE_INITIAL_BINDING("force-output", sp_forceout)
DEFINE_PROCEDURE(sp_forceout)
{
  SOBJ port = optarg() ? xlgaoport() : sv_curout;
  xllastarg();
  sxFlushOutput(port);
  return so_void;
}

/*#| (clear-output [oport]) |#*/
DEFINE_INITIAL_BINDING("clear-output", sp_clearout)
DEFINE_PROCEDURE(sp_clearout)
{
  SOBJ port = optarg() ? xlgaoport() : sv_curout;
  xllastarg();
  sxClearOutput(port);
  return so_void;
}


/*#| (print-length [n]) |#*/
DEFINE_INITIAL_BINDING("print-length", sp_curprlen)
DEFINE_VARIABLE_VARINIT(sv_curprlen, so_false)
DEFINE_PROCEDURE(sp_curprlen)
{
  if (moreargs()) { 
    SOBJ newval = xlonearg();
    if (falsep(newval) || fixp(newval)) sv_curprlen = newval;
    else sxErr(T("not a nonnegative fixnum or #f"), newval);
    return so_void;
  } else { 
    xllastarg(); 
    return sv_curprlen; 
  }
}

/*#| (print-level [n]) |#*/
DEFINE_INITIAL_BINDING("print-level", sp_curprlevel)
DEFINE_VARIABLE_VARINIT(sv_curprlevel, so_false)
DEFINE_PROCEDURE(sp_curprlevel)
{
  if (moreargs()) { 
    SOBJ newval = xlonearg();
    if (falsep(newval) || fixp(newval)) sv_curprlevel = newval;
    else sxErr(T("not a nonnegative fixnum or #f"), newval);
    return so_void;
  } else { 
    xllastarg(); 
    return sv_curprlevel; 
  }
}


/*#| (fprintf port format arg ...) |#*/
DEFINE_INITIAL_BINDING("fprintf", sp_fprintf)
DEFINE_PROCEDURE(sp_fprintf)
{
  SOBJ port = nextarg();
  SOBJ val = xlgastring();
  if (port == so_default) port = sv_curout;
  else if (!portp(port)) sxae_type(port, NT_PORT);
  switch (sxFormat(getstring(val), port, vmargc, vmsp)) {
    case 3: sxFail(T("too many arguments in format"));
    case 2: sxFail(T("invalid format directive"));
    case 1: sxFail(T("bad or missing argument in format"));
    case 0: drop(vmargc); vmargc = 0; break;
    default: assert(FALSE);
  }
  return so_void;
}

/*#| (printf format arg ...) |#*/
DEFINE_INITIAL_BINDING("printf", sp_printf)
DEFINE_PROCEDURE(sp_printf)
{
  SOBJ val = xlgastring();
  switch (sxFormat(getstring(val), sv_curout, vmargc, vmsp)) {
    case 3: sxFail(T("too many arguments in format"));
    case 2: sxFail(T("invalid format directive"));
    case 1: sxFail(T("bad or missing argument in format"));
    case 0: drop(vmargc); vmargc = 0; break;
    default: assert(FALSE);
  }
  return so_void;
}

/*#| (cprintf format arg ...) |#*/
DEFINE_INITIAL_BINDING("cprintf", sp_cprintf)
DEFINE_PROCEDURE(sp_cprintf)
{
  SOBJ val = xlgastring();
  switch (sxFormat(getstring(val), sv_conout, vmargc, vmsp)) {
    case 3: sxFail(T("too many arguments in format"));
    case 2: sxFail(T("invalid format directive"));
    case 1: sxFail(T("bad or missing argument in format"));
    case 0: drop(vmargc); vmargc = 0; break;
    default: assert(FALSE);
  }
  return so_void;
}


/*#| (copy-port-contents inport outport [stopchar]) |#*/
DEFINE_INITIAL_BINDING("copy-port-contents", sp_copyportcont)
DEFINE_PROCEDURE(sp_copyportcont)
{
  tint_t ch;
  SOBJ inport  = xlgaiport();
  SOBJ outport = xlgaoport();
  tint_t tch =  optarg() ? getchcode(xlgachar()) : TEOF;
  xllastarg();
  while ((ch = sxReadChar(inport)) != tch && ch != TEOF)
    sxWriteChar((tchar_t)ch, outport);
  return (ch == TEOF) ? so_eof : cvchar((tchar_t)ch);
}

/*#| (write-substring string [from] [to] [port]) |#*/
DEFINE_INITIAL_BINDING("write-substring", sp_writesubstr)
DEFINE_PROCEDURE(sp_writesubstr)
{
  SOBJ str = xlgastring();
  ss_size_t len = getslength(str) - 1;
  FIXTYPE start = optarg() ? getsfixnum(xlgasfixnum()) : 0;
  FIXTYPE end = optarg() ? getsfixnum(xlgasfixnum()) : len;
  SOBJ port = optarg() ? xlgaoport() : sv_curout;
  xllastarg();

  /* range checks */
  if (start < 0 || start > (FIXTYPE)len)
    sxae_range(cvsfixnum(start), cvsfixnum(len));
  else if (end < start || end > (FIXTYPE)len)
    sxae_range(cvsfixnum(end), cvsfixnum(len));
  
  { /* write string contents to a port */
    const tchar_t* cp = getstring(str);
    for (;start < end; start++) sxWriteChar(cp[start], port);
  }
  return so_void;
}


/*#| (write-u8 u8int [binport]) |#*/
DEFINE_INITIAL_BINDING("write-u8", sp_iwriteu8)
DEFINE_PROCEDURE(sp_iwriteu8)
{
  byte_t b = (byte_t)getsfixnum(xlgabyte());
  SOBJ port = optarg() ? xlgaoport() : sv_curout;
  xllastarg();
  sxWriteByte(b, port);
  return so_void;
}

/*#| (write-u8-block u8vec [from] [to] [port]) |#*/
DEFINE_INITIAL_BINDING("write-u8-block", sp_writeu8blk)
DEFINE_PROCEDURE(sp_writeu8blk)
{
  SOBJ vec = xlgabvector();
  ss_size_t len = getbcount(vec);
  FIXTYPE start = optarg() ? getsfixnum(xlgasfixnum()) : 0;
  FIXTYPE end = optarg() ? getsfixnum(xlgasfixnum()) : len;
  SOBJ port = optarg() ? xlgaoport() : sv_curout;
  xllastarg();

  /* range checks */
  if (start < 0 || start > (FIXTYPE)len)
    sxae_range(cvsfixnum(start), cvsfixnum(len));
  else if (end < start || end > (FIXTYPE)len)
    sxae_range(cvsfixnum(end), cvsfixnum(len));
  
  { /* write string contents to a port */
    byte_t* bp = getbytes(vec) + start;
    size_t nwritten = sxWriteBytes(bp, (size_t)(end-start), port);
    return cvfixnum((FIXTYPE)nwritten);
  }
}

/*#| (external-representation-length obj) |#*/
DEFINE_INITIAL_BINDING("external-representation-length", sp_extreplen)
DEFINE_PROCEDURE(sp_extreplen)
{
  SOBJ thunk;
  SOBJ obj = xlonearg();
  PUSH_ARG(obj);
  thunk = CURRY(sp_write, 1); /* thunk to write obj to sv_curout */
  PUSH_ARG(thunk);
  PROC_GOTO(sp_with_ocnt, 1); /* with-output-to-counter will count chars */
}
