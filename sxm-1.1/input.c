/* input.c - standard procedures 6.10.2 */

#include "sxm.h"
#include "sxread.h"
#include "define.h"
#include "extern.h"
#include "sxio.h"

EXTERN_VARIABLE(sv_curin)
EXTERN_VARIABLE(sv_curcasesensv)

/*#| (read [iport]) |#*/
DEFINE_INITIAL_BINDING("read", sp_read)
DEFINE_PROCEDURE(sp_read)
{
  SOBJ val;
  SOBJ port = optarg() ? xlgaiport() : sv_curin;
  xllastarg();
  if (sxRead(port, &val)) return val;
  PROC_RAISE(sxGetLastReadError());
}


EXTERN_DATUM(sv_read_cond)

/*#| (read-error? obj) |#*/
DEFINE_INITIAL_BINDING("read-error?", sp_readerrp)
DEFINE_PROCEDURE(sp_readerrp)
{
  SOBJ obj = xlonearg();
  return cvbool(obj == sv_read_cond);
}

/*#| (read-error-tag rerr) |#*/
DEFINE_INITIAL_BINDING("read-error-tag", sp_readerrtag)
DEFINE_PROCEDURE(sp_readerrtag)
{
  SOBJ obj = xlonearg(); SOBJ res = so_false;
  if (sxReadErrorDetails(obj, &res, NULL, NULL)) return res;
  return sxErr(T("not a read error object"), obj);
}

/*#| (read-error-port rerr) |#*/
DEFINE_INITIAL_BINDING("read-error-port", sp_readerrport)
DEFINE_PROCEDURE(sp_readerrport)
{
  SOBJ obj = xlonearg(); SOBJ res = so_false;
  if (sxReadErrorDetails(obj, NULL, &res, NULL)) return res;
  return sxErr(T("not a read error object"), obj);
}

/*#| (read-error-arg rerr) |#*/
DEFINE_INITIAL_BINDING("read-error-arg", sp_readerrarg)
DEFINE_PROCEDURE(sp_readerrarg)
{
  SOBJ obj = xlonearg(); SOBJ res = so_false;
  if (sxReadErrorDetails(obj, NULL, NULL, &res)) return res;
  return sxErr(T("not a read error object"), obj);
}


/*#| (char-ready? [iport]) |#*/
DEFINE_INITIAL_BINDING("char-ready?", sp_charreadyp)
DEFINE_PROCEDURE(sp_charreadyp)
{
  SOBJ port = optarg() ? xlgaiport() : sv_curin;
  xllastarg();
  return cvbool(sxListen(port));
}


/*#| (read-char [iport]) |#*/
DEFINE_INITIAL_BINDING("read-char", sp_readchar)
DEFINE_PROCEDURE(sp_readchar)
{
  tint_t ch;
  SOBJ port = optarg() ? xlgaiport() : sv_curin;
  xllastarg();
  ch = sxReadChar(port);
  return (ch == TEOF ? so_eof: cvchar((tchar_t)ch));
}


/*#| (peek-char [iport]) |#*/
DEFINE_INITIAL_BINDING("peek-char", sp_peekchar)
DEFINE_PROCEDURE(sp_peekchar)
{
  tint_t ch;
  SOBJ port = optarg() ? xlgaiport() : sv_curin;
  xllastarg();
  ch = sxReadChar(port);
  sxUnreadChar(ch, port);
  return (ch == TEOF ? so_eof: cvchar((tchar_t)ch));
}

/*#| (unread-char char [iport]) |#*/
DEFINE_INITIAL_BINDING("unread-char", sp_unreadchar)
DEFINE_PROCEDURE(sp_unreadchar)
{
  tint_t ch = getchcode(xlgachar());
  SOBJ port = optarg() ? xlgaiport() : sv_curin;
  xllastarg();
  sxUnreadChar(ch, port);
  return so_void;
}

/*#| (eof-object? obj) |#*/
DEFINE_INITIAL_BINDING("eof-object?", sp_eofobjp)
DEFINE_PROCEDURE(sp_eofobjp)
{
  SOBJ arg = xlonearg();
  return cvbool(arg == so_eof);
}

/*#| (clear-input [iport]) |#*/
DEFINE_INITIAL_BINDING("clear-input", sp_clearin)
DEFINE_PROCEDURE(sp_clearin)
{
  SOBJ port = optarg() ? xlgaiport() : sv_curin;
  xllastarg();
  sxClearInput(port);
  return so_void;
}

/*#| (case-sensitive [newbool]) |#*/
DEFINE_INITIAL_BINDING("case-sensitive", sp_curcasesensv)
DEFINE_VARIABLE_VARINIT(sv_curcasesensv, so_false)
DEFINE_PROCEDURE_VARACCESS(sp_curcasesensv, sv_curcasesensv, NT__ANY)


/*#| (read-line! str [iport] [nlchar]) |#*/
DEFINE_INITIAL_BINDING("read-line!", sp_ireadline)
DEFINE_PROCEDURE(sp_ireadline)
{
  SOBJ str = xlgastring();
  SOBJ port = optarg() ? xlgaiport() : sv_curin;
  tint_t nlchar = optarg() ? getchcode(xlgachar()) : T('\n');
  xllastarg();
  { /* read char-by char until eof, nlchar or no room */
    tchar_t* cp = getstring(str);
    int nread = 0;
    int nmore = getslength(str) - 1;
    tint_t ch;
    while (TRUE) {
      if (nmore <= 0) break;
      /* there's still a place for at least one char */
      ch = sxReadChar(port);
      if (ch == TEOF) return (nread == 0 ? so_eof : cvfixnum((FIXTYPE)nread));
      if (ch == nlchar) return cvfixnum((FIXTYPE)nread);
      cp[nread] = ch;
      nread++; nmore--;
    }
    /* string is full: look at next char */
    ch = sxReadChar(port);
    if (ch == TEOF) return (nread == 0 ? so_eof : cvfixnum((FIXTYPE)nread));
    if (ch == nlchar) return cvfixnum((FIXTYPE)nread);
    sxUnreadChar(ch, port);
    return so_false; /* extra read-line is required */
  }
}


/*#| (read-substring! string [from] [to] [port]) |#*/
DEFINE_INITIAL_BINDING("read-substring!", sp_ireadsubstr)
DEFINE_PROCEDURE(sp_ireadsubstr)
{
  SOBJ str = xlgastring();
  ss_size_t len = getslength(str) - 1;
  FIXTYPE start = optarg() ? getsfixnum(xlgasfixnum()) : 0;
  FIXTYPE end = optarg() ? getsfixnum(xlgasfixnum()) : len;
  SOBJ port = optarg() ? xlgaiport() : sv_curin;
  xllastarg();

  /* range checks */
  if (start < 0 || start > (FIXTYPE)len)
    sxae_range(cvsfixnum(start), cvsfixnum(len));
  else if (end < start || end > (FIXTYPE)len)
    sxae_range(cvsfixnum(end), cvsfixnum(len));
  
  { /* read string contents from a port */
    tchar_t* cp = getstring(str);
    int nread = 0;
    tint_t ch;
    while (start < end) {
      /* there's still a place for at least one char */
      ch = sxReadChar(port);
      if (ch == TEOF) break;
      cp[start] = ch;
      nread++; start++;
    }
    return cvfixnum((FIXTYPE)nread);
  }
}

/*#| (read-u8 [binport]) |#*/
DEFINE_INITIAL_BINDING("read-u8", sp_ireadu8)
DEFINE_PROCEDURE(sp_ireadu8)
{
  byte_t b; size_t nread;
  SOBJ port = optarg() ? xlgaiport() : sv_curin;
  xllastarg(); 
  nread = sxReadBytes(&b, 1, port);
  if (nread == 1) return cvfixnum((FIXTYPE)b);
  return so_eof;
}

/*#| (read-u8-block! u8vec [from] [to] [binport]) |#*/
DEFINE_INITIAL_BINDING("read-u8-block!", sp_ireadu8blk)
DEFINE_PROCEDURE(sp_ireadu8blk)
{
  SOBJ vec = xlgabvector();
  ss_size_t len = getbcount(vec);
  FIXTYPE start = optarg() ? getsfixnum(xlgasfixnum()) : 0;
  FIXTYPE end = optarg() ? getsfixnum(xlgasfixnum()) : len;
  SOBJ port = optarg() ? xlgaiport() : sv_curin;
  xllastarg();

  /* range checks */
  if (start < 0 || start > (FIXTYPE)len)
    sxae_range(cvsfixnum(start), cvsfixnum(len));
  else if (end < start || end > (FIXTYPE)len)
    sxae_range(cvsfixnum(end), cvsfixnum(len));
  
  { /* read vector contents from a port */
    byte_t* bp = getbytes(vec) + start;
    size_t nread = sxReadBytes(bp, (size_t)(end-start), port);
    return cvfixnum((FIXTYPE)nread);
  }
}

