/* iostr.c - string ports */

#include "sxm.h"
#include "sxread.h"
#include "sxwrite.h"
#include "io.h"
#include "sxio.h"
#include "chqueue.h"
#include "define.h"
#include "extern.h"

/*
 *  Defined port classes:  xp_istr, xp_ostr, xp_tstr
 */

/******************** string ports ***********************/

/* dp is tchar_queue_t* */

PORT_OP void str_close(PORTDPTR dp)
{
  cq_free((tchar_queue_t*)dp);
}

PORT_OP tint_t str_ungetc(PORTDPTR dp, tint_t c)
{
  return cq_ungetc((tchar_queue_t*)dp, c);
}

PORT_OP tint_t str_getc(PORTDPTR dp)
{
  return cq_getc((tchar_queue_t*)dp);
}

PORT_OP tint_t str_putc(PORTDPTR dp, tchar_t c)
{
  return cq_putc((tchar_queue_t*)dp, c);
}

PORT_OP int str_puts(PORTDPTR dp, const tchar_t* str)
{
  while (*str && cq_putc((tchar_queue_t*)dp, *str) != TEOF)
    str++;
  return *str ? TEOF : 0;
}

PORT_OP void str_clear(PORTDPTR dp)
{
  cq_empty((tchar_queue_t*)dp);
}

PORT_OP void str_save(PORTDPTR dp, SOBJ stream)
{
  tchar_queue_t* cqp = (tchar_queue_t*)dp;
  size_t cnt = cq_count(cqp); 
  tchar_t* cp = (tchar_t*)malloc(cnt*sizeof(tchar_t));
  if (cp == NULL) { 
    sxWriteDWord(0, stream);
    return;
  }
  cq_gets(cqp, cp);
  sxWriteDWord((long)cnt, stream);
  sxWriteBytes((byte_t*)cp, cnt*sizeof(tchar_t), stream);
  free(cp);
}

PORT_OP bool_t str_restore(PORTDPTR* pdp, SOBJ stream)
{
  long lcnt = sxReadDWord(stream);
  if (lcnt == 0) {
    tchar_queue_t* pcq = cq_alloc(CQ_INCREMENT, T(""), 0);
    if (pcq == NULL) return FALSE;
    *pdp = (PORTDPTR)pcq;
    return TRUE;
  } else if (lcnt >= 0) {
    size_t cnt = (size_t)lcnt;
    tchar_t* cp = (tchar_t*)malloc(cnt*sizeof(tchar_t));
    tchar_queue_t* pcq;
    if (cp == NULL) return FALSE;
    sxReadBytes((byte_t*)cp, cnt*sizeof(tchar_t), stream);
    pcq = cq_alloc(cnt, cp, cnt);
    free(cp);
    if (pcq == NULL) return FALSE;
    *pdp = (PORTDPTR)pcq;
    return TRUE;
  } else 
    return FALSE; 
}

PORT_OP void str_print(PORTDPTR dp, SOBJ oport)
{
  tchar_t buf[60];
  tchar_queue_t* cqp = (tchar_queue_t*)dp;
  stprintf(buf,T("string port [%ld:%ld]"), (long)cq_size(cqp), (long)cq_count(cqp));
  sxWriteString(buf, oport);
}

DEFINE_PORT_CLASS(xp_istr, PF_INPUT)
  str_print, sxp_mark, str_save, str_restore,
  str_close, err_putc, str_getc, str_ungetc, err_puts,
  err_read, err_write, sxp_listen,
  err_flush, str_clear, err_clearo
ENDDEF_PORT_CLASS

DEFINE_PORT_CLASS(xp_ostr, PF_OUTPUT)
  str_print, sxp_mark, str_save, str_restore,
  str_close, str_putc, err_getc, err_ungetc, str_puts,
  err_read, err_write, sxp_listen,
  sxp_flush, err_cleari, str_clear
ENDDEF_PORT_CLASS

DEFINE_PORT_CLASS(xp_tstr, PF_INPUT|PF_OUTPUT)
  str_print, sxp_mark, str_save, str_restore,
  str_close, str_putc, str_getc, str_ungetc, str_puts,
  err_read, err_write, sxp_listen,
  sxp_flush, str_clear, str_clear
ENDDEF_PORT_CLASS


bool_t sti_open(const tchar_t* str, size_t len, PORTVPTR* pvp, PORTDPTR* pdp)
{
  size_t cqlen = (len == 0) ? CQ_INCREMENT : len;
  tchar_queue_t* pcq = cq_alloc(cqlen, str, len);
  if (pcq == NULL) return FALSE;
  *pvp = xp_istr;
  *pdp = (PORTDPTR)pcq;
  return TRUE;
}

bool_t sto_open(PORTVPTR* pvp, PORTDPTR* pdp)
{
  tchar_queue_t* pcq = cq_alloc(CQ_INCREMENT, T(""), 0);
  if (pcq == NULL) return FALSE;
  *pvp = xp_ostr;
  *pdp = (PORTDPTR)pcq;
  return TRUE;
}

bool_t str_open(const tchar_t* str, size_t len, PORTVPTR* pvp, PORTDPTR* pdp)
{
  size_t cqlen = (len == 0) ? CQ_INCREMENT : len;
  tchar_queue_t* pcq = cq_alloc(cqlen, str, len);
  if (pcq == NULL) return FALSE;
  *pvp = xp_tstr;
  *pdp = (PORTDPTR)pcq;
  return TRUE;
}

/*#| (open-input-string string) |#*/
/*#| (string->input-port string) |#*/
DEFINE_INITIAL_BINDING("open-input-string", sp_openinstring)
DEFINE_INITIAL_BINDING("string->input-port", sp_openinstring)
DEFINE_PROCEDURE(sp_openinstring)
{
  PORTVPTR vp; PORTDPTR dp;
  SOBJ str = xlgastring();
  xllastarg();
  if (sti_open(getstring(str), getslength(str)-1, &vp, &dp)) 
    return cvport(vp, dp);
  return sxSignalOpenError(T("string-input"), str);
}

/*#| (open-output-string) |#*/
DEFINE_INITIAL_BINDING("open-output-string", sp_openoutstring)
DEFINE_PROCEDURE(sp_openoutstring)
{
  PORTVPTR vp; PORTDPTR dp;
  xllastarg();
  if (sto_open(&vp, &dp)) return cvport(vp, dp);
  return sxSignalOpenError(T("string-output"), so_unbound);
}

/*#| (open-io-string) |#*/
DEFINE_INITIAL_BINDING("open-io-string", sp_openiostring)
DEFINE_PROCEDURE(sp_openiostring)
{
  PORTVPTR vp; PORTDPTR dp;
  SOBJ str = so_unbound;
  if (optarg()) {
    str = xlgastring();
    xllastarg();
    if (str_open(getstring(str), getslength(str)-1, &vp, &dp)) 
      return cvport(vp, dp);
  } else {
    xllastarg();
    if (str_open(T(""), 0, &vp, &dp)) return cvport(vp, dp);
  }
  return sxSignalOpenError(T("string-io"), str);
}

EXTERN_VARIABLE(sv_curout)

/*#| (get-output-string [ostrport]) |#*/
DEFINE_INITIAL_BINDING("get-output-string", sp_getoutstring)
DEFINE_PROCEDURE(sp_getoutstring)
{
  SOBJ port = optarg() ? xlgaoport() : sv_curout;
  xllastarg();
  if (getvp(port) != xp_ostr && getvp(port) != xp_tstr)
    sxErr(T("not an output string port"), port);
  { /* else */
    tchar_queue_t* cqp = (tchar_queue_t*)getdp(port);
    size_t cnt = cq_count(cqp); 
    SOBJ str; tchar_t* cp;
    gcLock(port);
    str = newstring(cnt+1);
    cp = getstring(str);
    cq_gets(cqp, cp);
    cp[cnt] = T('\0');
    gcUnlock(1);
    /* cq_empty(cqp); -- removed in conformance with SRFI 6 */
    return str;
  }
}
