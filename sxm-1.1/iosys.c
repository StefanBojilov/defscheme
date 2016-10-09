/* iosys.c - system ports (used internally) */

#include "sxm.h"
#include "sxwrite.h"
#include "io.h"
#include "sxio.h"
#include "sxwrite.h"
#include "sxread.h"
#include "define.h"
#include "extern.h"

/*
 *  Defined port classes: xp_closed, xp_null, xp_cnt
 */


/******************* closed ports ***********************/

/* dp is NULL (not used) */

PORT_OP void clo_close(PORTDPTR unused)
{
  /* nothing to close */
}

PORT_OP void clo_save(PORTDPTR dp, SOBJ stream)
{
  /* nothing to save */
}

PORT_OP bool_t clo_restore(PORTDPTR* pdp, SOBJ stream)
{
  /* nothing to restore */
  *pdp = NULL;
  return TRUE;
}

PORT_OP void clo_print(PORTDPTR dp, SOBJ stream)
{
  sxWriteString(T("closed port"), stream);
}

DEFINE_PORT_CLASS(xp_closed, PF_INPUT|PF_OUTPUT|PF_BINARY)
  clo_print, sxp_mark, clo_save, clo_restore,
  clo_close, err_putc, err_getc, err_ungetc, err_puts,
  err_read, err_write, err_listen,
  sxp_flush, sxp_cleari, sxp_clearo
ENDDEF_PORT_CLASS

void clo_open(PORTVPTR* pvp, PORTDPTR* pdp)
{
  *pvp = xp_closed;
  *pdp = NULL;
}

/*#| (port-closed? port) |#*/
DEFINE_INITIAL_BINDING("port-closed?", sp_portclosedp)
DEFINE_PROCEDURE(sp_portclosedp)
{
  SOBJ port = xlgaport();
  xllastarg();
  if (getvp(port) == xp_closed) return so_true;
  return so_false;
}


/******************* null ports ***********************/

/* dp is NULL (not used) */

PORT_OP void nul_close(PORTDPTR unused)
{
  /* nothing to close */
}

PORT_OP tint_t nul_getc(PORTDPTR unused)
{
  return TEOF;
}

PORT_OP tint_t nul_ungetc(PORTDPTR unused, tint_t c)
{
  return c;
}

PORT_OP tint_t nul_putc(PORTDPTR dp, tchar_t c)
{
  return c;
}

PORT_OP int nul_puts(PORTDPTR dp, const tchar_t* str)
{
  return 0;
}

PORT_OP size_t nul_read(PORTDPTR dp, byte_t* buf, size_t size)
{
  return 0;
}

PORT_OP size_t nul_write(PORTDPTR dp, const byte_t* buf, size_t size)
{
  return size;
}

PORT_OP void nul_save(PORTDPTR dp, SOBJ stream)
{
  /* nothing to save */
}

PORT_OP bool_t nul_restore(PORTDPTR* pdp, SOBJ stream)
{
  /* nothing to restore */
  *pdp = NULL;
  return TRUE;
}

PORT_OP void nul_print(PORTDPTR dp, SOBJ stream)
{
  sxWriteString(T("null port"), stream);
}


DEFINE_PORT_CLASS(xp_null, PF_INPUT|PF_OUTPUT|PF_BINARY)
  nul_print, sxp_mark, nul_save, nul_restore,
  nul_close, nul_putc, nul_getc, nul_ungetc, nul_puts,
  nul_read, nul_write, sxp_listen,
  sxp_flush, sxp_cleari, sxp_clearo
ENDDEF_PORT_CLASS

static bool_t nul_open(PORTVPTR* pvp, PORTDPTR* pdp)
{
  *pvp = xp_null;
  *pdp = NULL;
  return TRUE;
}

/*#| (open-io-null) |#*/
DEFINE_INITIAL_BINDING("open-io-null", sp_openionull)
DEFINE_PROCEDURE(sp_openionull)
{
  PORTVPTR vp; PORTDPTR dp;
  xllastarg();
  if (nul_open(&vp, &dp)) return cvport(vp, dp);
  return sxSignalOpenError(T("null-io"), so_unbound);
}



/*************** char counter ports *******************/

/* dp is fcnt_file* */

typedef struct fcnt_tag {
  FIXTYPE count;
} fcnt_file;

/*
PORT_OP void nul_serialize(SOBJ port, SOBJ stream, bool_t savep)
{
  if (savep) {
    sxWriteDWord(((fnul_file*)getpp(port))->count, stream);
  } else {
    long l = sxReadDWord(stream);
    setlp(port, l);
  }
}
*/

PORT_OP void cnt_close(PORTDPTR dp)
{
  free((fcnt_file*)dp);
}

PORT_OP tint_t cnt_putc(PORTDPTR dp, tchar_t c)
{
  ((fcnt_file*)dp)->count++;
  return c;
}

PORT_OP int cnt_puts(PORTDPTR dp, const tchar_t* str)
{
  ((fcnt_file*)dp)->count += tcslen(str);
  return 0;
}

PORT_OP void cnt_clearo(PORTDPTR dp)
{
  ((fcnt_file*)dp)->count = 0;
}

PORT_OP void cnt_save(PORTDPTR dp, SOBJ stream)
{
  sxWriteDWord((long)((fcnt_file*)dp)->count, stream);
}

PORT_OP bool_t cnt_restore(PORTDPTR* pdp, SOBJ stream)
{
  long l = sxReadDWord(stream);
  fcnt_file *fp = (fcnt_file*)malloc(sizeof(fcnt_file));
  if (fp == NULL) return FALSE;
  fp->count = (FIXTYPE)l;
  *pdp = (PORTDPTR)fp;
  return TRUE;
}

PORT_OP void cnt_print(PORTDPTR dp, SOBJ stream)
{
  tchar_t buf[60];
  stprintf(buf, T("char counter port [%lu]"), (unsigned long)((fcnt_file*)dp)->count);
  sxWriteString(buf, stream);
}


DEFINE_PORT_CLASS(xp_cnt, PF_OUTPUT)
  cnt_print, sxp_mark, cnt_save, cnt_restore,
  cnt_close, cnt_putc, err_getc, err_ungetc, cnt_puts,
  err_read, err_write, err_listen,
  sxp_flush, err_cleari, cnt_clearo
ENDDEF_PORT_CLASS

bool_t cnt_open(PORTVPTR* pvp, PORTDPTR* pdp)
{
  fcnt_file *fp = (fcnt_file*)malloc(sizeof(fcnt_file));
  if (fp == NULL) return FALSE;
  fp->count = 0;
  *pvp = xp_cnt;
  *pdp = (PORTDPTR)fp;
  return TRUE;
}

/*#| (open-output-counter) |#*/
DEFINE_INITIAL_BINDING("open-output-counter", sp_openoutcnt)
DEFINE_PROCEDURE(sp_openoutcnt)
{
  PORTVPTR vp; PORTDPTR dp;
  xllastarg();
  if (cnt_open(&vp, &dp)) return cvport(vp, dp);
  return sxSignalOpenError(T("output-counter"), so_unbound);
}

EXTERN_VARIABLE(sv_curout)

/*#| (chars-written [ocntport]) |#*/
DEFINE_INITIAL_BINDING("chars-written", sp_charswritten)
DEFINE_PROCEDURE(sp_charswritten)
{
  SOBJ port = optarg() ? xlgaoport() : sv_curout;
  xllastarg();
  if (getvp(port) != xp_cnt) sxErr(T("not a counter port"), port);
  return cvfixnum(((fcnt_file*)getdp(port))->count);
}


