/* iopipucs.c - pipe ports (Unix/Win32, UCS2) */

#include "../sxm.h"
#include "../io.h"
#include "../os.h"
#include "../sxio.h"
#include "../sxwrite.h"
#include "../define.h"
#include "chucs2io.h"

/*
 *  Defined port classes:  xp_ipipe, xp_opipe
 */

/******************* file ports ***********************/

/* dp is u_file* */

static bool_t open_std_pipe(PORTDPTR* pdp, const tchar_t* cmd, 
                            const tchar_t* mode)
{
  FILE* fp; bool_t readp;
  /* this would work only if wide char has two bytes */
  assert(sizeof(ucschar_t) == sizeof(tchar_t));
  assert(cmd != NULL); assert(mode != NULL);
  readp = (mode[0] != T('w'));
  /* don't try to bind fp: std binding is useless */
  fp = ospopen(cmd, mode, 0);
  if (fp != NULL) {
    /* determine orientation later... */
    u_file* ufp = utfwrap(fp, readp, ORI_UNBOUND, NULL);
    if (ufp != NULL) {
      *pdp = (PORTDPTR)ufp;
      return TRUE;
    }
  }
  return FALSE;
}

PORT_OP void p_close(PORTDPTR dp)
{
  FILE *fp = utfunwrap((u_file*)dp);
  if (fp != NULL) ospclose(fp);
}

PORT_OP tint_t po_putc(PORTDPTR dp, tchar_t c)
{
  return (tint_t)utfputc((ucschar_t)c, (u_file*)dp);
}

PORT_OP tint_t pi_getc(PORTDPTR dp)
{
  return (tint_t)utfgetc((u_file*)dp);
}

PORT_OP tint_t pi_ungetc(PORTDPTR dp, tint_t c)
{
  return (tint_t)utfungetc((ucsint_t)c, (u_file*)dp);
}

PORT_OP int po_puts(PORTDPTR dp, const tchar_t* s)
{
  /* this would work only if wide char has two bytes */
  assert(sizeof(ucschar_t) == sizeof(tchar_t));
  return utfputs((ucschar_t*)s, (u_file*)dp);
}

PORT_OP size_t pi_read(PORTDPTR dp, byte_t* buf, size_t size)
{
  return utfread(buf, size, (u_file*)dp);
}

PORT_OP size_t po_write(PORTDPTR dp, const byte_t* buf, size_t size)
{
  return utfwrite(buf, size, (u_file*)dp);
}

PORT_OP void po_flush(PORTDPTR dp)
{
  utfflush((u_file*)dp);
}


PORT_OP void pi_print(PORTDPTR dp, SOBJ stream)
{
  tchar_t buf[60];
  stprintf(buf, T("pipe input port @%ld"), (long)dp);
  sxWriteString(buf, stream);
}

PORT_OP void po_print(PORTDPTR dp, SOBJ stream)
{
  tchar_t buf[60];
  stprintf(buf, T("pipe output port @%ld"), (long)dp);
  sxWriteString(buf, stream);
}


DEFINE_PORT_CLASS(xp_ipipe, PF_INPUT)
  pi_print, sxp_mark, sxp_save, sxp_restore,
  p_close, err_putc, pi_getc, pi_ungetc, err_puts,
  pi_read, err_write, sxp_listen,
  err_flush, sxp_cleari, err_clearo
ENDDEF_PORT_CLASS

DEFINE_PORT_CLASS(xp_opipe, PF_OUTPUT)
  po_print, sxp_mark, sxp_save, sxp_restore,
  p_close, po_putc, err_getc, err_ungetc, po_puts,
  err_read, po_write, sxp_listen,
  po_flush, err_cleari, sxp_clearo
ENDDEF_PORT_CLASS

bool_t pi_open(const tchar_t* cmd, PORTVPTR* pvp, PORTDPTR* pdp)
{
  *pvp = xp_ipipe;
  return open_std_pipe(pdp, cmd, T("rb"));
}

bool_t po_open(const tchar_t* cmd, PORTVPTR* pvp, PORTDPTR* pdp)
{
  *pvp = xp_opipe;
  return open_std_pipe(pdp, cmd, T("wb"));
}

/*#| (open-input-pipe command) |#*/
DEFINE_INITIAL_BINDING("open-input-pipe", sp_openinpipe)
DEFINE_PROCEDURE(sp_openinpipe)
{
  PORTVPTR vp; PORTDPTR dp;
  SOBJ str = xlgastring();
  const tchar_t* cmd = getstring(str);
  xllastarg();
  if (pi_open(cmd, &vp, &dp)) return cvport(vp, dp);
  return sxSignalOpenError(T("pipe-input"), str);
}

/*#| (open-output-pipe command) |#*/
DEFINE_INITIAL_BINDING("open-output-pipe", sp_openoutpipe)
DEFINE_PROCEDURE(sp_openoutpipe)
{
  PORTVPTR vp; PORTDPTR dp;
  SOBJ str = xlgastring();
  const tchar_t* cmd = getstring(str);
  xllastarg();
  if (po_open(cmd, &vp, &dp)) return cvport(vp, dp);
  return sxSignalOpenError(T("pipe-output"), str);
}
