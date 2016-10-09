/* iottywin.c - tty ports (Windows CRTL version) */

#include <conio.h>
#include "../sxm.h"
#include "../sxwrite.h"
#include "../sxio.h"
#include "../io.h"
#include "../os.h"
#include "../define.h"
#include "../extern.h"

#if (CS == CS_WIDE)
#error CS_WIDE not supported here 
#elif (CS == CS_ANSI)
/* OK */
#else
#error unknown CS
#endif /* CS == ... */

/*
 *  Defined port classes: xp_tty
 */

/********************* tty port ************************/

/* dp is NULL or FILE* (used for transcript) */

PORT_OP void tty_close(PORTDPTR dp)
{
  if (dp) fclose((FILE*)dp);
}

PORT_OP tint_t tty_putc(PORTDPTR dp, tchar_t c)
{
  if (dp) fputtc(c, (FILE*)dp);
  return _putch(c);
}

PORT_OP tint_t tty_getc(PORTDPTR dp)
{
  tint_t c = _getch(); /* _getche() */
  if (dp && c != TEOF) fputtc(c, (FILE*)dp);
  return c;
}

PORT_OP tint_t tty_ungetc(PORTDPTR unused, tint_t c)
{
  return _ungetch(c);
}

PORT_OP int tty_puts(PORTDPTR dp, const tchar_t* str)
{
  if (dp) fputts(str, (FILE*)dp);
  return _cputs(str);
}

PORT_OP bool_t tty_listen(PORTDPTR unused)
{
  return _kbhit() != 0;
}

PORT_OP void tty_flush(PORTDPTR dp)
{
  if (dp) fflush((FILE*)dp);
}

PORT_OP void tty_save(PORTDPTR dp, SOBJ stream)
{
  /* nothing to save */
}

PORT_OP bool_t tty_restore(PORTDPTR* pdp, SOBJ stream)
{
  /* just reopen */
  *pdp = NULL; /* no transcript */
  return TRUE;
}

PORT_OP void tty_print(PORTDPTR dp, SOBJ stream)
{
  tchar_t buf[120];
  stprintf(buf, T("tty port [transcript %s]"), dp ? T("on") : T("off"));
  sxWriteString(buf, stream);
}

DEFINE_PORT_CLASS(xp_tty, PF_INPUT|PF_OUTPUT)
  tty_print, sxp_mark, tty_save, tty_restore,
  tty_close, tty_putc, tty_getc, tty_ungetc, tty_puts,
  err_read, err_write, tty_listen,
  tty_flush, sxp_cleari, sxp_clearo
ENDDEF_PORT_CLASS

bool_t tty_open(PORTVPTR* pvp, PORTDPTR* pdp)
{
  *pvp = xp_tty;
  *pdp = NULL; /* no transcript */
  return TRUE;
}


/* transcript is supported */
EXTERN_VARIABLE(sv_conout)

/*#| (transcript-on filename) |#*/
DEFINE_INITIAL_BINDING("transcript-on", sp_transcron)
DEFINE_PROCEDURE(sp_transcron)
{
  SOBJ port = sv_conout;
  SOBJ str = xlgastring();
  const tchar_t* name = getstring(str);
  xllastarg();
  if (getvp(port) != xp_tty) sxErr(T("not a console port"), port);
  if (getdp(port) != NULL) {
    fclose((FILE*)getdp(port));
    setdp(port, NULL);
  }
  name = osfprobe(name, QF_WRITE);
  if (name != NULL) {
    FILE* fp = osfopen(name, T("w"), 1);
    if (fp != NULL) {
      setdp(port, (PORTDPTR)fp);
      { /* output transcript header */
        time_t t = time(NULL);
        tchar_t buf[121];
        struct tm *ptm = localtime(&t);
        size_t n = tcsftime(buf, 100, T(" [%x %X]"), ptm);
        sxWriteString(T("SXM Transcript"), sv_conout);
        if (n != 0) sxWriteString(buf, sv_conout);
        sxNewline(sv_conout);
      }
      return so_void; 
    }
  }
  return sxSignalOpenError(T("transcript"), str);
}

/*#| (transcript-off) |#*/
DEFINE_INITIAL_BINDING("transcript-off", sp_transcroff)
DEFINE_PROCEDURE(sp_transcroff)
{
  SOBJ port = sv_conout;
  xllastarg();
  if (getvp(port) != xp_tty) sxErr(T("not a console port"), port);
  if (getdp(port) != NULL) {
    fclose((FILE*)getdp(port));
    setdp(port, NULL);
  }
  return so_void; 
}
