/* iottyucs.c - tty ports (Standard C, UCS2) */

#include "../sxm.h"
#include "../sxwrite.h"
#include "../sxio.h"
#include "../io.h"
#include "../os.h"
#include "../define.h"
#include "../extern.h"
#include "chucs2io.h"

/*
 *  Defined port classes: xp_tty
 */

/********************* tty port ************************/

/* dp is NULL or u_file* (used for transcript) */

static u_file* ustdin = NULL;
static u_file* ustdout = NULL;
static long tty_count = 0;

static void initwraps(void)
{
  const tchar_t* pmap8b = osgetmap8b(NULL); /* current */
  int ori = (pmap8b == NULL) ? ORI_UTF8 : ORI_XASCII;
  if (ustdin == NULL) ustdin = utfwrap(stdin, TRUE, ori, pmap8b);
  if (ustdout == NULL) ustdout = utfwrap(stdout, FALSE, ori, pmap8b);
}

static void freewraps(void)
{
  if (ustdin != NULL) { utfunwrap(ustdin); ustdin = NULL; }
  if (ustdout != NULL) { utfunwrap(ustdout); ustdout = NULL; }
}

PORT_OP void tty_close(PORTDPTR dp)
{
  if (dp) {
    FILE *fp = utfunwrap((u_file*)dp);
    if (fp != NULL) fclose(fp);
  }
  if (--tty_count == 0) freewraps();
}

PORT_OP tint_t tty_putc(PORTDPTR dp, tchar_t c)
{
  if (dp) utfputc((ucschar_t)c, (u_file*)dp);
  return (tint_t)utfputc((ucschar_t)c, ustdout);
}

PORT_OP tint_t tty_getc(PORTDPTR dp)
{
  ucsint_t c = utfgetc(ustdin);
  if (dp && c != UCSEOF) utfputc((ucschar_t)c, (u_file*)dp);
  return (tint_t)c;
}

PORT_OP tint_t tty_ungetc(PORTDPTR unused, tint_t c)
{
  return (tint_t)utfungetc((ucsint_t)c, ustdin);
}

PORT_OP int tty_puts(PORTDPTR dp, const tchar_t* str)
{
  /* this would work only if wide char has two bytes */
  assert(sizeof(ucschar_t) == sizeof(tchar_t));
  if (dp) utfputs((ucschar_t*)str, (u_file*)dp);
  return utfputs((ucschar_t*)str, ustdout);
}

PORT_OP bool_t tty_listen(PORTDPTR unused)
{
  return TRUE;  /* no standard solution??? */
}

PORT_OP void tty_flush(PORTDPTR dp)
{
  if (dp) utfflush((u_file*)dp);
  utfflush(ustdout);
}

PORT_OP void tty_print(PORTDPTR dp, SOBJ stream)
{
  tchar_t buf[120];
  stprintf(buf, T("tty port [transcript %s]"), dp ? T("on") : T("off"));
  sxWriteString(buf, stream);
}

PORT_OP void tty_save(PORTDPTR dp, SOBJ stream)
{
  /* nothing to save */
}

PORT_OP bool_t tty_restore(PORTDPTR* pdp, SOBJ stream)
{
  /* check for redirection first */
  if (!osisatty(stdin)) return FALSE;
  if (!osisatty(stdout)) return FALSE;
  /* no redirection: stdin/stdout are attached to console (?) */
  if (tty_count++ == 0) initwraps();
  *pdp = NULL; /* no transcript */
  return TRUE;
}


DEFINE_PORT_CLASS(xp_tty, PF_INPUT|PF_OUTPUT)
  tty_print, sxp_mark, tty_save, tty_restore,
  tty_close, tty_putc, tty_getc, tty_ungetc, tty_puts,
  err_read, err_write, tty_listen,
  tty_flush, sxp_cleari, sxp_clearo
ENDDEF_PORT_CLASS

bool_t tty_open(PORTVPTR* pvp, PORTDPTR* pdp)
{
  /* check for redirection first */
  if (!osisatty(stdin)) return FALSE;
  if (!osisatty(stdout)) return FALSE;
  /* no redirection: stdin/stdout are attached to console (?) */
  /* this would work only if wide char has two bytes */
  assert(sizeof(ucschar_t) == sizeof(tchar_t));
  if (tty_count++ == 0) initwraps();
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
    FILE *fp = utfunwrap((u_file*)getdp(port));
    if (fp != NULL) fclose(fp);
    setdp(port, NULL);
  }
  name = osfprobe(name, QF_WRITE);
  if (name != NULL) {
    /* don't try to bind fp: std binding is useless */
    FILE* fp = osfopen(name, T("w"), 0);
    if (fp != NULL) {
      u_file* ufp = utfwrap(fp, FALSE, ORI_UTF8, NULL);
      if (ufp != NULL) {
        setdp(port, (PORTDPTR)ufp);
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
      fclose(fp);
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
    FILE *fp = utfunwrap((u_file*)getdp(port));
    if (fp != NULL) fclose(fp);
    setdp(port, NULL);
  }
  return so_void; 
}
