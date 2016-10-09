/* iottywnt.c - tty ports (UNICODE, Windows NT) */

#include "../unichar.h"
#if (CS == CS_WIDE)
#define UNICODE
#elif (CS == CS_ANSI)
#undef UNICODE
#else
#error unknown CS
#endif /* CS == ... */

#include <windows.h>
#include "../sxm.h"
#include "../sxwrite.h"
#include "../sxio.h"
#include "../io.h"
#include "../os.h"
#include "../define.h"
#include "../extern.h"

/*
 *  Defined port classes: xp_tty
 */

/********************* tty port ************************/

/* dp is NULL or FILE* (used for transcript) */

static HANDLE hcin = 0;
static HANDLE hcout = 0;
static long tty_count = 0;
static bool_t lookaheadp = FALSE;
static tint_t lachar = 0;


static void initwraps(void)
{
  SECURITY_ATTRIBUTES sa = { sizeof(SECURITY_ATTRIBUTES), NULL, TRUE };
  if (hcin == NULL) hcin = CreateFile(T("CONIN$"), GENERIC_READ|GENERIC_WRITE,
                                      FILE_SHARE_READ | FILE_SHARE_WRITE,
                                      &sa, OPEN_EXISTING, 0, 0); 
  if (hcout == NULL) hcout = CreateFile(T("CONOUT$"), GENERIC_READ|GENERIC_WRITE, 
                                      FILE_SHARE_READ | FILE_SHARE_WRITE,
                                      &sa, OPEN_EXISTING, 0, 0); 
}

static void freewraps(void)
{
  if (hcin != NULL) { CloseHandle(hcin); hcin = 0; } 
  if (hcout != NULL) { CloseHandle(hcout); hcout = 0; }
}

PORT_OP void tty_close(PORTDPTR dp)
{
  if (dp) fclose((FILE*)dp);
  if (--tty_count == 0) freewraps();
}

PORT_OP tint_t tty_putc(PORTDPTR dp, tchar_t c)
{
  DWORD written;
  assert(sizeof(tchar_t) == sizeof(TCHAR));
  if (dp) fputtc(c, (FILE*)dp);
  if (!WriteConsole(hcout, &c, 1, &written, NULL) || written != 1) 
    return TEOF;
  else
    return (tint_t)c;
}

PORT_OP tint_t tty_getc(PORTDPTR dp)
{
  tchar_t c; DWORD read;
  assert(sizeof(tchar_t) == sizeof(TCHAR));
  if (lookaheadp) {
    lookaheadp = FALSE;
    return lachar;
  }
  read_again:
  if (!ReadConsole(hcin, &c, 1, &read, NULL) || read != 1)
    return TEOF;
  else if (c == T('\r')) goto read_again;
  else if (c == T('\032')) return TEOF; /* ^Z */
  else {
    if (dp) fputtc(c, (FILE*)dp);
    return (tint_t)c;
  }
}

PORT_OP tint_t tty_ungetc(PORTDPTR unused, tint_t c)
{
  lookaheadp = TRUE;
  lachar = c;
  return c;
}

PORT_OP int tty_puts(PORTDPTR dp, const tchar_t* str)
{
  DWORD written;
  assert(sizeof(tchar_t) == sizeof(TCHAR));
  if (dp) fputts(str, (FILE*)dp);
  if (!WriteConsole(hcout, str, tcslen(str), &written, NULL)) 
    return -1;
  return 1;
}

PORT_OP bool_t tty_listen(PORTDPTR unused)
{
  INPUT_RECORD r; DWORD read;
  if (!PeekConsoleInput(hcin, &r, 1, &read)) 
    return TRUE;  /* something went wrong: no proof */
  return (read != 0); /* #f only if nothing is there */
}

PORT_OP void tty_flush(PORTDPTR dp)
{
  if (dp) fflush((FILE*)dp);
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
  /* just reopen */
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
  assert(sizeof(tchar_t) == sizeof(TCHAR));
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
