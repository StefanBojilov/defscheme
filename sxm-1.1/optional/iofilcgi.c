/* iofilcgi.c - file ports (Standard C version for CGIs) */

#include <limits.h>
#include "../sxm.h"
#include "../io.h"
#include "../os.h"
#include "../sxio.h"
#include "../sxwrite.h"
#include "../sxread.h"
#include "../define.h"
#include "../extern.h"


/*
 *  Defined port classes:  
 *  xp_itext, xp_otext, xp_ibin, xp_obin, xp_std, xp_isrc
 */

typedef struct f_tag {
  FILE *fptr;
  unsigned long count;
} f_file;

/******************* file ports ***********************/

/* dp is f_file* */


PORT_OP void fil_close(PORTDPTR dp)
{
  f_file *fifp = (f_file*)dp;
  assert(fifp); assert(fifp->fptr);
  fclose(fifp->fptr);
  free(fifp);
}

PORT_OP tint_t fil_putc(PORTDPTR dp, tchar_t c)
{
  f_file *fifp = (f_file*)dp;
  assert(fifp); assert(fifp->fptr);
  return fputtc(c, fifp->fptr);
}

PORT_OP tint_t fil_getc(PORTDPTR dp)
{
  f_file *fifp = (f_file*)dp;
  assert(fifp); assert(fifp->fptr);
  if (fifp->count == 0L) return TEOF;
  fifp->count--;
  return fgettc(fifp->fptr);
}

PORT_OP tint_t fil_ungetc(PORTDPTR dp, tint_t c)
{
  f_file *fifp = (f_file*)dp;
  assert(fifp); assert(fifp->fptr);
  fifp->count++;
  return ungettc(c, fifp->fptr);
}

PORT_OP int fil_puts(PORTDPTR dp, const tchar_t* s)
{
  f_file *fifp = (f_file*)dp;
  assert(fifp); assert(fifp->fptr);
  return fputts(s, fifp->fptr);
}

PORT_OP size_t fil_read(PORTDPTR dp, byte_t* buf, size_t size)
{
  f_file *fifp = (f_file*)dp;
  unsigned long cnt = (unsigned long)size;
  assert(fifp); assert(fifp->fptr);
  if (fifp->count < cnt) cnt = fifp->count;
  fifp->count -= cnt;
  if (cnt == 0L) return 0;
  return fread(buf, 1, (size_t)cnt, fifp->fptr);
}

PORT_OP size_t fil_write(PORTDPTR dp, const byte_t* buf, size_t size)
{
  f_file *fifp = (f_file*)dp;
  assert(fifp); assert(fifp->fptr);
  return fwrite(buf, 1, size, fifp->fptr);
}

PORT_OP void fil_flush(PORTDPTR dp)
{
  f_file *fifp = (f_file*)dp;
  assert(fifp); assert(fifp->fptr);
  fflush(fifp->fptr);
}


PORT_OP void fti_print(PORTDPTR dp, SOBJ stream)
{
  tchar_t buf[60];
  stprintf(buf, T("text input port @%ld"), (long)dp);
  sxWriteString(buf, stream);
}

PORT_OP void fto_print(PORTDPTR dp, SOBJ stream)
{
  tchar_t buf[60];
  stprintf(buf, T("text output port @%ld"), (long)dp);
  sxWriteString(buf, stream);
}

PORT_OP void fbi_print(PORTDPTR dp, SOBJ stream)
{
  tchar_t buf[60];
  stprintf(buf, T("binary input port @%ld"), (long)dp);
  sxWriteString(buf, stream);
}

PORT_OP void fbo_print(PORTDPTR dp, SOBJ stream)
{
  tchar_t buf[60];
  stprintf(buf, T("binary output port @%ld"), (long)dp);
  sxWriteString(buf, stream);
}


DEFINE_PORT_CLASS(xp_itext, PF_INPUT)
  fti_print, sxp_mark, sxp_save, sxp_restore,
  fil_close, err_putc, fil_getc, fil_ungetc, err_puts,
  err_read, err_write, sxp_listen,
  err_flush, sxp_cleari, err_clearo
ENDDEF_PORT_CLASS

DEFINE_PORT_CLASS(xp_otext, PF_OUTPUT)
  fto_print, sxp_mark, sxp_save, sxp_restore,
  fil_close, fil_putc, err_getc, err_ungetc, fil_puts,
  err_read, err_write, sxp_listen,
  fil_flush, err_cleari, sxp_clearo
ENDDEF_PORT_CLASS

DEFINE_PORT_CLASS(xp_ibin, PF_INPUT|PF_BINARY)
  fbi_print, sxp_mark, sxp_save, sxp_restore,
  fil_close, err_putc, err_getc, err_ungetc, err_puts,
  fil_read, err_write, sxp_listen,
  err_flush, sxp_cleari, err_clearo
ENDDEF_PORT_CLASS

DEFINE_PORT_CLASS(xp_obin, PF_OUTPUT|PF_BINARY)
  fbo_print, sxp_mark, sxp_save, sxp_restore,
  fil_close, err_putc, err_getc, err_ungetc, err_puts,
  err_read, fil_write, sxp_listen,
  fil_flush, err_cleari, sxp_clearo
ENDDEF_PORT_CLASS

static bool_t open_std_file(PORTDPTR* pdp, const tchar_t* name, 
                            const tchar_t* mode, bool_t ftext)
{
  assert(name != NULL);
  name = osfprobe(name, ((mode[0] == T('w')) ? QF_WRITE : QF_READ) | QF_WILD);
  if (name != NULL) {
    FILE* fp = osfopen(name, mode, ftext ? 1 : -1);
    if (fp != NULL) {
      f_file *fifp = (f_file*)malloc(sizeof(f_file));
      if (fifp != NULL) { 
        fifp->fptr = fp;
        fifp->count = ULONG_MAX;
        *pdp = (PORTDPTR)fifp;
        return TRUE;
      }
    }
  }
  return FALSE;
}

bool_t fti_open(const tchar_t* name, PORTVPTR* pvp, PORTDPTR* pdp)
{
  *pvp = xp_itext;
  return open_std_file(pdp, name, T("r"), TRUE);
}

bool_t fto_open(const tchar_t* name, PORTVPTR* pvp, PORTDPTR* pdp)
{
  *pvp = xp_otext;
  return open_std_file(pdp, name, T("w"), TRUE);
}

bool_t fbi_open(const tchar_t* name, PORTVPTR* pvp, PORTDPTR* pdp)
{
  *pvp = xp_ibin;
  return open_std_file(pdp, name, T("rb"), FALSE);
}

bool_t fbo_open(const tchar_t* name, PORTVPTR* pvp, PORTDPTR* pdp)
{
  *pvp = xp_obin;
  return open_std_file(pdp, name, T("wb"), FALSE);
}

/*#| (open-input-file filename) |#*/
DEFINE_INITIAL_BINDING("open-input-file", sp_openinfile)
DEFINE_PROCEDURE(sp_openinfile)
{
  PORTVPTR vp; PORTDPTR dp;
  SOBJ str = xlgastring();
  const tchar_t* name = getstring(str);
  xllastarg();
  if (fti_open(name, &vp, &dp)) return cvport(vp, dp);
  return sxSignalOpenError(T("file-input"), str);
}

/*#| (open-output-file filename) |#*/
DEFINE_INITIAL_BINDING("open-output-file", sp_openoutfile)
DEFINE_PROCEDURE(sp_openoutfile)
{
  PORTVPTR vp; PORTDPTR dp;
  SOBJ str = xlgastring();
  const tchar_t* name = getstring(str);
  xllastarg();
  if (fto_open(name, &vp, &dp)) return cvport(vp, dp);
  return sxSignalOpenError(T("file-output"), str);
}

/*#| (open-binary-input-file filename) |#*/
DEFINE_INITIAL_BINDING("open-binary-input-file", sp_openbinfile)
DEFINE_PROCEDURE(sp_openbinfile)
{
  PORTVPTR vp; PORTDPTR dp;
  SOBJ str = xlgastring();
  const tchar_t* name = getstring(str);
  xllastarg();
  if (fbi_open(name, &vp, &dp)) return cvport(vp, dp);
  return sxSignalOpenError(T("file-binary-input"), str);
}

/*#| (open-binary-output-file filename) |#*/
DEFINE_INITIAL_BINDING("open-binary-output-file", sp_openboutfile)
DEFINE_PROCEDURE(sp_openboutfile)
{
  PORTVPTR vp; PORTDPTR dp;
  SOBJ str = xlgastring();
  const tchar_t* name = getstring(str);
  xllastarg();
  if (fbo_open(name, &vp, &dp)) return cvport(vp, dp);
  return sxSignalOpenError(T("file-binary-output"), str);
}


PORT_OP void fil_save(PORTDPTR dp, SOBJ stream)
{
  /* write 0 or standard port # */
  FILE *fp; FIXTYPE i;
  f_file *fifp = (f_file*)dp;
  assert(fifp); assert(fifp->fptr);
  fp = fifp->fptr;
  i = 0;
  if (fp == stdin) i = 1;
  else if (fp == stdout) i = 2;
  else if (fp == stderr) i = 3;
  sxWriteByte((byte_t)i, stream);
}

PORT_OP bool_t fil_restore(PORTDPTR* pdp, SOBJ stream)
{
  /* read port # and restore or close if 0 */
  FILE *fp = NULL;
  byte_t b = sxReadByte(stream);
  switch(b) {
    default: case 0:     break; /* not restorable */
    case 1: fp = stdin;  break;
    case 2: fp = stdout; break;
    case 3: fp = stderr; break;
  }
  if (fp != NULL) {
    f_file *fifp = (f_file*)malloc(sizeof(f_file));
    if (fifp != NULL) {
      fifp->fptr = fp;
      fifp->count = ULONG_MAX;
      *pdp = (PORTDPTR)fifp;
      return TRUE;
    }
  } 
  return FALSE;
}

PORT_OP void fil_print(PORTDPTR dp, SOBJ stream)
{
  tchar_t buf[60];
  stprintf(buf, T("stdio port @%ld"), (long)dp);
  sxWriteString(buf, stream);
}

DEFINE_PORT_CLASS(xp_std, PF_INPUT|PF_OUTPUT|PF_BINARY)
  fil_print, sxp_mark, fil_save, fil_restore,
  fil_close, fil_putc, fil_getc, fil_ungetc, fil_puts,
  fil_read, fil_write, sxp_listen,
  fil_flush, sxp_cleari, sxp_clearo
ENDDEF_PORT_CLASS

bool_t std_open(int fnum, PORTVPTR* pvp, PORTDPTR* pdp)
{
  FILE* fp = NULL;
  switch (fnum) {
    case 0: fp = stdin; break;
    case 1: fp = stdout; break;
    case 2: fp = stderr; break;
  }
  if (fp != NULL) {
    f_file *fifp = (f_file*)malloc(sizeof(f_file));
    if (fifp != NULL) {
      fifp->fptr = fp;
      fifp->count = ULONG_MAX;
      *pvp = xp_std;
      *pdp = (PORTDPTR)fifp;
      return TRUE;
    }
  }
  return FALSE;
}

bool_t fil_open(const tchar_t* name, const tchar_t* mode, 
                PORTVPTR* pvp, PORTDPTR* pdp)
{
  FILE* fp = osfopen(name, mode, 0); /* determine orientation later... */
  if (fp != NULL) {
    f_file *fifp = (f_file*)malloc(sizeof(f_file));
    if (fifp != NULL) {
      fifp->fptr = fp;
      fifp->count = ULONG_MAX;
      *pvp = xp_std;
      *pdp = (PORTDPTR)fifp;
      return TRUE;
    }
  }
  return FALSE;
}

/*#| (open-stdio-file filename mode) |#*/
DEFINE_INITIAL_BINDING("open-stdio-file", sp_openstdfile)
DEFINE_PROCEDURE(sp_openstdfile)
{
  PORTVPTR vp; PORTDPTR dp;
  SOBJ str = xlgastring();
  SOBJ mstr = xlgastring();
  const tchar_t* name = getstring(str);
  const tchar_t* mode = getstring(mstr);
  xllastarg();
  if (fil_open(name, mode, &vp, &dp)) return cvport(vp, dp);
  return sxSignalOpenError(T("stdio-file"), str);
}


/*#| (limit-input-length content-length [cgiport]) |#*/
DEFINE_INITIAL_BINDING("limit-input-length", sp_limitin)
DEFINE_PROCEDURE(sp_limitin)
{
  SOBJ limit = xlgafixnum();
  SOBJ port = optarg() ? xlgaiport() : sv_curin;
  PORTVPTR* pvp = getvp(port);
  xllastarg();
  if (pvp != xp_itext && pvp != xp_ibin)
    sxErr(T("not a CGI input port"), port);
  { /* else */
    f_file *fifp = (f_file*)getdp(port);
    FIXTYPE i = getfixnum(limit);
    if (i < 0 || i > LONG_MAX)
      sxae_range(limit, cvsfixnum(LONG_MAX));
    assert(fifp); assert(fifp->fptr);
    fifp->count = (unsigned long)i;
    return so_void;
  }
}


/******************* source file input port *******************/

/* dp is fsi_file* */

typedef struct fsi_tag {
  FILE *fptr;
  FIXTYPE lines;
} fsi_file;

PORT_OP void fsi_close(PORTDPTR dp)
{
  fsi_file *fsifp = (fsi_file*)dp;
  fclose(fsifp->fptr);
  free(fsifp);
}

PORT_OP tint_t fsi_getc(PORTDPTR dp)
{
  fsi_file *fsifp = (fsi_file*)dp;
  tint_t c = fgettc(fsifp->fptr);
  if (c == T('\n')) fsifp->lines++;
  return c;
}

PORT_OP tint_t fsi_ungetc(PORTDPTR dp, tint_t c)
{
  fsi_file *fsifp = (fsi_file*)dp;
  if (c == T('\n')) fsifp->lines--; /* assumes "normal" use!!! */
  return ungettc(c, fsifp->fptr);
}

PORT_OP void fsi_print(PORTDPTR dp, SOBJ stream)
{
  tchar_t buf[60];
  stprintf(buf, T("source input port @%ld"), (long)dp);
  sxWriteString(buf, stream);
}

DEFINE_PORT_CLASS(xp_isrc, PF_INPUT)
  fsi_print, sxp_mark, sxp_save, sxp_restore,
  fsi_close, err_putc, fsi_getc, fsi_ungetc, err_puts,
  err_read, err_write, sxp_listen,
  sxp_flush, sxp_cleari, sxp_clearo
ENDDEF_PORT_CLASS

bool_t fsi_open(const tchar_t* name, PORTVPTR* pvp, PORTDPTR* pdp)
{
  FILE *fp = NULL;
  assert(name != NULL);
  name = osfprobe(name, QF_READ | QF_WILD);
  if (name != NULL) fp = osfopen(name, T("r"), 1);
  if (fp != NULL) {
    fsi_file *fsifp = (fsi_file*)malloc(sizeof(fsi_file));
    if (fsifp == NULL) { fclose(fp); return FALSE; }
    fsifp->fptr = fp;
    fsifp->lines = 0;
    *pvp = xp_isrc;
    *pdp = (PORTDPTR)fsifp;
    return TRUE;
  }
  return FALSE;
}

/*#| (open-source-file filename) |#*/
DEFINE_INITIAL_BINDING("open-source-file", sp_opensrcfile)
DEFINE_PROCEDURE(sp_opensrcfile)
{
  PORTVPTR vp; PORTDPTR dp;
  SOBJ str = xlgastring();
  const tchar_t* name = getstring(str);
  xllastarg();
  if (fsi_open(name, &vp, &dp)) return cvport(vp, dp);
  return sxSignalOpenError(T("source-file"), str);
}

/*#| (source-file-port? port) |#*/
DEFINE_INITIAL_BINDING("source-file-port?", sp_srcfileportp)
DEFINE_PROCEDURE(sp_srcfileportp)
{
  SOBJ port = xlgaport();
  xllastarg();
  return cvbool(getvp(port) == xp_isrc);
}

EXTERN_VARIABLE(sv_curin)

/*#| (lines-read [isrcport]) |#*/
DEFINE_INITIAL_BINDING("lines-read", sp_linesread)
DEFINE_PROCEDURE(sp_linesread)
{
  SOBJ port = optarg() ? xlgaiport() : sv_curin;
  xllastarg();
  if (getvp(port) != xp_isrc) sxErr(T("not a source file port"), port);
  return cvfixnum(((fsi_file*)getdp(port))->lines);
}
