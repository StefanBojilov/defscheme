/* iofilucs.c - file ports (UCS2) */

#include "../sxm.h"
#include "../io.h"
#include "../os.h"
#include "../sxio.h"
#include "../sxwrite.h"
#include "../sxread.h"
#include "../define.h"
#include "../extern.h"
#include "chucs2io.h"

#if (CS == CS_WIDE)
/* OK */
#elif (CS == CS_ANSI)
#error CS_ANSI not supported here 
#else
#error unknown CS
#endif /* CS == ... */

/*
 *  Defined port classes:  
 *  xp_itext, xp_otext, xp_ibin, xp_obin, xp_std, xp_isrc
 */

/**************** text file ports ***********************/

/* dp is u_file* */

PORT_OP void fil_close(PORTDPTR dp)
{
  FILE *fp = utfunwrap((u_file*)dp);
  /* never close std files */
  if (fp == stdin) return;
  if (fp == stdout) return;
  if (fp == stderr) return;
  if (fp != NULL) fclose(fp);
}

PORT_OP tint_t fil_putc(PORTDPTR dp, tchar_t c)
{
  return (tint_t)utfputc((ucschar_t)c, (u_file*)dp);
}

PORT_OP tint_t fil_getc(PORTDPTR dp)
{
  return (tint_t)utfgetc((u_file*)dp);
}

PORT_OP tint_t fil_ungetc(PORTDPTR dp, tint_t c)
{
  return (tint_t)utfungetc((ucsint_t)c, (u_file*)dp);
}

PORT_OP int fil_puts(PORTDPTR dp, const tchar_t* s)
{
  /* this would work only if wide char has two bytes */
  assert(sizeof(ucschar_t) == sizeof(tchar_t));
  return utfputs((ucschar_t*)s, (u_file*)dp);
}

PORT_OP size_t fil_read(PORTDPTR dp, byte_t* buf, size_t size)
{
  return utfread(buf, size, (u_file*)dp);
}

PORT_OP size_t fil_write(PORTDPTR dp, const byte_t* buf, size_t size)
{
  return utfwrite(buf, size, (u_file*)dp);
}

PORT_OP void fil_flush(PORTDPTR dp)
{
  utfflush((u_file*)dp);
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
                            const tchar_t* mode, int oripref,
                            const ucschar_t* pmap8b)
{
  bool_t readp;
  /* this would work only if wide char has two bytes */
  assert(sizeof(ucschar_t) == sizeof(tchar_t));
  assert(name != NULL);
  assert(mode != NULL);
  readp = (mode[0] != T('w'));
  name = osfprobe(name, (readp ? QF_READ : QF_WRITE) | QF_WILD);
  if (name != NULL) { 
    /* don't try to bind fp: std binding is useless */
    FILE* fp = osfopen(name, mode, 0);
    if (fp != NULL) {
      u_file* ufp = utfwrap(fp, readp, oripref, NULL);
      *pdp = (PORTDPTR)ufp;
      return ufp != NULL;
    }
  }
  return FALSE;
}

bool_t fti_open(const tchar_t* name, PORTVPTR* pvp, PORTDPTR* pdp)
{
  *pvp = xp_itext;
  return open_std_file(pdp, name, T("rb"), ORI_UTF8, NULL);
}

bool_t fto_open(const tchar_t* name, PORTVPTR* pvp, PORTDPTR* pdp)
{
  *pvp = xp_otext;
  return open_std_file(pdp, name, T("wb"), ORI_UTF8, NULL);
}

bool_t fbi_open(const tchar_t* name, PORTVPTR* pvp, PORTDPTR* pdp)
{
  *pvp = xp_ibin;
  return open_std_file(pdp, name, T("rb"), ORI_BINARY, NULL);
}

bool_t fbo_open(const tchar_t* name, PORTVPTR* pvp, PORTDPTR* pdp)
{
  *pvp = xp_obin;
  return open_std_file(pdp, name, T("wb"), ORI_BINARY, NULL);
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


/*#| ascii |#*/
DEFINE_INITIAL_BINDING("ascii", ss_enc_ascii)
DEFINE_DATUM_STRING(ss_enc_ascii, "#L0")

/*#| utf-8 |#*/
DEFINE_INITIAL_BINDING("utf-8", ss_enc_utf8)
DEFINE_DATUM_STRING(ss_enc_utf8, "#8N")

/*#| utf-16 |#*/
DEFINE_INITIAL_BINDING("utf-16", ss_enc_utf16)
DEFINE_DATUM_STRING(ss_enc_utf16, "#AE")

/*#| utf-16le |#*/
DEFINE_INITIAL_BINDING("utf-16le", ss_enc_utf16le)
DEFINE_DATUM_STRING(ss_enc_utf16le, "#LE")

/*#| utf-16be |#*/
DEFINE_INITIAL_BINDING("utf-16be", ss_enc_utf16be)
DEFINE_DATUM_STRING(ss_enc_utf16be, "#BE")

static bool_t parse_encoding(const tchar_t* enc, int* poripref, 
                             const ucschar_t** pmap8b)
{
  *pmap8b = NULL;
  if (tcscmp(enc, T("#L0")) == 0) {
    *poripref = ORI_XASCII; return TRUE;
  } else if (tcscmp(enc, T("#8N")) == 0) {
    *poripref = ORI_UTF8; return TRUE;
  } else if (tcscmp(enc, T("#AE")) == 0) {
    *poripref = ORI_UCS2; return TRUE;
  } else if (tcscmp(enc, T("#LE")) == 0) {
    *poripref = ORI_UCS2_FFFE; return TRUE;
  } else if (tcscmp(enc, T("#BE")) == 0) {
    *poripref = ORI_UCS2_FEFF; return TRUE;
  } else if (tcslen(enc) == 256) {
    /* this would work only if wide char has two bytes */
    assert(sizeof(ucschar_t) == sizeof(tchar_t));
    *poripref = ORI_XASCII; *pmap8b = (const ucschar_t*)enc;
    return TRUE;
  }
  return FALSE;
}

/*#| (open-text-input-file filename encoding) |#*/
DEFINE_INITIAL_BINDING("open-text-input-file", sp_opentinfile)
DEFINE_PROCEDURE(sp_opentinfile)
{
  PORTVPTR vp; PORTDPTR dp;
  int oripref; const ucschar_t* pmap8b;
  SOBJ str = xlgastring();
  const tchar_t* name = getstring(str);
  SOBJ encstr = xlgastring();
  const tchar_t* enc = getstring(encstr);
  xllastarg();
  if (!parse_encoding(enc, &oripref, &pmap8b))
    sxErr(T("unsupported encoding: "), encstr);
  vp = xp_itext;
  if (open_std_file(&dp, name, T("rb"), oripref, pmap8b))
    return cvport(vp, dp);
  return sxSignalOpenError(T("file-text-input"), str);
}

/*#| (open-text-output-file filename encoding) |#*/
DEFINE_INITIAL_BINDING("open-text-output-file", sp_opentoutfile)
DEFINE_PROCEDURE(sp_opentoutfile)
{
  PORTVPTR vp; PORTDPTR dp;
  int oripref; const ucschar_t* pmap8b;
  SOBJ str = xlgastring();
  const tchar_t* name = getstring(str);
  SOBJ encstr = xlgastring();
  const tchar_t* enc = getstring(encstr);
  xllastarg();
  if (!parse_encoding(enc, &oripref, &pmap8b))
    sxErr(T("unsupported encoding: "), encstr);
  vp = xp_otext;
  if (open_std_file(&dp, name, T("wb"), oripref, pmap8b))
    return cvport(vp, dp);
  return sxSignalOpenError(T("file-text-output"), str);
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
  FILE *fp = NULL;
  u_file* ufp = (u_file*)dp;
  FIXTYPE i = 0;
  if (ufp != NULL) fp = utfgetfp(ufp);
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
  bool_t readp;
  switch(b) {
    default: case 0:     break; /* not restorable */
    case 1: fp = stdin;  readp = TRUE; break;
    case 2: fp = stdout; readp = FALSE; break;
    case 3: fp = stderr; readp = FALSE; break;
  }
  if (fp != NULL) {
    /* restore one of standard ports */
    u_file* ufp = utfwrap(fp, readp, ORI_UNBOUND, NULL);
    *pdp = (PORTDPTR)ufp;
    return TRUE;
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
  bool_t readp;
  switch (fnum) {
    case 0: fp = stdin; readp = TRUE; break;
    case 1: fp = stdout; readp = FALSE; break;
    case 2: fp = stderr; readp = FALSE; break;
  }
  if (fp != NULL) {
    /* determine text file orientation on first use */
    u_file* ufp = utfwrap(fp, readp, ORI_UNBOUND, NULL);
    *pvp = xp_std;
    *pdp = (PORTDPTR)ufp;
    return ufp != NULL;
  }
  return FALSE;
}

bool_t fil_open(const tchar_t* name, const tchar_t* mode, 
                PORTVPTR* pvp, PORTDPTR* pdp)
{
  bool_t readp;
  /* this would work only if wide char has two bytes */
  assert(sizeof(ucschar_t) == sizeof(tchar_t));
  assert(name != NULL);
  assert(mode != NULL);
  readp = (mode[0] != T('w'));
  /* don't try to bind fp: std binding is useless */
  FILE* fp = osfopen(name, mode, 0); 
  if (fp != NULL) {
    /* determine text file orientation on first use */
    u_file* ufp = utfwrap(fp, readp, ORI_UNBOUND, NULL);
    *pvp = xp_std;
    *pdp = (PORTDPTR)ufp;
    return ufp != NULL;
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



/******************* source file input port *******************/

/* dp is fsi_file* */

typedef struct fsi_tag {
  u_file *ufp;
  FIXTYPE lines;
} fsi_file;

PORT_OP void fsi_close(PORTDPTR dp)
{
  fsi_file *fsifp = (fsi_file*)dp;
  FILE *fp = utfunwrap(fsifp->ufp);
  if (fp != NULL) fclose(fp);
  free(fsifp);
}

PORT_OP tint_t fsi_getc(PORTDPTR dp)
{
  fsi_file *fsifp = (fsi_file*)dp;
  tint_t c = (tint_t)utfgetc(fsifp->ufp);
  if (c == T('\n')) fsifp->lines++;
  return c;
}

PORT_OP tint_t fsi_ungetc(PORTDPTR dp, tint_t c)
{
  fsi_file *fsifp = (fsi_file*)dp;
  if (c == T('\n')) fsifp->lines--; /* assumes "normal" use!!! */
  return (tint_t)utfungetc((ucsint_t)c, fsifp->ufp);
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
  if (name != NULL) fp = osfopen(name, T("rb"), 0);
  if (fp != NULL) {
    /* determine text file orientation on first use */
    u_file* ufp = utfwrap(fp, TRUE, ORI_UNBOUND, NULL);
    if (ufp != NULL) {
      fsi_file *fsifp = (fsi_file*)malloc(sizeof(fsi_file));
      if (fsifp == NULL) { fclose(fp); return FALSE; }
      fsifp->ufp = ufp;
      fsifp->lines = 0;
      *pvp = xp_isrc;
      *pdp = (PORTDPTR)fsifp;
      return TRUE;
    }
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
