/* ioprocess.c - process ports (UNIX) */

#include "../sxm.h"
#include "../io.h"
#include "../os.h"
#include "../sxio.h"
#include "../sxwrite.h"
#include "../define.h"

/*
 *  Defined port classes:  
 *  xp_iprocess, xp_oprocess
 */

/**************** text file ports ***********************/

/* dp is FILE* */

PORT_OP void pro_close(PORTDPTR dp)
{
  fclose((FILE*)dp);
}

PORT_OP tint_t pro_putc(PORTDPTR dp, tchar_t c)
{
  return fputtc(c, (FILE*)dp);
}

PORT_OP tint_t pro_getc(PORTDPTR dp)
{
  return fgettc((FILE*)dp);
}

PORT_OP tint_t pro_ungetc(PORTDPTR dp, tint_t c)
{
  return ungettc(c, (FILE*)dp);
}

PORT_OP int pro_puts(PORTDPTR dp, const tchar_t* s)
{
  return fputts(s, (FILE*)dp);
}

PORT_OP void pro_flush(PORTDPTR dp)
{
  fflush((FILE*)dp);
}

PORT_OP void fpi_print(PORTDPTR dp, SOBJ stream)
{
  tchar_t buf[60];
  stprintf(buf, T("process input port @%ld"), (long)dp);
  sxWriteString(buf, stream);
}

PORT_OP void fpo_print(PORTDPTR dp, SOBJ stream)
{
  tchar_t buf[60];
  stprintf(buf, T("process output port @%ld"), (long)dp);
  sxWriteString(buf, stream);
}

DEFINE_PORT_CLASS(xp_iprocess, PF_INPUT)
  fpi_print, sxp_mark, sxp_save, sxp_restore,
  pro_close, err_putc, pro_getc, pro_ungetc, err_puts,
  err_read, err_write, sxp_listen,
  err_flush, sxp_cleari, err_clearo
ENDDEF_PORT_CLASS

DEFINE_PORT_CLASS(xp_oprocess, PF_OUTPUT)
  fpo_print, sxp_mark, sxp_save, sxp_restore,
  pro_close, pro_putc, err_getc, err_ungetc, pro_puts,
  err_read, err_write, sxp_listen,
  pro_flush, err_cleari, sxp_clearo
ENDDEF_PORT_CLASS


/*#| (process command) |#*/
DEFINE_INITIAL_BINDING("process", sp_process)
DEFINE_PROCEDURE(sp_process)
{
  int pid;
  FILE *proc_in;
  FILE *proc_out;
  tchar_t *cmd;
  SOBJ arg = xlgastring();

  xllastarg();
  gcLock(arg);
  sxMemGC(); /* get rid of garbage ports, handles etc... */
  gcUnlock(1);
  cmd = getstring(arg);
  if (osprocess(&proc_in, &proc_out, &pid, cmd)) {
    /* ok, return (proc_in proc_out pid) */
    SOBJ lst = cons(cvfixnum(pid), so_nil);
    gcLock(lst);
    lst = cons(cvport(xp_oprocess, (PORTDPTR)proc_out), lst);
    gcUnlock(1);
    gcLock(lst);
    lst = cons(cvport(xp_iprocess, (PORTDPTR)proc_in), lst);
    gcUnlock(1);
    return lst;
  } else {
    /* process failed ... what should we do? */
    return sxSignalOpenError(T("process"), arg);
  }
}
