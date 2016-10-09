/* iocomp.c - composite ports (optional) */

#include "../sxm.h"
#include "../sxwrite.h"
#include "../io.h"
#include "../sxio.h"
#include "../sxwrite.h"
#include "../sxread.h"
#include "../define.h"

/*
 *  Defined port classes: xp_broadcast, xp_concatenated, 
 *  xp_echo, xp_synonym, xp_twoway
 */


/******************* composite ports ***********************/

/* dp is comp_file* for all composite ports */

typedef struct comp_tag {
  FIXTYPE count;     /* total number of ports */
  FIXTYPE active;    /* active port: used by conc port only */
  SOBJ ports[1];     /* malloc allocates as many pointers as needed */
} comp_file;

PORT_OP void comp_mark(PORTDPTR dp)
{
  comp_file *fp = (comp_file*)dp;
  int i = 0; 
  while (i < fp->count) sxMemMark(fp->ports[i++]);
}


PORT_OP void comp_close(PORTDPTR dp)
{
  /* component ports are NOT closed */
  free((comp_file*)dp);
}

/******************* broadcast ports ***********************/

PORT_OP tint_t bro_putc(PORTDPTR dp, tchar_t c)
{
  comp_file *fp = (comp_file*)dp;
  int i = 0; 
  while (i < fp->count) {
    SOBJ p = fp->ports[i++];
    getvp(p)->v_putc(getdp(p), c);
  }
  return c;
}

PORT_OP int bro_puts(PORTDPTR dp, const tchar_t* s)
{
  comp_file *fp = (comp_file*)dp;
  int i = 0; 
  while (i < fp->count) {
    SOBJ p = fp->ports[i++];
    getvp(p)->v_puts(getdp(p), s);
  }
  return 0;
}

PORT_OP size_t bro_write(PORTDPTR dp, const byte_t* buf, size_t size)
{
  comp_file *fp = (comp_file*)dp;
  int i = 0; 
  while (i < fp->count) {
    SOBJ p = fp->ports[i++];
    getvp(p)->v_write(getdp(p), buf, size);
  }
  return size;
}

PORT_OP void bro_flush(PORTDPTR dp)
{
  comp_file *fp = (comp_file*)dp;
  int i = 0; 
  while (i < fp->count) {
    SOBJ p = fp->ports[i++];
    getvp(p)->v_flush(getdp(p));
  }
}

PORT_OP void bro_clearo(PORTDPTR dp)
{
  comp_file *fp = (comp_file*)dp;
  int i = 0; 
  while (i < fp->count) {
    SOBJ p = fp->ports[i++];
    getvp(p)->v_clearo(getdp(p));
  }
}

PORT_OP void bro_print(PORTDPTR dp, SOBJ stream)
{
  tchar_t buf[60];
  stprintf(buf, T("broadcast port @%ld"), (long)dp);
  sxWriteString(buf, stream);
}

DEFINE_PORT_CLASS(xp_broadcast, PF_OUTPUT|PF_BINARY)
  bro_print, comp_mark, sxp_save, sxp_restore, /* ??? ToDo: own serializer ??? */
  comp_close, bro_putc, err_getc, err_ungetc, bro_puts,
  err_read, bro_write, sxp_listen,
  bro_flush, err_cleari, bro_clearo
ENDDEF_PORT_CLASS

bool_t bro_open(SOBJ* pp, int cnt, PORTVPTR* pvp, PORTDPTR* pdp)
{
  int i;
  comp_file *fp = (comp_file*)malloc(sizeof(comp_file) + sizeof(SOBJ)*(cnt));
  if (fp == NULL) return FALSE;
  fp->count = cnt; fp->active = 0;
  for (i = 0; i < cnt; i++) fp->ports[i] = *pp++; 
  *pvp = xp_broadcast;
  *pdp = (PORTDPTR)fp;
  return TRUE;
}

/*#| (broadcast-port port ...) |#*/
DEFINE_INITIAL_BINDING("broadcast-port", sp_broadcastport)
DEFINE_PROCEDURE(sp_broadcastport)
{
  PORTVPTR vp; PORTDPTR dp; int i;
  SOBJ p = so_false;
  /* check that all args are output ports */
  int cnt = vmargc;
  for (i = 0; i < cnt; i++) if (!oportp(vmsp[i])) xlreqoport(vmsp[i]);
  if (bro_open(vmsp, cnt, &vp, &dp)) p = cvport(vp, dp);
  /* now we don't need args to be GC-protected any more */
  xlpoprest();
  if (p != so_false) return p;
  return sxSignalOpenError(T("broadcast-port"), so_unbound);
}


/******************* concatenated ports ***********************/


PORT_OP tint_t conc_getc(PORTDPTR dp)
{
  comp_file *fp = (comp_file*)dp;
  for (;;) {
    if (fp->active >= fp->count) return TEOF;
    else {
      SOBJ p = fp->ports[fp->active];
      tint_t c = getvp(p)->v_getc(getdp(p));
      if (c != TEOF) return c;
      fp->active++;
    }
  }
}

PORT_OP tint_t conc_ungetc(PORTDPTR dp, tint_t c)
{
  comp_file *fp = (comp_file*)dp;
  if (fp->active >= fp->count) return TEOF;
  else {
    SOBJ p = fp->ports[fp->active];
    return getvp(p)->v_ungetc(getdp(p), c);
  }
}

PORT_OP size_t conc_read(PORTDPTR dp, byte_t* buf, size_t size)
{
  comp_file *fp = (comp_file*)dp;
  size_t nread = 0;
  for (;;) {
    if (fp->active >= fp->count) return nread;
    else {
      SOBJ p = fp->ports[fp->active];
      size_t cnt = getvp(p)->v_read(getdp(p), buf, size);
      nread += cnt;
      assert(cnt <= size);
      if (cnt == size) return nread;
      /* cnt is less than size: eof? */
      size -= cnt;
      buf += cnt;
      fp->active++;
    }
  }
}

PORT_OP void conc_print(PORTDPTR dp, SOBJ stream)
{
  tchar_t buf[60];
  stprintf(buf, T("concatenated port @%ld"), (long)dp);
  sxWriteString(buf, stream);
}

DEFINE_PORT_CLASS(xp_concatenated, PF_INPUT|PF_BINARY)
  conc_print, comp_mark, sxp_save, sxp_restore, /* ??? ToDo: own serializer ??? */
  comp_close, err_putc, conc_getc, conc_ungetc, err_puts,
  conc_read, err_write, sxp_listen,
  err_flush, sxp_cleari, err_clearo
ENDDEF_PORT_CLASS

bool_t conc_open(SOBJ* pp, int cnt, PORTVPTR* pvp, PORTDPTR* pdp)
{
  int i;
  comp_file *fp = (comp_file*)malloc(sizeof(comp_file) + sizeof(SOBJ)*(cnt));
  if (fp == NULL) return FALSE;
  fp->count = cnt; fp->active = 0;
  for (i = 0; i < cnt; i++) fp->ports[i] = *pp++; 
  *pvp = xp_concatenated;
  *pdp = (PORTDPTR)fp;
  return TRUE;
}

/*#| (concatenated-port port ...) |#*/
DEFINE_INITIAL_BINDING("concatenated-port", sp_concport)
DEFINE_PROCEDURE(sp_concport)
{
  PORTVPTR vp; PORTDPTR dp; int i;
  SOBJ p = so_false;
  /* check that all args are input ports */
  int cnt = vmargc;
  for (i = 0; i < cnt; i++) if (!iportp(vmsp[i])) xlreqiport(vmsp[i]);
  if (conc_open(vmsp, cnt, &vp, &dp)) p = cvport(vp, dp);
  /* now we don't need args to be GC-protected any more */
  xlpoprest();
  if (p != so_false) return p;
  return sxSignalOpenError(T("concatenated-port"), so_unbound);
}


/******************* echo ports ***********************/


PORT_OP tint_t echo_getc(PORTDPTR dp)
{
  comp_file *fp = (comp_file*)dp;
  SOBJ pin = fp->ports[0];
  SOBJ pout = fp->ports[1];
  tint_t c = getvp(pin)->v_getc(getdp(pin));
  if (c != TEOF) {
    /* check if c has already been echoed before */
    if (fp->active > 0) fp->active--;
    else getvp(pout)->v_putc(getdp(pout), c);
  }
  return c;
}

PORT_OP tint_t echo_ungetc(PORTDPTR dp, tint_t c)
{
  comp_file *fp = (comp_file*)dp;
  SOBJ pin = fp->ports[0];
  /* increment number of future getc that should not be echoed */
  if (c != TEOF && ++fp->active > 1) sxFail(T("double unread-char"));
  return getvp(pin)->v_ungetc(getdp(pin), c);
}

PORT_OP tint_t echo_putc(PORTDPTR dp, tchar_t c)
{
  comp_file *fp = (comp_file*)dp;
  SOBJ pout = fp->ports[1];
  return getvp(pout)->v_putc(getdp(pout), c);
}

PORT_OP int echo_puts(PORTDPTR dp, const tchar_t* s)
{
  comp_file *fp = (comp_file*)dp;
  SOBJ pout = fp->ports[1];
  return getvp(pout)->v_puts(getdp(pout), s);
}

PORT_OP size_t echo_read(PORTDPTR dp, byte_t* buf, size_t size)
{
  comp_file *fp = (comp_file*)dp;
  SOBJ pin = fp->ports[0];
  SOBJ pout = fp->ports[1];
  size_t cnt = getvp(pin)->v_read(getdp(pin), buf, size);
  if (cnt > 0) getvp(pout)->v_write(getdp(pout), buf, cnt);
  return cnt;
}

PORT_OP size_t echo_write(PORTDPTR dp, const byte_t* buf, size_t size)
{
  comp_file *fp = (comp_file*)dp;
  SOBJ pout = fp->ports[1];
  return getvp(pout)->v_write(getdp(pout), buf, size);
}

PORT_OP void echo_flush(PORTDPTR dp)
{
  comp_file *fp = (comp_file*)dp;
  SOBJ pout = fp->ports[1];
  getvp(pout)->v_flush(getdp(pout));
}

PORT_OP void echo_cleari(PORTDPTR dp)
{
  comp_file *fp = (comp_file*)dp;
  SOBJ pin = fp->ports[0];
  getvp(pin)->v_cleari(getdp(pin));
}

PORT_OP void echo_clearo(PORTDPTR dp)
{
  comp_file *fp = (comp_file*)dp;
  SOBJ pout = fp->ports[1];
  getvp(pout)->v_clearo(getdp(pout));
}

PORT_OP void echo_print(PORTDPTR dp, SOBJ stream)
{
  tchar_t buf[60];
  stprintf(buf, T("echo port @%ld"), (long)dp);
  sxWriteString(buf, stream);
}

DEFINE_PORT_CLASS(xp_echo, PF_INPUT|PF_OUTPUT|PF_BINARY)
  echo_print, comp_mark, sxp_save, sxp_restore, /* ??? ToDo: own serializer ??? */
  comp_close, echo_putc, echo_getc, echo_ungetc, echo_puts,
  echo_read, echo_write, sxp_listen,
  echo_flush, echo_cleari, echo_clearo
ENDDEF_PORT_CLASS

bool_t echo_open(SOBJ pin, SOBJ pout, PORTVPTR* pvp, PORTDPTR* pdp)
{
  comp_file *fp = (comp_file*)malloc(sizeof(comp_file) + sizeof(SOBJ)*2);
  if (fp == NULL) return FALSE;
  fp->count = 2; 
  fp->active = 0; /* used as ungetc counter */
  fp->ports[0] = pin; 
  fp->ports[1] = pout; 
  *pvp = xp_echo;
  *pdp = (PORTDPTR)fp;
  return TRUE;
}

/*#| (echo-port iport oport) |#*/
DEFINE_INITIAL_BINDING("echo-port", sp_echoport)
DEFINE_PROCEDURE(sp_echoport)
{
  PORTVPTR vp; PORTDPTR dp;
  SOBJ pin = xlgaiport();
  SOBJ pout = xlgaoport();
  SOBJ p = so_false;
  xllastarg();
  gcLock(pin); gcLock(pout);
  if (echo_open(pin, pout, &vp, &dp)) p = cvport(vp, dp);
  /* now we don't need args to be GC-protected any more */
  gcUnlock(2);
  if (p != so_false) return p;
  return sxSignalOpenError(T("echo-port"), so_unbound);
}


/******************* synonym ports ***********************/


PORT_OP tint_t syn_getc(PORTDPTR dp)
{
  SOBJ p = ((comp_file*)dp)->ports[0];
  return getvp(p)->v_getc(getdp(p));
}

PORT_OP tint_t syn_ungetc(PORTDPTR dp, tint_t c)
{
  SOBJ p = ((comp_file*)dp)->ports[0];
  return getvp(p)->v_ungetc(getdp(p), c);
}

PORT_OP tint_t syn_putc(PORTDPTR dp, tchar_t c)
{
  SOBJ p = ((comp_file*)dp)->ports[0];
  return getvp(p)->v_putc(getdp(p), c);
}

PORT_OP int syn_puts(PORTDPTR dp, const tchar_t* s)
{
  SOBJ p = ((comp_file*)dp)->ports[0];
  return getvp(p)->v_puts(getdp(p), s);
}

PORT_OP size_t syn_read(PORTDPTR dp, byte_t* buf, size_t size)
{
  SOBJ p = ((comp_file*)dp)->ports[0];
  return getvp(p)->v_read(getdp(p), buf, size);
}

PORT_OP size_t syn_write(PORTDPTR dp, const byte_t* buf, size_t size)
{
  SOBJ p = ((comp_file*)dp)->ports[0];
  return getvp(p)->v_write(getdp(p), buf, size);
}

PORT_OP void syn_flush(PORTDPTR dp)
{
  SOBJ p = ((comp_file*)dp)->ports[0];
  getvp(p)->v_flush(getdp(p));
}

PORT_OP void syn_cleari(PORTDPTR dp)
{
  SOBJ p = ((comp_file*)dp)->ports[0];
  getvp(p)->v_cleari(getdp(p));
}

PORT_OP void syn_clearo(PORTDPTR dp)
{
  SOBJ p = ((comp_file*)dp)->ports[0];
  getvp(p)->v_clearo(getdp(p));
}

PORT_OP void syn_print(PORTDPTR dp, SOBJ stream)
{
  tchar_t buf[60];
  stprintf(buf, T("synonym port @%ld"), (long)dp);
  sxWriteString(buf, stream);
}

DEFINE_PORT_CLASS(xp_synonym, PF_INPUT|PF_OUTPUT|PF_BINARY)
  syn_print, comp_mark, sxp_save, sxp_restore, /* ??? ToDo: own serializer ??? */
  comp_close, syn_putc, syn_getc, syn_ungetc, syn_puts,
  syn_read, syn_write, sxp_listen,
  syn_flush, syn_cleari, syn_clearo
ENDDEF_PORT_CLASS

bool_t syn_open(SOBJ p, PORTVPTR* pvp, PORTDPTR* pdp)
{
  comp_file *fp = (comp_file*)malloc(sizeof(comp_file) + sizeof(SOBJ)*1);
  if (fp == NULL) return FALSE;
  fp->count = 1; 
  fp->active = 0; 
  fp->ports[0] = p; 
  *pvp = xp_synonym;
  *pdp = (PORTDPTR)fp;
  return TRUE;
}

/*#| (synonym-port port) |#*/
DEFINE_INITIAL_BINDING("synonym-port", sp_synonymport)
DEFINE_PROCEDURE(sp_synonymport)
{
  PORTVPTR vp; PORTDPTR dp;
  SOBJ porg = xlgaport();
  SOBJ p = so_false;
  xllastarg();
  gcLock(porg);
  if (syn_open(porg, &vp, &dp)) p = cvport(vp, dp);
  /* now we don't need args to be GC-protected any more */
  gcUnlock(1);
  if (p != so_false) return p;
  return sxSignalOpenError(T("synonym-port"), so_unbound);
}

/*#| (synonym-port? obj) |#*/
DEFINE_INITIAL_BINDING("synonym-port?", sp_synportp)
DEFINE_PROCEDURE(sp_synportp)
{
  SOBJ arg = xlonearg();
  return cvbool(portp(arg) && getvp(arg) == xp_synonym);
}

/*#| (synonym-port-value synport) |#*/
DEFINE_INITIAL_BINDING("synonym-port-value", sp_synportval)
DEFINE_PROCEDURE(sp_synportval)
{
  SOBJ port = xlgaport();
  xllastarg();
  if (getvp(port) != xp_synonym) sxErr(T("not a synonym port"), port);
  return ((comp_file*)getdp(port))->ports[0];
}

/*#| (set-synonym-port-value! synport port) |#*/
DEFINE_INITIAL_BINDING("set-synonym-port-value!", sp_setsynportval)
DEFINE_PROCEDURE(sp_setsynportval)
{
  SOBJ port = xlgaport();
  SOBJ newp = xlgaport();
  xllastarg();
  if (getvp(port) != xp_synonym) sxErr(T("not a synonym port"), port);
  ((comp_file*)getdp(port))->ports[0] = newp;
  return so_void;
}

/******************* two-way ports ***********************/

PORT_OP tint_t twy_getc(PORTDPTR dp)
{
  SOBJ p = ((comp_file*)dp)->ports[0];
  return getvp(p)->v_getc(getdp(p));
}

PORT_OP tint_t twy_ungetc(PORTDPTR dp, tint_t c)
{
  SOBJ p = ((comp_file*)dp)->ports[0];
  return getvp(p)->v_ungetc(getdp(p), c);
}

PORT_OP tint_t twy_putc(PORTDPTR dp, tchar_t c)
{
  SOBJ p = ((comp_file*)dp)->ports[1];
  return getvp(p)->v_putc(getdp(p), c);
}

PORT_OP int twy_puts(PORTDPTR dp, const tchar_t* s)
{
  SOBJ p = ((comp_file*)dp)->ports[1];
  return getvp(p)->v_puts(getdp(p), s);
}

PORT_OP size_t twy_read(PORTDPTR dp, byte_t* buf, size_t size)
{
  SOBJ p = ((comp_file*)dp)->ports[0];
  return getvp(p)->v_read(getdp(p), buf, size);
}

PORT_OP size_t twy_write(PORTDPTR dp, const byte_t* buf, size_t size)
{
  SOBJ p = ((comp_file*)dp)->ports[1];
  return getvp(p)->v_write(getdp(p), buf, size);
}

PORT_OP void twy_flush(PORTDPTR dp)
{
  SOBJ p = ((comp_file*)dp)->ports[1];
  getvp(p)->v_flush(getdp(p));
}

PORT_OP void twy_cleari(PORTDPTR dp)
{
  SOBJ p = ((comp_file*)dp)->ports[0];
  getvp(p)->v_cleari(getdp(p));
}

PORT_OP void twy_clearo(PORTDPTR dp)
{
  SOBJ p = ((comp_file*)dp)->ports[1];
  getvp(p)->v_clearo(getdp(p));
}

PORT_OP void twy_print(PORTDPTR dp, SOBJ stream)
{
  tchar_t buf[60];
  stprintf(buf, T("two-way port @%ld"), (long)dp);
  sxWriteString(buf, stream);
}


DEFINE_PORT_CLASS(xp_twoway, PF_INPUT|PF_OUTPUT|PF_BINARY)
  twy_print, comp_mark, sxp_save, sxp_restore, /* ??? ToDo: own serializer ??? */
  comp_close, twy_putc, twy_getc, twy_ungetc, twy_puts,
  twy_read, twy_write, sxp_listen,
  twy_flush, twy_cleari, twy_clearo
ENDDEF_PORT_CLASS

bool_t twy_open(SOBJ pin, SOBJ pout, PORTVPTR* pvp, PORTDPTR* pdp)
{
  comp_file *fp = (comp_file*)malloc(sizeof(comp_file) + sizeof(SOBJ)*2);
  if (fp == NULL) return FALSE;
  fp->count = 2; 
  fp->active = 0; /* used as ungetc counter */
  fp->ports[0] = pin; 
  fp->ports[1] = pout; 
  *pvp = xp_twoway;
  *pdp = (PORTDPTR)fp;
  return TRUE;
}

/*#| (two-way-port iport oport) |#*/
DEFINE_INITIAL_BINDING("two-way-port", sp_twowayport)
DEFINE_PROCEDURE(sp_twowayport)
{
  PORTVPTR vp; PORTDPTR dp;
  SOBJ pin = xlgaiport();
  SOBJ pout = xlgaoport();
  SOBJ p = so_false;
  xllastarg();
  gcLock(pin); gcLock(pout);
  if (twy_open(pin, pout, &vp, &dp)) p = cvport(vp, dp);
  /* now we don't need args to be GC-protected any more */
  gcUnlock(2);
  if (p != so_false) return p;
  return sxSignalOpenError(T("two-way-port"), so_unbound);
}

