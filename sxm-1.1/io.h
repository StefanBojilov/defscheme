/* io.h - port mechanics */

#ifndef __IO_H
#define __IO_H

#define PORT_OP static

/* default port methods */
extern void sxp_print(PORTDPTR dp, SOBJ ovp);
extern void sxp_mark(PORTDPTR dp);
extern void sxp_save(PORTDPTR dp, SOBJ s);
extern bool_t sxp_restore(PORTDPTR* pdp, SOBJ s);
extern bool_t sxp_listen(PORTDPTR dp);
extern void sxp_noop(PORTDPTR dp);
#define sxp_flush  (sxp_noop)
#define sxp_cleari (sxp_noop)
#define sxp_clearo (sxp_noop)

/* unsupported operation error handlers */
extern int sxp_unsupp(PORTDPTR dp);
#define err_putc   ((tint_t(*)(PORTDPTR, tchar_t))sxp_unsupp)
#define err_puts   ((int(*)(PORTDPTR, const tchar_t*))sxp_unsupp)
#define err_getc   ((tint_t(*)(PORTDPTR))sxp_unsupp)
#define err_ungetc ((tint_t(*)(PORTDPTR, tint_t))sxp_unsupp)
#define err_read   ((size_t(*)(PORTDPTR, byte_t*, size_t))sxp_unsupp)
#define err_write  ((size_t(*)(PORTDPTR, const byte_t*, size_t))sxp_unsupp)
#define err_listen ((bool_t(*)(PORTDPTR))sxp_unsupp)
#define err_flush  ((void(*)(PORTDPTR))sxp_unsupp)
#define err_cleari ((void(*)(PORTDPTR))sxp_unsupp)
#define err_clearo ((void(*)(PORTDPTR))sxp_unsupp)

/* constructors for required port types */
/* iostd.c: */
extern void clo_open(PORTVPTR* pvp, PORTDPTR* pdp);
extern bool_t cnt_open(PORTVPTR* pvp, PORTDPTR* pdp);
/* iotty...c: */
extern bool_t tty_open(PORTVPTR* pvp, PORTDPTR* pdp);
/* iofil...c: */
extern bool_t fti_open(const tchar_t* name, PORTVPTR* pvp, PORTDPTR* pdp);
extern bool_t fto_open(const tchar_t* name, PORTVPTR* pvp, PORTDPTR* pdp);
extern bool_t fbi_open(const tchar_t* name, PORTVPTR* pvp, PORTDPTR* pdp);
extern bool_t fbo_open(const tchar_t* name, PORTVPTR* pvp, PORTDPTR* pdp);
extern bool_t fil_open(const tchar_t* name, const tchar_t* mode, PORTVPTR* pvp, PORTDPTR* pdp);
extern bool_t std_open(int fnum, PORTVPTR* pvp, PORTDPTR* pdp);
extern bool_t fsi_open(const tchar_t* name, PORTVPTR* pvp, PORTDPTR* pdp);

#endif /* ndef __IO_H */

