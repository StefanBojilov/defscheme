/* sxwrite.h - output routines */

#ifndef __SXWRITE_H
#define __SXWRITE_H

#include "sxm.h"

/* Note: all output operations are GC-safe and always return */

/* standard primitive operations */
#define sxWriteChar(ch, p) (getvp(p)->v_putc(getdp(p), ch))
#define sxWriteString(str, p) (getvp(p)->v_puts(getdp(p), str))
#define sxNewline(p) (getvp(p)->v_putc(getdp(p), T('\n')))

/* sxFlushOutput tries to deliver buffered output */
#define sxFlushOutput(p) (getvp(p)->v_flush(getdp(p)))

/* sxClearOutput attempts to clear buffered output for urgent messages */
#define sxClearOutput(p) (getvp(p)->v_clearo(getdp(p)))

/* self-display for ports */
#define sxPortSelfDisplay(p, oport)(getvp(p)->v_print(getdp(p), oport))

/* sxDisplay and sxWrite */
extern void sxDisplay(SOBJ obj, SOBJ port);
extern void sxWrite(SOBJ obj, SOBJ port);

/* sxFormat returns 0=OK, 1=underflow, 2=bad fstr, 3=overflow */
extern int sxFormat(const tchar_t* fstr, SOBJ port, size_t n, SOBJ* args);

/* binary output */
#define sxWriteBytes(buf, nbytes, p) (getvp(p)->v_write(getdp(p), buf, nbytes))
extern void sxWriteByte(byte_t b, SOBJ port);
extern void sxWriteWord(short s, SOBJ port);
extern void sxWriteDWord(long s, SOBJ port);

/* numerical unparsers */
extern tchar_t* sxUnparseFixnum(FIXTYPE num, int radix);
extern tchar_t* sxUnparseFlonum(FLOTYPE num);

#endif /* ndef __SXWRITE_H */
