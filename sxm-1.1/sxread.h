/* sxread.h - input routines */

#ifndef __SXREAD_H
#define __SXREAD_H

#include "sxm.h"

#define sxReadChar(p) (getvp(p)->v_getc(getdp(p)))
#define sxUnreadChar(ch, p) (getvp(p)->v_ungetc(getdp(p),ch))

/* sxRead returns TRUE and object/eof in *pval or FALSE and sets read error */
extern bool_t sxRead(SOBJ port, SOBJ* pval);
extern SOBJ sxGetLastReadError(void);
extern bool_t sxReadErrorDetails(SOBJ rerr, SOBJ* pmsg, SOBJ* pport, SOBJ* parg);

/* sxClearInput attempts to clear buffered input for urgent messages */
#define sxClearInput(p) (getvp(p)->v_cleari(getdp(p)))

/* sxListen returns TRUE if port won't hang on read */
#define sxListen(p) (getvp(p)->v_listen(getdp(p)))

extern bool_t sxParseNumber(const tchar_t* str, SOBJ* pval, int radix);
extern bool_t sxParseInteger(const tchar_t* str, SOBJ* pval, int radix);

#define sxReadBytes(buf, nbytes, p) (getvp(p)->v_read(getdp(p), buf, nbytes))
extern int sxReadByte(SOBJ port);
extern int sxReadWord(SOBJ port);
extern long sxReadDWord(SOBJ port);

#endif /* ndef __SXREAD_H */
