/* sxio.h - port routines */

#ifndef __SXIO_H
#define __SXIO_H

#include "sxm.h"

extern void setup_port_tab(void);
extern void install_port_tab(void);

extern void sxPortClose(SOBJ p);
extern void sxPortSave(CSOBJ p, SOBJ stream);
extern void sxPortRestore(SOBJ p, SOBJ stream);
extern short sxPortClassToPcid(PORTVPTR vp);
extern PORTVPTR sxPcidToPortClass(short pcid);
extern SOBJ sxSignalOpenError(const tchar_t *typestr, SOBJ filename);

#endif /* ndef __SXIO_H */
