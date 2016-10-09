/* sxdisasm.h - VM disassembler */

#ifndef __SXDISASM_H
#define __SXDISASM_H

#include "sxm.h"

extern ss_size_t sxDisInst(SOBJ fptr, SOBJ code,
                           ss_size_t loc, SOBJ env, int pc);

extern void sxDisProc(SOBJ fptr, SOBJ fun, int pc);

#endif /* ndef __SXDISASM_H */
