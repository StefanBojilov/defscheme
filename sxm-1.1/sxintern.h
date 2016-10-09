/* sxintern.h - symbol/keyword table functions */

#ifndef __SXINTERN_H
#define __SXINTERN_H

#include "sxm.h"

/* NB: sxSymbolTableIntern works with static C strings only! */
extern SOBJ sxSymbolTableIntern(const tchar_t *name, SOBJ table, vmtag_t ntype);
extern SOBJ sxSymbolTableLookup(const tchar_t *name, SOBJ table);
extern void sxCompactSymbolTable(SOBJ table);

/* traditional shortcuts (sxInternString works with static strings!) */
extern SOBJ sxInternString(const tchar_t *name, vmtag_t ntype);
extern SOBJ sxIntern(SOBJ src, vmtag_t ntype);
extern bool_t sxIsInterned(SOBJ src, vmtag_t ntype);

/* compatibility macros for static C strings */
#define sxSymEnter(name) sxInternString((name), NT_SYMBOL)
#define sxKeyEnter(name) sxInternString((name), NT_KEYWORD)

#endif /* ndef __SXINTERN_H */

