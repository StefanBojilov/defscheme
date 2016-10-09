/* sxlocale.h - locale and gcell functions */

#ifndef __SXLOCALE_H
#define __SXLOCALE_H

#include "sxm.h"

extern SOBJ sxLocFindEx(SOBJ loc, SOBJ sym, SOBJ* pentry, bool_t* pfpub, bool_t* pfown);
extern SOBJ sxLocFind(SOBJ loc, SOBJ sym, bool_t public_only);
extern SOBJ sxLocEnter(SOBJ loc, SOBJ sym);
extern SOBJ sxCurrentLocEnter (SOBJ sym);
extern bool_t sxLocImport(SOBJ loc, SOBJ sym, SOBJ gcell, bool_t fpublish);

#endif /* ndef __SXLOCALE_H */
