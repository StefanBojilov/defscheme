/* sxhtab.h - hash table functions */

#ifndef __SXHTAB_H
#define __SXHTAB_H

#include "sxm.h"

enum { HTT_EQ, HTT_EQV, HTT_EQUAL, HTT_NUMBER, 
       HTT_CHAR, HTT_CHAR_CI, HTT_STRING, HTT_STRING_CI };
extern SOBJ sxMakeHashTable(int htt, sv_size_t hsize, SOBJ def);
extern SOBJ sxHashTableGet(SOBJ ht, SOBJ key, bool_t *pfound/*may be NULL*/);
extern bool_t sxHashTablePut(SOBJ ht, SOBJ key, SOBJ val, bool_t frepl);
extern SOBJ sxHashTableRemove(SOBJ ht, SOBJ key);

#endif /* ndef __SXHTAB_H */
