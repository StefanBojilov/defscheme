/* sxhash.h - hash functions */

#ifndef __SXHASH_H
#define __SXHASH_H

#include "sxm.h"

extern sv_size_t sxHashString(const tchar_t *str, size_t len, sv_size_t htsize);
extern sv_size_t sxHashStringCi(const tchar_t *str, size_t len, sv_size_t htsize);
extern sv_size_t sxHash(SOBJ obj, sv_size_t htsize);
extern sv_size_t sxHashq(SOBJ obj, sv_size_t htsize);
extern sv_size_t sxHashv(SOBJ obj, sv_size_t htsize);

typedef sv_size_t (*sobjhashfun_t)(SOBJ, sv_size_t);
extern sv_size_t hash(SOBJ obj, sv_size_t htsz);           /* hash function */
extern sv_size_t hashq(SOBJ obj, sv_size_t htsz);          /* hashq function */
extern sv_size_t hashv(SOBJ obj, sv_size_t htsz);          /* hashv function */
extern sv_size_t hash_char(SOBJ obj, sv_size_t htsz);      /* hash-char function (type-safe) */
extern sv_size_t hash_char_ci(SOBJ obj, sv_size_t htsz);   /* hash-char-ci function (type-safe) */
extern sv_size_t hash_string(SOBJ obj, sv_size_t htsz);    /* hash-string function (type-safe) */
extern sv_size_t hash_string_ci(SOBJ obj, sv_size_t htsz); /* hash-string-ci function (type-safe) */

#endif /* ndef __SXHASH_H */
