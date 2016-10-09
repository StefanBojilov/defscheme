/* sxhash.c - hash functions */

#include "sxm.h"

/* sxHashString - hash a string (case-sensitive) */
sv_size_t sxHashString(const tchar_t *str, size_t len, sv_size_t htsize)
{
  sv_size_t z = len & 1 ? str[len - 1] + NT_STRING : NT_STRING;
  for (len /= 2; len; len--, str += 2) {
    z = (z >> 8) | (z << 8);
    z += str[0];
    z += str[1] << 4;
  }
  return z % htsize;
}

/* sxHashStringCi - hash a string (case-insensitive) */
sv_size_t sxHashStringCi(const tchar_t *str, size_t len, sv_size_t htsize)
{
  tchar_t ch = str[len - 1];
  sv_size_t z = len & 1 ? totupper(ch) + NT_STRING : NT_STRING;
  for (len /= 2; len; len--, str += 2) {
    z = (z >> 8) | (z << 8);
    ch = str[0]; z += totupper(ch);
    ch = str[1]; z += totupper(ch) << 4;
  }
  return z % htsize;
}

/* sxHashq - hash an object (based on EQ? test) */
/* NOTE: cannot just hash pointer because hash numbers
 *       should live thru image save/restore */
sv_size_t sxHashq(SOBJ obj, sv_size_t htsize)
{
  int i;

  if (sfixp(obj))
    return (sv_size_t) (getsfixnum(obj) % htsize);

  /* nodep(obj) == TRUE */
  switch ((i = _ntype(obj))) {
    case NT_STRING:
      return sxHashString(getstring(obj), getslength(obj) - 1, htsize);
    case NT_GCELL:
      obj = getgcellname(obj);
      if (!symbolp(obj))
        return NT_GCELL % htsize;
    case NT_SYMBOL:
    case NT_KEYWORD:
      return sxHashStringCi(getpstring(obj), getpslength(obj) - 1, htsize);
    case NT_CHAR:
      return (sv_size_t) (getchcode(obj) % htsize);
    case NT_FROB:
      return getfrobcode(obj) % htsize;
    case NT_BOOLEAN:
      return falsep(obj);
    default:
      break;
  }
  return (i + 69) % htsize;
}

/* sxHashv - hash an object (based on EQV? test) */
sv_size_t sxHashv(SOBJ obj, sv_size_t htsize)
{
  int i;

  if (sfixp(obj))
    return (sv_size_t) (getsfixnum(obj) % htsize);

  /* nodep(obj) == TRUE */
  switch ((i = _ntype(obj))) {
    case NT_STRING:
      return sxHashString(getstring(obj), getslength(obj) - 1, htsize);
    case NT_GCELL:
      obj = getgcellname(obj);
      if (!symbolp(obj))
        return NT_GCELL % htsize;
    case NT_SYMBOL:
    case NT_KEYWORD:
      return sxHashStringCi(getpstring(obj), getpslength(obj) - 1, htsize);
    case NT_CHAR:
      return (sv_size_t) (getchcode(obj) % htsize);
    case NT_FIXNUM:
      return (sv_size_t) (getfixnum(obj) % htsize);
    case NT_FLONUM:
      return (sv_size_t) (((long)getflonum(obj)) % htsize);
    case NT_FROB:
      return getfrobcode(obj) % htsize;
    case NT_BOOLEAN:
      return falsep(obj);
    default:
      break;
  }
  return (i + 69) % htsize;
}

/* sxHash - hash an object (based on EQUAL? test) */
sv_size_t sxHash(SOBJ obj, sv_size_t htsize)
{
  int i;

  if (sfixp(obj))
    return (sv_size_t) (getsfixnum(obj) % htsize);

  /* nodep(obj) == TRUE */
  switch ((i = _ntype(obj))) {
    case NT_STRING:
      return sxHashString(getstring(obj), getslength(obj) - 1, htsize);
    case NT_GCELL:
      obj = getgcellname(obj);
      if (!symbolp(obj))
        return NT_GCELL % htsize;
    case NT_SYMBOL:
    case NT_KEYWORD:
      return sxHashStringCi(getpstring(obj), getpslength(obj) - 1, htsize);
    case NT_CHAR:
      return (sv_size_t) (getchcode(obj) % htsize);
    case NT_FIXNUM:
      return (sv_size_t) (getfixnum(obj) % htsize);
    case NT_FLONUM:
      return (sv_size_t) (((long)getflonum(obj)) % htsize);
    case NT_FROB:
      return getfrobcode(obj) % htsize;
    case NT_BOOLEAN:
      return falsep(obj);
    default:
      break;
  }
  /* ToDo: "improve" equal?-hashing of pairs and vectors
   * (they will fall each into a single bucket for now!!!) */
  return (i + 69) % htsize;
}


/* hash function */
sv_size_t hash(SOBJ obj, sv_size_t htsize)
{
  return sxHash(obj, htsize);
}

/* hashq function */
sv_size_t hashq(SOBJ obj, sv_size_t htsize)
{
  return sxHashq(obj, htsize);
}

/* hashv function */
sv_size_t hashv(SOBJ obj, sv_size_t htsize)
{
  return sxHashv(obj, htsize);
}

/* hash-char function (type-safe) */
sv_size_t hash_char(SOBJ obj, sv_size_t htsize)
{
  int nt = xntype(obj);
  if (nt == NT_CHAR) 
    return (sv_size_t) (getchcode(obj) % htsize);
  return (nt + 69) % htsize;
}

/* hash-char-ci function (type-safe) */
sv_size_t hash_char_ci(SOBJ obj, sv_size_t htsize)
{
  int nt = xntype(obj);
  if (nt == NT_CHAR) 
    return (sv_size_t) (totlower(getchcode(obj)) % htsize);
  return (nt + 69) % htsize;
}

/* hash-string function (type-safe) */
sv_size_t hash_string(SOBJ obj, sv_size_t htsize)
{
  int nt = xntype(obj);
  if (nt == NT_STRING) 
    return sxHashString(getstring(obj), getslength(obj)-1, htsize);
  return (nt + 69) % htsize;
}

/* hash-string-ci function (type-safe) */
sv_size_t hash_string_ci(SOBJ obj, sv_size_t htsize)
{
  int nt = xntype(obj);
  if (nt == NT_STRING) 
    return sxHashStringCi(getstring(obj), getslength(obj)-1, htsize);
  return (nt + 69) % htsize;
}

