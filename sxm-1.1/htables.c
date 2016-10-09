/* htables.c - hash table procedures */

#include "sxm.h"
#include "sxhtab.h"
#include "define.h"
#include "extern.h"

/*#| (hash-table? obj) |#*/
DEFINE_INITIAL_BINDING("hash-table?", sp_htabp)
DEFINE_PROCEDURE(sp_htabp)
{
  SOBJ arg = xlonearg();
  return cvbool(hashtablep(arg));
}

/* these are valid compare functions for make-hash-table */
EXTERN_PROCEDURE(sp_eq)
EXTERN_PROCEDURE(sp_eqv)
EXTERN_PROCEDURE(sp_equal)
EXTERN_PROCEDURE(sp_eql)
EXTERN_PROCEDURE(sp_chareql)
EXTERN_PROCEDURE(sp_charieql)
EXTERN_PROCEDURE(sp_streql)
EXTERN_PROCEDURE(sp_strieql)

/*#| (make-hash-table htsize) |# compfun=eq? default=#f */
/*#| (make-hash-table compfun [default [htsize]]) |#*/
DEFINE_INITIAL_BINDING("make-hash-table", sp_makehtab)
DEFINE_PROCEDURE(sp_makehtab)
{
  SOBJ size = xlgetarg();
  if (sfixp(size)) { /* for compatibility with old version */
    xllastarg();
    /* construct a new hash table with compfun=eq? and default=#f */
    return newhashtable((sv_size_t)getsfixnum(size));
  } else { /* new version */
    SOBJ proc = procedurep(size) ? size : xlreqproc(size);
    SOBJ def = optarg() ? xlgetarg() : so_default; /* allow so_default! */
    sv_size_t hsize = optarg() ? (sv_size_t)getsfixnum(xlgasfixnum()) : 17;
    int htt = HTT_EQ; /* initialize to shut up -Wall */
    if (proc == sp_eq) htt = HTT_EQ;
    else if (proc == sp_eqv) htt = HTT_EQV;
    else if (proc == sp_equal) htt = HTT_EQUAL;
    else if (proc == sp_eql) htt = HTT_NUMBER;
    else if (proc == sp_chareql) htt = HTT_CHAR;
    else if (proc == sp_charieql) htt = HTT_CHAR_CI;
    else if (proc == sp_streql) htt = HTT_STRING;
    else if (proc == sp_strieql) htt = HTT_STRING_CI;
    else sxErr(T("unsupported compare function"), proc);
    return sxMakeHashTable(htt, hsize, def);
  }
}

/*#| (hash-table-get ht key) |#*/
DEFINE_INITIAL_BINDING("hash-table-get", sp_htabget)
DEFINE_PROCEDURE(sp_htabget)
{
  /* get the table and property */
  SOBJ ht = xlgahashtable();
  SOBJ key = xlgetarg();
  xllastarg();
  /* retrieve the property value or default */
  return sxHashTableGet(ht, key, NULL);
}

/*#| (hash-table-lookup ht key) |# => val/default, found? */
DEFINE_INITIAL_BINDING("hash-table-lookup", sp_htablkup)
DEFINE_PROCEDURE(sp_htablkup)
{
  /* get the table and property */
  SOBJ ht = xlgahashtable();
  SOBJ key = xlgetarg();
  bool_t ffound; SOBJ val;
  xllastarg();
  /* lookup returns value/default + found? flag */
  val = sxHashTableGet(ht, key, &ffound);
  PUSH_ARG(cvbool(ffound));
  PUSH_ARG(val);
  PROC_RETURN_VALUES(2);
}

/*#| (hash-table-put! ht key obj) |#*/
DEFINE_INITIAL_BINDING("hash-table-put!", sp_htabput)
DEFINE_PROCEDURE(sp_htabput)
{
  /* get the symbol and property */
  SOBJ ht = xlgahashtable();
  SOBJ key = xlgetarg();
  SOBJ obj = xlgetarg();
  xllastarg();
  sxHashTablePut(ht, key, obj, TRUE); /* allow replace */
  return so_void;
}

/*#| (hash-table-remove! ht key) |# => boolean (success) */
DEFINE_INITIAL_BINDING("hash-table-remove!", sp_htabrem)
DEFINE_PROCEDURE(sp_htabrem)
{
  SOBJ ht = xlgahashtable();
  SOBJ key = xlgetarg();
  xllastarg();
  return sxHashTableRemove(ht, key);
}

