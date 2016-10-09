/* symbols.c - standard procedures 6.4 */

#include "sxm.h"
#include "sxintern.h"
#include "define.h"

/*#| (symbol? obj) |#*/
DEFINE_INITIAL_BINDING("symbol?", sp_symbolp)
DEFINE_PROCEDURE(sp_symbolp)
{
  SOBJ arg = xlonearg();
  return cvbool(symbolp(arg));
}

/*#| (uninterned-symbol? obj) |#*/
DEFINE_INITIAL_BINDING("uninterned-symbol?", sp_unintsymbolp)
DEFINE_PROCEDURE(sp_unintsymbolp)
{
  SOBJ arg = xlonearg();
  if (!symbolp(arg)) return so_false;
  return cvbool(!sxIsInterned(arg, NT_SYMBOL));
}

/*#| (keyword? obj) |#*/
DEFINE_INITIAL_BINDING("keyword?", sp_keywordp)
DEFINE_PROCEDURE(sp_keywordp)
{
  SOBJ arg = xlonearg();
  return cvbool(keywordp(arg));
}


/* tostring - convert symbols to strings */
static SOBJ tostring(vmtag_t stype)
{
  SOBJ val;
  ss_size_t size;
  SOBJ src = xlgetarg();
  xllastarg();
  if (!typep(src, stype)) sxae_type(src, stype);
  size = getslength(src);
  gcLock(src);
  val = newstring(size);
  gcUnlock(1);
  tmemcpy(getstring(val), getstring(src), size);
  return val;
}

/*#| (symbol->string symbol) |#*/
DEFINE_INITIAL_BINDING("symbol->string", sp_sym2str)
DEFINE_PROCEDURE(sp_sym2str)
{
  return tostring(NT_SYMBOL);
}

/*#| (keyword->string keyword) |#*/
DEFINE_INITIAL_BINDING("keyword->string", sp_key2str)
DEFINE_PROCEDURE(sp_key2str)
{
  return tostring(NT_KEYWORD);
}


/* tosymbol - convert strings to symbols */
static SOBJ tosymbol(vmtag_t ntype, bool_t fintern)
{
  SOBJ str = xlgastring();
  SOBJ val;
  xllastarg();
  if (fintern) val = sxIntern(str, ntype);
  else val = cvsym(str, ntype);
  return val;
}

/*#| (string->symbol string) |#*/
DEFINE_INITIAL_BINDING("string->symbol", sp_str2sym)
DEFINE_PROCEDURE(sp_str2sym)
{
  return tosymbol(NT_SYMBOL, TRUE);
}

/*#| (string->keyword str) |#*/
DEFINE_INITIAL_BINDING("string->keyword", sp_str2key)
DEFINE_PROCEDURE(sp_str2key)
{
  return tosymbol(NT_KEYWORD, TRUE);
}

/*#| (string->uninterned-symbol string) |#*/
DEFINE_INITIAL_BINDING("string->uninterned-symbol", sp_str2uisym)
DEFINE_PROCEDURE(sp_str2uisym)
{
  return tosymbol(NT_SYMBOL, FALSE);
}


/*#| (gensym-prefix [string]) |#*/
DEFINE_INITIAL_BINDING("gensym-prefix", sp_curgsyprefix)
DEFINE_DATUM_STRING(sd_stdgsyprefix, "g")
DEFINE_VARIABLE_VARINIT(sv_curgsyprefix, sd_stdgsyprefix)
DEFINE_PROCEDURE_VARACCESS(sp_curgsyprefix, sv_curgsyprefix, NT_STRING)

/*#| (gensym-count [posint]) |#*/
DEFINE_INITIAL_BINDING("gensym-count", sp_curgsycount)
DEFINE_VARIABLE_VARINIT(sv_curgsycount, so_fix0)
DEFINE_PROCEDURE_VARACCESS(sp_curgsycount, sv_curgsycount, NT_FIXNUM)

/*#| (gensym) |# => unique uninterned symbol */
DEFINE_INITIAL_BINDING("gensym", sp_gensym)
DEFINE_PROCEDURE(sp_gensym)
{
  tchar_t buf[SX_MAXSYMBOL+1];
  FIXTYPE gsycount = getfixnum(sv_curgsycount);
  tchar_t* gsyprefix = getstring(sv_curgsyprefix);
  xllastarg();

  /* make sure prefix is not too long */
  if (tcslen(gsyprefix) + 20 >= SX_MAXSYMBOL) gsyprefix = T("?");

  /* create the pname of the new symbol */
  stprintf(buf, T("%s%lu"), gsyprefix, (unsigned long)gsycount);

  /* increment the counter */
  sv_curgsycount = cvfixnum(gsycount+1);

  /* make an uninterned symbol with this print name */
  return mksym(buf, NT_SYMBOL);
}

