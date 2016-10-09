/* sxread.c - input routines */

#include "sxm.h"
#include "sxmathread.h"
#include "sxintern.h"
#include "sxlocale.h"
#include "sxhtab.h"
#include "sxread.h"
#include "define.h"
#include "extern.h"

/* read case sensitivity default (false) */
EXTERN_VARIABLE(sv_curcasesensv)
/* hash-table mapping names to registered rtds */
EXTERN_DATUM(sv_recreaders_ht)


int sxReadByte(SOBJ port)
{
  byte_t b;
  if (getvp(port)->v_read(getdp(port), &b, 1) == 1) return (int)b;
  else return EOF;
}

int sxReadWord(SOBJ port)
{
  byte_t b1 = sxReadByte(port);
  byte_t b2 = sxReadByte(port);
  unsigned short us = (b1 | (b2 << 8));
  return (int)us;
}

long sxReadDWord(SOBJ port)
{
  byte_t b1 = sxReadByte(port);
  byte_t b2 = sxReadByte(port);
  byte_t b3 = sxReadByte(port);
  byte_t b4 = sxReadByte(port);
  unsigned long ul = (b1 | (b2 << 8) | (b3 << 16) | (b4 << 24));
  return (long)ul;
}

/**************************************/
/* reader errors */

#define ID2LONG   T("id2long")
#define MDL       T("mdl")
#define ZIDSYN    T("zidsyn")
#define NUM2LONG  T("num2long")
#define INUMSYN   T("inumsyn")
#define ZNUMSYN   T("znumsyn")
#define IUVEC     T("iuvec")
#define UEOF      T("ueof")
#define ISCSPEC   T("iscspec")
#define CCOVER    T("ccover")
#define CN2LONG   T("cn2long")
#define UCNAME    T("ucname")
#define UNKNCONST T("unknconst")
#define UNXPDOT   T("unxpdot")
#define IU8INT    T("iu8int")
#define BADRPAR   T("badrpar")
#define MCPAR     T("mcpar")
#define UNXPRPAR  T("unxprpar")
#define ICH       T("ich")
#define UEOFCH    T("ueofch")
#define IHCH      T("ihch")
#define MRECNAM   T("mrecnam")
#define RECNAMNR  T("recnamnr")
#define MRECTER   T("mrecter")

/* read error condition */
DEFINE_DATUM_INIT(sv_read_cond)
{
  SOBJ cond;
  SOBJ tag = sxKeyEnter(T("read-error"));
  gcLock(tag);
  cond = newvector(4, so_false);
  setelement(cond, 0, tag);
  gcUnlock(1);
  return cond;
}

static void clearreaderror(void)
{
  setelement(sv_read_cond, 1, so_false);
  setelement(sv_read_cond, 2, so_false);
  setelement(sv_read_cond, 3, so_false);
}

static bool_t setreaderror(SOBJ port, const tchar_t *msg)
{
  setelement(sv_read_cond, 2, port);
  setelement(sv_read_cond, 3, so_false);
  setelement(sv_read_cond, 1, sxSymEnter(msg));
  return FALSE;
}

static bool_t setreaderrarg(SOBJ port, const tchar_t *msg, SOBJ arg)
{
  setelement(sv_read_cond, 2, port);
  setelement(sv_read_cond, 3, arg);
  setelement(sv_read_cond, 1, sxSymEnter(msg));
  return FALSE;
}

static bool_t setreaderrchar(SOBJ port, const tchar_t *msg, tint_t ch)
{
  setelement(sv_read_cond, 2, port);
  setelement(sv_read_cond, 3, cvchar(ch));
  setelement(sv_read_cond, 1, sxSymEnter(msg));
  return FALSE;
}

SOBJ sxGetLastReadError(void)
{
  return sv_read_cond;
}

bool_t sxReadErrorDetails(SOBJ rerr, SOBJ* pmsg, SOBJ* pport, SOBJ* parg)
{
  if (rerr != sv_read_cond) return FALSE;
  if (pmsg) *pmsg = getelement(sv_read_cond, 1);
  if (pport) *pport = getelement(sv_read_cond, 2);
  if (parg) *parg = getelement(sv_read_cond, 3);
  return TRUE;
}


/* isradixdigit - check to see if a character is a digit in a radix */
static bool_t isradixdigit(tint_t ch, int radix)
{
  switch (radix) {
    case 2:  return (ch >= T('0') && ch <= T('1'));
    case 8:  return (ch >= T('0') && ch <= T('7'));
    case 10: return (ch >= T('0') && ch <= T('9'));
    case 16: return ((ch >= T('0') && ch <= T('9')) ||
                     (ch >= T('A') && ch <= T('F')));
  }
  return FALSE;
}

/* getdigit - convert an ascii code to a digit */
static int getdigit(tint_t ch)
{
  return (ch <= T('9'))
           ? (int)(ch - T('0')) 
           : (int)(ch - T('A') + 10);
}

/* strconcat - concatenate two strings */
static SOBJ strconcat(SOBJ str1, SOBJ str2)
{
  ss_size_t len = (int)getslength(str1) - 1 + (int)getslength(str2) - 1;
  SOBJ str = newstring(len + 1);
  tchar_t *cp = getstring(str);

  /* combine the strings */
  len = (int)getslength(str1) - 1;
  tmemcpy(cp, getstring(str1), len);
  cp += len;
  len = (int)getslength(str2) - 1;
  tmemcpy(cp, getstring(str2), len);
  cp += len;
  *cp = T('\0');

  /* return the new string */
  return (str);
}


/***************************
 * Part 0. Char Utils
 ***************************/

#define whitespacep(ch) istspace(ch)
#define alphabeticp(ch) istalpha(ch)
#define digitp(ch) istdigit(ch)

static bool_t delimiterp(tint_t ch)
{
  return ch == TEOF
      || whitespacep(ch)
      || tcschr(T("()\";[]{}"), ch) != NULL;
}

/* not static: used in sxwrite.c */
bool_t symbol_leader_p(tint_t ch)
{
  return alphabeticp(ch)
      || tcschr(T("!$%&*/:<=>?^_~"), ch) != NULL;
}

/* not static: used in sxwrite.c */
bool_t symbol_constituent_p(tint_t ch)
{
  return symbol_leader_p(ch)
      || digitp(ch)
      || tcschr(T("+-.@"), ch) != NULL;
}

static bool_t number_leader_p(tint_t ch) /* not for prefixed numbers */
{
  return digitp(ch)
      || tcschr(T("+-."), ch) != NULL;
}

static bool_t prefixed_number_constituent_p(tint_t ch)
{
  return digitp(ch)
      || alphabeticp(ch) /* at least ABCDEFISDLOX (more for POTNUMs) */
      || tcschr(T("+-./@#"), ch) != NULL;
}

static bool_t char_name_leader_p(tint_t ch)
{
  return alphabeticp(ch)
      || tcschr(T("!$%&*/:<=>?^_~"), ch) != NULL;
}

static bool_t char_name_constituent_p(tint_t ch)
{
  return char_name_leader_p(ch)
      || digitp(ch)
      || tcschr(T("+-.@"), ch) != NULL;
}

static bool_t collect_symbol(SOBJ port, bool_t s2lc, tchar_t* buf, int maxcnt)
{
  bool_t ok = TRUE;
  tint_t ch; int i;
  assert(maxcnt > 1);
  for (i = 0;;) {
    ch = sxReadChar(port);
    if (ch == T('\\')) { /* take next char as-is */
      ch = sxReadChar(port);
      if (ch == TEOF) setreaderror(port, UEOF);
      if (i < maxcnt) buf[i++] = ch;
      else ok = setreaderror(port, ID2LONG);
      continue; 
    }
    if (ch == TEOF || !symbol_constituent_p(ch)) break;
    if (i < maxcnt) {
      if (s2lc && istupper(ch)) ch = totlower(ch);
      buf[i++] = ch;
    } else ok = setreaderror(port, ID2LONG);
  }
  buf[i] = T('\0');
  if (ok && !delimiterp(ch)) ok = setreaderror(port, MDL);
  if (ok && i == 0) ok = setreaderror(port, ZIDSYN);
  sxUnreadChar(ch, port); /* unread break char */
  return ok;
}

static bool_t collect_number(SOBJ port, bool_t s2lc, tchar_t* buf, int maxcnt)
{
  bool_t ok = TRUE;
  tint_t ch; int i;
  assert(maxcnt > 1);
  for (i = 0;;) {
    ch = sxReadChar(port);
    if (ch == TEOF || !prefixed_number_constituent_p(ch)) break;
    if (i < maxcnt) {
      buf[i++] = (s2lc && istupper(ch)) ? totlower(ch) : ch;
    } else ok = setreaderror(port, NUM2LONG); 
  }
  buf[i] = T('\0');
  if (ok && !delimiterp(ch)) ok = setreaderror(port, MDL);
  if (ok && i == 0) ok = setreaderror(port, ZNUMSYN);
  sxUnreadChar(ch, port); /* unread break char */
  return ok;
}

static bool_t check_delimiter(SOBJ port)
{
  tint_t nextch = sxReadChar(port);
  if (delimiterp(nextch)) {
    sxUnreadChar(nextch, port);
    return TRUE;
  }
  /* missing delimiter */
  return setreaderror(port, MDL);
}

/***************************
 * Part 1. Lexer
 ***************************/

/* token types */
typedef enum token_tag {
  /* specials */
  TT_EOF,
  TT_ATMOSPHERE,
  /* punctuation */
  TT_OPENLIST,    /* ( */
  TT_OPENVEC,     /* #( */
  TT_OPENU8VEC,   /* #u8( */
  TT_OPENVEC2,    /* #[ */
  TT_CLOSE,       /* ) */
  TT_QUOTE,       /* ' */
  TT_QQUOTE,      /* ` */
  TT_UNQUOTE,     /* , */
  TT_UNQSPL,      /* ,@ */
  TT_DOT,         /* . */
  TT_OPENLIST2,   /* [ */
  TT_CLOSE2,      /* ] */
  TT_OPENLIST3,   /* { */
  TT_CLOSE3,      /* } */
  TT_BOX,         /* #& */
  TT_SYNTAX,      /* #' */
  TT_PRIM,        /* #% */
  /* atomic object (has asssociated SOBJ) */
  TT_ATOM         /* id, boolean, number, char, string, nconst */ 
} token_t;

/* stream structure */
typedef struct istream_s {
  SOBJ port;
  bool_t symbols2lc;
  token_t nexttoken;
  SOBJ nexttokenobj;
  bool_t nexttokenempty;
} istream_t;

/* forward decls */
static bool_t lex_string(istream_t *ps, token_t* ptk, SOBJ* pobj);
static bool_t lex_symbol(istream_t *ps, token_t* ptk, SOBJ* pobj);
static bool_t lex_number(istream_t *ps, token_t* ptk, SOBJ* pobj, int radix, int e0i);
static bool_t lex_character(istream_t *ps, token_t* ptk, SOBJ* pobj);
static bool_t lex_named_constant(istream_t *ps, token_t* ptk, SOBJ* pobj);

/* main internal token parser */
static bool_t lex(istream_t *ps, token_t* ptk, SOBJ* pobj)
{
  /** syntax parser **/
  tint_t ch = sxReadChar(ps->port);
  /* individual chars */
  switch (ch) {
    case TEOF:    *ptk = TT_EOF; return TRUE;
    case T('('):  *ptk = TT_OPENLIST; return TRUE;
    case T(')'):  *ptk = TT_CLOSE; return TRUE;
    case T('\''): *ptk = TT_QUOTE; return TRUE;
    case T('`'):  *ptk = TT_QQUOTE; return TRUE;
    case T(','):  {
      ch = sxReadChar(ps->port);
      if (ch != T('@')) {
        sxUnreadChar(ch, ps->port);
        *ptk = TT_UNQUOTE;
      } else
        *ptk = TT_UNQSPL;
      return TRUE;
    }
    case T(';'): {
      do { ch = sxReadChar(ps->port); } 
        while (ch != T('\n') && ch != TEOF);
      *ptk = TT_ATMOSPHERE; 
      return TRUE;
    }
    case T('#'):  goto parse_hash_syntax;
    case T('['):  *ptk = TT_OPENLIST2; return TRUE;
    case T(']'):  *ptk = TT_CLOSE2; return TRUE;
    case T('{'):  *ptk = TT_OPENLIST3; return TRUE;
    case T('}'):  *ptk = TT_CLOSE3; return TRUE;
    case T('"'):  return lex_string(ps, ptk, pobj);
  }
  /* char groups */
  if (whitespacep(ch)) {
    do { ch = sxReadChar(ps->port); } 
    while (ch != TEOF && whitespacep(ch));
    sxUnreadChar(ch, ps->port);
    *ptk = TT_ATMOSPHERE; 
    return TRUE;
  }
  if (ch == T('\\') || symbol_leader_p(ch)) {
    sxUnreadChar(ch, ps->port);
    return lex_symbol(ps, ptk, pobj);
  }
  if (number_leader_p(ch)) {
    sxUnreadChar(ch, ps->port);
    /* this returns numbers as well as dot and peculiar symbols */
    return lex_number(ps, ptk, pobj, 0, 0);
  }
  /* illegal char */
  return setreaderrchar(ps->port, ICH, ch);

parse_hash_syntax:
  /** # syntax parser **/
  ch = sxReadChar(ps->port);
  if (ps->symbols2lc && ch != TEOF) ch = totlower(ch);
  /* dispatch chars */
  switch (ch) {
    case T('('): *ptk = TT_OPENVEC; return TRUE; 
    case T('['): *ptk = TT_OPENVEC2; return TRUE; 
    case T('&'): *ptk = TT_BOX; return TRUE; 
    case T('\''): *ptk = TT_SYNTAX; return TRUE; 
    case T('%'): *ptk = TT_PRIM; return TRUE; 

    case T('!'): return lex_named_constant(ps, ptk, pobj);
    case T('|'): {
      int level = 1;
      fsa_normal: 
      switch (sxReadChar(ps->port)) {
        case TEOF: goto fsa_eof;
        case T('#'): goto fsa_after_hash; 
        case T('|'): goto fsa_after_bar;
        default: goto fsa_normal; 
      }
      fsa_after_hash: 
      switch (sxReadChar(ps->port)) {
        case TEOF: goto fsa_eof;
        case T('#'): goto fsa_after_hash; 
        case T('|'): level++;
        default: goto fsa_normal; 
      }
      fsa_after_bar: 
      switch (sxReadChar(ps->port)) {
        case TEOF: goto fsa_eof;
        case T('|'): goto fsa_after_bar;
        case T('#'): if (!--level) { *ptk = TT_ATMOSPHERE; return TRUE; }
        default: goto fsa_normal; 
      }
      fsa_eof:
      return setreaderrchar(ps->port, UEOFCH, ch);
    }

    case T('\\'): return lex_character(ps, ptk, pobj);

    case T('e'): return lex_number(ps, ptk, pobj, 0, -1);
    case T('i'): return lex_number(ps, ptk, pobj, 0, 1); 
    case T('b'): return lex_number(ps, ptk, pobj, 2, 0);
    case T('o'): return lex_number(ps, ptk, pobj, 8, 0);
    case T('d'): return lex_number(ps, ptk, pobj, 10, 0);
    case T('x'): return lex_number(ps, ptk, pobj, 16, 0);

    /* we only support u8vectors from SRFI-4 */
    case T('u'): {
      if (sxReadChar(ps->port) != T('8') ||
          sxReadChar(ps->port) != T('(')) 
        return setreaderror(ps->port, IUVEC);
      *ptk = TT_OPENU8VEC; return TRUE; 
    }

    case T('t'): 
    case T('f'): {
      if (!check_delimiter(ps->port)) return FALSE;
      *ptk = TT_ATOM; *pobj = cvbool(ch == T('t')); 
      return TRUE;
    }
  }
  /* illegal hash-char */
  return setreaderrchar(ps->port, IHCH, ch);
}

/*** token parser helpers ***/

/* helper proc for lex_character and lex_string */
static bool_t lex_character_spec(istream_t *ps, tint_t *pch, bool_t instring)
{
  tint_t ch = sxReadChar(ps->port);
  if (ch == TEOF) return setreaderror(ps->port, UEOF);
  if (instring) {
    switch (ch) {
      case T('\\'): break;
      case T('"'):  break;
      case T('f'):  ch = T('\014'); break;
      case T('z'):  ch = T('\032'); break;
      case T('e'):  ch = T('\033'); break;
      case T('n'):  ch = T('\n'); break;
      case T('r'):  ch = T('\r'); break;
      case T('t'):  ch = T('\t'); break;
      case T('c'):  {
        unsigned long c = 0;
        for (;;) {
          int d;
          ch = sxReadChar(ps->port);
          if (ch == TEOF) return setreaderror(ps->port, UEOF);
          if (ch == ';') break;
          ch = totupper(ch);
          if (!isradixdigit(ch, 16))
            return setreaderror(ps->port, ISCSPEC);
          d = getdigit(ch);
          c = (c << 4) | (d & 0xF);
        }
        if ((c & TCHAR_UMASK) != c) 
          return setreaderror(ps->port, ISCSPEC);
        ch = (tint_t)c;
      } break;
      default: 
        return setreaderror(ps->port, ISCSPEC);
    }
    *pch = ch;
    return TRUE;
  } else { /* in char */
    if (char_name_leader_p(ch)) { /* may start character name */
      tchar_t buf[SX_MAXSYMBOL+1]; int i; 
      tint_t firstch = ch;
      for (i = 0;;) { /* try to collect char name */
        if (i < SX_MAXSYMBOL) {
          buf[i++] = (ps->symbols2lc && istupper(ch)) ? totlower(ch) : ch;
        } else return setreaderror(ps->port, CN2LONG);
        ch = sxReadChar(ps->port);
        if (ch == TEOF || !char_name_constituent_p(ch)) break;
      }
      buf[i] = T('\0'); /* we got at least one char in buf */
      /* #\char should be terminated by real delimiter */
      if (!delimiterp(ch)) return setreaderror(ps->port, MDL);
      sxUnreadChar(ch, ps->port);
      if (i == 1) { /* not a name! */
        *pch = firstch; /* firstch was not converted to lc */
        return TRUE;
      }
      /* here we have char name in buf */
      /* ToDo: ******* use real table here! ******* */
           if (!tcscmp(buf, T("space")))   *pch = T(' ');
      else if (!tcscmp(buf, T("tab")))     *pch = T('\t');
      else if (!tcscmp(buf, T("newline"))) *pch = T('\n');
      else if (!tcscmp(buf, T("return")))  *pch = T('\r');
      else if (!tcscmp(buf, T("page")))    *pch = T('\f');
      else if (buf[0] == T('c')) {
        unsigned long c = 0;
        for (i=1; buf[i] != 0; ++i) {
          int d; ch = totupper(buf[i]);
          if (!isradixdigit(ch, 16))
            return setreaderror(ps->port, UCNAME);
          d = getdigit(ch);
          c = (c << 4) | (d & 0xF);
        }
        if ((c & TCHAR_UMASK) != c) 
          return setreaderror(ps->port, UCNAME);
        *pch = (tint_t)c;
      }
      return TRUE;
    } else { /* ch cannot start char name */
      *pch = ch;
      return TRUE;
    }
  }
  never_returns(FALSE);
}

static bool_t lex_character(istream_t *ps, token_t* ptk, SOBJ* pobj)
{
  tint_t ch;
  if (!lex_character_spec(ps, &ch, /*instring=*/FALSE)) return FALSE;
  *ptk = TT_ATOM;
  *pobj = cvchar(ch);
  return TRUE;
} 

static bool_t lex_string(istream_t *ps, token_t* ptk, SOBJ* pobj)
{
  tchar_t buf[STRMAX+1];
  int i; tint_t ch;
  SOBJ str0, str;
  /* use stack top to collect strings */
  cpush(so_nil);
  /* get string characters */
  for (i = 0;;) {
    switch (ch = sxReadChar(ps->port)) {
      case TEOF:
        drop1(); 
        return setreaderror(ps->port, UEOF);
      case T('"'):
        goto finish_string;
      case T('\\'):
        if (!lex_character_spec(ps, &ch, /*instring=*/TRUE)) {
          drop1(); return FALSE;
        }
        /* ch has next char. ; if any is gobbled */
      default: ; /* ch is the char: append it */
    }
    if (i < STRMAX) {
      buf[i++] = (tchar_t)ch;
    } else { /* buffer overflow */
      /* allocate partial string */
      buf[i++] = T('\0');
      str = cvstringn(buf, i);
      /* concatenate if not first */
      if (!nullp(top())) {
        str0 = top();
        cpush(str);
        str = strconcat(str0, str);
        drop(1);
      }
      settop(str);
      /* reset counter */
      i = 0;
      buf[i++] = ch;
    }
  }

finish_string:
  buf[i++] = T('\0');
  str = cvstringn(buf, i);
  /* concatenate if not first */
  if (!nullp(top())) {
    str0 = top();
    cpush(str);
    str = strconcat(str0, str);
    drop(1);
  }
  drop(1);
  /* return a string */
  *ptk = TT_ATOM;
  *pobj = str;
  return TRUE;
}


static bool_t lex_symbol(istream_t *ps, token_t* ptk, SOBJ* pobj)
{
  tchar_t buf[SX_MAXSYMBOL+1];
  if (!collect_symbol(ps->port, ps->symbols2lc, buf, SX_MAXSYMBOL)) return FALSE;
  if (!check_delimiter(ps->port)) return FALSE;
  /* ToDo: keyword: ? */
  *pobj = sxSymEnter(buf);
  *ptk = TT_ATOM;
  return TRUE;
}

static bool_t lex_named_constant(istream_t *ps, token_t* ptk, SOBJ* pobj)
{
  tchar_t buf[SX_MAXSYMBOL+1];
  if (!collect_symbol(ps->port, ps->symbols2lc, buf, SX_MAXSYMBOL)) return FALSE;
  if (!check_delimiter(ps->port)) return FALSE;
  /* buf contains symbol chars. lowercase them if required by ci reader */
  if (ps->symbols2lc) {
    tchar_t *pc = buf; tchar_t ch;
    while ((ch = *pc) != 0) { if (istupper(ch)) *pc = totlower(ch); pc++; }
  }
  *ptk = TT_ATOM;
  if (!tcscmp(buf,T("optional"))) { *pobj = sc_optional; return TRUE; }
  if (!tcscmp(buf,T("rest"))) { *pobj = sc_rest; return TRUE; }
  if (!tcscmp(buf,T("key"))) { *pobj = sc_key; return TRUE; }
  if (!tcscmp(buf,T("aux"))) { *pobj = sc_aux; return TRUE; }
  if (!tcscmp(buf,T("current-value"))) { *pobj = sc_ac; return TRUE; }
  if (!tcscmp(buf,T("void"))) { *pobj = sc_void; return TRUE; }
  if (!tcscmp(buf,T("default"))) { *pobj = sc_default; return TRUE; }
  if (!tcscmp(buf,T("nonspecified"))) { *pobj = sc_nonspec; return TRUE; }
  if (!tcscmp(buf,T("too-many"))) { *pobj = sc_tmany; return TRUE; }
  if (!tcscmp(buf,T("too-few"))) { *pobj = sc_tfew; return TRUE; }
  return setreaderror(ps->port, UNKNCONST);
}

/* special symbols */
DEFINE_DATUM_SYMBOL(sd_sellipsis, "...")
DEFINE_DATUM_SYMBOL(sd_splus,  "+")
DEFINE_DATUM_SYMBOL(sd_sminus,  "-")
DEFINE_DATUM_SYMBOL(sd_soneplus,  "1+")
DEFINE_DATUM_SYMBOL(sd_sminusoneplus,  "-1+")

/* not static: used in sxwrite.c */
bool_t peculiar_symbol_p(const tchar_t* name)
{
  /* note: tcscmp works here because these things are ci */
  if (!tcscmp(name, T("..."))) return TRUE;
  if (!tcscmp(name, T("+")))   return TRUE;
  if (!tcscmp(name, T("-")))   return TRUE;
  if (!tcscmp(name, T("1+")))  return TRUE;
  if (!tcscmp(name, T("-1+"))) return TRUE;
  return FALSE;
}

static bool_t lex_number(istream_t *ps, token_t* ptk, SOBJ* pobj, int radix, int e0i)
{
  /* at this point one prefix might already been parsed */
  tchar_t buf[101]; /* adhoc max.possible length of a number */
  if (!collect_number(ps->port, ps->symbols2lc, buf, 100)) return FALSE;
  if (!check_delimiter(ps->port)) return FALSE;
  /* if not stated otherwise, the return value would be an atom */
  *ptk = TT_ATOM;
  if (!e0i && !radix) { /* no explicit prefixes yet */
    /* first, check for special stuff having pseudo-numerical syntax */
    /* note: tcscmp works here because these things are ci
       and because collect-number converts to ci when required */
    if (!tcscmp(buf, T("."))) { *ptk = TT_DOT; return TRUE; }
    if (!tcscmp(buf, T("..."))) { *pobj = sd_sellipsis; return TRUE; }
    if (!tcscmp(buf, T("+"))) { *pobj = sd_splus; return TRUE; }
    if (!tcscmp(buf, T("-"))) { *pobj = sd_sminus; return TRUE; }
    if (!tcscmp(buf, T("1+"))) { *pobj = sd_soneplus; return TRUE; }
    if (!tcscmp(buf, T("-1+"))) { *pobj = sd_sminusoneplus; return TRUE; }
  }
  { /* parse (still possibly prefixed) number and apply resulting prefixes */
    number_t num;
    const tchar_t *str = buf;
    /* parse number in given radix */
    if (!numParseNumber(&str, &num, radix, e0i)) 
      return setreaderror(ps->port, INUMSYN);
    /* check that there's nothing left in buf */
    if (*str != 0) /* parsing was not completed */
      return setreaderror(ps->port, INUMSYN);
    /* make and return SOBJ */
    *pobj = numMakeSOBJ(&num);
    return TRUE;
  }
}

/* reader interface to lexer - read */
static bool_t read_token(istream_t *ps, token_t* ptk, SOBJ* pobj)
{
  /* return pushback token if any */
  if (!ps->nexttokenempty) {
    *ptk = ps->nexttoken;
    *pobj = ps->nexttokenobj;
    ps->nexttokenempty = TRUE;
    return TRUE;
  }
  /* read next token automatically skipping intertoken atmosphere */
  for (;;) {
    if (!lex(ps, ptk, pobj)) return FALSE;
    if (*ptk != TT_ATMOSPHERE) return TRUE;
  }
}

/* reader interface to lexer - unread (once) */
static void unread_token(istream_t *ps, token_t tk, SOBJ obj)
{
  assert(ps->nexttokenempty);
  ps->nexttoken = tk;
  ps->nexttokenobj = obj;
  ps->nexttokenempty = FALSE;
}


/***************************
 * Part 2. Reader
 ***************************/

DEFINE_DATUM_SYMBOL(sd_squote,   "quote")
DEFINE_DATUM_SYMBOL(sd_sqquote,  "quasiquote")
DEFINE_DATUM_SYMBOL(sd_sunquote, "unquote")
DEFINE_DATUM_SYMBOL(sd_sunqspl,  "unquote-splicing")
DEFINE_DATUM_SYMBOL(sd_ssyntax,  "syntax")
DEFINE_DATUM_SYMBOL(sd_sprim,    "#primitive")

static bool_t parse_datum(istream_t *ps, SOBJ* pobj, bool_t reteof)
{
  token_t tk; bool_t u8;

  if (!read_token(ps, &tk, pobj)) return FALSE;
  switch (tk) {
    /* eof: return only if allowed by reteof flag */
    case TT_EOF:
      if (!reteof) return setreaderror(ps->port, UEOF);
      *pobj = so_eof; return TRUE;
    /* punctuation */
    case TT_BOX:         /* #& */
      if (!parse_datum(ps, pobj, FALSE)) return FALSE;
      *pobj = cvbox(*pobj, so_nil);
      return TRUE;

    case TT_OPENVEC:     /* #( */
      tk = TT_CLOSE; u8 = FALSE; goto p_vector;

    case TT_OPENU8VEC:  /* #u8( */
      tk = TT_CLOSE; u8 = TRUE; goto p_vector;

    /* vector reader: expects terminator paren in tk */
    p_vector: {
      sv_size_t i;
      sv_size_t len = 0;
      SOBJ last = so_nil;
      *pobj = so_nil;
      cpush(*pobj); /* gc-protect while reading */
      /* first, collect elements into a list and count them */
      for (;;) { /* loop with top of stack holding current *pobj */
        token_t nexttk;
        SOBJ x = so_nil; /* bounds checker likes it initialized */
        if (!read_token(ps, &nexttk, &x)) { drop1(); return FALSE; }
        /* here we have top of stack holding *pobj */
        switch (nexttk) {
          /* check for closing parens */
          case TT_CLOSE: case TT_CLOSE2: case TT_CLOSE3:
            if (tk != nexttk) { 
              drop1();
              return setreaderror(ps->port, UNXPDOT);
            }
            /* the list is in *pobj, len contains its length */
            if (u8) {
              /* convert list to byte vector */
              x = newbytevec(len);
              drop1(); /* no need to protect *pobj any longer */
              for (last = *pobj, i = 0; i < len; ++i, last = cdr(last)) {
                getbytes(x)[i] = (byte_t)getsfixnum(car(last));
              }
            } else {
              /* convert list to vector */
              x = newvector(len, NV_NO_INIT);
              drop1(); /* no need to protect *pobj any longer */
              for (last = *pobj, i = 0; i < len; ++i, last = cdr(last)) {
                setelement(x, i, car(last));
              }
            }
            *pobj = x;
            return TRUE;
          /* read the next element while tracking *pobj protected on top() */
          default: { /* nexttk starts list element? */
            /* unread_token doesn't protect x from gc ... */
            unread_token(ps, nexttk, x);
            /* ... but if it was there, we'll take it back right away */
            if (!parse_datum(ps, &x, FALSE)) { drop1(); return FALSE; }
            /* x is a list element: check, tconc and continue */
            if (u8 && !bytep(x)) {
              drop(1); return setreaderror(ps->port, IU8INT);
            }
            x = cons(x, so_nil); /* cons gc-protects its arguments */
            if (!nullp(last)) setcdr(last, x); 
            else { *pobj = x; settop(x); } /* protect first cons */
            last = x;
            ++len;
          }
        }
      }
      assert(0); /* shoudn't come here */
    } /* end of vector reader */

    case TT_OPENVEC2:    /* #[ */
    { /* record reader */
      token_t nexttk; SOBJ rtd; size_t rlen, i;
      SOBJ x = so_nil; /* initialize: bounds checker likes it this way */
      if (!read_token(ps, &nexttk, &x)) return FALSE;
      if (nexttk != TT_ATOM || !stringp(x))
        return setreaderror(ps->port, MRECNAM);
      /* get rtd from a table */
      rtd = sxHashTableGet(sv_recreaders_ht, x, NULL);
      if (!recordp(rtd) || getrsize(rtd) != 3) 
        return setreaderrarg(ps->port, RECNAMNR, x);
      rlen = length(getrelement(rtd, 2)) + 1;
      cpush(rtd); /* gc-protect while making a record */
      *pobj = newrecord((sv_size_t)rlen);
      setrelement(*pobj, 0, rtd);
      settop(*pobj); /* now protect the record itself */
      for (i = 1; i < rlen; ++i) { /* read rlen-1 elements */
        if (!parse_datum(ps, &x, FALSE)) { drop1(); return FALSE; }
        setrelement(*pobj, i, x);
      }
      /* expect closing bracket */
      if (!read_token(ps, &nexttk, &x)) { drop1(); return FALSE; }
      if (nexttk != TT_CLOSE2) { drop1(); return setreaderror(ps->port, MRECTER); }
      /* finish reading: record in *pobj already */
      drop1(); /* no need to protect *pobj any longer */
      return TRUE;
    } /* end of record reader */

    case TT_OPENLIST:    /* ( */
      tk = TT_CLOSE; goto p_list;
    case TT_OPENLIST2:   /* [ */
      tk = TT_CLOSE2; goto p_list;
    case TT_OPENLIST3:   /* { */
      tk = TT_CLOSE3; goto p_list;

    /* list reader: expects terminator paren in tk */
    p_list: {
      SOBJ last = so_nil;
      *pobj = so_nil;
      cpush(*pobj); /* gc-protect while reading */
      /* read until list is finished */
      for (;;) { /* loop with top of stack holding current *pobj */
        token_t nexttk;
        SOBJ x = so_nil; /* initialize: bounds checker likes it this way */
        if (!read_token(ps, &nexttk, &x)) { drop1(); return FALSE; }
        switch (nexttk) {
          /* check for closing parens */
          case TT_CLOSE: case TT_CLOSE2: case TT_CLOSE3:
            drop1(); /* no need to protect *pobj any longer */
            if (tk != nexttk) return setreaderror(ps->port, BADRPAR);
            /* the list is in *pobj already */
            return TRUE;
          /* dot? */
          case TT_DOT: {       /* . */
            /* finish reading dotted list */
            if (nullp(last)) {
              drop1();
              return setreaderror(ps->port, UNXPDOT);
            }
            /* read the cdr element while keeping *pobj protected on top() */
            if (!parse_datum(ps, &x, FALSE)) { drop1(); return FALSE; }
            /* finish the list */
            setcdr(last, x);
            drop1(); /* no need to protect *pobj any longer */
            /* look for the close paren */
            if (!read_token(ps, &nexttk, &x)) return FALSE;
            if (nexttk != tk) return setreaderror(ps->port, MCPAR);
            /* closing paren is read: the list is not gc-ed and in *pobj */
            return TRUE;
          }
          /* read the next element while tracking *pobj protected on top() */
          default: { /* nexttk starts list element? */
            /* unread_token doesn't protect x from gc ... */
            unread_token(ps, nexttk, x);
            /* ... but if it was there, we'll take it back right away */
            if (!parse_datum(ps, &x, FALSE)) { drop1(); return FALSE; }
            /* x is a list element: tconc and continue */
            x = cons(x, so_nil); /* cons gc-protects its arguments */
            if (!nullp(last)) setcdr(last, x); 
            else { *pobj = x; settop(x); } /* protect first cons */
            last = x;
          }
        }
      }
      assert(0); /* shoudn't come here */
    } /* end of list reader */

    case TT_DOT:         /* . */
      return setreaderror(ps->port, UNXPDOT);

    case TT_CLOSE:       /* ) */
    case TT_CLOSE2:      /* ] */
    case TT_CLOSE3:      /* } */
      return setreaderror(ps->port, UNXPRPAR);

    case TT_QUOTE:       /* ' */
      *pobj = sd_squote; goto p_abbrev;
    case TT_QQUOTE:      /* ` */
      *pobj = sd_sqquote; goto p_abbrev;
    case TT_UNQUOTE:     /* , */
      *pobj = sd_sunquote; goto p_abbrev;
    case TT_UNQSPL:      /* ,@ */
      *pobj = sd_sunqspl; goto p_abbrev;
    case TT_SYNTAX:      /* #' */
      *pobj = sd_ssyntax; goto p_abbrev;
    case TT_PRIM:        /* #% */
      *pobj = sd_sprim; goto p_abbrev;
    p_abbrev: {
      SOBJ x;
      if (!parse_datum(ps, &x, FALSE)) return FALSE;
      *pobj = cons(*pobj, cons(x, so_nil));
      return TRUE;
    }

    /* atomic object (has asssociated SOBJ) */
    case TT_ATOM:        /* id, boolean, number, char, string, nconst */ 
      /* *sobj already has the atom */
      return TRUE;
    case TT_ATMOSPHERE:
      /* should be skipped by read_token above */
      assert(FALSE);
    default:
      /* new token types? */
      assert(FALSE);
  }
  never_returns(FALSE);
}


EXTERN_VARIABLE(sv_curcasesensv)

/* sxRead - read one datum from port */
bool_t sxRead(SOBJ port, SOBJ* pobj)
{
  bool_t ok;
  istream_t is;
  is.port = port;
  is.symbols2lc = falsep(sv_curcasesensv);
  is.nexttoken = TT_EOF;
  is.nexttokenobj = so_nil;
  is.nexttokenempty = TRUE;
  cpush(port);
  *pobj = so_eof;
  clearreaderror();
  ok = parse_datum(&is, pobj, /*reteof=*/TRUE);
  drop1();
  return ok;
}
