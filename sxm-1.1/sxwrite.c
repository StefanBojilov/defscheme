/* sxwrite.c - output routines */

#include "sxm.h"
#include "sxwrite.h"
#include "extern.h"

/* meta-rtd object */
EXTERN_DATUM(sd_rtdrtd)

void sxWriteByte(byte_t b, SOBJ port)
{
  getvp(port)->v_write(getdp(port), &b, 1);
}

void sxWriteWord(short s, SOBJ port)
{
  sxWriteByte((byte_t)s, port);
  sxWriteByte((byte_t)(s >> 8), port);
}

void sxWriteDWord(long l, SOBJ port)
{
  sxWriteByte((byte_t)l, port);
  sxWriteByte((byte_t)(l >> 8), port);
  sxWriteByte((byte_t)(l >> 16), port);
  sxWriteByte((byte_t)(l >> 24), port);
}


/* sxWriteQuoted - write a string with quoting if required */
void sxWriteQuoted(const tchar_t *fname, SOBJ port, bool_t at, bool_t colon)
{
  if (fname != NULL) {
    bool_t quote = colon ? (tcschr(fname, T(' ')) != NULL) : TRUE;
    if (quote) sxWriteChar(T('"'), port);
    sxWriteString(fname, port);
    if (quote) sxWriteChar(T('"'), port);
  }
}

/* sxWritePlural - write a plural for a number */
void sxWritePlural(FIXTYPE n, SOBJ port, bool_t at, bool_t colon)
{
  if (at) {
    sxWriteString((n == 1) ? T("y") : T("ies"), port);
  } else {
    if (n != 1) sxWriteChar(T('s'), port);
  }
}

/* sxWriteASCII - write a number as an ASCII string */
void sxWriteASCII(FIXTYPE n, SOBJ port, int cnt, bool_t at)
{
  static byte_t buf[sizeof(FIXTYPE)];
  int i;
  *((FIXTYPE*)buf) = n;
  if (cnt < 0) { /* find a real length */
    for (i = 0; i < (int)sizeof(FIXTYPE); i++)
      if (buf[i] == 0) break;
    cnt = i;
  } else if (cnt > (int)sizeof(FIXTYPE)) {
    cnt = (int)sizeof(FIXTYPE);
  }
  /* output read-back prefix, if required */
  if (at) {
    sxWriteChar(T('#'), port); 
    if (cnt != 1) sxWriteChar((tchar_t)(T('0') + cnt), port); 
    sxWriteChar(T('/'), port);
  }
  /* output cnt chars */
  for (i = 0; i < cnt; i++) sxWriteChar((tchar_t)buf[i], port);
}

/* sxFormat - common formatting function */
int sxFormat(const tchar_t* fstr, SOBJ port, size_t argc, SOBJ* argv)
{
  tchar_t ch; int radix; tchar_t n; SOBJ val;
  bool_t at, colon;
  FIXTYPE numpar;

  /* the last processed arg is stored in val */
  val = so_false;
  
  /* get args one-by-one and write them according to format */
  for (;(ch = *fstr) != T('\0'); fstr++) {
    if (ch != T('~')) {
      sxWriteChar(ch, port);
      continue;
    }
    at = colon = FALSE; numpar = -1;
getnext:
    ch = *++fstr;
    switch (totlower(ch)) {
      case T('@'): at = TRUE; goto getnext;
      case T(':'): colon = TRUE; goto getnext;
      case T('%'): if (numpar < 0) sxNewline(port);
                   else while (numpar-- > 0) sxNewline(port);
                   break;
      case T('~'): sxWriteChar(T('~'), port); break;
      case T('!'): sxFlushOutput(port); break;
      case T('c'): if (!argc || !charp(*argv)) return 1; 
                   val = *argv++; --argc;
                   n = getchcode(val);
                   if (numpar < 0) { 
                     if (at) { 
                       sxWriteChar(T('#'), port); 
                       sxWriteChar(T('\\'), port);
                     }
                     sxWriteChar(n, port);
                   } else 
                     while (numpar-- > 0) sxWriteChar(n, port);
                   break;
      case T('/'): if (!argc || !fixp(*argv)) return 1; 
                   val = *argv++; --argc;
                   sxWriteASCII(getfixnum(val), port, (int)numpar, at); 
                   break;
      case T('_'): if (numpar < 0) sxWriteChar(T(' '), port);
                   else while (numpar-- > 0) sxWriteChar(T(' '), port);
                   break;
      case T('a'): if (!argc) return 1;
                   val = *argv++; --argc;
                   sxDisplay(val, port); break;
      case T('s'): if (!argc) return 1;
                   val = *argv++; --argc;
                   sxWrite(val, port); break;
      case T('q'): if (!argc || !stringp(*argv)) return 1;
                   val = *argv++; --argc;
                   sxWriteQuoted(getstring(val), port, at, colon); break;
      case T('p'): if (!colon) {
                     if (!argc || !fixp(*argv)) return 1; 
                     val = *argv++; --argc;
                   } else if (!fixp(val)) return 1;
                   sxWritePlural(getfixnum(val), port, at, colon); break;
      case T('b'): radix = 2; goto fmtfix;
      case T('o'): radix = 8; goto fmtfix;
      case T('d'): if (!argc || !numberp(*argv)) return 1;
                   val = *argv++; --argc;
                   sxWrite(val, port); break;
      case T('x'): radix = 16;
      fmtfix:      if (!argc || !fixp(*argv)) return 1;
                   val = *argv++; --argc;
                   sxWriteString(sxUnparseFixnum(getfixnum(val), radix), port);
                   break;
      case T('v'): if (!argc || !fixp(*argv)) return 1;
                   val = *argv++; --argc;
                   numpar = getfixnum(val); goto getnext;
      default:     { /* number parameter? */
                     tint_t x = ch - T('0');
                     if (x < 0 || x > 9) return 2;
                     if (numpar < 0) numpar = x; 
                     else { numpar = numpar * 10 + x; }
                     goto getnext;
                   }
    }
  }
  if (argc == 0) return 0;
  return 3;
}



/***************************
 * Printer
 ***************************/

/* stream structure */
typedef struct ostream_s {
  SOBJ port;
  bool_t symbols2lc;
  bool_t display;
  int prlength;
  int prlevel;
} ostream_t;

/* forward decls */
static void print_sym(ostream_t *ps, const tchar_t* name);
static void print_key(ostream_t *ps, const tchar_t* name);
static void print_string(ostream_t *ps, const tchar_t* str, size_t len);
static void print_subr(ostream_t *ps, const tchar_t* tag, SOBJ obj);
static void print_atom(ostream_t *ps, const tchar_t* tag, SOBJ obj);
static void print_fixnum(ostream_t *ps, FIXTYPE i);
static void print_flonum(ostream_t *ps, FLOTYPE f);
static void print_character(ostream_t *ps, tint_t ch);
static void print_hex(ostream_t *ps, tint_t ch);

/* main internal printer */
static void print_datum(ostream_t *ps, SOBJ obj, int level)
{
  int length, size, i;
  SOBJ nptr, next;

  /* check value type */
  switch (xntype(obj)) {
  case NT_BOOLEAN:
    sxWriteString(truep(obj) ? T("#t") : T("#f"), ps->port);
    break;
  case NT_FROB: {
    tchar_t* str = T("#<frob>");
    /* common unique objects */
    if (obj == so_nil)              str = T("()");
    else if (obj == so_default)     str = T("#<default>");
    else if (obj == so_eof)         str = T("#<eof>");
    else if (obj == so_void)        str = T("#<void>");
    else if (obj == so_unbound)     str = T("#<unbound>");
    else if (obj == so_mv_mismatch) str = T("#<...>");
    else if (obj == so_bwp)         str = T("#<bwp>");
    /* primitive continuations */
    else if (obj == sk_endexec) str = T("#<endexec continuation>");
    else if (obj == sk_popregs) str = T("#<popregs continuation>");
    /* named constants */
    else if (obj == sc_optional) str = T("#!optional");
    else if (obj == sc_rest)     str = T("#!rest");
    else if (obj == sc_key)      str = T("#!key");
    else if (obj == sc_aux)      str = T("#!aux");
    else if (obj == sc_ac)       str = T("#!current-value");
    else if (obj == sc_void)     str = T("#!void");
    else if (obj == sc_default)  str = T("#!default");
    else if (obj == sc_nonspec)  str = T("#!nonspecified");
    else if (obj == sc_tmany)    str = T("#!too-many");
    else if (obj == sc_tfew)     str = T("#!too-few");
    sxWriteString(str, ps->port);
  } break;
  case NT_SUBR:
    print_subr(ps, T("Subr"), obj);
    break;
  case NT_CSUBR:
    print_subr(ps, T("CSubr"), obj);
    break;
  case NT_CONS:
    if (ps->prlevel >= 0 && level >= ps->prlevel) {
      sxWriteString(T("(...)"), ps->port);
      break;
    }
    sxWriteChar(T('('), ps->port);
    length = 0;
    for (nptr = obj; !nullp(nptr); nptr = next) {
      if (ps->prlength >= 0 && length++ >= ps->prlength) {
        sxWriteString(T("..."), ps->port);
        break;
      }
      print_datum(ps, car(nptr), level + 1);
      next = cdr(nptr);
      if (!nullp(next)) {
        if (consp(next))
          sxWriteChar(T(' '), ps->port);
        else {
          sxWriteString(T(" . "), ps->port);
          print_datum(ps, next, level + 1);
          break;
        }
      }
    }
    sxWriteChar(T(')'), ps->port);
    break;
  case NT_VECTOR:
    size = getvsize(obj);
    sxWriteString(T("#("), ps->port);
    length = 0;
    for (i = 0; i < size; ++i) {
      if (i != 0) sxWriteChar(T(' '), ps->port);
      if (ps->prlength >= 0 && length++ >= ps->prlength) {
        sxWriteString(T("..."), ps->port);
        break;
      }
      print_datum(ps, getelement(obj, i), level + 1);
    }
    sxWriteChar(T(')'), ps->port);
    break;
  case NT_SYMBOL:
    print_sym(ps, getpstring(obj));
    break;
  case NT_KEYWORD:
    print_key(ps, getpstring(obj));
    break;
  case NT_GCELL:
    if (!ps->display) sxWriteString(T("#^"), ps->port);
    print_datum(ps, getgcellname(obj), level + 1);
    break;
  case NT_BOX:
    sxWriteString(T("#&"), ps->port);
    print_datum(ps, getboxval(obj), level + 1);
    break;
  case NT_HASHTABLE:
    print_atom(ps, T("Hash-table"), obj);
    break;
  case NT_RECORD:
    size = getrsize(obj);
    /* check if this is rtd or meta-rtd */
    if (size == 3 && getrelement(obj, 0) == sd_rtdrtd) { 
      /* rtd is #(rtdrtd name fields) */
      sxWriteString(T("#<record type "), ps->port);
      print_datum(ps, getrelement(obj, 1), level + 1);
      sxWriteString(T(">"), ps->port);
    } else { /* should be a normal record */
      SOBJ rtd = getrelement(obj, 0);
      if (!recordp(rtd) || getrsize(rtd) != 3) 
        print_atom(ps, T("record"), obj); /* broken */
      else { /* write as #[rtd.name field ...] */
        sxWriteString(T("#["), ps->port);
        print_datum(ps, getrelement(rtd, 1), level + 1);
        length = 0;
        for (i = 1; i < size; ++i) {
          if (i != 0) sxWriteChar(T(' '), ps->port);
          if (ps->prlength >= 0 && length++ >= ps->prlength) {
            sxWriteString(T("..."), ps->port);
            break;
          }
          print_datum(ps, getrelement(obj, i), level + 1);
        }
        sxWriteChar(T(']'), ps->port);
      }
    }
    break;
  case NT_PROMISE:
    if (!nullp(getpproc(obj)))
      print_atom(ps, T("Promise"), obj);
    else
      print_atom(ps, T("Forced-promise"), obj);
    break;
  case NT_FIXNUM:
    print_fixnum(ps, getfixnum(obj));
    break;
  case NT_FLONUM:
    print_flonum(ps, getflonum(obj));
    break;
  case NT_CHAR:
    if (ps->display)
      sxWriteChar(getchcode(obj), ps->port);
    else
      print_character(ps, getchcode(obj));
    break;
  case NT_STRING:
    if (ps->display)
      sxWriteString(getstring(obj), ps->port);
    else
      print_string(ps, getstring(obj), getslength(obj)-1);
    break;
  case NT_BYTEVEC:
    size = getbcount(obj);
    sxWriteString(T("#u8("), ps->port);
    for (i = 0; i < size;) {
      print_fixnum(ps, (FIXTYPE)(getbytes(obj)[i++]));
      if (i < size) sxWriteChar(T(' '), ps->port);
    }
    sxWriteChar(T(')'), ps->port);
    break;
  case NT_PORT:
    sxWriteString(T("#<"), ps->port);
    sxPortSelfDisplay(obj, ps->port);
    sxWriteChar(T('>'), ps->port);
    break;
  case NT_CLOSURE:
    print_atom(ps, T("Closure"), obj);
    break;
  case NT_CODE:
    print_atom(ps, T("Code"), obj);
    break;
  case NT_WEAKPAIR:
    if (ps->prlevel >= 0 && level >= ps->prlevel) {
      sxWriteString(T("{...}"), ps->port);
      break;
    }
    sxWriteChar(T('{'), ps->port);
    length = 0;
    for (nptr = obj; !nullp(nptr); nptr = next) {
      if (ps->prlength >= 0 && length++ >= ps->prlength) {
        sxWriteString(T("..."), ps->port);
        break;
      }
      print_datum(ps, car(nptr), level + 1);
      next = cdr(nptr);
      if (!nullp(next)) {
        if (weakpairp(next))
          sxWriteChar(T(' '), ps->port);
        else {
          sxWriteString(T(" . "), ps->port);
          print_datum(ps, next, level + 1);
          break;
        }
      }
    }
    sxWriteChar(T('}'), ps->port);
    break;
  case NT_HANDLE:
    print_atom(ps, T("Handle"), obj);
    break;
  case NT_FREE:
    print_atom(ps, T("Free"), obj);
    break;
  default:
    print_atom(ps, T("???"), obj);
    break;
  }
}

/* predicates implemented in sxread.c */
extern bool_t symbol_leader_p(tint_t ch);
extern bool_t symbol_constituent_p(tint_t ch);
extern bool_t peculiar_symbol_p(const tchar_t* name);

void print_sym(ostream_t *ps, const tchar_t* name)
{
  /* check for printing without escapes */
  if (ps->display || peculiar_symbol_p(name)) {
    sxWriteString(name, ps->port);
  } else { /* escape strange characters */
    bool_t charok = symbol_leader_p(*name);
    while (*name != 0) {
      if (!charok || (ps->symbols2lc && istupper(*name)))
        sxWriteChar(T('\\'), ps->port);
      sxWriteChar(*name, ps->port);
      ++name;
      charok = symbol_constituent_p(*name);
    }
  }
}

void print_key(ostream_t *ps, const tchar_t* name)
{
  /* check for printing without escapes */
  if (ps->display) {
    sxWriteString(name, ps->port);
    return;
  }
  /* new version: always output keyword in its original case */
  /* if the name contains spaces and such, they are not escaped */
  /* ToDo: escape? */
  sxWriteString(name, ps->port);
  sxWriteChar(T(':'), ps->port);
}

void print_string(ostream_t *ps, const tchar_t* str, size_t len)
{
  tint_t ch; int length;

  /* output the initial quote */
  sxWriteChar(T('"'), ps->port);
  length = 0;

  /* output each character in the string */
  for (;len > 0; len--, str++) {
    ch = *str;

    /* check for a control character */
    /* ToDo: anything else we don't want to print? */
    if (ch == T(' ')) {
      sxWriteChar(ch, ps->port);
    } else if (ch < 040 || ch == T('\\') || ch == T('"') || !istgraph(ch)) {
      sxWriteChar(T('\\'), ps->port);
      switch (ch) {
        case T('\\'):   sxWriteChar(T('\\'), ps->port); break;
        case T('"'):    sxWriteChar(T('"'), ps->port); break;
        case T('\014'): sxWriteChar(T('f'), ps->port); break;
        case T('\n'):   sxWriteChar(T('n'), ps->port); break;
        case T('\r'):   sxWriteChar(T('r'), ps->port); break;
        case T('\t'):   sxWriteChar(T('t'), ps->port); break;
        default:        sxWriteChar(T('c'), ps->port);
                        print_hex(ps, ch);
                        sxWriteChar(T(';'), ps->port);
      }
    } else /* output a normal character */
      sxWriteChar(ch, ps->port);
  }

  /* output the terminating quote */
  sxWriteChar(T('"'), ps->port);
}

void print_subr(ostream_t *ps, const tchar_t* tag, SOBJ obj)
{
  tchar_t buf[SX_MAXSYMBOL+100];
  stprintf(buf, T("#<%s %s>"), tag, getsid(obj));
  sxWriteString(buf, ps->port);
}

void print_atom(ostream_t *ps, const tchar_t* tag, SOBJ obj)
{
  tchar_t buf[SX_MAXSYMBOL+100];
  stprintf(buf, T("#<%s @"), tag);
  sxWriteString(buf, ps->port);
  stprintf(buf, T("%lx"), (long)obj);
  sxWriteString(buf, ps->port);
  sxWriteChar(T('>'), ps->port);
}

void print_fixnum(ostream_t *ps, FIXTYPE n)
{
  tchar_t buf[100];
  stprintf(buf, T(FIXFORMAT), n);
  sxWriteString(buf, ps->port);
}

void print_flonum(ostream_t *ps, FLOTYPE n)
{
  tchar_t* cp = sxUnparseFlonum(n);
  sxWriteString(cp, ps->port);
}

void print_character(ostream_t *ps, tint_t ch)
{
  switch (ch) {
    case T('\014'): sxWriteString(T("#\\page"), ps->port); break;
    case T('\n'):   sxWriteString(T("#\\newline"), ps->port); break;
    case T('\r'):   sxWriteString(T("#\\return"), ps->port); break;
    case T('\t'):   sxWriteString(T("#\\tab"), ps->port); break;
    case T(' '):    sxWriteString(T("#\\space"), ps->port); break;
    default:        if (ch >= 0 && ch < T(' ')) {
                      sxWriteString(T("#\\c"), ps->port); 
                      print_hex(ps, ch);
                    } else {
                      sxWriteString(T("#\\"), ps->port);
                      sxWriteChar(ch, ps->port);
                    }
  }
}

void print_hex(ostream_t *ps, tint_t ch)
{
  tchar_t buf[100];
#if (CS == CS_WIDE)
  stprintf(buf, T("%04X"), ch & 0xFFFF);
#elif (CS == CS_ANSI)
  stprintf(buf, T("%02X"), ch & 0x00FF);
#else
  #error unknown CS
#endif
  sxWriteString(buf, ps->port);
}


EXTERN_VARIABLE(sv_curcasesensv)
EXTERN_VARIABLE(sv_curprlen)
EXTERN_VARIABLE(sv_curprlevel)

/* sxDisplay - print datum with escapes */
void sxDisplay(SOBJ obj, SOBJ port)
{
  ostream_t os;
  os.port = port;
  os.display = TRUE;
  os.symbols2lc = falsep(sv_curcasesensv);
  os.prlength = fixp(sv_curprlen) ? (int)getfixnum(sv_curprlen) : -1;
  os.prlevel = fixp(sv_curprlevel) ? (int)getfixnum(sv_curprlevel) : -1;
  print_datum(&os, obj, 0);
}

/* sxWrite - print datum without escapes */
void sxWrite(SOBJ obj, SOBJ port)
{
  ostream_t os;
  os.port = port;
  os.display = FALSE;
  os.symbols2lc = falsep(sv_curcasesensv);
  os.prlength = fixp(sv_curprlen) ? (int)getfixnum(sv_curprlen) : -1;
  os.prlevel = fixp(sv_curprlevel) ? (int)getfixnum(sv_curprlevel) : -1;
  print_datum(&os, obj, 0);
}


/***************************
 * Math Printer
 ***************************/

/* digit to character */
static tint_t digittochar(int digit)
{
  return (digit < 10) ? (digit + T('0')) : (digit - 10 + T('a'));
}

/* unparse FIXTYPE to string with given radix */
tchar_t* sxUnparseFixnum(FIXTYPE num, int radix)
{
  static tchar_t buffer[34];
  tchar_t *ptr = buffer + (sizeof(buffer)/sizeof(tchar_t)) - 1;
  int neg = 0;
  if (num < 0) {
    neg = 1;
    num = -num;
  }
  do *--ptr = digittochar((int)(num % (FIXTYPE)radix));
  while (num /= (FIXTYPE)radix);
  if (neg) *--ptr = T('-');
  return ptr;
}

/* unparse FLOTYPE to string */
tchar_t* sxUnparseFlonum(FLOTYPE num)
{
  static tchar_t buffer[20];
  tchar_t *cp;
  stprintf(buffer, T("%.15g"), (double)num);
  /* ensure that the printed number can be distinguished from fixnum */
  for (cp = buffer; *cp != 0; cp++) if (*cp == T('e') || *cp == T('.')) break;
  if (*cp == 0) {  *cp++ = T('.'); *cp++ = T('0'); *cp = T('\0'); }
  return buffer;
}
