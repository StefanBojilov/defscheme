/* chars.c - standard procedures 6.6 */

#include "sxm.h"
#include "define.h"

/*#| (char? obj) |#*/
DEFINE_INITIAL_BINDING("char?", sp_charp)
DEFINE_PROCEDURE(sp_charp)
{
  SOBJ arg = xlonearg();
  return cvbool(charp(arg));
}

/* character order test */
enum { CMP_LT, CMP_LE, CMP_EQ, CMP_GE, CMP_GT };
static SOBJ chrcompare(int fcn, bool_t icase)
{
  tint_t ch1, ch2;
  SOBJ first = xlgachar();
  ch1 = getchcode(first);
  if (icase && istupper(ch1)) ch1 = totlower(ch1);
  for (; moreargs(); ch1 = ch2) {
    SOBJ next = xlgachar();
    ch2 = getchcode(next);
    if (icase && istupper(ch2)) ch2 = totlower(ch2);
    switch(fcn) {
      case CMP_LT: if (ch1 < ch2) continue; break;
      case CMP_LE: if (ch1 <= ch2) continue; break;
      case CMP_EQ: if (ch1 == ch2) continue; break;
      case CMP_GE: if (ch1 >= ch2) continue; break;
      case CMP_GT: if (ch1 > ch2) continue; break;
    }
    xlpoprest();
    return so_false;
  }
  return so_true;
}


/* character comparision functions (case sensitive) */

/*#| (char<? char1 char2) |#*/
DEFINE_INITIAL_BINDING("char<?", sp_charlss)
DEFINE_PROCEDURE(sp_charlss)
{
  return chrcompare(CMP_LT, FALSE);
}

/*#| (char<=? char1 char2) |#*/
DEFINE_INITIAL_BINDING("char<=?", sp_charleq)
DEFINE_PROCEDURE(sp_charleq)
{
  return chrcompare(CMP_LE, FALSE);
}

/*#| (char=? char1 char2) |#*/
DEFINE_INITIAL_BINDING("char=?", sp_chareql)
DEFINE_PROCEDURE(sp_chareql)
{
  return chrcompare(CMP_EQ, FALSE);
}

/*#| (char>=? char1 char2) |#*/
DEFINE_INITIAL_BINDING("char>=?", sp_chargeq)
DEFINE_PROCEDURE(sp_chargeq)
{
  return chrcompare(CMP_GE, FALSE);
}

/*#| (char>? char1 char2) |#*/
DEFINE_INITIAL_BINDING("char>?", sp_chargtr)
DEFINE_PROCEDURE(sp_chargtr)
{
  return chrcompare(CMP_GT, FALSE);
}


/* character comparision functions (case insensitive) */

/*#| (char-ci<? char1 char2) |#*/
DEFINE_INITIAL_BINDING("char-ci<?", sp_charilss)
DEFINE_PROCEDURE(sp_charilss)
{
  return chrcompare(CMP_LT, TRUE);
}

/*#| (char-ci<=? char1 char2) |#*/
DEFINE_INITIAL_BINDING("char-ci<=?", sp_charileq)
DEFINE_PROCEDURE(sp_charileq)
{
  return chrcompare(CMP_LE, TRUE);
}

/*#| (char-ci=? char1 char2) |#*/
DEFINE_INITIAL_BINDING("char-ci=?", sp_charieql)
DEFINE_PROCEDURE(sp_charieql)
{
  return chrcompare(CMP_EQ, TRUE);
}

/*#| (char-ci>=? char1 char2) |#*/
DEFINE_INITIAL_BINDING("char-ci>=?", sp_charigeq)
DEFINE_PROCEDURE(sp_charigeq)
{
  return chrcompare(CMP_GE, TRUE);
}

/*#| (char-ci>? char1 char2) |#*/
DEFINE_INITIAL_BINDING("char-ci>?", sp_charigtr)
DEFINE_PROCEDURE(sp_charigtr)
{
  return chrcompare(CMP_GT, TRUE);
}


/* chrtest - test/convert characters */
static SOBJ chrtest(int fcn)
{
  SOBJ arg = xlgachar();
  tint_t ch = getchcode(arg);
  xllastarg();
  switch (fcn) {
    case 'a':   return cvbool(istalpha(ch));
    case 'd':   return cvbool(istdigit(ch));
    case 's':   return cvbool(istspace(ch));
    case 'u':   return cvbool(istupper(ch));
    case 'l':   return cvbool(istlower(ch));
    case 'U':   return istlower(ch) ? cvchar((tchar_t)totupper(ch)) : arg;
    case 'D':   return istupper(ch) ? cvchar((tchar_t)totlower(ch)) : arg;
    default:    return so_void;
  }
}


/* character predicate functions */

/*#| (char-alphabetic? char) |#*/
DEFINE_INITIAL_BINDING("char-alphabetic?", sp_charalpha)
DEFINE_PROCEDURE(sp_charalpha)
{
  return chrtest('a');
}

/*#| (char-numeric? char) |#*/
DEFINE_INITIAL_BINDING("char-numeric?", sp_chardigit)
DEFINE_PROCEDURE(sp_chardigit)
{
  return chrtest('d');
}

/*#| (char-whitespace? char) |#*/
DEFINE_INITIAL_BINDING("char-whitespace?", sp_charspace)
DEFINE_PROCEDURE(sp_charspace)
{
  return chrtest('s');
}

/*#| (char-upper-case? char) |#*/
DEFINE_INITIAL_BINDING("char-upper-case?", sp_charupper)
DEFINE_PROCEDURE(sp_charupper)
{
  return chrtest('u');
}

/*#| (char-lower-case? char) |#*/
DEFINE_INITIAL_BINDING("char-lower-case?", sp_charlower)
DEFINE_PROCEDURE(sp_charlower)
{
  return chrtest('l');
}

/* character conversion functions */

/*#| (char->integer char) |#*/
DEFINE_INITIAL_BINDING("char->integer", sp_char2int)
DEFINE_PROCEDURE(sp_char2int)
{
  SOBJ arg = xlgachar();
  tint_t ch = getchcode(arg);
  unsigned long chcode = ((unsigned long)ch) & TCHAR_UMASK;
  xllastarg();
  return cvsfixnum((FIXTYPE)chcode);
}

/*#| (integer->char n) |#*/
DEFINE_INITIAL_BINDING("integer->char", sp_int2char)
DEFINE_PROCEDURE(sp_int2char)
{
  SOBJ arg = xlgasfixnum();
  FIXTYPE f = getsfixnum(arg);
  xllastarg();
  if ((f < 0) || (f > (FIXTYPE)TCHAR_UMASK))
     sxErr(T("char code out of range"), arg);
  return cvchar((tchar_t)f); /* possible conversion to signed value! */
}

/*#| (char-upcase char) |#*/
DEFINE_INITIAL_BINDING("char-upcase", sp_charupcase)
DEFINE_PROCEDURE(sp_charupcase)
{
  return chrtest('U');
}

/*#| (char-downcase char) |#*/
DEFINE_INITIAL_BINDING("char-downcase", sp_chardowncase)
DEFINE_PROCEDURE(sp_chardowncase)
{
  return chrtest('D');
}

/*#| (char- char1 char2) |#*/
DEFINE_INITIAL_BINDING("char-", sp_charminus)
DEFINE_PROCEDURE(sp_charminus)
{
  unsigned long cc1, cc2; SOBJ arg;
  /* get the characters */
  arg = xlgachar(); cc1 = (unsigned long)getchcode(arg) & TCHAR_UMASK;
  arg = xlgachar(); cc2 = (unsigned long)getchcode(arg) & TCHAR_UMASK;
  xllastarg();

  return cvsfixnum((FIXTYPE)cc1 - (FIXTYPE)cc2);
}
