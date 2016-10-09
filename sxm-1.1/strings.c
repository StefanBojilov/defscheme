/* strings.c - standard procedures 6.7 */

#include "sxm.h"
#include "define.h"

/*#| (string? obj) |#*/
DEFINE_INITIAL_BINDING("string?", sp_stringp)
DEFINE_PROCEDURE(sp_stringp)
{
  SOBJ arg = xlonearg();
  return cvbool(stringp(arg));
}

/*#| (string-length string) |#*/
DEFINE_INITIAL_BINDING("string-length", sp_strlen)
DEFINE_PROCEDURE(sp_strlen)
{
  SOBJ str = xlgastring();
  xllastarg();
  return cvsfixnum(getslength(str)-1);
}

/*#| (string-null? string) |#*/
DEFINE_INITIAL_BINDING("string-null?", sp_strnullp)
DEFINE_PROCEDURE(sp_strnullp)
{
  SOBJ str = xlgastring();
  xllastarg();
  return cvbool(getslength(str) == 1); /* 0-terminator */
}

/*#| (string char ...) |#*/
DEFINE_INITIAL_BINDING("string", sp_string)
DEFINE_PROCEDURE(sp_string)
{
  tchar_t* cp;
  SOBJ str = newstring(vmargc+1);
  for (cp = getstring(str); moreargs();)
    *cp++ = getchcode(xlgachar());
  return str;
}

/*#| (string-copy string) |#*/
DEFINE_INITIAL_BINDING("string-copy", sp_strcopy)
DEFINE_PROCEDURE(sp_strcopy)
{
  SOBJ str = xlgastring();
  ss_size_t size = getslength(str);
  SOBJ val;
  xllastarg();
  gcLock(str);
  val = newstring(size);
  gcUnlock(1);
  tmemcpy(getstring(val), getstring(str), size);
  return val;
}

/*#| (string-fill! string char) |#*/
DEFINE_INITIAL_BINDING("string-fill!", sp_strfill)
DEFINE_PROCEDURE(sp_strfill)
{
  SOBJ str = testmutable(xlgastring());
  SOBJ chr = xlgachar();
  xllastarg();
  tmemset(getstring(str), getchcode(chr), getslength(str)-1);
  return so_void;
}

/*#| (make-string k) |#*/
/*#| (make-string k char) |#*/
DEFINE_INITIAL_BINDING("make-string", sp_makestring)
DEFINE_PROCEDURE(sp_makestring)
{
  SOBJ k = xlgasfixnum();
  FIXTYPE flen = getsfixnum(k);
  tint_t ch = optarg() ? getchcode(xlgachar()) : TEOF;
  xllastarg();
  if (flen < 0 || flen > SX_MAX_STRING_CHARS) {
    sxae_range(k, cvfixnum(SX_MAX_STRING_CHARS));
    return so_false; /* never returns */
  } else {
    ss_size_t len = (ss_size_t)flen;
    SOBJ str = newstring(len+1);
    /* initialize the string */
    if (ch != TEOF) tmemset(getstring(str), ch, len);
    /* return the new string */
    return str;
  }
}

/*#| (string-append string ...) |#*/
DEFINE_INITIAL_BINDING("string-append", sp_strappend)
DEFINE_PROCEDURE(sp_strappend)
{
  SOBJ* savesp;
  SOBJ tmp, val;
  tchar_t* str;
  int saveargc;
  FIXTYPE alen;
  ss_size_t len;

  /* save the argument list */
  saveargc = vmargc;
  savesp = vmsp;

  /* find the length of the new string */
  for (alen = 0; moreargs();) {
    tmp = xlgastring();
    alen += getslength(tmp) - 1;
  }

  if (alen < 0 || alen > SX_MAX_STRING_CHARS)
    sxae_range(cvfixnum(alen), cvfixnum(SX_MAX_STRING_CHARS));
  len = (ss_size_t) alen;

  /* restore the argument list */
  vmargc = saveargc;
  vmsp = savesp;

  /* create the result string */
  val = newstring(len+1);

  /* GC NOTE: call all getstring()s *after* allocation! */
  str = getstring(val);

  /* combine the strings */
  while (moreargs()) {
    tmp = nextarg();
    len = getslength(tmp)-1;
    tmemcpy(str, getstring(tmp), len);
    str += len;
  }

  /* return the new string */
  return val;
}

/*#| (string-ref string k) |#*/
DEFINE_INITIAL_BINDING("string-ref", sp_strref)
DEFINE_PROCEDURE(sp_strref)
{
  /* get the string and the index */
  SOBJ str = xlgastring();
  SOBJ index = xlgasfixnum();
  FIXTYPE i = getsfixnum(index);
  xllastarg();
  if (i < 0 || i >= (FIXTYPE)getslength(str)-1)
    sxae_range(index, cvsfixnum(getslength(str)));
  return cvchar(getstring(str)[(ss_size_t)i]);
}

/*#| (string-set! string k char) |#*/
DEFINE_INITIAL_BINDING("string-set!", sp_strset)
DEFINE_PROCEDURE(sp_strset)
{
  SOBJ str = xlgastring();
  SOBJ index = xlgasfixnum();
  FIXTYPE i = getsfixnum(index);
  SOBJ chr = xlgachar();
  xllastarg();
  if (i < 0 || i >= (FIXTYPE)getslength(str)-1)
    sxae_range(index,cvsfixnum(getslength(str)));
  getstring(testmutable(str))[(ss_size_t)i] = getchcode(chr);
  return so_void;
}

/*#| (substring string start end) |#*/
DEFINE_INITIAL_BINDING("substring", sp_substring)
DEFINE_PROCEDURE(sp_substring)
{
  /* get string and starting and ending positions */
  SOBJ src = xlgastring();
  FIXTYPE len = getslength(src) - 1;
  FIXTYPE start = optarg() ? getsfixnum(xlgasfixnum()) : 0;
  FIXTYPE end = optarg() ? getsfixnum(xlgasfixnum()) : len;
  xllastarg();

  /* range checks */
  if (start < 0 || start > len)
    sxae_range(cvsfixnum(start), cvsfixnum(len));
  else if (end < start || end > len)
    sxae_range(cvsfixnum(end), cvsfixnum(len));

  { /* extract substring */
    size_t dstlen = (size_t)(end - start);
    SOBJ dst;
    gcLock(src);
    dst = newstring(dstlen+1);
    /* GC NOTE: call getstring(src) *after* allocation! */
    tmemcpy(getstring(dst), getstring(src) + (size_t)start, dstlen);
    gcUnlock(1);
    return dst;
  }
}

/*#| (string->list string) |#*/
DEFINE_INITIAL_BINDING("string->list", sp_str2list)
DEFINE_PROCEDURE(sp_str2list)
{
  SOBJ str = xlgastring();
  ss_size_t size = getslength(str)-1;
  SOBJ lst;
  xllastarg();
  /* GC NOTE: string data can be moved, don't use pointers! */
  gcLock(str);
  for (lst = so_nil; size > 0; size--) {
    lst = cons(cvchar(getstring(str)[size-1]), lst);
  }
  gcUnlock(1);
  return lst;
}

/*#| (list->string list) |#*/
DEFINE_INITIAL_BINDING("list->string", sp_list2str)
DEFINE_PROCEDURE(sp_list2str)
{
  SOBJ lst = xlgalist();
  ss_size_t size = length(lst);
  SOBJ str; tchar_t *p;
  xllastarg();
  gcLock(lst);
  str = newstring(size+1);
  gcUnlock(1);
  for (p = getstring(str); size; --size, lst = cdr(lst))
   if (charp(car(lst)))
     *p++ = getchcode(car(lst));
   else
     sxae_type(car(lst),NT_CHAR);
  return str;
}

/* string comparison functions */

enum { CMP_LT, CMP_LE, CMP_EQ, CMP_GE, CMP_GT };
static SOBJ strcompare(int fcn, bool_t icase)
{
  ss_size_t start1, end1, start2, end2;
  tint_t ch1, ch2;
  tchar_t *p1, *p2;

  /* get the strings */
  SOBJ str1 = xlgastring();
  SOBJ str2 = xlgastring();
  xllastarg();

  /* setup the string pointers */
  p1 = getstring(str1); start1 = 0; end1 = getslength(str1);
  p2 = getstring(str2); start2 = 0; end2 = getslength(str2);

  /* compare the strings */
  for (; start1 < end1 && start2 < end2; ++start1, ++start2) {
    ch1 = (*p1++) & (tchar_t)-1;
    ch2 = (*p2++) & (tchar_t)-1;
    if (icase) {
      if (istupper(ch1)) ch1 = totlower(ch1);
      if (istupper(ch2)) ch2 = totlower(ch2);
    }
    if (ch1 != ch2)
      switch(fcn) {
        case CMP_LT: return (cvbool(ch1 < ch2));
        case CMP_LE: return (cvbool(ch1 <= ch2));
        case CMP_EQ: return (so_false);
        case CMP_GE: return (cvbool(ch1 >= ch2));
        case CMP_GT: return (cvbool(ch1 > ch2));
      }
  }
  /* check the termination condition */
  switch (fcn) {
    case CMP_LT: return (cvbool(start1 >= end1 && start2 < end2));
    case CMP_LE: return (cvbool(start1 >= end1));
    case CMP_EQ: return (cvbool(start1 >= end1 && start2 >= end2));
    case CMP_GE: return (cvbool(start2 >= end2));
    case CMP_GT: return (cvbool(start2 >= end2 && start1 < end1));
  }
  return sxFail(T("bad string compare operation"));
}

/*#| (string<? string1 string2) |#*/
DEFINE_INITIAL_BINDING("string<?", sp_strlss)
DEFINE_PROCEDURE(sp_strlss)
{
  return (strcompare(CMP_LT, FALSE));
}

/*#| (string<=? string1 string2) |#*/
DEFINE_INITIAL_BINDING("string<=?", sp_strleq)
DEFINE_PROCEDURE(sp_strleq)
{
  return (strcompare(CMP_LE, FALSE));
}

/*#| (string=? string1 string2) |#*/
DEFINE_INITIAL_BINDING("string=?", sp_streql)
DEFINE_PROCEDURE(sp_streql)
{
  return (strcompare(CMP_EQ, FALSE));
}

/*#| (string>=? string1 string2) |#*/
DEFINE_INITIAL_BINDING("string>=?", sp_strgeq)
DEFINE_PROCEDURE(sp_strgeq)
{
  return (strcompare(CMP_GE, FALSE));
}

/*#| (string>? string1 string2) |#*/
DEFINE_INITIAL_BINDING("string>?", sp_strgtr)
DEFINE_PROCEDURE(sp_strgtr)
{
  return (strcompare(CMP_GT, FALSE));
}

/* string comparison functions (case insensitive) */
/*#| (string-ci<? string1 string2) |#*/
DEFINE_INITIAL_BINDING("string-ci<?", sp_strilss)
DEFINE_PROCEDURE(sp_strilss)
{
  return (strcompare(CMP_LT, TRUE));
}

/*#| (string-ci<=? string1 string2) |#*/
DEFINE_INITIAL_BINDING("string-ci<=?", sp_strileq)
DEFINE_PROCEDURE(sp_strileq)
{
  return (strcompare(CMP_LE, TRUE));
}

/*#| (string-ci=? string1 string2) |#*/
DEFINE_INITIAL_BINDING("string-ci=?", sp_strieql)
DEFINE_PROCEDURE(sp_strieql)
{
  return (strcompare(CMP_EQ, TRUE));
}

/*#| (string-ci>=? string1 string2) |#*/
DEFINE_INITIAL_BINDING("string-ci>=?", sp_strigeq)
DEFINE_PROCEDURE(sp_strigeq)
{
  return (strcompare(CMP_GE, TRUE));
}

/*#| (string-ci>? string1 string2) |#*/
DEFINE_INITIAL_BINDING("string-ci>?", sp_strigtr)
DEFINE_PROCEDURE(sp_strigtr)
{
  return (strcompare(CMP_GT, TRUE));
}

/* misc useful procedures */

static SOBJ substrsearch(int fun, bool_t icase, int fwd)
{
  /* get char, string and starting and ending positions */
  SOBJ arg = (fun == 'c') ? xlgachar() : xlgastring();
  SOBJ str = xlgastring();
  FIXTYPE len = getslength(str) - 1;
  FIXTYPE start = optarg() ? getsfixnum(xlgasfixnum()) : 0;
  FIXTYPE end = optarg() ? getsfixnum(xlgasfixnum()) : len;
  xllastarg();

  /* range checks */
  if (start < 0 || start > len)
    sxae_range(cvsfixnum(start), cvsfixnum(len));
  else if (end < start || end > len)
    sxae_range(cvsfixnum(end), cvsfixnum(len));

  /* search */
  if (fun == 'c') {
    /* look for first/last occurence of Char */
    tint_t ch = getchcode(arg);
    const tchar_t* cp = getstring(str);
    if (icase) {
      if (istupper(ch)) ch = totlower(ch);
      if (fwd) { 
        for (;start < end; start++) {
          tint_t ch1 = cp[start];
          if (istupper(ch1)) ch1 = totlower(ch1);
          if (ch1 == ch) return cvfixnum(start);
        }
      } else {
        while (end > start) {
          tint_t ch1 = cp[--end];
          if (istupper(ch1)) ch1 = totlower(ch1);
          if (ch1 == ch) return cvfixnum(end);
        }
      }
    } else {
      if (fwd) {
        const tchar_t *p = (const tchar_t *)
          tmemchr(cp+start, ch, (size_t)(end-start));
        if (p != NULL) return cvfixnum((FIXTYPE)(p - cp));
      } else {
        while (end > start) {
          if (cp[--end] == ch) return cvfixnum(end);
        }
      }
    }
  } else if (fun == 's') {
    /* look for first/last occurence of char in/notin Set (icase = in) */
    const tchar_t* set = getstring(arg);
    size_t setlen = getslength(arg) - 1;
    const tchar_t* cp = getstring(str);
    if (fwd) {
      while (start < end) {
        bool_t inset = (tmemchr(set, cp[start], setlen) != NULL);
        if (inset == icase) return cvfixnum((FIXTYPE)start);
        start++;
      }
    } else {
      while (start < end) {
        bool_t inset = (tmemchr(set, cp[--end], setlen) != NULL);
        if (inset == icase) return cvfixnum((FIXTYPE)end);
      }
    }
  } else if (fun == '=') {
    /* look for first occurence of string */
    const tchar_t* fs = getstring(arg);
    size_t fslen = getslength(arg) - 1;
    const tchar_t* cp = getstring(str);
    if (fwd) {
      if (fslen == 0) return cvfixnum((FIXTYPE)start);
      end -= fslen; /* end is a last start position for match */
      while (start <= end) {
        if (icase) {
          int i;
          for (i = 0; i < (int)fslen; i++) {
            tint_t ch1 = cp[start+i];
            tint_t ch2 = fs[i];
            if (istupper(ch1)) ch1 = totlower(ch1);
            if (istupper(ch2)) ch2 = totlower(ch2);
            if (ch1 != ch2) goto next_1;
          }
        } else {
          if (tmemcmp(cp + start, fs, fslen) != 0) goto next_1;
        }
        return cvfixnum((FIXTYPE)start);
      next_1:
        start++;
      }
    } else {
      if (fslen == 0) return cvfixnum((FIXTYPE)end);
      start += fslen; /* start is a last after-end position for match */
      while (start <= end) {
        int from =(int)(end-fslen);
        if (icase) {
          int i;
          for (i = 0; i < (int)fslen; i++) {
            tint_t ch1 = cp[from+i];
            tint_t ch2 = fs[i];
            if (istupper(ch1)) ch1 = totlower(ch1);
            if (istupper(ch2)) ch2 = totlower(ch2);
            if (ch1 != ch2) goto next_2;
          }
        } else {
          if (tmemcmp(cp + from, fs, fslen) != 0) goto next_2;
        }
        return cvfixnum((FIXTYPE)from);
      next_2:
        end--;
      }
    }
  }
  return so_false;
}

/*#| (substring-first-position char string [start] [end]) |#*/
DEFINE_INITIAL_BINDING("substring-first-position", sp_substrchr)
/*#| (string-position char string) |#*/
DEFINE_INITIAL_BINDING("string-position", sp_substrchr)
/*#| (string-member? char string) |#*/
DEFINE_INITIAL_BINDING("string-member?", sp_substrchr)
DEFINE_PROCEDURE(sp_substrchr)
{
  return substrsearch('c', FALSE, TRUE);
}

/*#| (substring-last-position char string [start] [end]) |#*/
DEFINE_INITIAL_BINDING("substring-last-position", sp_substrrchr)
DEFINE_PROCEDURE(sp_substrrchr)
{
  return substrsearch('c', FALSE, FALSE);
}

/*#| (substring-first-position-ci char string [start] [end]) |#*/
DEFINE_INITIAL_BINDING("substring-first-position-ci", sp_substrichr)
/*#| (string-position-ci char string]) |#*/
DEFINE_INITIAL_BINDING("string-position-ci", sp_substrichr)
/*#| (string-member-ci? char string]) |#*/
DEFINE_INITIAL_BINDING("string-member-ci?", sp_substrichr)
DEFINE_PROCEDURE(sp_substrichr)
{
  return substrsearch('c', TRUE, TRUE);
}

/*#| (substring-last-position-ci char string [start] [end]) |#*/
DEFINE_INITIAL_BINDING("substring-last-position-ci", sp_substrrichr)
DEFINE_PROCEDURE(sp_substrrichr)
{
  return substrsearch('c', TRUE, FALSE);
}

/*#| (substring-first-position-in-set chset string [start] [end]) |#*/
DEFINE_INITIAL_BINDING("substring-first-position-in-set", sp_substrset)
/*#| (string-position-in-set chset string) |#*/
DEFINE_INITIAL_BINDING("string-position-in-set", sp_substrset)
DEFINE_PROCEDURE(sp_substrset)
{
  return substrsearch('s', TRUE, TRUE);
}

/*#| (substring-last-position-in-set chset string [start] [end]) |#*/
DEFINE_INITIAL_BINDING("substring-last-position-in-set", sp_substrrset)
DEFINE_PROCEDURE(sp_substrrset)
{
  return substrsearch('s', TRUE, FALSE);
}

/*#| (substring-first-position-not-in-set chset string [start] [end]) |#*/
DEFINE_INITIAL_BINDING("substring-first-position-not-in-set", sp_substrnset)
/*#| (string-position-not-in-set chset string) |#*/
DEFINE_INITIAL_BINDING("string-position-not-in-set", sp_substrnset)
DEFINE_PROCEDURE(sp_substrnset)
{
  return substrsearch('s', FALSE, TRUE);
}

/*#| (substring-last-position-not-in-set chset string [start] [end]) |#*/
DEFINE_INITIAL_BINDING("substring-last-position-not-in-set", sp_substrrnset)
DEFINE_PROCEDURE(sp_substrrnset)
{
  return substrsearch('s', FALSE, FALSE);
}

/*#| (substring-first-position-string= patt string [start] [end]) |#*/
DEFINE_INITIAL_BINDING("substring-first-position-string=", sp_substrstr)
DEFINE_PROCEDURE(sp_substrstr)
{
  return substrsearch('=', FALSE, TRUE);
}

/*#| (substring-first-position-string-ci= patt string [start] [end]) |#*/
DEFINE_INITIAL_BINDING("substring-first-position-string-ci=", sp_substristr)
DEFINE_PROCEDURE(sp_substristr)
{
  return substrsearch('=', TRUE, TRUE);
}

/*#| (substring-last-position-string= patt string [start] [end]) |#*/
DEFINE_INITIAL_BINDING("substring-last-position-string=", sp_substrrstr)
DEFINE_PROCEDURE(sp_substrrstr)
{
  return substrsearch('=', FALSE, FALSE);
}

/*#| (substring-last-position-string-ci= patt string [start] [end]) |#*/
DEFINE_INITIAL_BINDING("substring-last-position-string-ci=", sp_substrristr)
DEFINE_PROCEDURE(sp_substrristr)
{
  return substrsearch('=', TRUE, FALSE);
}

/*#| (substring-fill! string start end char) |#*/
DEFINE_INITIAL_BINDING("substring-fill!", sp_substrfill)
DEFINE_PROCEDURE(sp_substrfill)
{
  SOBJ str = testmutable(xlgastring());
  FIXTYPE len = getslength(str) - 1;
  FIXTYPE start = optarg() ? getsfixnum(xlgasfixnum()) : 0;
  FIXTYPE end = optarg() ? getsfixnum(xlgasfixnum()) : len;
  SOBJ chr = xlgachar();
  xllastarg();

  /* range checks */
  if (start < 0 || start > len)
    sxae_range(cvsfixnum(start), cvsfixnum(len));
  else if (end < start || end > len)
    sxae_range(cvsfixnum(end), cvsfixnum(len));

  tmemset(getstring(str) + start, getchcode(chr), (size_t)(end - start));
  return so_void;
}


static SOBJ substrmove(int fun)
{
  /* get strings and starting and ending positions */
  SOBJ str1 = xlgastring();
  FIXTYPE len1 = getslength(str1) - 1;
  FIXTYPE start1 = optarg() ? getsfixnum(xlgasfixnum()) : 0;
  FIXTYPE end1 = optarg() ? getsfixnum(xlgasfixnum()) : len1;
  SOBJ str2 = optarg() ? xlgastring() : str1;
  FIXTYPE len2 = getslength(str2) - 1;
  FIXTYPE start2 = optarg() ? getsfixnum(xlgasfixnum()) : 0;
  FIXTYPE cnt = end1 - start1;
  FIXTYPE end2 = start2 + cnt;
  xllastarg();
  
  /* range checks : 
   * 0 <= start1 <= end1 <= (string-length string1)
   * 0 <= start2 <= end1-start1+start2 <= (string-length string2).
   */
  if (start1 < 0) sxae_range(cvsfixnum(0), cvsfixnum(start1));
  if (end1 < start1) sxae_range(cvsfixnum(start1), cvsfixnum(end1));
  if (end1 > len1) sxae_range(cvsfixnum(end1), cvsfixnum(len1));
  if (start2 < 0) sxae_range(cvsfixnum(0), cvsfixnum(start2));
  if (end2 < start2) sxae_range(cvsfixnum(start2), cvsfixnum(end2));
  if (end2 > len2) sxae_range(cvsfixnum(end2), cvsfixnum(len2));
  
  /* substrmove('l') stores characters in time order of increasing
     indices. substrmove('r') stores characters in time order of
     decreasing indices. 
     substrmove('c') == memcpy; substrmove('m') == memmove */
  
  if (fun == 'c' || str1 != str2) {
    tmemcpy(getstring(testmutable(str2))+start2, 
            getstring(str1)+start1, 
            (size_t)cnt);
  } else if (fun == 'm') {
    tmemmove(getstring(testmutable(str2))+start2, 
             getstring(str1)+start1, 
             (size_t)cnt);
  } else if (fun == 'l') {
    tchar_t *cp = getstring(testmutable(str1));
    while (start1 < end1) {
      cp[start2] = cp[start1];
      start1++; start2++;
    }
  } else if (fun == 'r') {
    tchar_t *cp = getstring(testmutable(str1));
    while (end1 > start1) {
      end1--; end2--;
      cp[end2] = cp[end1];
    }
  }
  return cvfixnum(cnt);
}

/*#| (substring-move-left! string1 [start1 end1 string2 start2]) |#*/
DEFINE_INITIAL_BINDING("substring-move-left!", sp_substrmovel)
DEFINE_PROCEDURE(sp_substrmovel)
{
  return substrmove('l');
}

/*#| (substring-move-right! string1 [start1 end1 string2 start2]) |#*/
DEFINE_INITIAL_BINDING("substring-move-right!", sp_substrmover)
DEFINE_PROCEDURE(sp_substrmover)
{
  return substrmove('r');
}

/*#| (substring-move! string1 start1 end1 string2 start2) |#*/
DEFINE_INITIAL_BINDING("substring-move!", sp_substrmovem)
DEFINE_PROCEDURE(sp_substrmovem)
{
  return substrmove('m');
}


static SOBJ substrmodify(int fun)
{
  /* get string and starting and ending positions */
  SOBJ str = xlgastring();
  FIXTYPE len = getslength(str) - 1;
  FIXTYPE start = optarg() ? getsfixnum(xlgasfixnum()) : 0;
  FIXTYPE end = optarg() ? getsfixnum(xlgasfixnum()) : len;
  tchar_t *cp = getstring(testmutable(str));
  xllastarg();

  /* range checks */
  if (start < 0 || start > len)
    sxae_range(cvsfixnum(start), cvsfixnum(len));
  else if (end < start || end > len)
    sxae_range(cvsfixnum(end), cvsfixnum(len));

  /* modify */
  if (fun == 'u') { /* upcase! */
    while (start < end) {
      tint_t ch = cp[start];
      cp[start++] = istlower(ch) ? totupper(ch) : ch;
    }
  } else if (fun == 'l') { /* downcase! */
    while (start < end) {
      tint_t ch = cp[start];
      cp[start++] = istupper(ch) ? totlower(ch) : ch;
    }
  } else if (fun == 'c') { /* capitalize! */
    /* From scheme version of Dirk Lutzebaeck 
       (lutzebaeck@fokus.gmd.berlin.dbp.de) 
       "hello" -> "Hello"   "hELLO" -> "Hello"
       "*hello" -> "*Hello" "hello you" -> "Hello You"
     */
    int non_first_alpha = FALSE;
    for (;start < end; start++) {
      tint_t ch = cp[start];
      if (istalpha(ch)) {
        if (non_first_alpha) 
          cp[start] = totlower(ch);
        else {
          non_first_alpha = TRUE;
          cp[start] = totupper(ch);
        }
      } else
        non_first_alpha = FALSE;
    }
  } else if (fun == 'r') { /* reverse! */
    FIXTYPE n = (end - start) / 2;
    tchar_t *sp = cp + start;
    tchar_t *ep = cp + end;
    while (n-- > 0) { 
      tchar_t ch = *sp;
      *sp++ = *--ep;
      *ep = ch;
    }
  }
  return str;
}

/*#| (substring-upcase! string [start] [end]) |#*/
DEFINE_INITIAL_BINDING("substring-upcase!", sp_substrmodu)
DEFINE_INITIAL_BINDING("string-upcase!", sp_substrmodu)
DEFINE_PROCEDURE(sp_substrmodu)
{
  return substrmodify('u');
}

/*#| (substring-downcase! string [start] [end]) |#*/
DEFINE_INITIAL_BINDING("substring-downcase!", sp_substrmodl)
DEFINE_INITIAL_BINDING("string-downcase!", sp_substrmodl)
DEFINE_PROCEDURE(sp_substrmodl)
{
  return substrmodify('l');
}

/*#| (substring-capitalize! string [start] [end]) |#*/
DEFINE_INITIAL_BINDING("substring-capitalize!", sp_substrmodc)
DEFINE_INITIAL_BINDING("string-capitalize!", sp_substrmodc)
DEFINE_PROCEDURE(sp_substrmodc)
{
  return substrmodify('c');
}

/*#| (substring-reverse! string [start] [end]) |#*/
DEFINE_INITIAL_BINDING("substring-reverse!", sp_substrmodr)
DEFINE_INITIAL_BINDING("string-reverse!", sp_substrmodr)
DEFINE_PROCEDURE(sp_substrmodr)
{
  return substrmodify('r');
}

/* use less generic name? */
/*#| (string-match? string model) |#*/
DEFINE_INITIAL_BINDING("string-match?", sp_strwicmp)
DEFINE_PROCEDURE(sp_strwicmp)
{
  SOBJ str = xlgastring();
  SOBJ model = xlgastring();
  xllastarg();
  return cvbool(wildicmp(getstring(str), getstring(model)));
}

/* substring comparison functions */

static SOBJ substrcompare(int fcn, bool_t icase)
{
  /* get strings and starting and ending positions */
  SOBJ str1 = xlgastring();
  FIXTYPE len1 = getslength(str1) - 1;
  FIXTYPE start1 = optarg() ? getsfixnum(xlgasfixnum()) : 0;
  FIXTYPE end1 = optarg() ? getsfixnum(xlgasfixnum()) : len1;
  SOBJ str2 = optarg() ? xlgastring() : str1;
  FIXTYPE len2 = getslength(str2) - 1;
  FIXTYPE start2 = optarg() ? getsfixnum(xlgasfixnum()) : 0;
  FIXTYPE end2 = optarg() ? getsfixnum(xlgasfixnum()) : len2;
  tchar_t *p1, *p2;
  tint_t ch1, ch2;
  xllastarg();
  
  /* range checks : 
   * 0 <= start1 <= end1 <= (string-length string1)
   * 0 <= start2 <= end2 <= (string-length string2).
   */
  if (start1 < 0) sxae_range(cvsfixnum(0), cvsfixnum(start1));
  if (end1 < start1) sxae_range(cvsfixnum(start1), cvsfixnum(end1));
  if (end1 > len1) sxae_range(cvsfixnum(end1), cvsfixnum(len1));
  if (start2 < 0) sxae_range(cvsfixnum(0), cvsfixnum(start2));
  if (end2 < start2) sxae_range(cvsfixnum(start2), cvsfixnum(end2));
  if (end2 > len2) sxae_range(cvsfixnum(end2), cvsfixnum(len2));
  
  p1 = getstring(str1);
  p2 = getstring(str2);

  while (start1 < end1 && start2 < end2) {
    ch1 = (p1[start1]); /* & (tchar_t)-1; ? */
    ch2 = (p2[start2]); /* & (tchar_t)-1; ? */
    if (icase) {
      if (istupper(ch1)) ch1 = totlower(ch1);
      if (istupper(ch2)) ch2 = totlower(ch2);
    }
    if (ch1 != ch2)
      switch(fcn) {
        case CMP_LT: return (cvbool(ch1 < ch2));
        case CMP_LE: return (cvbool(ch1 <= ch2));
        case CMP_EQ: return (so_false);
        case CMP_GE: return (cvbool(ch1 >= ch2));
        case CMP_GT: return (cvbool(ch1 > ch2));
      }
    start1++; start2++;
  }
  /* check the termination condition */
  switch (fcn) {
    case CMP_LT: return (cvbool(start1 >= end1 && start2 < end2));
    case CMP_LE: return (cvbool(start1 >= end1));
    case CMP_EQ: return (cvbool(start1 >= end1 && start2 >= end2));
    case CMP_GE: return (cvbool(start2 >= end2));
    case CMP_GT: return (cvbool(start2 >= end2 && start1 < end1));
  }
  return sxFail(T("bad string compare operation"));
}

/*#| (substring<? string1 [start1 end1 string2 start2 end2]) |#*/
DEFINE_INITIAL_BINDING("substring<?", sp_substrlss)
DEFINE_PROCEDURE(sp_substrlss)
{
  return (substrcompare(CMP_LT, FALSE));
}

/*#| (substring<=? string1 [start1 end1 string2 start2 end2]) |#*/
DEFINE_INITIAL_BINDING("substring<=?", sp_substrleq)
DEFINE_PROCEDURE(sp_substrleq)
{
  return (substrcompare(CMP_LE, FALSE));
}

/*#| (substring=? string1 [start1 end1 string2 start2 end2]) |#*/
DEFINE_INITIAL_BINDING("substring=?", sp_substreql)
DEFINE_PROCEDURE(sp_substreql)
{
  return (substrcompare(CMP_EQ, FALSE));
}

/*#| (substring>=? string1 [start1 end1 string2 start2 end2]) |#*/
DEFINE_INITIAL_BINDING("substring>=?", sp_substrgeq)
DEFINE_PROCEDURE(sp_substrgeq)
{
  return (substrcompare(CMP_GE, FALSE));
}

/*#| (substring>? string1 [start1 end1 string2 start2 end2]) |#*/
DEFINE_INITIAL_BINDING("substring>?", sp_substrgtr)
DEFINE_PROCEDURE(sp_substrgtr)
{
  return (substrcompare(CMP_GT, FALSE));
}

/*#| (substring-ci<? string1 [start1 end1 string2 start2 end2]) |#*/
DEFINE_INITIAL_BINDING("substring-ci<?", sp_substrilss)
DEFINE_PROCEDURE(sp_substrilss)
{
  return (substrcompare(CMP_LT, TRUE));
}

/*#| (substring-ci<=? string1 [start1 end1 string2 start2 end2]) |#*/
DEFINE_INITIAL_BINDING("substring-ci<=?", sp_substrileq)
DEFINE_PROCEDURE(sp_substrileq)
{
  return (substrcompare(CMP_LE, TRUE));
}

/*#| (substring-ci=? string1 [start1 end1 string2 start2 end2]) |#*/
DEFINE_INITIAL_BINDING("substring-ci=?", sp_substrieql)
DEFINE_PROCEDURE(sp_substrieql)
{
  return (substrcompare(CMP_EQ, TRUE));
}

/*#| (substring-ci>=? string1 [start1 end1 string2 start2 end2]) |#*/
DEFINE_INITIAL_BINDING("substring-ci>=?", sp_substrigeq)
DEFINE_PROCEDURE(sp_substrigeq)
{
  return (substrcompare(CMP_GE, TRUE));
}

/*#| (substring-ci>? string1 [start1 end1 string2 start2 end2]) |#*/
DEFINE_INITIAL_BINDING("substring-ci>?", sp_substrigtr)
DEFINE_PROCEDURE(sp_substrigtr)
{
  return (substrcompare(CMP_GT, TRUE));
}
