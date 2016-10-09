/* more.c - task-specific procedures */

#include "../sxm.h"
#include "../define.h"
#include "../handles.h"
#include "../os.h"

/*#| (substring-decode-uri! string [start] [end]) => result-length |#*/
DEFINE_INITIAL_BINDING("substring-decode-uri!", sp_decodeuri)
/*#| (string-decode-uri! string) => result-length |#*/
DEFINE_INITIAL_BINDING("string-decode-uri!", sp_decodeuri)
DEFINE_PROCEDURE(sp_decodeuri)
{
  SOBJ str = xlgastring();
  tchar_t *buf = getstring(str);
  ss_size_t len = getslength(str) - 1;
  FIXTYPE start = optarg() ? getsfixnum(xlgasfixnum()) : 0;
  FIXTYPE end = optarg() ? getsfixnum(xlgasfixnum()) : len;
  xllastarg();

  /* range checks */
  if (start < 0 || start > (FIXTYPE)len)
    sxae_range(cvsfixnum(start), cvsfixnum(len));
  else if (end < start || end > (FIXTYPE)len)
    sxae_range(cvsfixnum(end), cvsfixnum(len));

  { /* start unescaping %xx -> hex value and + -> space */
    FIXTYPE iread = start;
    FIXTYPE iwrite = start;
    while (iread < end) {
      tint_t ch = buf[iread++];
      if (ch == T('+')) {
        buf[iwrite++] = T(' ');     
      } else if (ch == T('%')) {
        int d1, d2;
        if (iread + 2 > end) break; /* ??? %is unfinished */
        ch = buf[iread++]; ch = totupper(ch);
        d1 = (ch <= T('9')) ? (ch - T('0')) : (ch - T('A') + 10);
        ch = buf[iread++]; ch = totupper(ch);
        d2 = (ch <= T('9')) ? (ch - T('0')) : (ch - T('A') + 10);
        ch = ((d1 << 4) | (d2 & 0xF)) & 0xFF; /* UNICODE-??? */
        buf[iwrite++] = ch;
      } else {
        buf[iwrite++] = ch;
      }
    }
    return cvfixnum(iwrite - start);
  }
}

extern SOBJ sxSignalOpenError(const tchar_t *typestr, SOBJ filename);

/*#| (obtain-file-lock filename) => lock-handle |#*/
DEFINE_INITIAL_BINDING("obtain-file-lock", sp_trycreatlf)
DEFINE_PROCEDURE(sp_trycreatlf)
{
  SOBJ str = xlgastring();
  tchar_t *filename = getstring(str);
  OSHANDLE h;
  xllastarg();
  h = oscreatelockfile(filename);
  if (h == 0L)
    sxSignalOpenError(T("obtain-file-lock"), str);
  return cvhandle((OSHANDLE)h, HT_FILE_LOCK);
}


/*#| (limit-run-time timeout) |#*/
DEFINE_INITIAL_BINDING("limit-run-time", sp_limruntime)
DEFINE_PROCEDURE(sp_limruntime)
{
  SOBJ x = xlganumber();
  xllastarg();
  if (fixp(x)) osalarm((unsigned)getfixnum(x));
  else osalarm((unsigned)getflonum(x));
  return so_void;
}

/*#| (current-time) |#*/
DEFINE_INITIAL_BINDING("current-time", sp_time)
DEFINE_PROCEDURE(sp_time)
{
  time_t t = time(NULL);
  xllastarg();
  return cvfixnum((FIXTYPE)t);
}

/*#| (format-time time [format [local?]]) => string |#*/
DEFINE_INITIAL_BINDING("format-time", sp_strftime)
DEFINE_PROCEDURE(sp_strftime)
{
  SOBJ fx = xlgafixnum();
  time_t t = (time_t)getfixnum(fx);
  SOBJ str = optarg() ? xlgastring() : so_false;
  tchar_t *format = (str == so_false) ? T("%x %X") : getstring(str);
  int gmt = optarg() ? (xlgetarg() == so_nil) : FALSE;
  static tchar_t buf[121];
  struct tm *ptm = gmt ? gmtime(&t) : localtime(&t);
  size_t n = tcsftime(buf, 100, format, ptm);
  xllastarg();
  if (n == 0) return so_false;
  return cvstring(buf);
}

/*#| (date-and-time) => string |#*/
DEFINE_INITIAL_BINDING("date-and-time", sp_datentime)
DEFINE_PROCEDURE(sp_datentime)
{
  time_t t = time(NULL);
  static tchar_t buf[121];
  struct tm *ptm = localtime(&t);
  size_t n = tcsftime(buf, 100, T("%x %X"), ptm);
  xllastarg();
  if (n == 0) return so_false;
  return cvstring(buf);
}

/*#| (file-last-modification-time filename) => time |#*/
DEFINE_INITIAL_BINDING("file-last-modification-time", sp_flmodtime)
DEFINE_PROCEDURE(sp_flmodtime)
{
  SOBJ str = xlgastring();
  tchar_t *filename = getstring(str);
  int res; time_t mt;
  void *buf = malloc(osstatbufsize());
  assert(buf != NULL);
  xllastarg();
  res = osstat(filename, buf);
  mt = osstatbufmtime(buf);
  free(buf);
  if (res == -1) return sxSignalOpenError(T("file-last-modification-time"), str);
  return cvfixnum((FIXTYPE)mt);
}

/*#| (time-difference endtime starttime) |#*/
DEFINE_INITIAL_BINDING("time-difference", sp_difftime)
DEFINE_PROCEDURE(sp_difftime)
{
  SOBJ fx1 = xlgafixnum();
  time_t end = (time_t)getfixnum(fx1);
  SOBJ fx2 = xlgafixnum();
  time_t start = (time_t)getfixnum(fx2);
  xllastarg();
  return cvflonum(difftime(end, start));
}
