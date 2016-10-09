/* oswin32.c - OS-specific routines for Win32 console app */

#include <time.h>
#include <stdio.h>
#include <io.h>
#include <sys/stat.h>
#include <windows.h>
#include <winnls.h>
#include <fcntl.h>
#include <process.h>
#include "../sxm.h"
#include "../os.h"

#if (CS == CS_WIDE)

#define tmain wmain
#define tgetenv _wgetenv
#define taccess _waccess
#define tremove _wremove
#define trename _wrename
#define ttmpnam _wtmpnam
#define tstat _wstat
#define tsystem _wsystem
#define tspawnvp _wspawnvp
#define tfdopen _wfdopen

/* give our versions of some wide char predicates */
#include "../unicode/chucs2.h"
int istalnum(wint_t c) { return ucs_isalnum(c); }
int istalpha(wint_t c) { return ucs_isalpha(c); }
int istcntrl(wint_t c) { return ucs_iscntrl(c); }
int istdigit(wint_t c) { return iswdigit(c); } /* ucs_ equivalent is no good here */
int istgraph(wint_t c) { return iswgraph(c); } /* no ucs_ equivalent */
int istlower(wint_t c) { return ucs_islower(c); }
int istprint(wint_t c) { return iswprint(c); } /* no ucs_ equivalent */
int istpunct(wint_t c) { return ucs_ispunct(c); }
int istspace(wint_t c) { return ucs_isspace(c); }
int istupper(wint_t c) { return ucs_isupper(c); }
int istxdigit(wint_t c) { return iswxdigit(c); } /* no ucs_ equivalent */
wint_t totlower(wint_t c) { return (wint_t)ucs_tolower(c); }
wint_t totupper(wint_t c) { return (wint_t)ucs_toupper(c); }

#elif (CS == CS_ANSI)

#define tmain main
#define tgetenv getenv
#define taccess access
#define tremove remove
#define trename rename
#define ttmpnam tmpnam
#define tstat _stat
#define tsystem system
#define tspawnvp spawnvp
#define tfdopen _fdopen

#else

#error unknown CS

#endif /* CS == ... */


/* external variables */
/* extern volatile int errno; */

static tchar_t sxmpath[_MAX_PATH+1];

/* tmain */

#ifdef  __cplusplus
extern "C" {
int tmain(int argc, tchar_t **argv);
}
#endif

int tmain(int argc, tchar_t **argv)
{
  tcsncpy(sxmpath, argv[0], _MAX_PATH);
  sxmpath[_MAX_PATH] = T('\0');
  return sxMain(argc, argv);
}

/* osinit - initialize */
void osinit(const tchar_t *banner)
{
  ftprintf(stderr, 
     T("%s C%ldS%ldI%ldL%ldF%ldD%ldP%ldN%ld\n"),
     banner, (long) sizeof(tchar_t),
     (long) sizeof(short), (long) sizeof(int), (long) sizeof(long),
     (long) sizeof(float), (long) sizeof(double),
     (long) sizeof(void *), (long) sizeof(NODE));
}

/* osmsg - print an info message */
void osmsg(const tchar_t *msg)
{
  ftprintf(stderr, T("%s\n"), msg);
}

/* ospanic - panic! print an error message and bail out */
void ospanic(const tchar_t *msg)
{
  ftprintf(stderr, T("Fatal Error: %s\n"), msg);
  exit(1);
}

/* osfreehandle - deallocate handle */
void osfreehandle(OSHANDLE h, int htype)
{
  if (h != (OSHANDLE) 0)
    ospanic(T("Cannot free nonzero handle!!!"));
}

/* osrand - return a random number between 0 and n-1 */
int osrand(int n)
{
  return rand() % n;
}


/* osrandomize - reseed random generator */
void osrandomize(void)
{
  srand((unsigned)time(NULL));
}

FILE* osfopen(const tchar_t* name, const tchar_t* mode, int ori)
{
  FILE* fp = NULL;
  if (name == NULL) return NULL;
  if (mode == NULL) return NULL;
  /* to get rid of false debug asserts in ms libc */
  if (*name == 0) return NULL;

#if (CS == CS_WIDE)
  /* check system version first */
  if (GetVersion() < 0x80000000) { /* Windows NT */
    fp = _wfopen(name, mode);
  } else { /* Win95? UNICODE fopen not supported */
    CHAR namebuf[_MAX_PATH+1];
    CHAR modebuf[5];
    int nbchars = WideCharToMultiByte(CP_ACP, 0, name, -1, namebuf, _MAX_PATH, NULL, NULL);
    int mbchars = WideCharToMultiByte(CP_ACP, 0, mode, -1, modebuf, 4, NULL, NULL);
    if (nbchars && mbchars) {
      namebuf[nbchars] = 0;
      modebuf[mbchars] = 0;
      fp = fopen(namebuf, modebuf);
    } else
      fp = NULL;
  }
  /* make stream wide/byte oriented if needed */
  if (fp != NULL && ori != 0) fwide(fp, ori);
#elif (CS == CS_ANSI)
  fp = fopen(name, mode);
#else
#error unknown CS
#endif /* CS == ... */

  return fp;
}

FILE* ospopen(const tchar_t* cmd, const tchar_t* mode, int ori)
{
  FILE* fp = NULL;
  if (cmd == NULL) return NULL;
  if (mode == NULL) return NULL;

#if (CS == CS_WIDE)
  fp = _wpopen(cmd, mode);
  /* make stream wide/byte oriented if needed */
  if (fp != NULL && ori != 0) fwide(fp, ori);
#elif (CS == CS_ANSI)
  fp = _popen(cmd, mode);
#else
#error unknown CS
#endif /* CS == ... */

  return fp;
}

int ospclose(FILE* pipe)
{
  return _pclose(pipe);
}

/* osprocess - execute a system command with stdin and stderr 
 *  connected to ports */
bool_t osprocess(FILE **in, FILE **out, int *pid, const tchar_t *cmd)
{
  int din, dout, pin[2] = {-1, -1}, pout[2] = {-1, -1};

  if (_pipe(pin, 512, O_TEXT | O_NOINHERIT) != 0) goto err0;
  if (_pipe(pout, 512, O_TEXT | O_NOINHERIT) != 0) goto err0;

  if ((din = _dup(_fileno(stdin))) < 0) goto err0;
  if ((dout = _dup(_fileno(stdout))) < 0) {
    close(din); goto err0;
  }

  if (_dup2(pout[0], _fileno(stdin)) != 0) assert(FALSE);
  if (_dup2(pin[1], _fileno(stdout)) != 0) assert(FALSE);
  
  { /* Spawn child process */
    tchar_t *sh = tgetenv(T("COMSPEC"));
#if 0 /* was: (CS == CS_WIDE) */
    tchar_t *argv[4];
    argv[0] = T("/U");
    argv[1] = T("/C");
    argv[2] = (tchar_t *)cmd; /* cast off const */
    argv[3] = 0;
#else
    tchar_t *argv[3];
    argv[0] = T("/C");
    argv[1] = (tchar_t *)cmd; /* cast off const */
    argv[2] = 0;
#endif
    *pid = tspawnvp(P_NOWAIT, sh? sh: T("CMD"), argv);
  }

  if (_dup2(din, _fileno(stdin)) != 0 || _dup2(dout, _fileno(stdout)) != 0) 
    ospanic(T("failed to restore stdio ports after 'process'"));

  close(pout[0]);
  close(pin[1]);

  close(din);
  close(dout);

  if (*pid < 0) goto err2;

  if ((*in = tfdopen(pin[0], T("r"))) == NULL) goto err2;
  if ((*out = tfdopen(pout[1], T("w"))) == NULL) goto err1;

  return TRUE;

 err0:
  if (pin[0] != -1) close(pin[0]); 
  if (pin[1] != -1) close(pin[1]);
  if (pout[0] != -1) close(pout[0]); 
  if (pout[1] != -1) close(pout[1]);
  return FALSE; 
 err1:
  fclose(*in); pin[0] = -1;
 err2:
  if (pin[0] != -1) close(pin[0]); 
  if (pout[1] != -1) close(pout[1]); 
  return FALSE;
}

bool_t osisatty(FILE* file)
{
  if (file == NULL) return FALSE;
  return _isatty(_fileno(file)) != 0; 
}

const tchar_t* osgetmap8b(const tchar_t* encoding)
{
#if (CS == CS_WIDE)
  static tchar_t wbuf[128];
  //encoding = T("Test");
  if (encoding == NULL) {
    /* generate on the fly from current code page */
    CHAR cbuf[128];
    int i;
    UINT cp = GetACP();
    CPINFO info;
    /* check for DBCS */
    if (!GetCPInfo(cp, &info) || info.MaxCharSize > 1) return NULL;
    for (i = 0; i < 128; i++) cbuf[i] = i+128;
    i = MultiByteToWideChar(cp, MB_PRECOMPOSED|MB_ERR_INVALID_CHARS, 
                          cbuf, 128, wbuf, 128);
    if (i != 128) return NULL;
  } else {
    /* try to load code page */
    tchar_t buf[_MAX_PATH + 100];
    tchar_t *path = tgetenv(T("SXMCPPATH"));
    FILE* fp; int nread;
    if (path == NULL) {
      tchar_t *cp;
      tcsncpy(buf, sxmpath, _MAX_PATH);
      cp = tcsrchr(buf, T('\\'));
      if (cp == NULL) cp = buf; else cp++;
      tcscpy(cp, T("cp\\"));
    } else {
      tcsncpy(buf, path, _MAX_PATH);
    }
    tcsncat(buf, encoding, 80);
    tcscat(buf, T(".cp"));
    osmsg(buf);
    fp = osfopen(buf, T("rb"), 0);
    if (fp == NULL) return NULL;
    nread = fread(wbuf, sizeof(tchar_t), 1, fp);
    if (nread != 1) { fclose(fp); return NULL; }
    /* check for local BOM */
    /* ToDo: support reversed BOM */
    if (wbuf[0] != 0xFEFF) { fclose(fp); return NULL; }
    nread = fread(wbuf, sizeof(tchar_t), 128, fp);
    fclose(fp);
    if (nread != 128) return NULL;
  }
  return wbuf;
#elif (CS == CS_ANSI)
  return NULL;
#else
#error unknown CS
#endif /* CS == ... */
}


tchar_t *osgetenv(const tchar_t *name)
{
  return tgetenv(name);
}


/* ossystem - execute a system command */
int ossystem(const tchar_t *cmd)
{
  if (cmd == NULL)
    cmd = tgetenv(T("COMSPEC"));	/* run shell process ???? */
  return !tsystem(cmd) ? 0 : errno;
}

/* osfprobe - check for file existence */
const tchar_t *osfprobe(const tchar_t *fn, int mode)
{
  if (mode & QF_WILD)
    return fn;
#if (CS == CS_WIDE)
  /* check system version first */
  if (GetVersion() < 0x80000000) { /* Windows NT */
    return !_waccess(fn, mode & 6) ? fn : NULL;
  } else { /* Win95? UNICODE _waccess not supported */
    CHAR namebuf[_MAX_PATH+1];
    int nbchars = WideCharToMultiByte(CP_ACP, 0, fn, -1, namebuf, _MAX_PATH, NULL, NULL);
    if (nbchars) {
      namebuf[nbchars] = 0;
      return !access(namebuf, mode & 6) ? fn : NULL;
    } else
      return NULL;
  }
#elif (CS == CS_ANSI)
  return !access(fn, mode & 6) ? fn : NULL;
#else
#error unknown CS
#endif /* CS == ... */
}

/* ossetexecflag - set file's executable flag */
void ossetexecflag(const tchar_t *fn)
{
#if !defined(_MSC_VER)
  chmod(fn, 0755);
#endif
}


size_t osstatbufsize(void)
{
  return sizeof(struct _stat);
}

int osstat(const tchar_t *path, void *buffer)
{
  return tstat(path, (struct _stat *)buffer);
}

time_t osstatbufmtime(void *buffer)
{
  return ((struct _stat *)buffer)->st_mtime;
}

OSHANDLE oscreatelockfile(const tchar_t *filename)
{
#ifdef _WIN32 
  return 0L; 
#else
  int fd = open(filename, O_CREAT|O_RDWR, 0666);
  if (fd == -1) return sxSignalOpenError(T("create-lock-file"), str);
  if (lockf(fd, F_LOCK, 0) == -1) {
    sxSignalOpenError(strerror(errno), str); /* UNICODE-??? */
  }
  return (OSHANDLE)fd;
#endif
}

void osalarm(unsigned timer)
{
#if defined(_MSC_VER)
  /* alarms are not supported by plain windows */
#else
  alarm(timer);
#endif
}

/* osunlink - unlink file */
int osunlink(const tchar_t *fn)
{
  return tremove(fn);  /* ANSI-compatible: <io.h> */
}

/* osrename - rename file */
int osrename(const tchar_t *oldfn, const tchar_t *newfn)
{
  return trename(oldfn, newfn);  /* ANSI-compatible: <io.h> */
}

/* ostmpnam - generate temporary filename */
tchar_t *ostmpnam(void)
{
  tchar_t buf[L_tmpnam];
  return ttmpnam(buf);       /* ANSI-compatible: <io.h> */
}

/* osrtunits - return internal run time units */
double osrtunits(void)
{
  return CLOCKS_PER_SEC;
}

/* osruntime - return internal run time */
long osruntime(void)
{
  return clock();
}


static long gc_time = 0L;

/* osgchook - visualize gc state */
void osgchook(int before)
{
  static long gc_start;

  if (before) {
    gc_start = time(0L);
  } else {
    gc_time += time(0L) - gc_start;
  }
}

/* osgctime - return internal gc time */
long osgctime(void)
{
  return gc_time;
}

/* ostype - return os id name */
const tchar_t *ostype(void)
{
  return T("Win32");
}


/*
 * strange routines
 */

const tchar_t* osmkdefpath(const tchar_t* szFileName)
{
  static tchar_t szPathName[_MAX_PATH+10];
  tchar_t *cp;
  tcsncpy(szPathName, sxmpath, _MAX_PATH);
  cp = tcsrchr(szPathName, T('\\'));
  if (cp != NULL) {
    if (szFileName == NULL)
      cp[1] = T('\0');
    else 
      tcscpy(cp+1, szFileName);
    return szPathName;
  }
  return szFileName;
}

