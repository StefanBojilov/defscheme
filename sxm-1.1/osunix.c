/* osunix.c - OS-specific routines for unknown brand of UNIX */

#include <time.h>
#include <stdio.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <fcntl.h>
#include <unistd.h> 
#include <errno.h> 
#include <signal.h> 
#include "sxm.h"
#include "os.h"
#include "handles.h"

#if (CS == CS_WIDE)

#error CS_WIDE is not supported under UNIX

#elif (CS == CS_ANSI)

#define tmain main
#define tgetenv getenv
#define taccess access
#define tremove remove
#define trename rename
#define ttmpnam tmpnam
#define tstat stat
#define tsystem system
#define tfdopen fdopen
#define texecv execv

#else

#error unknown CS

#endif /* CS == ... */

#ifndef SA_RESTART  /* to restart interrupted syscalls by the kernel */
#define SA_RESTART 0
#endif

#ifndef SA_INTERRUPT  /* ALARM will interrupt system calls if possible */
#define SA_INTERRUPT 0
#endif


/* external variables */
/* extern volatile int errno; */

static tchar_t sxmpath[FILENAME_MAX+1];

/* tmain */

#ifdef  __cplusplus
extern "C" {
int tmain(int argc, tchar_t **argv);
}
#endif

static void sigint_handler (int ignored)
{
  vmInterruptRequest(VMI_KBINT);
}

#ifdef SIGCLD
static void sigcld_handler (int ignored)
{
  while (waitpid(-1, NULL, WNOHANG) > 0)
    ;
}
#endif

static int ossignal(int sig, void (*handler)(int)) /* because signal() differs */
{
  struct sigaction sa;

  memset(&sa, 0, sizeof (sa));
  sa.sa_handler = handler;
  sigemptyset(&sa.sa_mask);
  sa.sa_flags = 0;
  if (sig == SIGALRM) {
    sa.sa_flags |= SA_INTERRUPT;
  } else {
    sa.sa_flags |= SA_RESTART;
  }
  return sigaction(sig, &sa, NULL);  
}

int tmain(int argc, tchar_t **argv)
{
  int status;
  tcsncpy(sxmpath, argv[0], FILENAME_MAX);
  sxmpath[FILENAME_MAX] = T('\0');

  ossignal(SIGINT, sigint_handler);
#ifdef SIGCLD
  ossignal(SIGCLD, sigcld_handler);
#endif
  ossignal(SIGPIPE, SIG_IGN);

  status = sxMain(argc, argv);

  while (waitpid(-1, NULL, WNOHANG) > 0)
    ;
  return status;
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
#if defined(F_ULOCK) /* lockf is supported? */
  if (htype == HT_FILE_LOCK) {
    int fd = (int) h;
    lockf(fd, F_ULOCK, 0);
    close(fd);
    return;
  }
#endif
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
  fp = fopen(name, mode);
  return fp;
}

FILE* ospopen(const tchar_t* cmd, const tchar_t* mode, int ori)
{
  FILE* fp = NULL;
  if (cmd == NULL) return NULL;
  if (mode == NULL) return NULL;
  fp = popen(cmd, mode);
  return fp;
}

int ospclose(FILE* pipe)
{
  return pclose(pipe);
}

/* osprocess - execute a system command with stdin and stderr 
 *  connected to ports */
bool_t osprocess(FILE **in, FILE **out, int *pid, const tchar_t *cmd)
{
  int pin[2], pout[2];
  sigset_t oss, nss;

  if (pipe(pin) != 0)
    goto err1;

  if (pipe(pout) != 0)
    goto err2;

  /* block all signals before forking */
  sigfillset(&nss);
  sigprocmask(SIG_BLOCK, &nss, &oss);

  if ((*pid = fork()) == -1)
    goto err3;

  if (*pid == 0) {
    /* the offsping */
    tchar_t *sh = tgetenv(T("SHELL"));
    tchar_t *argv[4];

    /* put child process in a new group to avoid getting parent's signals */
    setpgid(0, 0); 

    close(pout[1]);
    dup2(pout[0], 0);
    close(pout[0]);

    close(pin[0]);
    dup2(pin[1],1);
    close(pin[1]);

    argv[0] = sh? sh: T("/bin/sh");
    argv[1] = T("-c");
    argv[2] = (tchar_t *)cmd; /* cast off const */
    argv[3] = 0;
    texecv(argv[0], argv);
    /* was unable to exec /bin/sh? Get a life! */
    exit(127);
  }
  /* signal hangling dance: posix step */
  setpgid(*pid, *pid);
  /* restore signal mask in the parent */
  sigprocmask(SIG_SETMASK, &oss, NULL);

  close(pin[1]);
  close(pout[0]);
  if (((*in = tfdopen(pin[0], T("r"))) == NULL) ||
      ((*out = tfdopen(pout[1], T("w"))) == NULL))
    goto err0;

  return TRUE;

 err0:
  /* run out of FILE handles in the parent */
  /* XXX: should we kill the child somehow? */
  close(pin[0]);
  close(pout[1]);
  return FALSE;
 err3:
  /* fork() failed, both pipes are to be closed, signal mask restored */
  sigprocmask(SIG_SETMASK, &oss, NULL);
  close(pout[0]);
  close(pout[1]);
 err2:
  /* pipe(pout) failed, close pin only */
  close(pin[0]);
  close(pin[1]);
 err1:
  /* no need to close pipes, accept the failure */
  return FALSE;
}

bool_t osisatty(FILE* file)
{
  if (file == NULL) return FALSE;
  return isatty(fileno(file)); 
}

const tchar_t* osgetmap8b(const tchar_t* encoding)
{
  return NULL;
}


tchar_t *osgetenv(const tchar_t *name)
{
  return tgetenv(name);
}


/* ossystem - execute a system command */
int ossystem(const tchar_t *cmd)
{
  if (cmd == NULL) cmd = T("/bin/sh");
  return !tsystem(cmd) ? 0 : errno;
}


/* osfprobe - check for file existence */
const tchar_t *osfprobe(const tchar_t *fn, int mode)
{
  if (mode & QF_WILD)
    return fn;
  return !access(fn, mode & 6) ? fn : NULL;
}

/* ossetexecflag - set file's executable flag */
void ossetexecflag(const tchar_t *fn)
{
  chmod(fn, 0755);
}

size_t osstatbufsize(void)
{
  return sizeof(struct stat);
}

int osstat(const tchar_t *path, void *buffer)
{
  return tstat(path, (struct stat *)buffer);
}

time_t osstatbufmtime(void *buffer)
{
  return ((struct stat *)buffer)->st_mtime;
}

OSHANDLE oscreatelockfile(const tchar_t *filename)
{
  extern SOBJ sxSignalOpenError(const tchar_t *typestr, SOBJ filename);
#if !defined (F_LOCK) 
  return 0L; /* lockf not supported */
#else
  int fd = open(filename, O_CREAT|O_RDWR, 0666);
  if (fd == -1) {
    sxSignalOpenError(T("create-lock-file"), so_false);
    return 0;
  }
  if (lockf(fd, F_LOCK, 0) == -1) {
    sxSignalOpenError(strerror(errno), so_false); /* UNICODE-??? */
  }
  return (OSHANDLE)fd;
#endif
}

void osalarm(unsigned timer)
{
  alarm(timer);
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
  return T("UNIX?");
}


/*
 * strange routines
 */

const tchar_t* osmkdefpath(const tchar_t* filename)
{
  static tchar_t buffer[2*FILENAME_MAX+1];
  tchar_t *cp;
  tcsncpy(buffer, sxmpath, FILENAME_MAX);
  cp = tcsrchr(buffer, T('/'));
  if (cp != NULL) {
    if (filename == NULL)
      cp[1] = T('\0');
    else 
      tcscpy(cp+1, filename);
    return buffer;
  }
  return filename;
}

