/* osplan9.c - OS-specific routines for Plan 9 release 3 */

#if !defined(_Plan9r3)

#error "This file requires Plan9"

#endif

#define _BSD_EXTENSION /* for popen() and pclose() */
#include <time.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/wait.h>
#include <fcntl.h>
#include <unistd.h> 
#include <errno.h>
#include <signal.h>
#include "sxm.h"
#include "os.h"
#include "handles.h"


#if (CS == CS_WIDE)

#error "CS_WIDE is not supported under Plan9 yet"

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

#error "Unknown CS"

#endif /* CS == ... */

#ifndef SA_RESTART  /* to restart interrupted syscalls by the kernel */
#define SA_RESTART 0
#endif

#ifndef SA_INTERRUPT  /* ALARM will interrupt system calls if possible */
#define SA_INTERRUPT 0
#endif

static tchar_t *INTRNOTE = "interrupted";

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


static tchar_t sxmpath[FILENAME_MAX+1];

/* tmain */
int main(int argc, tchar_t **argv);

static void sigint_handler (int)
{
  vmInterruptRequest(VMI_KBINT);
}

static void sigcld_handler (int)
{
  while (waitpid(-1, NULL, WNOHANG) > 0)
    ;
}

static int ossignal(int sig, void (*handler)()) /* because signal() differs */
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
  ossignal(SIGCHLD, sigcld_handler);
  ossignal(SIGPIPE, SIG_IGN);

  status = sxMain(argc, argv);

  while (waitpid(-1, NULL, WNOHANG) > 0)
    ;
  return status;
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

    argv[0] = T("/bin/rc");
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
  static tchar_t buffer[FILENAME_MAX+1];  /* because original getenv() is broken */
  tchar_t *ptr = getenv(name);

  if (ptr == NULL)
     return ptr;
  tcsncpy(buffer, ptr, FILENAME_MAX);
  buffer[FILENAME_MAX] = T('\0');
  free(ptr);
  return buffer;
}


/* ossystem - execute a system command */
int ossystem(const tchar_t *cmd)
{
  if (cmd == NULL) cmd = T("/bin/rc");
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
  return 0L; /* lockf not supported */
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
  return T("Plan9");
}

