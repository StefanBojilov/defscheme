/* sxmain.c - main routine */

#include "sxm.h"
#include "sxintern.h"
#include "sxlocale.h"
#include "sximage.h"
#include "define.h"
#include "extern.h"
#include "os.h"

int sx_argc, sx_argpos;
tchar_t **sx_argv;

EXTERN_VARIABLE(sv_curstart)
EXTERN_VARIABLE(sv_curnogreeting)
DEFINE_VARIABLE(sv_arglist) /* initialized here */

/* command line options set by getnextopt */
static bool_t endopts = FALSE;
static tchar_t *sx_load_heap_file = NULL;
static tchar_t *sx_save_heap_file = NULL;
bool_t sx_use_tty = FALSE;
bool_t sx_exit_after_load = FALSE;

/* processes option and returns number of processed args or 0 */
int getnextopt(int curargc, tchar_t **curargv)
{
  if (!endopts && curargc >= 1) {
    tchar_t *arg = curargv[0];
    if (tcscmp(arg, T("--")) == 0) {
      endopts = TRUE; /* following args are "files" */
      return 1;
    } else if (tcscmp(arg, T("-t")) == 0) {
      sx_use_tty = TRUE;
      return 1;
    } else if (tcscmp(arg, T("-e")) == 0) {
      sx_exit_after_load = TRUE;
      return 1;
    } else if (arg[0] == T('-') && arg[1] == T('h')) {
      if (arg[2] != 0) { /* filename follows opt */
        sx_load_heap_file = arg + 2;
        return 1;
      } else if (curargc >= 2) { /* filename is in the next arg */
        sx_load_heap_file = curargv[1];
        return 2;
      } else
        ospanic(T("Missing file parameter for -h option"));
    } else if (arg[0] == T('-') && arg[1] == T('s')) {
      if (arg[2] != 0) { /* filename follows opt */
        sx_save_heap_file = arg + 2;
        return 1;
      } else if (curargc >= 2) { /* filename is in the next arg */
        sx_save_heap_file = curargv[1];
        return 2;
      } else
        ospanic(T("Missing file parameter for -s option"));
    } else if (arg[0] == T('-')) {
      tchar_t buf [120];
      stprintf(buf, T("Unknown command line option: %s"), arg);
      ospanic(buf);
    }
  }
  return 0;
}

/* sxMain() - the main routine */
int sxMain(int argc, tchar_t *argv[])
{
  SOBJ code;
  int retcode;
  sv_size_t ssize = 20000;           /* initial stack size */

  /* perform first-time initialization */
  sxSetup();

  /* process command line */
  { 
    int curargc = argc - 1;
    tchar_t **curargv = argv + 1;
    int optargs;

    /* if nothing is given, display usage info */
    if (!curargc) {
      osmsg(T("Usage: sxm [options] file ...\n")
            T("Options:\n")
            T("  -h <file>  Load heap image from <file>\n")
            T("  -s <file>  Save heap image on normal exit in <file>\n")
            T("  -t         Initialize image for interactive use (ignored if -h)\n")
            T("  -e         Exit after loading files on command line (ignored if -h)\n")
            T("  --         Treat rest of command line as file names (not options)\n"));
      return 0;
    }

    /* process options */
    while ((optargs = getnextopt(curargc, curargv)) != 0) {
      curargc -= optargs; curargv += optargs;
    }

    /* everything else is left to scheme-start */
    sx_argc = argc;
    sx_argv = argv;
    sx_argpos = argc - curargc;
  }

  /* restore the given image if any, otherwise create a new one */
  if (sx_load_heap_file != NULL) {
    code = sxImgRestore(sx_load_heap_file);
    if (nullp(code)) ospanic(T("cannot load heap image file"));
  } else {
    code = sxImgInit(ssize);
    if (nullp(code)) ospanic(T("cannot initialize image"));
  }

  /* collect argument list */
  { 
    int curargc = sx_argc;
    tchar_t **argp = sx_argv + sx_argc; /* from the end */
    for (sv_arglist = so_nil; curargc > sx_argpos; curargc--) {
      --argp;
      sv_arglist = cons(cvstring(*argp), sv_arglist);
    }
  }

  /* start with the greeting (unless supressed) */
  if (!sx_exit_after_load && falsep(sv_curnogreeting)) {
    osmsg(BANNER);
    osmsg(COPYRT);
    osmsg(T(""));
  }

  /* call VM with the start code */
  retcode = 0;
  code = sxExecute(code, sv_arglist);
  if (fixp(code)) retcode = (int)getfixnum(code);

  /* save resulting image if required */
  if (retcode == 0 && sx_save_heap_file != NULL) {
    /* osmsg(T("Saving image file to "));
     * osmsg(sx_save_heap_file);
     * osmsg(T("...\n")); */
    if (!sxImgSave(
           sx_save_heap_file,
           sv_curstart,
           FALSE, 
           T("#!")T(SXMPATH)T(" -h")
       )) 
    {
      osmsg(T("Cannot save image file\n"));
      retcode = 1;
    }
  }

  /* cleanup before exit - close files, handles, free memory, ... */
  sxCleanup();
  return retcode;
}

