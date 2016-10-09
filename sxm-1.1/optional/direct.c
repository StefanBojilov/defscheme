/* direct.c - the current directory parameter */

#if defined(_MSC_VER)
#include <direct.h>
#else
#include <unistd.h>
#define _getcwd getcwd
#define _chdir chdir
#endif
#include <stdio.h>

#include "../sxm.h"
#include "../define.h"
#include "../sxio.h"

#if (CS == CS_WIDE)

#define tgetcwd _wgetcwd
#define tchdir _wchdir

#elif (CS == CS_ANSI)

#define tgetcwd _getcwd
#define tchdir _chdir

#else

#error unknown CS

#endif /* CS == ... */


/*#| (current-directory [new]) |#*/
DEFINE_INITIAL_BINDING("current-directory", sp_curdir)
DEFINE_PROCEDURE(sp_curdir)
{
  SOBJ str = (optarg()) ? xlgastring() : so_default;
  xllastarg();
  if (str == so_default) { /* pwd */
    tchar_t buffer[FILENAME_MAX + 1];
    if (tgetcwd(buffer, FILENAME_MAX) == NULL) return so_false;
    return cvstring(buffer);
  } else { /* cd */
    if (tchdir(getstring(str)) != 0) sxSignalOpenError(T("directory"), str);
    return so_void;
  }
}

