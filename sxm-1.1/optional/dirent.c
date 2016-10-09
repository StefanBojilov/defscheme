/* dirent.c - directory listing function (optional) */

#if !defined(_MSC_VER)
#include <sys/types.h>
#include <dirent.h>   /* POSIX */
#endif

#include "../sxm.h"
#include "../define.h"

/*#| (file-list [path] [mask]) |#*/
DEFINE_INITIAL_BINDING("file-list", sp_filelist)
DEFINE_PROCEDURE(sp_filelist)
{
  tchar_t* path = (optarg()) ? getstring(xlgastring()) : T("");
  SOBJ mask = (optarg()) ? xlgastring() : so_default;
  xllastarg();
#if defined(_MSC_VER)
  return so_false;
#else
  { DIR* dirp;
    struct dirent* ent;
    SOBJ fp, lst, tail;
    if ((dirp = opendir(path)) == NULL) return so_false;
    gcLock(mask);
    lst = tail = so_nil;
    while ((ent = readdir(dirp)) != NULL) {
      if (mask != so_default && !wildicmp(ent->d_name, getstring(mask)))
        continue;
      gcLock(lst);
      fp = cons(cvstring(ent->d_name), so_nil);
      gcUnlock(1);
      if (nullp(tail)) lst = fp;
      else setcdr(tail,fp);
      tail = fp;
    }
    closedir(dirp);
    gcUnlock(1);
    return lst;
  }
#endif
}


