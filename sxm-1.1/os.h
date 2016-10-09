/* os.h - os-dependent procedures */

#ifndef __OS_H
#define __OS_H

#include "sxm.h"

extern void    osinit(const tchar_t* banner);
extern void    osmsg(const tchar_t* msg);
extern void    ospanic(const tchar_t* msg);
extern void    osfreehandle(OSHANDLE h, int htype);
extern int     osrand(int n);
extern void    osrandomize(void);
extern FILE*   osfopen(const tchar_t* fname, const tchar_t* mode, int ori);
extern FILE*   ospopen(const tchar_t* cmd, const tchar_t* mode, int ori);
extern int     ospclose(FILE* pipe);
extern bool_t  osprocess(FILE **in, FILE **out, int *pid, const tchar_t *cmd);
extern bool_t  osisatty(FILE* file);
extern int     ossystem(const tchar_t* cmd);
extern tchar_t* osgetenv(const tchar_t* name);
extern const tchar_t* osfprobe(const tchar_t* fn, int qf_mode);
extern void    ossetexecflag(const tchar_t* fn);
extern size_t  osstatbufsize(void);
extern int     osstat(const tchar_t *path, void *buffer);
extern time_t  osstatbufmtime(void *buffer);
extern OSHANDLE oscreatelockfile(const tchar_t *filename);
extern void    osalarm(unsigned timer);
extern long    osruntime(void);
extern long    osgctime(void);
extern double  osrtunits(void);
extern const tchar_t* ostype(void);
extern void    osgchook(int before);
/* osfprobe() modes */
#define QF_EXIST  0
#define QF_EXEC   1
#define QF_WRITE  2
#define QF_READ   4
#define QF_WARN   0x100
#define QF_WILD   0x200
/* os-dependent unlink, rename, tmpnam [?] */
extern int osunlink(const tchar_t* fn);
extern int osrename(const tchar_t* oldfn, const tchar_t* newfn);
extern tchar_t* ostmpnam(void);
/* more... */
extern void osclearwaitcursor();
extern void osrestorewaitcursor();
extern void osshowmenubar(int showp);
extern const tchar_t* osmkdefpath(const tchar_t* filename);
extern const tchar_t* osgetmap8b(const tchar_t* encoding);

#endif /* ndef __OS_H */
