/* chucs2io.h - unicode i/o  */

#ifndef __CHUCS2IO_H
#define __CHUCS2IO_H

#include "chucs2.h"

#define ORI_BINARY      -1
#define ORI_UNBOUND      0
#define ORI_XASCII       1
#define ORI_UTF8         2
#define ORI_UCS2         3
#define ORI_REVERSE_UCS2 4
#define ORI_UCS2_FFFE    5
#define ORI_UCS2_FEFF    6

typedef struct u_tag u_file;

extern u_file* utfwrap(FILE *fp, bool_t readp, 
                       int oripref, const ucschar_t* pmap8b);
extern FILE* utfgetfp(u_file *ufp);
extern FILE* utfunwrap(u_file *ufp);
extern bool_t utfmap8b(u_file *ufp, const ucschar_t* pmap8b);
extern ucsint_t utfgetc(u_file *ufp);
extern ucsint_t utfungetc(ucsint_t c, u_file *ufp);
extern ucsint_t utfputc(ucschar_t c, u_file *ufp);
extern int utfputs(ucschar_t *str, u_file *ufp);
extern size_t utfread(void *buffer, size_t count, u_file *ufp);
extern size_t utfwrite(const void *buffer, size_t count, u_file *ufp);
extern void utfflush(u_file *ufp);

#endif /* ndef __CHUCS2IO_H */
