/* chucs2.h - unicode chars  */

#ifndef __CHUCS2_H
#define __CHUCS2_H

/* unicode type definitions */
typedef unsigned short ucschar_t;
typedef long ucsint_t;

/* end-of-file 'char' */
#define UCSEOF ((ucsint_t)-1)

/* non-UCS2 unicode is mapped to this char: */
#define UCSUNK ((ucsint_t)0xFFFD)

/* predicates */
extern int ucs_defined(ucsint_t);
extern int ucs_isalpha(ucsint_t);
extern int ucs_isalnum(ucsint_t);
extern int ucs_isdigit(ucsint_t);
extern int ucs_isupper(ucsint_t);
extern int ucs_islower(ucsint_t);
extern int ucs_istitle(ucsint_t);
extern int ucs_isspace(ucsint_t);
extern int ucs_ispunct(ucsint_t);
extern int ucs_iscntrl(ucsint_t);
extern int ucs_isascii(ucsint_t);

/* converters */
extern ucsint_t ucs_toupper(ucsint_t);
extern ucsint_t ucs_tolower(ucsint_t);
extern ucsint_t ucs_totitle(ucsint_t);
extern int      ucs_todigit(ucsint_t);

#endif /* ndef __CHUCS2_H */
