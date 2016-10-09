/* unichar.h  - universal characters */

#ifndef __UNICHAR_H
#define __UNICHAR_H

#define CS_ANSI 0
#define CS_WIDE 1

#if (CS == CS_WIDE)
/* Wide Characters */

#include <wchar.h>
#include <wctype.h>

typedef wchar_t tchar_t; /* 16-bit UNICODE character */
typedef wint_t tint_t;  /* int type to represent chars & eof */
#define TEOF WEOF
#define TCHAR_MIN WCHAR_MIN
#define TCHAR_MAX WCHAR_MAX
#define TCHAR_UMASK (0xFFFF) /* 16-bit char assumed! */
#define __T(quote) L##quote

#define fgettc fgetwc
#define fgetts fgetws
#define fputtc fputwc
#define fputts fputws
#define ftprintf fwprintf
#define ftscanf fwscanf
#define gettc getwc
#define gettchar getwchar
#define puttc putwc
#define puttchar putwchar
#define stprintf swprintf
#define stscanf swscanf
#define ungettc ungetwc
#define vftprintf vfwprintf
#define vstprintf vswprintf
#define vtprintf vwprintf
#define tcscat wcscat
#define tcschr wcschr
#define tcscmp wcscmp
#define tcscoll wcscoll
#define tcscpy wcscpy
#define tcscspn wcscspn
#define tcsftime wcsftime
#define tcslen wcslen
#define tcsncat wcsncat
#define tcsncmp wcsncmp
#define tcsncpy wcsncpy
#define tcspbrk wcspbrk
#define tcsrchr wcsrchr
#define tcsspn wcsspn
#define tcsstr wcsstr
#define tcstod wcstod
#define tcstok wcstok
#define tcstol wcstol
#define tcstoul wcstoul
#define tmemchr wmemchr
#define tmemcmp wmemcmp
#define tmemcpy wmemcpy
#define tmemmove wmemmove
#define tmemset wmemset
#define tprintf wprintf
#define tscanf wscanf

#if 0 /*  iswxxx are usually implemented incorrectly */
#define istalnum iswalnum
#define istalpha iswalpha
#define istcntrl iswcntrl
#define istdigit iswdigit
#define istgraph iswgraph
#define istlower iswlower
#define istprint iswprint
#define istpunct iswpunct
#define istspace iswspace
#define istupper iswupper
#define istxdigit iswxdigit
#define totlower towlower
#define totupper towupper
#else /* allow local implementation */
extern int istalnum(wint_t c);
extern int istalpha(wint_t c);
extern int istcntrl(wint_t c);
extern int istdigit(wint_t c);
extern int istgraph(wint_t c);
extern int istlower(wint_t c);
extern int istprint(wint_t c);
extern int istpunct(wint_t c);
extern int istspace(wint_t c);
extern int istupper(wint_t c);
extern int istxdigit(wint_t c);
extern wint_t totlower(wint_t c);
extern wint_t totupper(wint_t c);
#endif

#elif (CS == CS_ANSI)
/* 1-byte characters (ANSI) */

typedef char tchar_t; /* 8-bit ANSI character */
typedef int tint_t;  /* int type to represent chars & eof */
#define TCHAR_MIN CHAR_MIN
#define TCHAR_MAX CHAR_MAX
#define TCHAR_UMASK (0xFF) /* 8-bit char assumed! */
#define TEOF EOF

#define __T(quote) quote

#define fgettc fgetc
#define fgetts fgets
#define fputtc fputc
#define fputts fputs
#define ftprintf fprintf
#define ftscanf fscanf
#define gettc getc
#define gettchar getchar
#define puttc putc
#define puttchar putchar
#define stprintf sprintf
#define stscanf sscanf
#define ungettc ungetc
#define vftprintf vfprintf
#define vstprintf vsprintf
#define vtprintf vprintf
#define tcscat strcat
#define tcschr strchr
#define tcscmp strcmp
#define tcscoll strcoll
#define tcscpy strcpy
#define tcscspn strcspn
#define tcsftime strftime
#define tcslen strlen
#define tcsncat strncat
#define tcsncmp strncmp
#define tcsncpy strncpy
#define tcspbrk strpbrk
#define tcsrchr strrchr
#define tcsspn strspn
#define tcsstr strstr
#define tcstod strtod
#define tcstok strtok
#define tcstol strtol
#define tcstoul strtoul
#define tmemchr memchr
#define tmemcmp memcmp
#define tmemcpy memcpy
#define tmemmove memmove
#define tmemset memset
#define tprintf printf
#define tscanf scanf

#define istalnum isalnum
#define istalpha isalpha
#define istcntrl iscntrl
#define istdigit isdigit
#define istgraph isgraph
#define istlower islower
#define istprint isprint
#define istpunct ispunct
#define istspace isspace
#define istupper isupper
#define istxdigit isxdigit
#define totlower tolower
#define totupper toupper

#else

#error unknown CS

#endif /* CS == ... */

#define T(quote) __T(quote)

#endif /* ndef __UNICHAR_H */
