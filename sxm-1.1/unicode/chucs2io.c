/* chucs2io.c - unicode file i/o */

#include "../sxm.h"
#include "chucs2io.h"

/* opaque u_tag structure */
struct u_tag {
  FILE *fptr;
  bool_t readp;
  int ori;
  bool_t lookaheadp;
  ucsint_t lachar;
  bool_t nextcharp;
  ucsint_t nxchar;
  ucschar_t map8b[256]; /* used only if ori == ORI_XASCII */
}; /* typedef'd to u_file in utf.h */

static int _utforient(u_file *ufp, int ori)
{
  if (ufp == NULL) return ORI_UNBOUND;
  if (ori == ORI_UNBOUND) return ufp->ori;
  if (ufp->ori != ORI_UNBOUND) 
    return ufp->ori; /* cannot re-orient */
  /* ufp is not oriented yet and we need to orient it */
  /* try some orientation-dependent stuff*/
  if (ori == ORI_XASCII || ori == ORI_BINARY) {
    ufp->ori = ori;
  } else { /* fancy text stream stuff */
    const ucschar_t bom = (ucschar_t)0xFEFF;
    FILE *fp = ufp->fptr;
    /* map x-endian ori preferences to direct/reverse */
    if (ori == ORI_UCS2_FFFE)
      ori = (*((byte_t *)&bom) == 0xFF) ? ORI_UCS2 : ORI_REVERSE_UCS2;
    else if (ori == ORI_UCS2_FEFF)
      ori = (*((byte_t *)&bom) == 0xFE) ? ORI_UCS2 : ORI_REVERSE_UCS2;
    /* poke or prepare stream */
    if (ufp->readp) { 
      /* stream contents overrides preferred ori */
      int c = fgetc(fp);
      if (c == EOF) {
        ufp->ori = ORI_UTF8;
      } else if ((c & 0xFF) == 0xFF) {
        c = fgetc(fp);
        if ((c & 0xFF) == 0xFE) {
          ufp->ori = (*((byte_t *)&bom) == 0xFF) ? ORI_UCS2 : ORI_REVERSE_UCS2;
        } else { /* wrong format!!! */
          fclose(fp);
          ufp->ori = ORI_UNBOUND;
        }
      } else if ((c & 0xFF) == 0xFE) {
        c = fgetc(fp);
        if ((c & 0xFF) == 0xFF) {
          ufp->ori = (*((byte_t *)&bom) == 0xFE) ? ORI_UCS2 : ORI_REVERSE_UCS2;
        } else {
          fclose(fp);
          ufp->ori = ORI_UNBOUND;
        }
      } else {
        ungetc(c, fp);
        ufp->ori = ori;
      }
    } else { /* write */
      ufp->ori = ori;
      /* prepare stream according to ori */
      if (ufp->ori == ORI_UCS2) {
        if (fwrite(&bom, sizeof(ucschar_t), 1, ufp->fptr) != 1) {
          fclose(fp);
          ufp->ori = ORI_UNBOUND;
        }
      } else if (ufp->ori == ORI_REVERSE_UCS2) {
        const ucschar_t mob = (ucschar_t)0xFFFE;
        if (fwrite(&mob, sizeof(ucschar_t), 1, ufp->fptr) != 1) {
          fclose(fp);
          ufp->ori = ORI_UNBOUND;
        }
      }  
    }
  }
  return ufp->ori;
}


u_file* utfwrap(FILE *fp, bool_t readp, 
                int ori, const ucschar_t* pmap8b)
{
  u_file *ufp;

  if (fp == NULL)
    return NULL;

  ufp = (u_file *)malloc(sizeof(u_file));

  if (ufp == NULL) {
    return NULL;
  }

  ufp->fptr = fp;
  ufp->ori = ORI_UNBOUND;
  ufp->readp = readp;
  ufp->lookaheadp = 0;
  ufp->lachar = 0;
  ufp->nextcharp = 0;
  ufp->nxchar = 0;
  if (pmap8b != NULL) { /* copy mapping table */
    int i; for (i = 0; i < 256; i++) ufp->map8b[i] = *pmap8b++;
  } else { /* assume 7-bit ascii */
    int i; for (i = 0; i < 128; i++) ufp->map8b[i] = (ucschar_t)i;
    for (i = 128; i < 256; i++) ufp->map8b[i] = UCSUNK;
  }
 
  if (ori != ORI_UNBOUND) {
    /* a certain orientation is requested */
    if (_utforient(ufp, ori) == ORI_UNBOUND) {
      /* orientation failed! */
      free(ufp);
      return NULL;
    }  
  }

  return ufp;
}

/* this allows set ORI_XASCII on the fly */
bool_t utfmap8b(u_file *ufp, const ucschar_t* pmap8b)
{
  if (ufp == NULL || pmap8b == NULL) return FALSE;
  switch (ufp->ori) {
    case ORI_UNBOUND:
    case ORI_XASCII:
    case ORI_UTF8: {
      /* copy 8th bit mapping table */
      int i; for (i = 0; i < 256; i++) ufp->map8b[i] = *pmap8b++;
      ufp->ori = ORI_XASCII;
      return TRUE;
    } break;
    default:
      /* cannot reorient other orientations */
      return FALSE;
  }
}

FILE* utfgetfp(u_file *ufp)
{
  if (ufp == NULL) return NULL;
  return ufp->fptr;
}

FILE* utfunwrap(u_file *ufp)
{
  if (ufp == NULL) return NULL;
  else {
    FILE* fp = ufp->fptr;
    free(ufp);
    return fp;
  }
}

static bool_t _utfbind(u_file *ufp, bool_t binp, bool_t readp)
{
  if (ufp->readp != readp) {
    return FALSE;
  } else if (ufp->ori == ORI_UNBOUND) {
    /* bind it with defaults */
    _utforient(ufp, binp ? ORI_BINARY : ORI_UTF8);
    return ufp->ori != ORI_UNBOUND;
  } else if (binp) {
    return ufp->ori == ORI_BINARY;
  } else { /* text */
    return TRUE;
  }
}

static ucsint_t _utfgetc_raw(u_file *ufp)
{
  switch (ufp->ori) {
    case ORI_REVERSE_UCS2: {
      ucschar_t c;
      if (fread(&c, sizeof(ucschar_t), 1, ufp->fptr) != 1) {
        return UCSEOF;
      }
      return ((c >> 8) & 0xFF) | ((c & 0xFF) << 8);
    } break;
    case ORI_UCS2: {
      ucschar_t c;
      if (fread(&c, sizeof(ucschar_t), 1, ufp->fptr) != 1) {
        return UCSEOF;
      }
      return c;
    } break;
    case ORI_XASCII: {
      int c = fgetc(ufp->fptr);
      if (c == EOF) return UCSEOF; else c &= 0xFF;
      return (ucsint_t)ufp->map8b[c];
    } break;
    case ORI_UTF8: {
      long z, y, x, w, v, u;
      z = fgetc(ufp->fptr);
      if (z == EOF) return UCSEOF; else z &= 0xFF;
      if (z < 0xC0) { 
        return (ucsint_t)(z & 0xFFFF);
      }
      y = fgetc(ufp->fptr);
      if (y == EOF) return UCSEOF; else y &= 0xFF;
      if (z < 0xE0) {
        return (ucsint_t)((((z-0xC0) << 6) + (y-0x80)) & 0xFFFF);
      }
      x = fgetc(ufp->fptr);
      if (x == EOF) return UCSEOF; else x &= 0xFF;
      if (z < 0xF0) {
        return (ucsint_t)((((z-0xE0) << 12) + ((y-0x80) << 6) + (x-0x80)) & 0xFFFF);
      }
      w = fgetc(ufp->fptr);
      if (w == EOF) return UCSEOF; else w &= 0xFF;
      if (z < 0xF8) {
        return UCSUNK;
      }
      v = fgetc(ufp->fptr);
      if (v == EOF) return UCSEOF; else v &= 0xFF;
      if (z < 0xFC) {
        return UCSUNK;
      }
      u = fgetc(ufp->fptr);
      if (u == EOF) return UCSEOF; else u &= 0xFF;
      return UCSUNK;
    } break;
    default:
      /* boo! */
      return UCSEOF;
  }
}


ucsint_t utfgetc(u_file *ufp)
{
  ucsint_t c;
  if (ufp == NULL || !_utfbind(ufp, FALSE, TRUE)) 
    return UCSEOF;
  if (ufp->lookaheadp) {
    ufp->lookaheadp = FALSE;
    return ufp->lachar;
  } 
  if (ufp->nextcharp) {
    ufp->nextcharp = FALSE;
    return ufp->nxchar;
  } 
  c = _utfgetc_raw(ufp);
  switch (c) {
    case 0x000d: { /* cr */
      /* DOS or MAC format */
      ucsint_t c1 = _utfgetc_raw(ufp);
      if (c1 != 0x000a) { /* not lf: mac format */
        ufp->nextcharp = TRUE;
        ufp->nxchar = (c1 == 0x000d) ? (ucsint_t)'\n' : c1;
      }
      return (ucsint_t)'\n';
    } break;
    case 0x000a: /* lf */
      /* UNIX format */
      return (ucsint_t)'\n';
    default:
      return c;
  }
}

ucsint_t utfungetc(ucsint_t c, u_file *ufp)
{
  if (ufp != NULL && _utfbind(ufp, FALSE, TRUE)) {
    ufp->lachar = c;
    ufp->lookaheadp = TRUE;
  }
  return c;
}

ucsint_t _utfputc_raw(ucschar_t c, u_file *ufp)
{
  switch (ufp->ori) {
    case ORI_REVERSE_UCS2: {
      ucschar_t x = ((c >> 8) & 0xFF) | ((c & 0xFF) << 8);
      if (fwrite(&x, sizeof(ucschar_t), 1, ufp->fptr) != 1) 
        return UCSEOF;
    } break;
    case ORI_UCS2:
      if (fwrite(&c, sizeof(ucschar_t), 1, ufp->fptr) != 1)
        return UCSEOF;
      break;
    case ORI_XASCII: {
      int c8 = 0x1a; /* default is ASCII substitute char */
      { /* try to find it in the mapping table */
        int i;
        for (i = 0; i < 256; i++) {
          if (ufp->map8b[i] == c) { c8 = i; break; }
        }
      }
      if (fputc((int)c, ufp->fptr) == EOF) {
        return UCSEOF;
      }
    } break;
    case ORI_UTF8:
      if (c < 0x0080) {
        if (fputc((int)c, ufp->fptr) == EOF) {
          return UCSEOF;
        }
      } else if (c < 0x0800) {
        if ((fputc((int)(0xC0 | (0x1F & (c >> 6))), ufp->fptr) == EOF) ||
            (fputc((int)(0x80 | (0x3F & c)), ufp->fptr) == EOF)) {
          return UCSEOF;
        }
      } else if (c <= 0xFFFF) {
        if ((fputc((int)(0xE0 | (0x0F & (c >> 12))), ufp->fptr) == EOF) ||
            (fputc((int)(0x80 | (0x3F & (c >> 6))), ufp->fptr) == EOF) ||
            (fputc((int)(0x80 | (0x3F & c)), ufp->fptr) == EOF)) {
          return UCSEOF;
        }
      } else {
        /* UCS-4 comes here */
        return UCSEOF;
      }
      break;
    default:
      /* boo! */
      return UCSEOF;
  }
  return c;
}

ucsint_t utfputc(ucschar_t c, u_file *ufp)
{
  if (ufp == NULL || !_utfbind(ufp, FALSE, FALSE)) {
    return UCSEOF;
  }
  if (c == (ucschar_t)'\n') {
#if defined(_MAC)
    return _utfputc_raw((ucschar_t)'\015', ufp);
#elif defined(MSDOS)
    if (_utfputc_raw((ucschar_t)'\r', ufp) == UCSEOF) return UCSEOF;
    return _utfputc_raw((ucschar_t)'\n', ufp);
#else /* UNIX */
    return _utfputc_raw((ucschar_t)'\n', ufp);
#endif
  } else
    return _utfputc_raw(c, ufp);
}

int utfputs(ucschar_t *str, u_file *ufp)
{
  ucschar_t c;
  if (str == NULL || ufp == NULL || !_utfbind(ufp, FALSE, FALSE)) {
    return -1;
  }
  while ((c = *str++) != 0) {
    if (c == (ucschar_t)'\n') {
#if defined(_MAC)
      if (_utfputc_raw((ucschar_t)'\015', ufp) == UCSEOF) return UCSEOF;
#elif defined(MSDOS)
      if (_utfputc_raw((ucschar_t)'\r', ufp) == UCSEOF) return UCSEOF;
      if (_utfputc_raw((ucschar_t)'\n', ufp) == UCSEOF) return UCSEOF;
#else /* UNIX */
      if (_utfputc_raw((ucschar_t)'\n', ufp) == UCSEOF) return UCSEOF;
#endif
    } else {
      if (_utfputc_raw(c, ufp) == UCSEOF) return UCSEOF;
    }
  }
  return 1;
}

size_t utfread(void *buffer, size_t count, u_file *ufp)
{
  if (ufp == NULL || !_utfbind(ufp, TRUE, TRUE)) {
    return 0;
  }
  return fread(buffer, 1, count, ufp->fptr);
}

size_t utfwrite(const void *buffer, size_t count, u_file *ufp)
{
  if (ufp == NULL || !_utfbind(ufp, TRUE, FALSE)) {
    return 0;
  }
  return fwrite(buffer, 1, count, ufp->fptr);
}

void utfflush(u_file *ufp)
{
  if (ufp != NULL)
    fflush(ufp->fptr);
}


/* things to define for standalone test
#include <stdio.h>
#include <stdlib.h>

#define TRUE 1
#define FALSE 0
typedef int bool_t
typedef unsigned char byte_t;

int main (int argc, char** argv)
{
  if (argc == 3) {
    ucsint_t c;
    u_file *ufpin = utfopen(argv[1], 1);
    u_file *ufpout = utfopen(argv[2], 0);
    while ((c = utfgetc(ufpin)) != UCSEOF)
       utfputc((ucschar_t)c, ufpout);
    utfclose(ufpin);
    utfclose(ufpout);
  }
  return 0;
}
*/
