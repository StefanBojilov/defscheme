#line 1 "chucs2.tt"
/* chucs2.c - implementation of basic Unicode char type functions */
/* (adopted from AVP's implementation with automatic table generator) */

#include <stdlib.h>
#include <string.h>
#include "chucs2.h"

/* internal types: */
typedef unsigned short uint16;  /* must be unsigned 16 bit */
typedef uint16 ucs_char;        /* must be unsigned 16 bit */
typedef int ucs_int;            /* must be longer than 16 bit */

/*
#ident "@(#) $Id$  [table generator]"
#line 15 "chucs2.tt"
static const char unicode_table[] = "UNIDATA2.TXT";
#line 16 "chucs2.tt"
*/

/** inplementation: **/
#define UCS_HIBYTE(uc)  ((uc) >> 8)
#define UCS_LOBYTE(uc)  ((uc) & 0xff)
typedef struct {
  ucs_char from;       /* starting char of the range (inclusive) */
  ucs_char to;         /* ending char of the range (inclusive) */
  uint16   arg1;       /* generator argument # 1 */
  uint16   arg2;       /* generator argument # 2 */
  uint16    memoize_p;
} packed_table;

/** inplementation: I predicates **/
/* local masks */
#define UCS_EXIST 1
#define UCS_CNTRL 2
#define UCS_ALPHA 4
#define UCS_DIGIT 8
#define UCS_SPACE 16
#define UCS_PUNCT 32
#define UCS_UPPER 64
#define UCS_LOWER 128
#define UCS_TITLE 256
#line 32 "chucs2.tt"

typedef unsigned short pmask_t;  /* enough bits to hold all masks */

#define FUN_PREDICATE(name,mask)                        \
   int ucs_##name(ucsint_t uc)                          \
   {                                                    \
     return fun_predicate((ucs_char)uc, (mask));        \
   }

/* local stuff */
static pmask_t *UCS_predicate[256];
static const packed_table TBL_predicate[] = { /* will be constructed */
   {0x0000, 0x0008, 3, 3, 1},
   {0x0009, 0x000d, 19, 19, 1},
   {0x000e, 0x001f, 3, 3, 1},
   {0x0020, 0x0021, 17, 33, 1},
   {0x0022, 0x0023, 33, 33, 1},
   {0x0024, 0x0025, 1, 33, 1},
   {0x0026, 0x002a, 33, 33, 1},
   {0x002b, 0x002c, 33, 1, 1},
   {0x002d, 0x002f, 33, 33, 1},
   {0x0030, 0x0039, 9, 9, 1},
   {0x003a, 0x003b, 33, 33, 1},
   {0x003c, 0x003e, 1, 1, 1},
   {0x003f, 0x0040, 33, 33, 1},
   {0x0041, 0x005a, 69, 69, 1},
   {0x005b, 0x005d, 33, 33, 1},
   {0x005e, 0x0060, 1, 33, 1},
   {0x0061, 0x007a, 133, 133, 1},
   {0x007b, 0x007e, 1, 33, 1},
   {0x007f, 0x009f, 3, 3, 1},
   {0x00a0, 0x00a1, 17, 33, 1},
   {0x00a2, 0x00a9, 1, 1, 1},
   {0x00aa, 0x00ab, 133, 33, 1},
   {0x00ac, 0x00ae, 1, 33, 1},
   {0x00af, 0x00b4, 1, 1, 1},
   {0x00b5, 0x00b6, 1, 133, 1},
   {0x00b7, 0x00b8, 1, 33, 1},
   {0x00b9, 0x00ba, 133, 1, 1},
   {0x00bb, 0x00bc, 1, 33, 1},
   {0x00bd, 0x00be, 1, 1, 1},
   {0x00bf, 0x00c0, 69, 33, 1},
   {0x00c1, 0x00d6, 69, 69, 1},
   {0x00d7, 0x00d8, 69, 1, 1},
   {0x00d9, 0x00de, 69, 69, 1},
   {0x00df, 0x00f6, 133, 133, 1},
   {0x00f7, 0x00f8, 133, 1, 1},
   {0x00f9, 0x00ff, 133, 133, 1},
   {0x0100, 0x0137, 69, 133, 1},
   {0x0138, 0x0148, 133, 69, 1},
   {0x0149, 0x0178, 69, 133, 1},
   {0x0179, 0x017e, 133, 69, 1},
   {0x017f, 0x0180, 133, 133, 1},
   {0x0181, 0x0182, 69, 69, 1},
   {0x0183, 0x0186, 69, 133, 1},
   {0x0187, 0x0189, 133, 69, 1},
   {0x018a, 0x018b, 69, 69, 1},
   {0x018c, 0x018d, 133, 133, 1},
   {0x018e, 0x0191, 69, 69, 1},
   {0x0192, 0x0193, 133, 69, 1},
   {0x0194, 0x0196, 69, 133, 1},
   {0x0197, 0x0198, 69, 69, 1},
   {0x0199, 0x019b, 133, 133, 1},
   {0x019c, 0x019d, 69, 69, 1},
   {0x019e, 0x019f, 133, 69, 1},
   {0x01a0, 0x01a6, 69, 133, 1},
   {0x01a7, 0x01a9, 133, 69, 1},
   {0x01aa, 0x01ab, 5, 133, 1},
   {0x01ac, 0x01ae, 69, 133, 1},
   {0x01af, 0x01b1, 133, 69, 1},
   {0x01b2, 0x01b3, 69, 69, 1},
   {0x01b4, 0x01b7, 133, 69, 1},
   {0x01b8, 0x01b9, 69, 133, 1},
   {0x01ba, 0x01bb, 133, 5, 1},
   {0x01bc, 0x01bd, 69, 133, 1},
   {0x01be, 0x01c3, 5, 5, 1},
   {0x01c4, 0x01c5, 69, 261, 1},
   {0x01c6, 0x01c7, 133, 69, 1},
   {0x01c8, 0x01c9, 261, 133, 1},
   {0x01ca, 0x01cb, 69, 261, 1},
   {0x01cc, 0x01dc, 133, 69, 1},
   {0x01dd, 0x01ef, 69, 133, 1},
   {0x01f0, 0x01f1, 133, 69, 1},
   {0x01f2, 0x01f3, 261, 133, 1},
   {0x01f4, 0x01f5, 69, 133, 1},
   {0x01fa, 0x0217, 69, 133, 1},
   {0x0250, 0x02a8, 133, 133, 1},
   {0x02b0, 0x02b8, 5, 5, 1},
   {0x02b9, 0x02ba, 1, 1, 1},
   {0x02bb, 0x02c1, 5, 5, 1},
   {0x02c2, 0x02cf, 1, 1, 1},
   {0x02d0, 0x02d1, 5, 5, 1},
   {0x02d2, 0x02de, 1, 1, 1},
   {0x02e0, 0x02e4, 5, 5, 1},
   {0x02e5, 0x02e9, 1, 1, 1},
   {0x0300, 0x0345, 1, 1, 1},
   {0x0360, 0x0361, 1, 1, 1},
   {0x0374, 0x0375, 33, 33, 1},
   {0x037a, 0x037a, 5, 5, 1},
   {0x037e, 0x037e, 33, 33, 1},
   {0x0384, 0x0385, 1, 1, 1},
   {0x0386, 0x0388, 69, 33, 1},
   {0x0389, 0x038a, 69, 69, 1},
   {0x038c, 0x038c, 69, 69, 1},
   {0x038e, 0x038f, 69, 69, 1},
   {0x0390, 0x0391, 133, 69, 1},
   {0x0392, 0x03a1, 69, 69, 1},
   {0x03a3, 0x03ab, 69, 69, 1},
   {0x03ac, 0x03ce, 133, 133, 1},
   {0x03d0, 0x03d1, 133, 133, 1},
   {0x03d2, 0x03d4, 69, 69, 1},
   {0x03d5, 0x03d6, 133, 133, 1},
   {0x03da, 0x03da, 69, 69, 1},
   {0x03dc, 0x03dc, 69, 69, 1},
   {0x03de, 0x03de, 69, 69, 1},
   {0x03e0, 0x03e0, 69, 69, 1},
   {0x03e2, 0x03ef, 69, 133, 1},
   {0x03f0, 0x03f2, 133, 133, 1},
   {0x03f3, 0x03f3, 5, 5, 1},
   {0x0401, 0x040c, 69, 69, 1},
   {0x040e, 0x042f, 69, 69, 1},
   {0x0430, 0x044f, 133, 133, 1},
   {0x0451, 0x045c, 133, 133, 1},
   {0x045e, 0x045f, 133, 133, 1},
   {0x0460, 0x0481, 69, 133, 1},
   {0x0482, 0x0486, 1, 1, 1},
   {0x0490, 0x04bf, 69, 133, 1},
   {0x04c0, 0x04c1, 5, 69, 1},
   {0x04c2, 0x04c4, 133, 69, 1},
   {0x04c7, 0x04c8, 133, 69, 1},
   {0x04cb, 0x04cc, 133, 69, 1},
   {0x04d0, 0x04eb, 69, 133, 1},
   {0x04ee, 0x04f5, 69, 133, 1},
   {0x04f8, 0x04f9, 69, 133, 1},
   {0x0531, 0x0556, 69, 69, 0},
   {0x0559, 0x055a, 33, 5, 0},
   {0x055b, 0x055f, 33, 33, 0},
   {0x0561, 0x0587, 133, 133, 0},
   {0x0589, 0x0589, 33, 33, 0},
   {0x0591, 0x05a1, 1, 1, 0},
   {0x05a3, 0x05b9, 1, 1, 0},
   {0x05bb, 0x05bd, 1, 1, 0},
   {0x05be, 0x05c1, 33, 1, 0},
   {0x05c2, 0x05c4, 1, 33, 0},
   {0x05d0, 0x05ea, 5, 5, 0},
   {0x05f0, 0x05f2, 5, 5, 0},
   {0x05f3, 0x05f4, 33, 33, 0},
   {0x060c, 0x060c, 33, 33, 0},
   {0x061b, 0x061b, 33, 33, 0},
   {0x061f, 0x061f, 33, 33, 0},
   {0x0621, 0x063a, 5, 5, 0},
   {0x0640, 0x064a, 5, 5, 0},
   {0x064b, 0x0652, 1, 1, 0},
   {0x0660, 0x0669, 9, 9, 0},
   {0x066a, 0x066d, 33, 33, 0},
   {0x0670, 0x0671, 1, 5, 0},
   {0x0672, 0x06b7, 5, 5, 0},
   {0x06ba, 0x06be, 5, 5, 0},
   {0x06c0, 0x06ce, 5, 5, 0},
   {0x06d0, 0x06d3, 5, 5, 0},
   {0x06d4, 0x06d5, 33, 5, 0},
   {0x06d6, 0x06e4, 1, 1, 0},
   {0x06e5, 0x06e6, 5, 5, 0},
   {0x06e7, 0x06ed, 1, 1, 0},
   {0x06f0, 0x06f9, 9, 9, 0},
   {0x0901, 0x0903, 1, 1, 0},
   {0x0905, 0x0939, 5, 5, 0},
   {0x093c, 0x093e, 1, 5, 0},
   {0x093f, 0x094d, 1, 1, 0},
   {0x0950, 0x0954, 1, 1, 0},
   {0x0958, 0x0961, 5, 5, 0},
   {0x0962, 0x0963, 1, 1, 0},
   {0x0964, 0x0965, 33, 33, 0},
   {0x0966, 0x096f, 9, 9, 0},
   {0x0970, 0x0970, 33, 33, 0},
   {0x0981, 0x0983, 1, 1, 0},
   {0x0985, 0x098c, 5, 5, 0},
   {0x098f, 0x0990, 5, 5, 0},
   {0x0993, 0x09a8, 5, 5, 0},
   {0x09aa, 0x09b0, 5, 5, 0},
   {0x09b2, 0x09b2, 5, 5, 0},
   {0x09b6, 0x09b9, 5, 5, 0},
   {0x09bc, 0x09bc, 1, 1, 0},
   {0x09be, 0x09c4, 1, 1, 0},
   {0x09c7, 0x09c8, 1, 1, 0},
   {0x09cb, 0x09cd, 1, 1, 0},
   {0x09d7, 0x09d7, 1, 1, 0},
   {0x09dc, 0x09dd, 5, 5, 0},
   {0x09df, 0x09e1, 5, 5, 0},
   {0x09e2, 0x09e3, 1, 1, 0},
   {0x09e6, 0x09ef, 9, 9, 0},
   {0x09f0, 0x09f1, 5, 5, 0},
   {0x09f2, 0x09fa, 1, 1, 0},
   {0x0a02, 0x0a02, 1, 1, 0},
   {0x0a05, 0x0a0a, 5, 5, 0},
   {0x0a0f, 0x0a10, 5, 5, 0},
   {0x0a13, 0x0a28, 5, 5, 0},
   {0x0a2a, 0x0a30, 5, 5, 0},
   {0x0a32, 0x0a33, 5, 5, 0},
   {0x0a35, 0x0a36, 5, 5, 0},
   {0x0a38, 0x0a39, 5, 5, 0},
   {0x0a3c, 0x0a3c, 1, 1, 0},
   {0x0a3e, 0x0a42, 1, 1, 0},
   {0x0a47, 0x0a48, 1, 1, 0},
   {0x0a4b, 0x0a4d, 1, 1, 0},
   {0x0a59, 0x0a5c, 5, 5, 0},
   {0x0a5e, 0x0a5e, 5, 5, 0},
   {0x0a66, 0x0a6f, 9, 9, 0},
   {0x0a70, 0x0a71, 1, 1, 0},
   {0x0a72, 0x0a74, 5, 5, 0},
   {0x0a81, 0x0a83, 1, 1, 0},
   {0x0a85, 0x0a8b, 5, 5, 0},
   {0x0a8d, 0x0a8d, 5, 5, 0},
   {0x0a8f, 0x0a91, 5, 5, 0},
   {0x0a93, 0x0aa8, 5, 5, 0},
   {0x0aaa, 0x0ab0, 5, 5, 0},
   {0x0ab2, 0x0ab3, 5, 5, 0},
   {0x0ab5, 0x0ab9, 5, 5, 0},
   {0x0abc, 0x0abe, 1, 5, 0},
   {0x0abf, 0x0ac5, 1, 1, 0},
   {0x0ac7, 0x0ac9, 1, 1, 0},
   {0x0acb, 0x0acd, 1, 1, 0},
   {0x0ad0, 0x0ad0, 1, 1, 0},
   {0x0ae0, 0x0ae0, 5, 5, 0},
   {0x0ae6, 0x0aef, 9, 9, 0},
   {0x0b01, 0x0b03, 1, 1, 0},
   {0x0b05, 0x0b0c, 5, 5, 0},
   {0x0b0f, 0x0b10, 5, 5, 0},
   {0x0b13, 0x0b28, 5, 5, 0},
   {0x0b2a, 0x0b30, 5, 5, 0},
   {0x0b32, 0x0b33, 5, 5, 0},
   {0x0b36, 0x0b39, 5, 5, 0},
   {0x0b3c, 0x0b3e, 1, 5, 0},
   {0x0b3f, 0x0b43, 1, 1, 0},
   {0x0b47, 0x0b48, 1, 1, 0},
   {0x0b4b, 0x0b4d, 1, 1, 0},
   {0x0b56, 0x0b57, 1, 1, 0},
   {0x0b5c, 0x0b5d, 5, 5, 0},
   {0x0b5f, 0x0b61, 5, 5, 0},
   {0x0b66, 0x0b6f, 9, 9, 0},
   {0x0b70, 0x0b70, 1, 1, 0},
   {0x0b82, 0x0b83, 1, 1, 0},
   {0x0b85, 0x0b8a, 5, 5, 0},
   {0x0b8e, 0x0b90, 5, 5, 0},
   {0x0b92, 0x0b95, 5, 5, 0},
   {0x0b99, 0x0b9a, 5, 5, 0},
   {0x0b9c, 0x0b9c, 5, 5, 0},
   {0x0b9e, 0x0b9f, 5, 5, 0},
   {0x0ba3, 0x0ba4, 5, 5, 0},
   {0x0ba8, 0x0baa, 5, 5, 0},
   {0x0bae, 0x0bb5, 5, 5, 0},
   {0x0bb7, 0x0bb9, 5, 5, 0},
   {0x0bbe, 0x0bc2, 1, 1, 0},
   {0x0bc6, 0x0bc8, 1, 1, 0},
   {0x0bca, 0x0bcd, 1, 1, 0},
   {0x0bd7, 0x0bd7, 1, 1, 0},
   {0x0be7, 0x0bef, 9, 9, 0},
   {0x0bf0, 0x0bf2, 1, 1, 0},
   {0x0c01, 0x0c03, 1, 1, 0},
   {0x0c05, 0x0c0c, 5, 5, 0},
   {0x0c0e, 0x0c10, 5, 5, 0},
   {0x0c12, 0x0c28, 5, 5, 0},
   {0x0c2a, 0x0c33, 5, 5, 0},
   {0x0c35, 0x0c39, 5, 5, 0},
   {0x0c3e, 0x0c44, 1, 1, 0},
   {0x0c46, 0x0c48, 1, 1, 0},
   {0x0c4a, 0x0c4d, 1, 1, 0},
   {0x0c55, 0x0c56, 1, 1, 0},
   {0x0c60, 0x0c61, 5, 5, 0},
   {0x0c66, 0x0c6f, 9, 9, 0},
   {0x0c82, 0x0c83, 1, 1, 0},
   {0x0c85, 0x0c8c, 5, 5, 0},
   {0x0c8e, 0x0c90, 5, 5, 0},
   {0x0c92, 0x0ca8, 5, 5, 0},
   {0x0caa, 0x0cb3, 5, 5, 0},
   {0x0cb5, 0x0cb9, 5, 5, 0},
   {0x0cbe, 0x0cc4, 1, 1, 0},
   {0x0cc6, 0x0cc8, 1, 1, 0},
   {0x0cca, 0x0ccd, 1, 1, 0},
   {0x0cd5, 0x0cd6, 1, 1, 0},
   {0x0cde, 0x0cde, 5, 5, 0},
   {0x0ce0, 0x0ce1, 5, 5, 0},
   {0x0ce6, 0x0cef, 9, 9, 0},
   {0x0d02, 0x0d03, 1, 1, 0},
   {0x0d05, 0x0d0c, 5, 5, 0},
   {0x0d0e, 0x0d10, 5, 5, 0},
   {0x0d12, 0x0d28, 5, 5, 0},
   {0x0d2a, 0x0d39, 5, 5, 0},
   {0x0d3e, 0x0d43, 1, 1, 0},
   {0x0d46, 0x0d48, 1, 1, 0},
   {0x0d4a, 0x0d4d, 1, 1, 0},
   {0x0d57, 0x0d57, 1, 1, 0},
   {0x0d60, 0x0d61, 5, 5, 0},
   {0x0d66, 0x0d6f, 9, 9, 0},
   {0x0e01, 0x0e2e, 5, 5, 0},
   {0x0e2f, 0x0e30, 5, 33, 0},
   {0x0e31, 0x0e32, 5, 1, 0},
   {0x0e33, 0x0e34, 1, 5, 0},
   {0x0e35, 0x0e3a, 1, 1, 0},
   {0x0e3f, 0x0e40, 5, 1, 0},
   {0x0e41, 0x0e46, 5, 5, 0},
   {0x0e47, 0x0e4f, 1, 1, 0},
   {0x0e50, 0x0e59, 9, 9, 0},
   {0x0e5a, 0x0e5b, 33, 33, 0},
   {0x0e81, 0x0e82, 5, 5, 0},
   {0x0e84, 0x0e84, 5, 5, 0},
   {0x0e87, 0x0e88, 5, 5, 0},
   {0x0e8a, 0x0e8a, 5, 5, 0},
   {0x0e8d, 0x0e8d, 5, 5, 0},
   {0x0e94, 0x0e97, 5, 5, 0},
   {0x0e99, 0x0e9f, 5, 5, 0},
   {0x0ea1, 0x0ea3, 5, 5, 0},
   {0x0ea5, 0x0ea5, 5, 5, 0},
   {0x0ea7, 0x0ea7, 5, 5, 0},
   {0x0eaa, 0x0eab, 5, 5, 0},
   {0x0ead, 0x0eae, 5, 5, 0},
   {0x0eaf, 0x0eb0, 5, 33, 0},
   {0x0eb1, 0x0eb2, 5, 1, 0},
   {0x0eb3, 0x0eb4, 1, 5, 0},
   {0x0eb5, 0x0eb9, 1, 1, 0},
   {0x0ebb, 0x0ebc, 1, 1, 0},
   {0x0ebd, 0x0ebd, 5, 5, 0},
   {0x0ec0, 0x0ec4, 5, 5, 0},
   {0x0ec6, 0x0ec6, 5, 5, 0},
   {0x0ec8, 0x0ecd, 1, 1, 0},
   {0x0ed0, 0x0ed9, 9, 9, 0},
   {0x0edc, 0x0edd, 5, 5, 0},
   {0x0f00, 0x0f03, 1, 1, 0},
   {0x0f04, 0x0f12, 33, 33, 0},
   {0x0f13, 0x0f1f, 1, 1, 0},
   {0x0f20, 0x0f29, 9, 9, 0},
   {0x0f2a, 0x0f39, 1, 1, 0},
   {0x0f3a, 0x0f3d, 33, 33, 0},
   {0x0f3e, 0x0f3f, 1, 1, 0},
   {0x0f40, 0x0f47, 5, 5, 0},
   {0x0f49, 0x0f69, 5, 5, 0},
   {0x0f71, 0x0f84, 1, 1, 0},
   {0x0f85, 0x0f86, 1, 33, 0},
   {0x0f87, 0x0f8b, 1, 1, 0},
   {0x0f90, 0x0f95, 1, 1, 0},
   {0x0f97, 0x0f97, 1, 1, 0},
   {0x0f99, 0x0fad, 1, 1, 0},
   {0x0fb1, 0x0fb7, 1, 1, 0},
   {0x0fb9, 0x0fb9, 1, 1, 0},
   {0x10a0, 0x10c5, 69, 69, 0},
   {0x10d0, 0x10f6, 133, 133, 0},
   {0x10fb, 0x10fb, 33, 33, 0},
   {0x1100, 0x1159, 5, 5, 0},
   {0x115f, 0x11a2, 5, 5, 0},
   {0x11a8, 0x11f9, 5, 5, 0},
   {0x1e00, 0x1e95, 69, 133, 0},
   {0x1e96, 0x1e9b, 133, 133, 0},
   {0x1ea0, 0x1ef9, 69, 133, 0},
   {0x1f00, 0x1f07, 133, 133, 0},
   {0x1f08, 0x1f0f, 69, 69, 0},
   {0x1f10, 0x1f15, 133, 133, 0},
   {0x1f18, 0x1f1d, 69, 69, 0},
   {0x1f20, 0x1f27, 133, 133, 0},
   {0x1f28, 0x1f2f, 69, 69, 0},
   {0x1f30, 0x1f37, 133, 133, 0},
   {0x1f38, 0x1f3f, 69, 69, 0},
   {0x1f40, 0x1f45, 133, 133, 0},
   {0x1f48, 0x1f4d, 69, 69, 0},
   {0x1f50, 0x1f57, 133, 133, 0},
   {0x1f59, 0x1f59, 69, 69, 0},
   {0x1f5b, 0x1f5b, 69, 69, 0},
   {0x1f5d, 0x1f5d, 69, 69, 0},
   {0x1f5f, 0x1f60, 133, 69, 0},
   {0x1f61, 0x1f67, 133, 133, 0},
   {0x1f68, 0x1f6f, 69, 69, 0},
   {0x1f70, 0x1f7d, 133, 133, 0},
   {0x1f80, 0x1f87, 133, 133, 0},
   {0x1f88, 0x1f8f, 69, 69, 0},
   {0x1f90, 0x1f97, 133, 133, 0},
   {0x1f98, 0x1f9f, 69, 69, 0},
   {0x1fa0, 0x1fa7, 133, 133, 0},
   {0x1fa8, 0x1faf, 69, 69, 0},
   {0x1fb0, 0x1fb4, 133, 133, 0},
   {0x1fb6, 0x1fb7, 133, 133, 0},
   {0x1fb8, 0x1fbc, 69, 69, 0},
   {0x1fbd, 0x1fbf, 69, 1, 0},
   {0x1fc0, 0x1fc1, 1, 1, 0},
   {0x1fc2, 0x1fc4, 133, 133, 0},
   {0x1fc6, 0x1fc7, 133, 133, 0},
   {0x1fc8, 0x1fcc, 69, 69, 0},
   {0x1fcd, 0x1fcf, 1, 1, 0},
   {0x1fd0, 0x1fd3, 133, 133, 0},
   {0x1fd6, 0x1fd7, 133, 133, 0},
   {0x1fd8, 0x1fdb, 69, 69, 0},
   {0x1fdd, 0x1fdf, 1, 1, 0},
   {0x1fe0, 0x1fe7, 133, 133, 0},
   {0x1fe8, 0x1fec, 69, 69, 0},
   {0x1fed, 0x1fef, 1, 1, 0},
   {0x1ff2, 0x1ff4, 133, 133, 0},
   {0x1ff6, 0x1ff7, 133, 133, 0},
   {0x1ff8, 0x1ffc, 69, 69, 0},
   {0x1ffd, 0x1ffe, 1, 1, 0},
   {0x2000, 0x200b, 17, 17, 0},
   {0x200c, 0x200f, 1, 1, 0},
   {0x2010, 0x2027, 33, 33, 0},
   {0x2028, 0x2029, 17, 17, 0},
   {0x202a, 0x202e, 1, 1, 0},
   {0x2030, 0x2043, 33, 33, 0},
   {0x2044, 0x2045, 1, 33, 0},
   {0x2046, 0x2046, 33, 33, 0},
   {0x206a, 0x2070, 1, 1, 0},
   {0x2074, 0x207c, 1, 1, 0},
   {0x207d, 0x207e, 33, 33, 0},
   {0x207f, 0x2080, 1, 133, 0},
   {0x2081, 0x208c, 1, 1, 0},
   {0x208d, 0x208e, 33, 33, 0},
   {0x20a0, 0x20ab, 1, 1, 0},
   {0x20d0, 0x20e1, 1, 1, 0},
   {0x2100, 0x2101, 1, 1, 0},
   {0x2102, 0x2103, 69, 1, 0},
   {0x2104, 0x2106, 1, 1, 0},
   {0x2107, 0x2108, 1, 69, 0},
   {0x2109, 0x210a, 133, 1, 0},
   {0x210b, 0x210d, 69, 69, 0},
   {0x210e, 0x210f, 133, 133, 0},
   {0x2110, 0x2112, 69, 69, 0},
   {0x2113, 0x2114, 1, 133, 0},
   {0x2115, 0x2116, 1, 69, 0},
   {0x2117, 0x2118, 69, 1, 0},
   {0x2119, 0x211d, 69, 69, 0},
   {0x211e, 0x2123, 1, 1, 0},
   {0x2124, 0x212a, 69, 1, 0},
   {0x212b, 0x212d, 69, 69, 0},
   {0x212e, 0x212f, 133, 133, 0},
   {0x2130, 0x2131, 69, 69, 0},
   {0x2132, 0x2133, 1, 69, 0},
   {0x2134, 0x2135, 133, 5, 0},
   {0x2136, 0x2138, 5, 5, 0},
   {0x2153, 0x2182, 1, 1, 0},
   {0x2190, 0x21ea, 1, 1, 0},
   {0x2200, 0x22f1, 1, 1, 0},
   {0x2300, 0x2300, 1, 1, 0},
   {0x2302, 0x2328, 1, 1, 0},
   {0x2329, 0x232a, 33, 33, 0},
   {0x232b, 0x237a, 1, 1, 0},
   {0x2400, 0x2424, 1, 1, 0},
   {0x2440, 0x244a, 1, 1, 0},
   {0x2460, 0x24ea, 1, 1, 0},
   {0x2500, 0x2595, 1, 1, 0},
   {0x25a0, 0x25ef, 1, 1, 0},
   {0x2600, 0x2613, 1, 1, 0},
   {0x261a, 0x266f, 1, 1, 0},
   {0x2701, 0x2704, 1, 1, 0},
   {0x2706, 0x2709, 1, 1, 0},
   {0x270c, 0x2727, 1, 1, 0},
   {0x2729, 0x274b, 1, 1, 0},
   {0x274d, 0x274d, 1, 1, 0},
   {0x274f, 0x2752, 1, 1, 0},
   {0x2756, 0x2756, 1, 1, 0},
   {0x2758, 0x275e, 1, 1, 0},
   {0x2761, 0x2767, 1, 1, 0},
   {0x2776, 0x2794, 1, 1, 0},
   {0x2798, 0x27af, 1, 1, 0},
   {0x27b1, 0x27be, 1, 1, 0},
   {0x3000, 0x3001, 17, 33, 0},
   {0x3002, 0x3003, 33, 33, 0},
   {0x3004, 0x3005, 1, 5, 0},
   {0x3006, 0x3008, 33, 1, 0},
   {0x3009, 0x3011, 33, 33, 0},
   {0x3012, 0x3013, 1, 1, 0},
   {0x3014, 0x301f, 33, 33, 0},
   {0x3020, 0x302f, 1, 1, 0},
   {0x3030, 0x3031, 33, 5, 0},
   {0x3032, 0x3035, 5, 5, 0},
   {0x3036, 0x3037, 1, 1, 0},
   {0x303f, 0x303f, 1, 1, 0},
   {0x3041, 0x3094, 5, 5, 0},
   {0x3099, 0x309a, 1, 1, 0},
   {0x309b, 0x309e, 5, 5, 0},
   {0x30a1, 0x30fa, 5, 5, 0},
   {0x30fb, 0x30fc, 5, 33, 0},
   {0x30fd, 0x30fe, 5, 5, 0},
   {0x3105, 0x312c, 5, 5, 0},
   {0x3131, 0x318e, 5, 5, 0},
   {0x3190, 0x319f, 1, 1, 0},
   {0x3200, 0x321c, 1, 1, 0},
   {0x3220, 0x3243, 1, 1, 0},
   {0x3260, 0x327b, 1, 1, 0},
   {0x327f, 0x32b0, 1, 1, 0},
   {0x32c0, 0x32cb, 1, 1, 0},
   {0x32d0, 0x32fe, 1, 1, 0},
   {0x3300, 0x3376, 1, 1, 0},
   {0x337b, 0x33dd, 1, 1, 0},
   {0x33e0, 0x33fe, 1, 1, 0},
   {0x4e00, 0x4e00, 5, 5, 0},
   {0x9fa5, 0x9fa5, 5, 5, 0},
   {0xac00, 0xac00, 5, 5, 0},
   {0xd7a3, 0xd7a3, 5, 5, 0},
   {0xd800, 0xd800, 3, 3, 0},
   {0xdb7f, 0xdb80, 3, 3, 0},
   {0xdbff, 0xdc00, 3, 3, 0},
   {0xdfff, 0xe000, 3, 3, 0},
   {0xf8ff, 0xf900, 5, 3, 0},
   {0xfa2d, 0xfa2d, 5, 5, 0},
   {0xfb00, 0xfb06, 133, 133, 0},
   {0xfb13, 0xfb17, 133, 133, 0},
   {0xfb1e, 0xfb1f, 1, 5, 0},
   {0xfb20, 0xfb28, 5, 5, 0},
   {0xfb29, 0xfb2a, 5, 1, 0},
   {0xfb2b, 0xfb36, 5, 5, 0},
   {0xfb38, 0xfb3c, 5, 5, 0},
   {0xfb3e, 0xfb3e, 5, 5, 0},
   {0xfb40, 0xfb41, 5, 5, 0},
   {0xfb43, 0xfb44, 5, 5, 0},
   {0xfb46, 0xfbb1, 5, 5, 0},
   {0xfbd3, 0xfd3d, 5, 5, 0},
   {0xfd3e, 0xfd3f, 33, 33, 0},
   {0xfd50, 0xfd8f, 5, 5, 0},
   {0xfd92, 0xfdc7, 5, 5, 0},
   {0xfdf0, 0xfdfb, 5, 5, 0},
   {0xfe20, 0xfe23, 1, 1, 0},
   {0xfe30, 0xfe44, 33, 33, 0},
   {0xfe49, 0xfe52, 33, 33, 0},
   {0xfe54, 0xfe61, 33, 33, 0},
   {0xfe62, 0xfe64, 1, 33, 0},
   {0xfe65, 0xfe66, 1, 1, 0},
   {0xfe68, 0xfe6a, 33, 1, 0},
   {0xfe6b, 0xfe6b, 33, 33, 0},
   {0xfe70, 0xfe72, 5, 5, 0},
   {0xfe74, 0xfe74, 5, 5, 0},
   {0xfe76, 0xfefc, 5, 5, 0},
   {0xfeff, 0xfeff, 1, 1, 0},
   {0xff01, 0xff03, 33, 33, 0},
   {0xff04, 0xff05, 1, 33, 0},
   {0xff06, 0xff0a, 33, 33, 0},
   {0xff0b, 0xff0c, 33, 1, 0},
   {0xff0d, 0xff0f, 33, 33, 0},
   {0xff10, 0xff19, 9, 9, 0},
   {0xff1a, 0xff1b, 33, 33, 0},
   {0xff1c, 0xff1e, 1, 1, 0},
   {0xff1f, 0xff20, 33, 33, 0},
   {0xff21, 0xff3a, 69, 69, 0},
   {0xff3b, 0xff3d, 33, 33, 0},
   {0xff3e, 0xff40, 1, 33, 0},
   {0xff41, 0xff5a, 133, 133, 0},
   {0xff5b, 0xff5e, 1, 33, 0},
   {0xff61, 0xff65, 33, 33, 0},
   {0xff66, 0xffbe, 5, 5, 0},
   {0xffc2, 0xffc7, 5, 5, 0},
   {0xffca, 0xffcf, 5, 5, 0},
   {0xffd2, 0xffd7, 5, 5, 0},
   {0xffda, 0xffdc, 5, 5, 0},
   {0xffe0, 0xffe6, 1, 1, 0},
   {0xffe8, 0xffee, 1, 1, 0},
   {0xfffd, 0xfffd, 1, 1, 0}
#line 45 "chucs2.tt"
};

/* find which stripe uc belong in the table if size  elements.
 * return -1 if not found
 */
static int find_stripe(const packed_table table[], int size, ucs_char uc)
{
  int low, high;

  /* find the stripe */
  low = 0;
  high = size - 1;

  if ((uc < table[low].from) ||
      (uc > table[high].to)) {
    return -1;
  }

  if (uc <= table[low].to)
    return low;
  else if (uc >= table[high].from)
    return high;
  else for (;;) {
    int stripe = (low + high) / 2;

    if (stripe == low)
      return -1;

    if (uc < table[stripe].from) {
      high = stripe;
    } else if (uc > table[stripe].to) {
      low = stripe;
    } else {
      return stripe;
    }
  }
}

static pmask_t build_predicate(ucs_char uc)
{
  int stripe_count = sizeof (TBL_predicate) / sizeof(TBL_predicate[0]);
  int stripe = find_stripe(TBL_predicate, stripe_count, uc);

  /* now, TBL[stripe] contains the uc mask */

  /* actually, we are more lazy than a sloth: if the char does not belong to
   * a stripe, the page will not be built
   */
  if (stripe == -1)
    return 0;

  /* depending on the constructor of the stripe ...*/
  if (TBL_predicate[stripe].memoize_p) {
    pmask_t *page = (pmask_t *) malloc(sizeof(pmask_t) * 256);
    ucs_char uc0= uc & ~0x00ff;
    int k;

    /* if not enough memory, calculate the value at the run time */
    if (page == 0)
      goto run_time;

    /* empty all predicates */
    UCS_predicate[UCS_HIBYTE(uc)] = page;
    memset((void *)page, 0, sizeof(pmask_t) * 256);

    /* find the first stripe of the page */
    while ((stripe > 0) &&
           (TBL_predicate[stripe].from > uc0))
      stripe--;

    /* fill the table */
    for (k = 0; (stripe < stripe_count) && (k < 256);) {
      if (uc0 > TBL_predicate[stripe].to) {
        stripe++;
      } else if (uc0 < TBL_predicate[stripe].from) {
        uc0++, k++;
      } else {
        if ((uc0 & 1) == 0) /* even */
          page[k] =  TBL_predicate[stripe].arg1;
        else /* odd */
          page[k] =  TBL_predicate[stripe].arg2;
        uc0++; k++;
      }
    }

    return page[UCS_LOBYTE(uc)];
  }

  /* catch allocation failures here */
run_time:

  if ((uc & 1) == 0) /* even */
    return TBL_predicate[stripe].arg1;
  else /* odd */
    return TBL_predicate[stripe].arg2;
}

/* universal predicate lookup */
static int fun_predicate(ucs_char uc, pmask_t mask) 
{
  pmask_t *pr = UCS_predicate[UCS_HIBYTE(uc)];

  if (pr) {
    return (mask & pr[UCS_LOBYTE(uc)]);
  } else {
    return (mask & build_predicate(uc));
  }
}


/* public functions */
FUN_PREDICATE(defined, UCS_EXIST)
FUN_PREDICATE(isdigit, UCS_DIGIT)
FUN_PREDICATE(isalpha, UCS_ALPHA)
FUN_PREDICATE(isalnum, UCS_DIGIT | UCS_ALPHA)
FUN_PREDICATE(islower, UCS_LOWER)
FUN_PREDICATE(isupper, UCS_UPPER)
FUN_PREDICATE(istitle, UCS_TITLE)
FUN_PREDICATE(isspace, UCS_SPACE)
FUN_PREDICATE(ispunct, UCS_PUNCT)
FUN_PREDICATE(iscntrl, UCS_CNTRL)

int ucs_isascii(ucsint_t uc)
{
  return ((ucs_char)uc < 128);
}

/****************************************************************************/
#define FUN_CONVERTER(name)                                                 \
   ucsint_t ucs_to##name(ucsint_t uc)                                       \
   {                                                                        \
     return (ucsint_t)fun_caseconv((ucs_char)uc, name##_cache, name##_table,\
                         sizeof (name##_table) / sizeof (name##_table[0])); \
   }

/*** conversion tables **/
static ucs_char *upper_cache[256];
static const packed_table upper_table[] = {
   {0x0061, 0x007a, 65504, 65504, 1},
   {0x00e0, 0x00f6, 65504, 65504, 1},
   {0x00f8, 0x00fe, 65504, 65504, 1},
   {0x00ff, 0x0100, 0, 121, 1},
   {0x0101, 0x0130, 0, 65535, 1},
   {0x0131, 0x0132, 0, 65304, 1},
   {0x0133, 0x0138, 0, 65535, 1},
   {0x013a, 0x0149, 65535, 0, 1},
   {0x014b, 0x0178, 0, 65535, 1},
   {0x017a, 0x017e, 65535, 0, 1},
   {0x017f, 0x0180, 0, 65236, 1},
   {0x0183, 0x0186, 0, 65535, 1},
   {0x0188, 0x0189, 65535, 0, 1},
   {0x018c, 0x018d, 65535, 0, 1},
   {0x0192, 0x0193, 65535, 0, 1},
   {0x0199, 0x019a, 0, 65535, 1},
   {0x01a1, 0x01a6, 0, 65535, 1},
   {0x01a8, 0x01a9, 65535, 0, 1},
   {0x01ad, 0x01ae, 0, 65535, 1},
   {0x01b0, 0x01b1, 65535, 0, 1},
   {0x01b4, 0x01b7, 65535, 0, 1},
   {0x01b9, 0x01ba, 0, 65535, 1},
   {0x01bd, 0x01be, 0, 65535, 1},
   {0x01c5, 0x01c6, 65534, 65535, 1},
   {0x01c8, 0x01c9, 65535, 65534, 1},
   {0x01cb, 0x01cc, 65534, 65535, 1},
   {0x01ce, 0x01dd, 65535, 0, 1},
   {0x01df, 0x01f0, 0, 65535, 1},
   {0x01f2, 0x01f3, 65535, 65534, 1},
   {0x01f5, 0x01f6, 0, 65535, 1},
   {0x01fb, 0x0218, 0, 65535, 1},
   {0x0253, 0x0254, 65330, 65326, 1},
   {0x0256, 0x0257, 65331, 65331, 1},
   {0x0258, 0x0259, 65334, 65334, 1},
   {0x025b, 0x025c, 0, 65333, 1},
   {0x0260, 0x0261, 65331, 0, 1},
   {0x0263, 0x0264, 0, 65329, 1},
   {0x0268, 0x0269, 65327, 65325, 1},
   {0x026f, 0x0270, 0, 65325, 1},
   {0x0272, 0x0273, 65323, 0, 1},
   {0x0283, 0x0284, 0, 65318, 1},
   {0x0288, 0x0289, 65318, 0, 1},
   {0x028a, 0x028b, 65319, 65319, 1},
   {0x0292, 0x0293, 65317, 0, 1},
   {0x03ac, 0x03ad, 65498, 65499, 1},
   {0x03ae, 0x03af, 65499, 65499, 1},
   {0x03b1, 0x03c1, 65504, 65504, 1},
   {0x03c3, 0x03cb, 65504, 65504, 1},
   {0x03cc, 0x03cd, 65472, 65473, 1},
   {0x03ce, 0x03cf, 65473, 0, 1},
   {0x03d0, 0x03d1, 65474, 65479, 1},
   {0x03d5, 0x03d6, 65482, 65489, 1},
   {0x03e3, 0x03ef, 0, 65535, 1},
   {0x03f0, 0x03f1, 65450, 65456, 1},
   {0x0430, 0x044f, 65504, 65504, 1},
   {0x0451, 0x045c, 65456, 65456, 1},
   {0x045e, 0x045f, 65456, 65456, 1},
   {0x0461, 0x0482, 0, 65535, 1},
   {0x0491, 0x04c0, 0, 65535, 1},
   {0x04c2, 0x04c5, 65535, 0, 1},
   {0x04c8, 0x04c9, 65535, 0, 1},
   {0x04cc, 0x04cd, 65535, 0, 1},
   {0x04d1, 0x04ec, 0, 65535, 1},
   {0x04ef, 0x04f6, 0, 65535, 1},
   {0x04f9, 0x04fa, 0, 65535, 1},
   {0x0561, 0x0586, 65488, 65488, 0},
   {0x1e01, 0x1e96, 0, 65535, 0},
   {0x1ea1, 0x1efa, 0, 65535, 0},
   {0x1f00, 0x1f07, 8, 8, 0},
   {0x1f10, 0x1f15, 8, 8, 0},
   {0x1f20, 0x1f27, 8, 8, 0},
   {0x1f30, 0x1f37, 8, 8, 0},
   {0x1f40, 0x1f45, 8, 8, 0},
   {0x1f51, 0x1f58, 0, 8, 0},
   {0x1f60, 0x1f67, 8, 8, 0},
   {0x1f70, 0x1f71, 74, 74, 0},
   {0x1f72, 0x1f75, 86, 86, 0},
   {0x1f76, 0x1f77, 100, 100, 0},
   {0x1f78, 0x1f79, 128, 128, 0},
   {0x1f7a, 0x1f7b, 112, 112, 0},
   {0x1f7c, 0x1f7d, 126, 126, 0},
   {0x1f80, 0x1f87, 8, 8, 0},
   {0x1f90, 0x1f97, 8, 8, 0},
   {0x1fa0, 0x1fa7, 8, 8, 0},
   {0x1fb0, 0x1fb1, 8, 8, 0},
   {0x1fb3, 0x1fb4, 0, 9, 0},
   {0x1fc3, 0x1fc4, 0, 9, 0},
   {0x1fd0, 0x1fd1, 8, 8, 0},
   {0x1fe0, 0x1fe1, 8, 8, 0},
   {0x1fe5, 0x1fe6, 0, 7, 0},
   {0x1ff3, 0x1ff4, 0, 9, 0},
   {0x2170, 0x217f, 65520, 65520, 0},
   {0x24d0, 0x24e9, 65510, 65510, 0},
   {0xff41, 0xff5a, 65504, 65504, 0}
#line 184 "chucs2.tt"
};

static ucs_char *lower_cache[256];
static const packed_table lower_table[] = {
   {0x0041, 0x005a, 32, 32, 1},
   {0x00c0, 0x00d6, 32, 32, 1},
   {0x00d8, 0x00de, 32, 32, 1},
   {0x0100, 0x012f, 1, 0, 1},
   {0x0130, 0x0131, 65337, 0, 1},
   {0x0132, 0x0137, 1, 0, 1},
   {0x0139, 0x0148, 0, 1, 1},
   {0x014a, 0x0177, 1, 0, 1},
   {0x0178, 0x0179, 65415, 1, 1},
   {0x017b, 0x017e, 0, 1, 1},
   {0x0181, 0x0182, 1, 210, 1},
   {0x0184, 0x0185, 1, 0, 1},
   {0x0186, 0x0187, 206, 1, 1},
   {0x0189, 0x018a, 205, 205, 1},
   {0x018b, 0x018c, 0, 1, 1},
   {0x018e, 0x018f, 202, 202, 1},
   {0x0190, 0x0191, 203, 1, 1},
   {0x0193, 0x0194, 207, 205, 1},
   {0x0196, 0x0197, 211, 209, 1},
   {0x0198, 0x0199, 1, 0, 1},
   {0x019c, 0x019d, 211, 213, 1},
   {0x01a0, 0x01a5, 1, 0, 1},
   {0x01a7, 0x01a8, 0, 1, 1},
   {0x01a9, 0x01aa, 0, 218, 1},
   {0x01ac, 0x01ad, 1, 0, 1},
   {0x01ae, 0x01af, 218, 1, 1},
   {0x01b1, 0x01b2, 217, 217, 1},
   {0x01b3, 0x01b6, 0, 1, 1},
   {0x01b7, 0x01b8, 1, 219, 1},
   {0x01bc, 0x01bd, 1, 0, 1},
   {0x01c4, 0x01c5, 2, 1, 1},
   {0x01c7, 0x01c8, 1, 2, 1},
   {0x01ca, 0x01cb, 2, 1, 1},
   {0x01cd, 0x01dc, 0, 1, 1},
   {0x01de, 0x01ef, 1, 0, 1},
   {0x01f1, 0x01f2, 1, 2, 1},
   {0x01f4, 0x01f5, 1, 0, 1},
   {0x01fa, 0x0217, 1, 0, 1},
   {0x0386, 0x0387, 38, 0, 1},
   {0x0388, 0x038a, 37, 37, 1},
   {0x038c, 0x038d, 64, 0, 1},
   {0x038e, 0x038f, 63, 63, 1},
   {0x0391, 0x03a1, 32, 32, 1},
   {0x03a3, 0x03ab, 32, 32, 1},
   {0x03e2, 0x03ef, 1, 0, 1},
   {0x0401, 0x040c, 80, 80, 1},
   {0x040e, 0x040f, 80, 80, 1},
   {0x0410, 0x042f, 32, 32, 1},
   {0x0460, 0x0481, 1, 0, 1},
   {0x0490, 0x04bf, 1, 0, 1},
   {0x04c1, 0x04c4, 0, 1, 1},
   {0x04c7, 0x04c8, 0, 1, 1},
   {0x04cb, 0x04cc, 0, 1, 1},
   {0x04d0, 0x04eb, 1, 0, 1},
   {0x04ee, 0x04f5, 1, 0, 1},
   {0x04f8, 0x04f9, 1, 0, 1},
   {0x0531, 0x0556, 48, 48, 0},
   {0x10a0, 0x10c5, 48, 48, 0},
   {0x1e00, 0x1e95, 1, 0, 0},
   {0x1ea0, 0x1ef9, 1, 0, 0},
   {0x1f08, 0x1f0f, 65528, 65528, 0},
   {0x1f18, 0x1f1d, 65528, 65528, 0},
   {0x1f28, 0x1f2f, 65528, 65528, 0},
   {0x1f38, 0x1f3f, 65528, 65528, 0},
   {0x1f48, 0x1f4d, 65528, 65528, 0},
   {0x1f59, 0x1f60, 0, 65528, 0},
   {0x1f68, 0x1f6f, 65528, 65528, 0},
   {0x1f88, 0x1f8f, 65528, 65528, 0},
   {0x1f98, 0x1f9f, 65528, 65528, 0},
   {0x1fa8, 0x1faf, 65528, 65528, 0},
   {0x1fb8, 0x1fb9, 65528, 65528, 0},
   {0x1fba, 0x1fbb, 65462, 65462, 0},
   {0x1fbc, 0x1fbd, 65527, 0, 0},
   {0x1fc8, 0x1fcb, 65450, 65450, 0},
   {0x1fcc, 0x1fcd, 65527, 0, 0},
   {0x1fd8, 0x1fd9, 65528, 65528, 0},
   {0x1fda, 0x1fdb, 65436, 65436, 0},
   {0x1fe8, 0x1fe9, 65528, 65528, 0},
   {0x1fea, 0x1feb, 65424, 65424, 0},
   {0x1fec, 0x1fed, 65529, 0, 0},
   {0x1ff8, 0x1ff9, 65408, 65408, 0},
   {0x1ffa, 0x1ffb, 65410, 65410, 0},
   {0x1ffc, 0x1ffd, 65527, 0, 0},
   {0x2160, 0x216f, 16, 16, 0},
   {0x24b6, 0x24cf, 26, 26, 0},
   {0xff21, 0xff3a, 32, 32, 0}
#line 189 "chucs2.tt"
};

static ucs_char *title_cache[256];
static const packed_table title_table[] = {
   {0x0061, 0x007a, 65504, 65504, 1},
   {0x00e0, 0x00f6, 65504, 65504, 1},
   {0x00f8, 0x00fe, 65504, 65504, 1},
   {0x00ff, 0x0100, 0, 121, 1},
   {0x0101, 0x0130, 0, 65535, 1},
   {0x0131, 0x0132, 0, 65304, 1},
   {0x0133, 0x0138, 0, 65535, 1},
   {0x013a, 0x0149, 65535, 0, 1},
   {0x014b, 0x0178, 0, 65535, 1},
   {0x017a, 0x017e, 65535, 0, 1},
   {0x017f, 0x0180, 0, 65236, 1},
   {0x0183, 0x0186, 0, 65535, 1},
   {0x0188, 0x0189, 65535, 0, 1},
   {0x018c, 0x018d, 65535, 0, 1},
   {0x0192, 0x0193, 65535, 0, 1},
   {0x0199, 0x019a, 0, 65535, 1},
   {0x01a1, 0x01a6, 0, 65535, 1},
   {0x01a8, 0x01a9, 65535, 0, 1},
   {0x01ad, 0x01ae, 0, 65535, 1},
   {0x01b0, 0x01b1, 65535, 0, 1},
   {0x01b4, 0x01b7, 65535, 0, 1},
   {0x01b9, 0x01ba, 0, 65535, 1},
   {0x01bd, 0x01be, 0, 65535, 1},
   {0x01c4, 0x01c5, 1, 0, 1},
   {0x01c6, 0x01c7, 65535, 1, 1},
   {0x01c9, 0x01ca, 1, 65535, 1},
   {0x01cc, 0x01dd, 65535, 0, 1},
   {0x01df, 0x01f0, 0, 65535, 1},
   {0x01f1, 0x01f2, 0, 1, 1},
   {0x01f3, 0x01f6, 0, 65535, 1},
   {0x01fb, 0x0218, 0, 65535, 1},
   {0x0253, 0x0254, 65330, 65326, 1},
   {0x0256, 0x0257, 65331, 65331, 1},
   {0x0258, 0x0259, 65334, 65334, 1},
   {0x025b, 0x025c, 0, 65333, 1},
   {0x0260, 0x0261, 65331, 0, 1},
   {0x0263, 0x0264, 0, 65329, 1},
   {0x0268, 0x0269, 65327, 65325, 1},
   {0x026f, 0x0270, 0, 65325, 1},
   {0x0272, 0x0273, 65323, 0, 1},
   {0x0283, 0x0284, 0, 65318, 1},
   {0x0288, 0x0289, 65318, 0, 1},
   {0x028a, 0x028b, 65319, 65319, 1},
   {0x0292, 0x0293, 65317, 0, 1},
   {0x03ac, 0x03ad, 65498, 65499, 1},
   {0x03ae, 0x03af, 65499, 65499, 1},
   {0x03b1, 0x03c1, 65504, 65504, 1},
   {0x03c3, 0x03cb, 65504, 65504, 1},
   {0x03cc, 0x03cd, 65472, 65473, 1},
   {0x03ce, 0x03cf, 65473, 0, 1},
   {0x03e3, 0x03f0, 0, 65535, 1},
   {0x0430, 0x044f, 65504, 65504, 1},
   {0x0451, 0x045c, 65456, 65456, 1},
   {0x045e, 0x045f, 65456, 65456, 1},
   {0x0461, 0x0482, 0, 65535, 1},
   {0x0491, 0x04c0, 0, 65535, 1},
   {0x04c2, 0x04c5, 65535, 0, 1},
   {0x04c8, 0x04c9, 65535, 0, 1},
   {0x04cc, 0x04cd, 65535, 0, 1},
   {0x04d1, 0x04ec, 0, 65535, 1},
   {0x04ef, 0x04f6, 0, 65535, 1},
   {0x04f9, 0x04fa, 0, 65535, 1},
   {0x0561, 0x0586, 65488, 65488, 0},
   {0x1e01, 0x1e96, 0, 65535, 0},
   {0x1ea1, 0x1efa, 0, 65535, 0},
   {0x1f00, 0x1f07, 8, 8, 0},
   {0x1f10, 0x1f15, 8, 8, 0},
   {0x1f20, 0x1f27, 8, 8, 0},
   {0x1f30, 0x1f37, 8, 8, 0},
   {0x1f40, 0x1f45, 8, 8, 0},
   {0x1f51, 0x1f58, 0, 8, 0},
   {0x1f60, 0x1f67, 8, 8, 0},
   {0x1f70, 0x1f71, 74, 74, 0},
   {0x1f72, 0x1f75, 86, 86, 0},
   {0x1f76, 0x1f77, 100, 100, 0},
   {0x1f78, 0x1f79, 128, 128, 0},
   {0x1f7a, 0x1f7b, 112, 112, 0},
   {0x1f7c, 0x1f7d, 126, 126, 0},
   {0x1f80, 0x1f87, 8, 8, 0},
   {0x1f90, 0x1f97, 8, 8, 0},
   {0x1fa0, 0x1fa7, 8, 8, 0},
   {0x1fb0, 0x1fb1, 8, 8, 0},
   {0x1fb3, 0x1fb4, 0, 9, 0},
   {0x1fc3, 0x1fc4, 0, 9, 0},
   {0x1fd0, 0x1fd1, 8, 8, 0},
   {0x1fe0, 0x1fe1, 8, 8, 0},
   {0x1fe5, 0x1fe6, 0, 7, 0},
   {0x1ff3, 0x1ff4, 0, 9, 0},
   {0x2170, 0x217f, 65520, 65520, 0},
   {0x24d0, 0x24e9, 65510, 65510, 0},
   {0xff41, 0xff5a, 65504, 65504, 0}
#line 194 "chucs2.tt"
};

/* case translation */
static ucs_char build_caseconv(ucs_char uc,
                               ucs_char *cache[],
                               const packed_table *pack,
                               int pack_size)
{
  int stripe = find_stripe(pack, pack_size, uc);

  /* now, pack[stripe] contains the uc mask */

  /* actually, we are more lazy than a sloth: if the char does not belong to
   * a stripe, the page will not be built
   */
  if (stripe == -1)
    return uc;

  /* depending on the constructor of the stripe ...*/
  if (pack[stripe].memoize_p) {
    ucs_char *page = (ucs_char *) malloc(sizeof (ucs_char) * 256);
    ucs_char uc0= uc & ~0x00ff;
    int k;

    /* if not enough memory, calculate the value at the run time */
    if (page == 0)
      goto run_time;

    /* empty all predicates */
    cache[UCS_HIBYTE(uc)] = page;
    for (k = 256; k--;) {
      page[k] = uc0 + k;
    }

    /* find the first stripe of the page */
    while ((stripe > 0) &&
           (pack[stripe].from > uc0))
      stripe--;

    /* fill the table */
    for (k = 0; (stripe < pack_size) && (k < 256);) {
      if (uc0 > pack[stripe].to) {
        stripe++;
      } else if (uc0 < pack[stripe].from) {
        uc0++, k++;
      } else {
        if ((uc0 & 1) == 0) /* even */
          page[k] =  uc0 + pack[stripe].arg1;
        else /* odd */
          page[k] =  uc0 + pack[stripe].arg2;
        uc0++; k++;
      }
    }

    return page[UCS_LOBYTE(uc)];
  }

  /* catch allocation failures here */
run_time:

  if ((uc & 1) == 0) /* even */
    return (uc + pack[stripe].arg1);
  else /* odd */
    return (uc + pack[stripe].arg2);
}

/* universal converter lookup */
static ucs_char fun_caseconv(ucs_char uc,
                             ucs_char *cache[],
                             const packed_table *pack,
                             int pack_size) 
{
  ucs_char *pr = cache[UCS_HIBYTE(uc)];
 
  if (pr) {
    return pr[UCS_LOBYTE(uc)];
  } else {
    return build_caseconv(uc, cache, pack, pack_size);
  }
}
 
FUN_CONVERTER(lower)
FUN_CONVERTER(upper)
FUN_CONVERTER(title)

/****************************************************************************/
static signed char *UCS_digit[256];
static const packed_table TBL_digit[] = { /* will be constructed */
   {0x0030, 0x0039, 48, 48, 1},
   {0x0041, 0x005a, 55, 55, 1},
   {0x0061, 0x007a, 87, 87, 1},
   {0x0660, 0x0669, 1632, 1632, 0},
   {0x06f0, 0x06f9, 1776, 1776, 0},
   {0x0966, 0x096f, 2406, 2406, 0},
   {0x09e6, 0x09ef, 2534, 2534, 0},
   {0x0a66, 0x0a6f, 2662, 2662, 0},
   {0x0ae6, 0x0aef, 2790, 2790, 0},
   {0x0b66, 0x0b6f, 2918, 2918, 0},
   {0x0be7, 0x0bef, 3046, 3046, 0},
   {0x0c66, 0x0c6f, 3174, 3174, 0},
   {0x0ce6, 0x0cef, 3302, 3302, 0},
   {0x0d66, 0x0d6f, 3430, 3430, 0},
   {0x0e50, 0x0e59, 3664, 3664, 0},
   {0x0ed0, 0x0ed9, 3792, 3792, 0},
   {0x0f20, 0x0f29, 3872, 3872, 0},
   {0xff10, 0xff19, 65296, 65296, 0}
#line 283 "chucs2.tt"
};

#define NONDIGIT -1
#define DIGIT_STRIPE_COUNT (sizeof (TBL_digit) / sizeof (TBL_digit[0]))

int ucs_todigit(ucsint_t ui)
{
  int low, high;
  int stripe;
  ucs_char uc = (ucs_char)ui;
  signed char *pr = UCS_digit[UCS_HIBYTE(uc)];
  int k;

  if (pr) {
    return pr[UCS_LOBYTE(uc)];
  } 

  /* find the stripe */
  low = 0;
  high = DIGIT_STRIPE_COUNT - 1;

  if ((uc < TBL_digit[low].from) ||
      (uc > TBL_digit[high].to)) {
    return NONDIGIT;
  } else if (uc <= TBL_digit[low].to) {
    stripe = low;
  } else if (uc >= TBL_digit[high].from) {
    stripe = high;
  } else for (;;) {
    stripe = (low + high) / 2;

    if (stripe == low) {
      return NONDIGIT;
    } else if (uc < TBL_digit[stripe].from) {
      high = stripe;
    } else if (uc > TBL_digit[stripe].to) {
      low = stripe;
    } else {
      break;
    }
  }

  /* now, TBL[stripe] contains the uc mask */

  /* depending on the constructor of the stripe ...*/
  if (TBL_digit[stripe].memoize_p) {
    signed char *page = (signed char *) malloc(sizeof (signed char) * 256);
    ucs_char uc0= uc & ~0x00ff;

    /* if not enough memory, calculate the value at the run time */
    if (page == 0)
      goto run_time;

    /* empty all predicates */
    UCS_digit[UCS_HIBYTE(uc)] = page;
    memset((void *)page, (signed char)NONDIGIT, sizeof(signed char) * 256);

    /* find the first stripe of the page */
    while ((stripe > 0) &&
           (TBL_digit[stripe].from > uc0))
      stripe--;
    
    /* fill the table */
    for (k = 0; (stripe < DIGIT_STRIPE_COUNT) && (k < 256);) {
      if (uc0 > TBL_digit[stripe].to) {
        stripe++;
      } else if (uc0 < TBL_digit[stripe].from) {
        uc0++, k++;
      } else {
        page[k] =  uc0 - TBL_digit[stripe].arg1;
        uc0++, k++;
      }
    }

    return page[UCS_LOBYTE(uc)];
  }

  /* catch allocation failures here */
run_time:

  return (uc - TBL_digit[stripe].arg1);
}

/****************************************************************************/
/** memory control **/
/* to check for memory leaks, it's convenient to remove all memory
   that is accounted for before exit */

void ucs_fini(void)
{
  int i;

  for (i = 256; i--;) {
    /* predicate pages */
    if (UCS_predicate[i]) {
      free(UCS_predicate[i]);
      UCS_predicate[i] = 0;
    }
    
    /* digit table */
    if (UCS_digit[i]) {
      free(UCS_digit[i]);
      UCS_digit[i] = 0;
    }
    
    /* case tables */
    if (lower_cache[i]) {
      free(lower_cache[i]);
      lower_cache[i] = 0;
    }
    if (title_cache[i]) {
      free(title_cache[i]);
      title_cache[i] = 0;
    }
    if (upper_cache[i]) {
      free(upper_cache[i]);
      upper_cache[i] = 0;
    }    
  }
}


/* memory is tight, please remove all unnecessary pages */
void ucs_compact(void)
{
  /* this is a stupid version, we simply remove all pages */
  ucs_fini();
}

#ifdef  __cplusplus
// automatically compact on program exit
static struct _compacter {
  ~_compacter() { ucs_fini(); }
} _c;
#endif
