/* mem.h  - memory manager */

#ifndef __MEM_H
#define __MEM_H

/* memory allocator definitions */

/* node segment structure */
typedef struct nsegment {
  struct nsegment* ns_next;       /* next node segment */
  size_t           ns_size;       /* number of nodes in this segment */
  NODE             ns_data[1];    /* segment data */
} NSEGMENT;

/* vector segment structure */
typedef struct vsegment {
  struct vsegment* vs_next;       /* next vector segment */
  SOBJ*            vs_free;       /* next free location in this segment */
  SOBJ*            vs_top;        /* top of segment (plus one) */
  SOBJ             vs_data[1];    /* segment data */
} VSEGMENT;

/** external variables **/

/* node space */
extern NSEGMENT* nsegments;         /* list of node segments */

/* vector (and string) space */
extern VSEGMENT* vsegments;         /* list of vector segments */

/* these variables used only in (collect) */
extern FIXTYPE nnodes, nfree, gccalls, total;
extern size_t  nscount, vscount;
extern FIXTYPE vgccalls;

/* macros to compute the size of a segment */
#define nsegsize(n) (sizeof(NSEGMENT)+((n)-1)*sizeof(NODE))
#define vsegsize(n) (sizeof(VSEGMENT)+((n)-1)*sizeof(SOBJ))

/* macro to convert a byte size to a word size */
#define btow_size(n) ((sv_size_t)(((n) + (sizeof(SOBJ)-1)) / sizeof(SOBJ)))

/* macro to convert a text char size to a word size */
#define ttow_size(n) ((sv_size_t)(((n*sizeof(tchar_t)) + (sizeof(SOBJ)-1)) / sizeof(SOBJ)))

/* function prototypes */
SOBJ* getvspace(SOBJ node, sv_size_t size);
NSEGMENT* newnsegment(sv_size_t n);
VSEGMENT* newvsegment(sv_size_t n);

#endif /* ndef __MEM_H */
