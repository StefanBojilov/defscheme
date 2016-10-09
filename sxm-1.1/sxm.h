/* sxm.h - global definitions for sxm */

#ifndef __SXM_H
#define __SXM_H

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <limits.h>
#include <setjmp.h>
#include <assert.h>
#include <time.h>

/* debug options */
#ifdef _DEBUG
/* turn on _DEBUG_GC for some serious debugging! */
/* #define _DEBUG_GC */
#else
#undef _DEBUG_GC
#endif

/* universal characters (ANSI/UNICODE) */
#include "unichar.h"

/* for byte-size integers, use byte_t or sbyte_t */
typedef signed char sbyte_t;
typedef unsigned char byte_t;
typedef char cchar_t;

/* little extra typing for booleans */
#ifdef __cplusplus
typedef bool bool_t;
#ifndef TRUE
#define TRUE    true
#define FALSE   false
#endif
#else /* C */
#ifndef TRUE
#define TRUE    1
#define FALSE   0
#endif
typedef int bool_t;
#endif

/* to make sure noone uses char anymore... */
#define char c_h_a_r

/* gcc and friends are too smart for their own good;
 * we need to undef macros expanding to references to char */
#ifdef memset
#undef memset
#endif
#ifdef strchr
#undef strchr
#endif
#ifdef strcmp
#undef strcmp
#endif
#ifdef strcpy
#undef strcpy
#endif
#ifdef strncpy
#undef strncpy
#endif
#ifdef strstr
#undef strstr
#endif
/* ... add more if your compiler is smarter than gcc ... */

/* the program banner */
#define BANNER  (T("SXM (CXEMA) Version 1.1 ") T(__DATE__))
#define COPYRT  (T("Copyright (c) 1990-2001 Sergei Egorov"))

/* SYSTEM- AND COMPILER-SPECIFIC DEFINITIONS  */
/* <define>     <description> (<default>)     */

/* NONREC_GC    GC: non-recursive mark */
/* INDEXPC      VM: program counter is int index, not pointer () */
/* OFFTYPE      unsigned number the size of an address (unsigned long) */
/* INTPTYPE     signed number the size of an address (long) */
/* FIXTYPE      data type for fixed point numbers (long) */
/* FIXMIN       most negative fixnum (LONG_MIN) */
/* FIXMAX       most positive fixnum (LONG_MAX) */
/* FLOTYPE      data type for floating point numbers (float) */
/* FIXFORMAT    printf format for FIXTYPE ("%ld") */
/* le2bint_t    little-endian two-byte integer type if any () */
/* INSEGMENT    test for pointer in heap-allocated node segment (generic) */
/* MSDOS        MSDOS () */
/* SFIXMIN      minimal small fixnum (-0x3FFFFFFF) */
/* SFIXMAX      maximal small fixnum ( 0x3FFFFFFF) */
/* SX_NSSIZE    number of nodes in node segment (4000) */
/* SX_VSSIZE    number of pointers in vector segment (8000) */
/* OSHANDLE     number the size of an OS (Mswin/Mac?) handle (long) */

/* #define USEINT64 */

/* for Microsoft C compilers */
#ifdef _MSC_VER
#define OFFTYPE         unsigned long
#define INTPTYPE        long
#ifdef USEINT64
#define FIXTYPE         __int64
#define FIXMIN          (-9223372036854775807i64-1i64)
#define FIXMAX          (9223372036854775807i64)
#define FIXFORMAT       "%I64d"
#endif
#define OSHANDLE        long
#define le2bint_t       short
#ifdef WIN32
// flat memory: use default INSEGMENT 
#else  // win16
#define INSEGMENT(n,s)  (((OFFTYPE)(n) >> 16) == ((OFFTYPE)(s) >> 16))
#pragma warning(disable : 4759 4049)
#endif // win16
#define MSDOS           1
#define OFF_LSB_PART(i) ((short)(i))
#endif


/* for the GNU C++ compiler */
#ifdef __GNUC__
#define OFFTYPE         unsigned long
#define INTPTYPE        long
#define FIXTYPE         long
#define FIXMIN          (LONG_MIN)
#define FIXMAX          (LONG_MAX)
#define FIXFORMAT       "%ld"
#define OSHANDLE        long
#ifdef MSDOS
#define le2bint_t       short
#define OFF_LSB_PART(i) ((short)(i))
#endif
/*#define cdecl*/
#endif

/* size of each type of memory segment */
#ifndef SX_NSSIZE
#define SX_NSSIZE 4000    /* number of nodes per node segment */
#endif
#ifndef SX_VSSIZE
 #ifdef _DEBUG_GC /* increase SX_VSSIZE for better vec mem debugging */
 #define SX_VSSIZE 80000    /* number of SOBJ's per vector segment */
 #else
 #define SX_VSSIZE 8000    /* number of SOBJ's per vector segment */
 #endif
#endif

/* memory allocator parameters (assumes that size_t is long or longer) */
#define SX_MAX_VECTOR_ELTS ((long)(LONG_MAX/sizeof(SOBJ)) - 1L)
#define SX_MAX_STRING_CHARS ((long)(LONG_MAX/sizeof(tchar_t)) - 1L)
#define SX_MAX_BVECTOR_BYTES ((long)LONG_MAX - 1L)

#define SX_MAXSYMBOL    100     /* max length of symbolic name for read */
#define SX_STSIZE       199     /* symbol hash table size  */
#define SX_KTSIZE       19      /* keyword hash table size */

/* program parameters (should die!) */
#define STRMAX          100     /* maximum length of a printname */

/* VECTORs and STRINGs shares the same segments so their size_types should
 * be the same (and limited by size_t). We separate them only for clarity.
 */
typedef size_t sv_size_t; /* VECTOR size and index types */
typedef size_t ss_size_t; /* STRING size and index types */

#ifndef OFFTYPE
#define OFFTYPE         unsigned long
#endif
#ifndef INTPTYPE
#define INTPTYPE        long
#endif
#ifndef FIXTYPE
#define FIXTYPE         long
#define FIXMIN          (LONG_MIN)
#define FIXMAX          (LONG_MAX)
#define FIXFORMAT       "%ld"
#endif
#ifndef FLOTYPE
#define FLOTYPE         double
#endif
#ifndef SFIXMIN
#define SFIXMIN         (-(FIXTYPE)0x40000000L)   /* -1048576L */
#define SFIXMAX         ( (FIXTYPE)0x3FFFFFFFL)   /*  1048575L */
#endif
#ifndef INSEGMENT
#define INSEGMENT(n, s) ((n) >= &(s)->ns_data[0] \
                      && (n) <  &(s)->ns_data[0] + (s)->ns_size)
#endif

#ifndef OSHANDLE
#define OSHANDLE        long
#endif

/* macros to cut compiler warnings */
#define never_returns(x) return (x)

/* cast to least significant bits of OFFTYPE (no cast by default) */
#ifndef OFF_LSB_PART
#define OFF_LSB_PART(i) (i)
#endif

#define LSB_PART(x)     OFF_LSB_PART((OFFTYPE)(x))

/* node check for debug (purify mode) */
#ifdef _DEBUG_GC
#define cknode(s) _checknode(s)
#define ckvdata(p) _checkvdata(p)
#define cksdata(p) _checksdata(p)
#define ckbdata(p) _checkbdata(p)
#else
#define cknode(s) (s)
#define ckvdata(p) (p)
#define cksdata(p) (p)
#define ckbdata(p) (p)
#endif

/* stack manipulation macros */
#define check(n)        ((vmsp - (n) < vmstkbase) ? vm_stkover() : 0)
#define cpush(v)        ((vmsp <= vmstkbase) ? vm_stkover() : push(v))
#define push(v)         (*--vmsp = cknode(v))
#define pushsref(v)     (--vmsp, *vmsp = vmsp[(v)+1])
#define pop()           (*vmsp++)
#define top()           (*vmsp)
#define settop(v)       (*vmsp = (v))
#define drop(n)         (vmsp += (n))
#define drop1()         (++vmsp)

/* argument list parsing macros */
#define moreargs()      (vmargc > 0)
#define nextarg()       (--vmargc, *vmsp++)
#define testarg(e)      ((vmargc <= 0) ? sxae_toofew() : (e))
#define xlgetarg()      (testarg(nextarg()))
#define xllastarg()     ((vmargc != 0) ? sxae_toomany() : 0)
#define xlpoprest()     (vmsp += vmargc)
#define typearg(tp, t)  (tp(*vmsp) ? nextarg() : sxae_type(*vmsp, t))
#define xlonearg()      ((vmargc != 1) ? sxae_justone() : *vmsp++)
#define optarg()        (moreargs() ? \
                           (*vmsp == so_default ? (--vmargc, ++vmsp,FALSE) : TRUE)\
                           : FALSE)

/* macros to get arguments of a particular type */
#define xlgacons()      (testarg(typearg(consp, NT_CONS)))
#define xlgasymbol()    (testarg(typearg(symbolp, NT_SYMBOL)))
#define xlgakeyword()   (testarg(typearg(keywordp, NT_KEYWORD)))
#define xlgahashtable() (testarg(typearg(hashtablep, NT_HASHTABLE)))
#define xlgarecord()    (testarg(typearg(recordp, NT_RECORD)))
#define xlgabox()       (testarg(typearg(boxp, NT_BOX)))
#define xlgastring()    (testarg(typearg(stringp, NT_STRING)))
#define xlgabvector()   (testarg(typearg(bvectorp, NT_BYTEVEC)))
#define xlgasfixnum()   (testarg(typearg(sfixp, NT_FIXNUM)))
#define xlgafixnum()    (testarg(typearg(fixp, NT_FIXNUM)))
#define xlgachar()      (testarg(typearg(charp, NT_CHAR)))
#define xlgavector()    (testarg(typearg(vectorp, NT_VECTOR)))
#define xlgaport()      (testarg(typearg(portp, NT_PORT)))
#define xlgaclosure()   (testarg(typearg(closurep, NT_CLOSURE)))
#define xlgahandle()    (testarg(typearg(handlep, NT_HANDLE)))
#define xlgagcell()     (testarg(typearg(gcellp, NT_GCELL)))

#define xlgaprocedure() (testarg(typearg(procedurep, NT__PROC)))
#define xlreqproc(x)    sxae_type(x, NT__PROC)
#define xlganumber()    (testarg(typearg(numberp, NT__NUMBER)))
#define xlreqnumber(x)  sxae_type(x, NT__NUMBER)
#define xlreqfixnum(x)  sxae_type(x, NT_FIXNUM)
#define xlgalist()      (testarg(typearg(consornullp, NT__LIST)))
#define xlreqlist(x)    sxae_type(x, NT__LIST)
#define xlgabyte()      (testarg(typearg(bytep, NT__BYTE)))
#define xlreqbyte(x)    sxae_type(x, NT__BYTE)
#define xlgaiport()     (testarg(typearg(iportp, NT__IPORT)))
#define xlreqiport(x)   sxae_type(x, NT__IPORT)
#define xlgaoport()     (testarg(typearg(oportp, NT__OPORT)))
#define xlreqoport(x)   sxae_type(x, NT__OPORT)

#define xlreqother(x)   sxae_type(x, 0)

/* node tags */
#define NTAG(name, num, type) NT_##name = num,
enum nodetag_t  {
#include "ntag.t"
  NT_MAXTAG,    /* TOTAL number of defined tags */
  NT__LIST   = -1,
  NT__NUMBER = -2,
  NT__PROC   = -3,
  NT__BYTE   = -4,
  NT__IPORT  = -5,
  NT__OPORT  = -6,
  NT__ANY    = -7
};
#undef NTAG

/* small fixnum access macros */
#define so_fix0         ((SOBJ)1)
#define so_fix1         ((SOBJ)3)
#define so_fix2         ((SOBJ)5)
#define SFIX1DELTA      2
#define sfixp(x)        (LSB_PART(x) & LSB_PART(1))
#define sfix2p(x,y)     (LSB_PART(x) & LSB_PART(y) & LSB_PART(1))
#define sfixcmp(x,op,y) (((FIXTYPE)(x)) op ((FIXTYPE)(y)))
#define cvsfixnum(x)    ((SOBJ)(((FIXTYPE)(x) << 1) | 1L))
#define getsfixnum(x)   ((FIXTYPE)((INTPTYPE)(x) >> 1))
#define getsfixshort(x) ((short)(INTPTYPE)(x) >> 1)    /* 32Kb code */
/* bytes are sfixnums 0..255 with odd bit patterns 1..511) */
#define bytep(x)        ((((INTPTYPE)(x)) & ~((INTPTYPE)(0xFF << 1))) == 1L)

/* new node access macros */
/* macro to determine if a value is a regular NODE pointer */
#define nodep(x)        (!sfixp(x))
/* are both x & y nodes ? */
#define node2p(x,y)     (!(LSB_PART(1) & (LSB_PART(x) | LSB_PART(y))))
/* _ntype(x) is applicable only if nodep(x) != FALSE */
#define _ntype(x)       ((x)->n_type)
#define _typep(x,type)  (_ntype(cknode(x)) == (type))
#define typep(x,type)   (nodep(x) && _typep(x, type))
#define xntype(x)       (sfixp(x) ? NT_FIXNUM : _ntype(x))

/* so_nil is a frob object (not C NULL pointer): see below */
#define nullp(x)        ((x) == so_nil)

/* primitive type predicates */
#define fixp(x)         (sfixp(x) || _typep(x, NT_FIXNUM))
#define consp(x)        typep(x, NT_CONS)
#define stringp(x)      typep(x, NT_STRING)
#define bvectorp(x)     typep(x, NT_BYTEVEC)
#define symbolp(x)      typep(x, NT_SYMBOL)
#define keywordp(x)     typep(x, NT_KEYWORD)
#define hashtablep(x)   typep(x, NT_HASHTABLE)
#define recordp(x)      typep(x, NT_RECORD)
#define boxp(x)         typep(x, NT_BOX)
#define portp(x)        typep(x, NT_PORT)
#define floatp(x)       typep(x, NT_FLONUM)
#define vectorp(x)      typep(x, NT_VECTOR)
#define closurep(x)     typep(x, NT_CLOSURE)
#define codep(x)        typep(x, NT_CODE)
#define subrp(x)        typep(x, NT_SUBR)
#define charp(x)        typep(x, NT_CHAR)
#define promisep(x)     typep(x, NT_PROMISE)
#define handlep(x)      typep(x, NT_HANDLE)
#define booleanp(x)     typep(x, NT_BOOLEAN)
#define frobp(x)        typep(x, NT_FROB)
#define gcellp(x)       typep(x, NT_GCELL)
#define weakpairp(x)    typep(x, NT_WEAKPAIR)

/* combined type predicates */
#define falsep(x)       ((x) == so_false)
#define truep(x)        ((x) != so_false)
#define numberp(x)      (sfixp(x) || (nodep(x) && (_typep(x, NT_FLONUM) || _typep(x, NT_FIXNUM))))
#define procedurep(x)   (nodep(x) && (_typep(x, NT_SUBR) || _typep(x, NT_CLOSURE)))
#define iportp(x)       (portp(x) && (getpflags(x) & PF_INPUT))
#define oportp(x)       (portp(x) && (getpflags(x) & PF_OUTPUT))
#define binportp(x)     (portp(x) && (getpflags(x) & PF_BINARY))
#define consornullp(x)  (nullp(x) || consp(x))
#define scharp(x)       (charp(x) && !(getchcode(x) & SCHAR_MASK))
#define bwpp(x)         ((x) == so_bwp)

/* macros to define/change mutability for nodes */
#define ismutable(x)    (!((x)->n_flags & NF_IMMUTABLE))
#define isimmutable(x)  (0 != ((x)->n_flags & NF_IMMUTABLE))
#define protect(x)      (((x)->n_flags |= NF_IMMUTABLE))
#define unprotect(x)    (((x)->n_flags &= ~NF_IMMUTABLE))

/* forward declarations of structs */
typedef struct node_s NODE, *SOBJ;
typedef const struct node_s *CSOBJ;

typedef byte_t vmop_t;       /* VM opcode type */
typedef byte_t vmtag_t;      /* signed/unsigned - time critical! */

/* node header structure */
struct nheader_s {
  vmtag_t nh_type;           /* type of node */
  vmtag_t nh_flags;          /* flag bits */
};

/* SUBR/CSUBR procedure types */
typedef SOBJ (*vm_subr_t)(void);    /* regular built-in procedure */
typedef SOBJ (*vm_csubr_t)(SOBJ);   /* continuation built-in proc */

/* opaque port data pointer */
typedef struct { int _dummy; } *PORTDPTR;

/* port virtual method / data table */
typedef struct tagPORTVTAB {
    short v_flags, v_id;
    void   (*v_print)(PORTDPTR, SOBJ);
    void   (*v_mark)(PORTDPTR);
    void   (*v_save)(PORTDPTR, SOBJ);
    bool_t (*v_restore)(PORTDPTR*, SOBJ);
    void   (*v_close)(PORTDPTR);
    tint_t (*v_putc)(PORTDPTR, tchar_t);
    tint_t (*v_getc)(PORTDPTR);
    tint_t (*v_ungetc)(PORTDPTR, tint_t);
    int    (*v_puts)(PORTDPTR, const tchar_t*);
    size_t (*v_read)(PORTDPTR, byte_t*, size_t);
    size_t (*v_write)(PORTDPTR, const byte_t*, size_t);
    bool_t (*v_listen)(PORTDPTR);
    void   (*v_flush)(PORTDPTR);
    void   (*v_cleari)(PORTDPTR);
    void   (*v_clearo)(PORTDPTR);
} PORTVTAB, *PORTVPTR;

/**************************************************************************/
/* node flags */
#define NF_MARK         1
#define NF_LEFT         2
#define NF_SHARED       4
#define NF_GCFLAGS      7   /* mask for all GC flags */
#define NF_IMMUTABLE    8
#define NF_FREE         0   /* flags for a NT_FREE node */

/* port flags */
#define PF_INPUT        1
#define PF_OUTPUT       2
#define PF_BINARY       4


/* numerical data union */
union numdata_s {
    FIXTYPE xn_fix;  /* fixnum variant */
    FLOTYPE xn_flo;  /* flonum variant */
};

/* node structure */
struct node_s {
    struct nheader_s n_header;  /* type and flags */
    union ninfo_s {             /* value */
      struct ni_cons_s {        /* list node (cons) */
          SOBJ xl_car;              /* the car pointer */
          SOBJ xl_cdr;              /* the cdr pointer */
      } ni_pair;
      struct ni_num_s {        /* number node (for non-small fixnums etc.) */
          union numdata_s xd_num;   /* value representation depends on tag */
      } ni_num;
      struct ni_char_s {        /* character node (for large characters) */
          tchar_t xc_char;          /* character value */
      } ni_char;
      struct ni_port_s {        /* port node */
          PORTVPTR xp_vp;
          PORTDPTR xp_dp;           /* port-specific data ptr */
      } ni_port;
      struct ni_hndl_s {        /* os handle node */
          OSHANDLE xf_h;            /* the handle */
          int xf_htype;             /* handle type */
      } ni_hndl;
      struct ni_vect_s {        /* vector/string node */
        union {
          SOBJ*   xv_data;          /* vector data */
          tchar_t* xs_str;          /* string pointer */
          byte_t*  xb_str;          /* byte vector pointer */
          } niv_data;
        union {
          sv_size_t xv_size;        /* vector size */
          ss_size_t xs_length;      /* string length */
          ss_size_t xb_length;      /* byte vector length */
          } niv_size;
      } ni_vect;
      struct ni_subr_s {        /* subr/csubr node */
          vm_subr_t xs_subr;        /* function pointer */
          const tchar_t* xs_id;     /* id (statically allocated) */
      } ni_subr;
    } n_info;
};
                                /* node fields accessors */

/* node header */
#define n_type              n_header.nh_type
#define n_flags             n_header.nh_flags
/* NT_CONS   */
#define n_car               n_info.ni_pair.xl_car
#define n_cdr               n_info.ni_pair.xl_cdr
/* NT_FIXNUM */
#define n_int               n_info.ni_num.xd_num.xn_fix
/* NT_FLONUM */
#define n_flonum            n_info.ni_num.xd_num.xn_flo
/* NT_CHAR */
#define n_char              n_info.ni_char.xc_char
/* NT_PORT   */
#define n_vp                n_info.ni_port.xp_vp
#define n_dp                n_info.ni_port.xp_dp
/* NT_HANDLE */
#define n_h                 n_info.ni_hndl.xf_h
#define n_htype             n_info.ni_hndl.xf_htype
/* NT_VECTOR */
#define n_vsize             n_info.ni_vect.niv_size.xv_size
#define n_vdata             n_info.ni_vect.niv_data.xv_data
/* NT_STRING, NT_SYMBOL, NT_KEYWORD */
#define n_strlen            n_info.ni_vect.niv_size.xs_length
#define n_str               n_info.ni_vect.niv_data.xs_str
/* NT_BYTEVEC */
#define n_bveclen           n_info.ni_vect.niv_size.xb_length
#define n_bvec              n_info.ni_vect.niv_data.xb_str
/* NT_SUBR, NT_CSUBR */
#define n_subr              n_info.ni_subr.xs_subr
#define n_id                n_info.ni_subr.xs_id

/* define illegal SOBJ value to use as special marker
   (NULL is not a legal SOBJ and != so_nil) */
#define so_illegal      ((SOBJ)(NULL))

/* booleans */
extern NODE bool_tab[2];
#define cvbool(x)       ((x) ? so_true : so_false)
#define so_false        (bool_tab + 0)
#define so_true         (bool_tab + 1)

/* common objects (frobs) */
#define NFROBS  19
extern NODE frob_tab[NFROBS];
#define so_eof          (frob_tab + 0)
#define so_default      (frob_tab + 1)
#define so_void         (frob_tab + 2)
#define so_unbound      (frob_tab + 3)
#define so_nil          (frob_tab + 4) 
#define so_mv_mismatch  (frob_tab + 5)
#define so_bwp          (frob_tab + 6)
/* primitive continuations */
#define sk_endexec      (frob_tab + 7)
#define sk_popregs      (frob_tab + 8)
/* named constants */
#define sc_optional     (frob_tab + 9)  /* #!optional */
#define sc_rest         (frob_tab + 10) /* #!rest */
#define sc_key          (frob_tab + 11) /* #!key */
#define sc_aux          (frob_tab + 12) /* #!aux */
#define sc_ac           (frob_tab + 13) /* #!current-value */
#define sc_void         (frob_tab + 14) /* #!void */
#define sc_default      (frob_tab + 15) /* #!default */
#define sc_nonspec      (frob_tab + 16) /* #!nonspecified */
#define sc_tmany        (frob_tab + 17) /* #!too-many */
#define sc_tfew         (frob_tab + 18) /* #!too-few */


#define cvfrob(i)       (frob_tab + (i))
#define getfrobcode(x)  ((x) - frob_tab)

/* characters */
#define NSCHARS  128
#define SCHAR_MASK (~0x7f)
extern NODE small_char_tab[NSCHARS];
#define cvchar(i)       (((i) & SCHAR_MASK) ? makechar(i) : (small_char_tab + (i)))

/**************************************************************************/

SOBJ cvsym(SOBJ symbolic, vmtag_t ntype);
SOBJ mksym(const tchar_t* static_name, vmtag_t ntype);
SOBJ consa(SOBJ a, SOBJ d, vmtag_t ntype);
/* NT_CONS   */
#define cons(a,d)           consa(a,d,NT_CONS)
#define car(s)              ((s)->n_car)
#define cdr(s)              ((s)->n_cdr)
#define setcar(s,y)         ((s)->n_car = (y))
#define setcdr(s,y)         ((s)->n_cdr = (y))
/* NT_BOX    */
#define cvbox(obj,tag)      consa(tag,obj,NT_BOX)
#define getboxval(s)        cdr(s)
#define setboxval(s,v)      setcdr(s,v)
#define getboxtag(s)        car(s)
#define setboxtag(s,v)      setcar(s,v)
/* NT_RECORD    */
#define getrsize(s)         getvsize(s)
#define getrelement(s,i)    getelement(s,i)
#define setrelement(s,i,v)  setelement(s,i,v)
/* NT_CLOSURE */
#define cvclosure(code,env) consa(code,env,NT_CLOSURE)
#define getcode(s)          car(s)
#define getcenv(s)          cdr(s)
/* NT_PROMISE */
#define cvpromise(closure)  consa(closure,so_nil,NT_PROMISE)
#define getpproc(s)         car(s)
#define setpproc(s,v)       setcar(s,v)
#define getpvalue(s)        cdr(s)
#define setpvalue(s,v)      setcdr(s,v)
/* NT_VECTOR  */
#define getvsize(s)         ((s)->n_vsize)
#define getvdata(s)         ckvdata((s)->n_vdata)
#define getelement(s,i)     (ckvdata((s)->n_vdata)[i])
#define setelement(s,i,v)   (ckvdata((s)->n_vdata)[i] = (v))
/* NT_FIXNUM/NT_FLONUM */
#define getfixnum(s)        (sfixp(s) ? getsfixnum(s) : (s)->n_int)
#define getflonum(s)        ((s)->n_flonum)
/* NT_CHAR */
#define getchcode(s)        ((s)->n_char)
/* NT_STRING */
#define getstring(s)        cksdata(((s)->n_str))
#define getslength(s)       ((s)->n_strlen)
/* NT_BYTEVEC */
#define getbytes(s)         ((byte_t*)ckbdata((s)->n_bvec))
#define getbcount(s)        ((s)->n_bveclen)
/* NT_SYMBOL/NT_KEYWORD */
#define getpstring(s)       getstring(s)
#define getpslength(s)      getslength(s)
/* NT_GCELL */
#define cvgcell(val,sym,loc) consa(val,cons(sym,loc),NT_GCELL)
#define getgcellval(x)      car(x)
#define setgcellval(x,v)    setcar(x,v)
#define getgcellname(x)     car(cdr(x))
#define getgcellhome(x)     cdr(cdr(x))
#define gcellboundp(x)      (getgcellval(x) != so_unbound)
/* NT_HANDLE */
#define gethandle(s)        ((s)->n_h)
#define sethandle(s,v)      ((s)->n_h = (v))
#define gethtype(s)         ((s)->n_htype)
#define sethtype(s,v)       ((s)->n_htype = (v))
/* NT_SUBR/NT_CSUBR */
#define getsubr(s)          ((s)->n_subr)
#define getsid(s)           ((s)->n_id)
/* NT_HASHTABLE */
#define gethsize(s)         getvsize(s)
#define gethelement(s,i)    getelement(s,i)
#define sethelement(s,i,v)  setelement(s,i,v)


#define newlocale(hsize)    newvector(FIRSTLENTRY+(hsize),so_nil)

/* symbol -> global value association macros */
#define sym2gcell(s)        sxCurrentLocEnter((s))
#define getsymval(s)        getgcellval(sym2gcell(s))
#define setsymval(s,v)      setgcellval(sym2gcell(s),v)

/* locale access macros */
#define getlname(s)         getelement(s,0)
#define setlname(s,v)       setelement(s,0,v)
#define getlexport(s)       getelement(s,1)
#define setlexport(s,v)     setelement(s,1,v)
#define FIRSTLENTRY         2

/* code/env definition & access macros */
#define getbcode(s)         getelement(s,0)
#define setbcode(s,v)       setelement(s,0,v)
#define getcname(s)         getelement(s,1)
#define setcname(s,v)       setelement(s,1,v)
#define FIRSTLIT            2 /* 0 - code-str, 1 - name | (name args ...)   */

/* environment frame access */
#define FIRSTARG            2 /* arguments are placed starting from slot # */
#define env0(e)             (e)                         /* vars vector */
#define env1(e)             getelement((e),0)           /* parent link */
#define envar(e,i)          (getvdata(env0(e))[i])
#define envnames(e)         envar(e,FIRSTARG-1)

/* port access macros */
#define getvp(s)            ((s)->n_vp)
#define setvp(s,v)          ((s)->n_vp = (PORTVPTR)(v))
#define getdp(s)            ((s)->n_dp)
#define setdp(s,v)          ((s)->n_dp = (v))
#define getpflags(s)        (((s)->n_vp)->v_flags)

/* global args and options */
extern  int sx_argc, sx_argpos; /* prog. arg. count, first arg position */
extern  tchar_t** sx_argv;      /* prog. args array */
extern  bool_t sx_use_tty;         /* -t option */
extern  bool_t sx_exit_after_load; /* -e option */

/* VM registers */
extern  SOBJ vmfun;             /* current function */
extern  SOBJ vmenv;             /* current environment */
extern  int  vmargc;            /* argument count for current call */
extern  SOBJ vmdstate;          /* current dynamic state chain */
extern  int  vmint;             /* VM interrupt register */
#define VMI_GC      0x01
#define VMI_TIMER   0x02
#define VMI_KBINT   0x04
#define vmInterruptRequest(i)   (vmint |= (i))

/* VM stack */
extern SOBJ* vmstkbase;         /* base of value stack */
extern SOBJ* vmstktop;          /* top of value stack */
extern SOBJ* vmsp;              /* value stack pointer */
#define sobjcpy(dst,src,nv)     memcpy((dst),(src),(nv)*sizeof(SOBJ))  /* non-overlap */
#define sobjmvup(dst,src,nv)    memmove((dst),(src),(nv)*sizeof(SOBJ)) /* dst < src */
#define sobjmvdn(dst,src,nv)    memmove((dst),(src),(nv)*sizeof(SOBJ)) /* dst > src */
#define sobjmove(dst,src,nv)    memmove((dst),(src),(nv)*sizeof(SOBJ)) /* overlap */


/* external routine declarations */

/* sxcom.c */
extern SOBJ sxCompile(SOBJ expr, SOBJ locale);

/* sxmem.c */
#ifdef _DEBUG_GC
extern SOBJ _checknode(SOBJ s);
extern SOBJ* _checkvdata(SOBJ* p);
extern tchar_t* _checksdata(tchar_t* p);
extern byte_t* _checkbdata(byte_t* p);
#endif
extern SOBJ cvlist(SOBJ* data, sv_size_t len, SOBJ tail);
extern SOBJ cvstring(const tchar_t* str);
extern SOBJ cvstringn(const tchar_t* str, ss_size_t len);
extern SOBJ cvbytevec(const byte_t* buf, ss_size_t len);
extern SOBJ cvfixnum(FIXTYPE n);
extern SOBJ cvflonum(FLOTYPE n);
extern SOBJ cvport(PORTVPTR vp, PORTDPTR dp);
extern SOBJ makechar(tchar_t c);
extern SOBJ makesubr(vm_subr_t subr, const tchar_t* id);
extern SOBJ makecsubr(vm_csubr_t csubr, const tchar_t* id);
extern SOBJ cvhandle(OSHANDLE h, int htype);
#define NV_NO_INIT so_illegal
extern SOBJ newframe(SOBJ parent, int size, SOBJ name);
extern SOBJ newvector(sv_size_t size, SOBJ fill);
extern SOBJ newstring(ss_size_t size);
extern SOBJ newbytevec(ss_size_t size);
extern SOBJ newcode(sv_size_t nlits);
extern SOBJ newcontinuation(void);
extern SOBJ newhashtable(sv_size_t hsize);
extern SOBJ newrecord(sv_size_t hsize);
extern SOBJ testmutable(SOBJ x);
/* gc root structure */
typedef struct gcroot_s {
  struct gcroot_s* next;
  int rtype; /* GCRT_xxx */
  OFFTYPE data1, data2;
} gcroot_t;
/* gc root types */
#define GCRT_SOBJ      0  /* data1 is SOBJ */
#define GCRT_SOBJ2     1  /* data1,2 are SOBJ */
#define GCRT_SOBJVAR   2  /* data1 is SOBJ* */
#define GCRT_SOBJVEC   3  /* data1,2 are ulong,SOBJ* */
#define GCRT_SOBJRANGE 4  /* data1,2 are SOBJ** */
#define GCRT_ENUMPROC  5  /* data1 is SOBJ* (*)(OFFTYPE*) */
/* global chain of gc roots */
extern gcroot_t* gcroots;
#define pushroot(r) ((r).next = gcroots, gcroots = &(r))
#define poproot() do { assert(gcroots); gcroots = gcroots->next; } while (0)
/* access to handy set of locks */
extern  gcroot_t _gclroot;
#define gcLock(s)           (((SOBJ*)_gclroot.data2)[_gclroot.data1++] = (s))
#define gcUnlock(n)         (_gclroot.data1 -= (n))
#define gcUnlockAll()       (_gclroot.data1 = 0)
/* memory utils */
extern void sxMemGC(void);
extern void sxMemMark(SOBJ node);
extern void sxMemInit(void);
extern void sxMemFree(void);
extern void sxDstateInit(void);
extern void sxDstateFree(void);
extern void sxStackInit(sv_size_t stacksize);
extern void sxStackFree(void);
extern void sxMemExpand(size_t ns, size_t vs);

/* sxinit.c */
extern void get_subr(const tchar_t* name, vm_subr_t* ps, const tchar_t** pid);
extern void get_csubr(const tchar_t* name, vm_csubr_t* ps, const tchar_t** pid);
extern SOBJ* enum_subr_table(OFFTYPE* rpos);
extern SOBJ* enum_csubr_table(OFFTYPE* rpos);
extern SOBJ* enum_data_table(OFFTYPE* rpos);
extern SOBJ* enum_var_table(OFFTYPE* rpos);

extern void sxSetup(void);
extern void sxCleanup(void);
extern SOBJ sxImgInit(sv_size_t stacksize);

/* sxvm.c */
extern SOBJ sxExecute(SOBJ fun, SOBJ arglist);
extern void vm_restart(SOBJ proc);
extern void vm_reset(void);
extern void vm_exit(SOBJ retcode);
extern void vm_error0(const tchar_t* fstr);
extern void vm_error1(const tchar_t* fstr, SOBJ arg);
extern SOBJ vm_stkover(void);
extern long vm_gettimer(void);
extern void vm_settimer(long val);
extern SOBJ vm_curry(SOBJ proc, int argc);
extern void vm_push_cmv_cont(SOBJ receiver);
extern void vm_push_value_cont(SOBJ val);
extern void vm_push_values_cont(int argc);
extern SOBJ vm_values(int argc);
extern SOBJ vm_apply(SOBJ proc, int argc);
extern SOBJ vm_reroot(SOBJ newds);
extern int  vm_flatten_star_args(int argc);
extern SOBJ vm_dynwind(SOBJ before, SOBJ during, SOBJ after);
extern SOBJ vm_fluidbind(SOBJ setproc, SOBJ newval, SOBJ thunk, SOBJ old);
extern SOBJ vm_prepare_handler(SOBJ exn);
/* useful macros to write procedures in C */
#define PROC_ARGC()           (vmargc)
#define PROC_RETURN(v)        return (v)
#define PROC_RETURN_VALUES(n) return vm_values(n)
#define PROC_GOTO(proc, n)    return vm_apply(proc, n)
#define PROC_DYNAMIC_WIND(b, d, a) return vm_dynwind(b, d, a)
#define PROC_FLUID_BIND(s, n, t, o) return vm_fluidbind(s, n, t, o)
#define PROC_RAISE(exn)       PROC_GOTO(vm_prepare_handler(exn), 1)
/* useful macros to write continuations in C */
#define CONT_ENV()            (vmenv)
#define CONT_RETURN(val)      return(vmargc=-1, (val))
#define CONT_GOTO(proc, n)    return(vmargc=(n), (proc))
#define CONT_RAISE(exn)       CONT_GOTO(vm_prepare_handler(exn), 1)
/* useful macros to push args and continuations in C */
#define CURRY(proc, n)        vm_curry(proc, n)
#define PUSH_ARG(arg)         cpush(arg)
#define PUSH_CONT(sk, env)    do { check(2); push(sk); push(env); } while (0)
#define PUSH_CALLMV_CONT(rc)  vm_push_cmv_cont(rc)
#define PUSH_VALUE_CONT(val)  vm_push_value_cont(val)
#define PUSH_VALUES_CONT(n)   vm_push_values_cont(n)
#define INSERT_CONT(sk, env)  do { check(2); sobjmvup(vmsp - 2, vmsp, vmargc); \
                                   vmsp -= 2; vmsp[vmargc] = env; \
                                   vmsp[vmargc+1] = sk; } while (0) 


/* miscellaneous */
typedef bool_t (*sobjcmpfun_t)(SOBJ, SOBJ);
extern bool_t eq(SOBJ arg1, SOBJ arg2);           /* eq? predicate */
extern bool_t eqv(SOBJ arg1, SOBJ arg2);          /* eqv? predicate */
extern bool_t equal(SOBJ arg1, SOBJ arg2);        /* equal? predicate */
extern bool_t char_eq(SOBJ arg1, SOBJ arg2);      /* char=? predicate (type-safe) */
extern bool_t char_ci_eq(SOBJ arg1, SOBJ arg2);   /* char-ci=? predicate (type-safe) */
extern bool_t string_eq(SOBJ arg1, SOBJ arg2);    /* string=? predicate (type-safe) */
extern bool_t string_ci_eq(SOBJ arg1, SOBJ arg2); /* string-ci=? predicate (type-safe) */

extern bool_t listp(SOBJ list);               /* list? safe predicate */
extern size_t length(SOBJ list);              /* length of simple list */
extern bool_t wildicmp(const tchar_t* src, const tchar_t* model);
extern SOBJ sxFail(const tchar_t* msg);
extern SOBJ sxErr(const tchar_t* msg, SOBJ arg);
extern void sxSysErr(const tchar_t* msg);
extern void sxError0(const tchar_t* fstr);
extern void sxError1(const tchar_t* fstr, SOBJ arg);

/* slow type checker accepting "extended" node types like NT__PROC */
extern bool_t sx_typep(SOBJ val, int nt);

/* these functions will never return */
extern SOBJ sxae_toofew(void);
extern SOBJ sxae_toomany(void);
extern SOBJ sxae_justone(void);
extern SOBJ sxae_type(SOBJ val, int nt);
extern SOBJ sxae_range(SOBJ index, SOBJ val);
extern SOBJ sxae_immutable(SOBJ val);
extern void sxae_argc(int argc);
#define CHECKARGS_EQ(n)     do { if( vmargc != (n) ) sxae_argc(n); } while (0)
#define CHECKARGS_GE(n)     do { if( vmargc < (n) ) sxae_argc(n); } while (0)
#define CHECKARGS(min,max)  do { if( vmargc < (min) || vmargc > (max) ) sxae_argc(min); } while (0)
#define CHECKARGTYPE(n,t,m) do { if( !t(vmsp[n]) ) sxae_type(vmsp[n],m); } while (0)
#define DROPALLARGS()       xlpoprest()
#define DROPARGS(n)         drop(n)

extern int  sxMain(int argc, tchar_t** argv);

#endif /* __SXM_H */
