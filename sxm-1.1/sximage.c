/* sximage.c - memory image save/restore functions */

#include "sxm.h"
#include "sxio.h"
#include "sxread.h"
#include "sxwrite.h"
#include "sxintern.h"
#include "sximage.h"
#include "mem.h"
#include "os.h"
#include "extern.h"
#include "io.h"

EXTERN_VARIABLE(sv_symtable)
EXTERN_VARIABLE(sv_keytable)
EXTERN_VARIABLE(sv_curloc)
EXTERN_VARIABLE(sv_curintprocs)
EXTERN_PORT_CLASS(xp_obin)
EXTERN_PORT_CLASS(xp_ibin)

/* local variables */
static OFFTYPE off, foff;
static NODE imageportnode; /* save/restore image STATIC! port */
static SOBJ imgio = &imageportnode;

/*
 * Some nonzero even offsets are reserved for static  table NODEs! 
 * small chars: 0..NSCHARS-1,
 * booleans: (NSCHARS-1)+1 (NSCHARS-1)+2
 * frobs: (NSCHARS-1)+3...(NSCHARS-1)+3+NFROBS-1
 */
#define ENUM2OFF(n)   ((OFFTYPE)(2+(n)*2))
#define OFF2ENUM(o)   (((int)(o))/2-1)
/* if off is even and nonzero, test for reserved enode offsets */
#define CHAROFFP(o) ((o) >= ENUM2OFF(0) && (o) <= ENUM2OFF(NSCHARS-1))
#define BOOLOFFP(o) ((o) == ENUM2OFF(NSCHARS) || (o) == ENUM2OFF(NSCHARS+1))
#define FROBOFFP(o) ((o) >= ENUM2OFF(NSCHARS+2) && (o) <= ENUM2OFF(NSCHARS+NFROBS+1))
/* if it is a char offset, convert it to char SOBJ */
#define OFF2CHAR(o) (cvchar((tchar_t)OFF2ENUM(o)))
#define CHAR2OFF(c) (ENUM2OFF(getchcode(c)))
/* if it is a boolean offset, convert it to boolean SOBJ */
#define OFF2BOOL(o) (cvbool(OFF2ENUM(o) == NSCHARS+1))
#define BOOL2OFF(b) (ENUM2OFF((b) == so_true ? NSCHARS+1 : NSCHARS))
/* if it is a frob offset, convert it to frob SOBJ */
#define OFF2FROB(o) (cvfrob(OFF2ENUM(o) - NSCHARS-2))
#define FROB2OFF(f) (ENUM2OFF(getfrobcode(f) + NSCHARS+2))
/* true NODE offsets starts from here: */
#define NODESTARTOFF  ENUM2OFF(NSCHARS+NFROBS+2)

/*
 * hairy signature must take into account some possible image incompatibility
 * reasons
 */
#define SIGNATURE ((OFFTYPE)(SX_NSSIZE*5L+NODESTARTOFF*2L+SX_STSIZE+SX_KTSIZE+\
                             sizeof(int)*19+sizeof(tchar_t)*17+1))

/* forward declarations */

static void setoffset(void);
static void writetagnflags(SOBJ node);
static void writeinfo(SOBJ node);
static void writeptr(OFFTYPE off);
static void readinfo(SOBJ node);
static SOBJ cviptr(OFFTYPE o);
static SOBJ readtagnflags(vmtag_t type, OFFTYPE off);
static OFFTYPE cvoptr(SOBJ p);
static OFFTYPE readptr(void);
static void writeheader(const tchar_t *header);
static void readheader(void);

#define stripgcell(c) setcdr(c,so_nil) /* must be in sxm.h[?] */

/*#define _TRACEIMAGE*/

/* sxImgSave - save the memory image */
bool_t sxImgSave(const tchar_t *fn, SOBJ thunk, bool_t strip,
                 const tchar_t *hdr)
{
  tchar_t *cp;
  byte_t *bp;
  NSEGMENT *nseg;
  sv_size_t size;
  int n;
  OFFTYPE pos;
  SOBJ p;
  SOBJ *vp;

  /* make sure that file name and header will live thru GC... */
  static tchar_t fname[FILENAME_MAX+1];
  static tchar_t header[FILENAME_MAX+65];
  if (fn == NULL) return FALSE; /* ??? */
  tcsncpy(fname, fn, FILENAME_MAX); fname[FILENAME_MAX] = T('\0');
  if (hdr == NULL) header[0] = T('\0'); 
  else tcsncpy(header, hdr, FILENAME_MAX+64); header[FILENAME_MAX+64] = T('\0');

  /* manually open the output file */
  {
    PORTVPTR vp; PORTDPTR dp;
    if (!fbo_open(fname, &vp, &dp)) return FALSE;
    setvp(imgio, vp);
    setdp(imgio, dp);
    imgio->n_type = NT_PORT;
  }

  /* save the thunk before gc-ing */
  cpush(thunk);

  /* strip-up memory image */
  if (strip) { /* stripped image required */
    /* todo: reset error handlers? */

    /* clear integrable proc table */
    sv_curintprocs = so_unbound;

    /* clear stack and registers */
    thunk = pop();
    vmsp = vmstktop;
    vmfun = vmenv = vmdstate = so_nil;
    push(thunk);

    /* clear locale & shake tree */
    if (vectorp(sv_curloc)) {
      setlexport(sv_curloc, so_nil);
      for (n = FIRSTLENTRY; n < (int)getvsize(sv_curloc); n++) {
        /* clear all gcell names, gcell->symbol will not work */
        for (p = getelement(sv_curloc, n); consp(p); p = cdr(p))
          if (gcellp(car(p))) stripgcell(car(p));
        setelement(sv_curloc, n, so_nil);
      }
    }
  }
  /* first call GC to clean up memory and release weak pointers */
  sxMemGC();
  /* remove dead entries from symbol/keyword tables */
  sxCompactSymbolTable(sv_symtable);
  sxCompactSymbolTable(sv_keytable);
  /* collect dead entries */
  sxMemGC();

  /* pop thunk */
  thunk = pop();

  /* first of all, write out header and the SIGNATURE */
  writeheader(header);
  writeptr(SIGNATURE);

  /* write out the stack size */
  writeptr((OFFTYPE) (vmstktop - vmstkbase));

  /* write out dstate as is */
  writeptr(cvoptr(vmdstate));

  /* write out thunk and system obj tables */
  writeptr(cvoptr(thunk));

  for (pos = 0; (vp = enum_subr_table(&pos)) != NULL;) 
    writeptr(cvoptr(*vp));
  for (pos = 0; (vp = enum_csubr_table(&pos)) != NULL;)
    writeptr(cvoptr(*vp));

  for (pos = 0; (vp = enum_data_table(&pos)) != NULL;)
    writeptr(cvoptr(*vp));
  for (pos = 0; (vp = enum_var_table(&pos)) != NULL;)
    writeptr(cvoptr(*vp));

  /* setup the initial file offsets */
  off = foff = NODESTARTOFF;

#ifdef _TRACEIMAGE
  FILE*log = fopen("outlog", "w+"); /******/
#endif
  /* write out all nodes that are still in use */
  for (nseg = nsegments; nseg != NULL; nseg = nseg->ns_next) {
    p = nseg->ns_data;
    n = nseg->ns_size;
    for (; --n >= 0; ++p, off += sizeof(NODE)) {
#ifdef _TRACEIMAGE
      if (p->n_type != NT_FREE) {
        if (off != foff) fprintf(log, "%lu: %d\n", (unsigned long)foff, 0); /******/
        fprintf(log, "%lu: %d\n", (unsigned long)off, (int)p->n_type); /******/
      }
#endif
      switch (p->n_type) { /* only nodes here */
        case NT_FREE:
          continue;
        case NT_CONS:   /* all CONS-like nodes */
        case NT_CLOSURE:
        case NT_WEAKPAIR:
        case NT_BOX:
        case NT_GCELL:
        case NT_PROMISE:
          writetagnflags(p);
          writeptr(cvoptr(car(p)));
          writeptr(cvoptr(cdr(p)));
          break;
        case NT_VECTOR: /* all VECTOR-like nodes */
        case NT_HASHTABLE:
        case NT_RECORD:
        case NT_CODE:
          writetagnflags(p);
          size = getvsize(p);
          writeptr((OFFTYPE)size);
          for (vp = getvdata(p); size != 0; --size)
            writeptr(cvoptr(*vp++));
          break;
        case NT_STRING: /* all STRING-like nodes */
        case NT_SYMBOL:
        case NT_KEYWORD:
          writetagnflags(p);
          size = getslength(p);
          writeptr((OFFTYPE)size);
          for (cp = getstring(p); size != 0; --size) {
            int cnt;
            bp = (byte_t*)cp;
            cnt = sizeof(tchar_t);
            while (cnt--) sxWriteByte(*bp++, imgio);
            cp++;
          }
          break;
        case NT_BYTEVEC:
          writetagnflags(p);
          size = getbcount(p);
          writeptr((OFFTYPE)size);
          for (bp = getbytes(p); size != 0; --size) {
            sxWriteByte(*bp++, imgio);
          }
          break;
        case NT_PORT:
          writetagnflags(p);
          sxPortSave(p, imgio);
          break;
        case NT_SUBR:
        case NT_CSUBR:
          writetagnflags(p);
          /* write id size followed by id */
          size = tcslen(getsid(p));
          writeptr((OFFTYPE)size);
          for (cp = (tchar_t*)getsid(p); size != 0; --size) {
            int cnt;
            bp = (byte_t*)cp;
            cnt = sizeof(tchar_t);
            while (cnt--) sxWriteByte(*bp++, imgio);
            cp++;
          }
          break;
        case NT_HANDLE:
        case NT_FLONUM:
        case NT_FIXNUM: /* large fixnums */
        case NT_CHAR:   /* large characters */
          writetagnflags(p);
          writeinfo(p);
          break;
        case NT_BOOLEAN: /* never stored in a node! */
        case NT_FROB:    /* never stored in a node! */
        default: /* unknown type?? */
          /* should never happen! */
          assert(0);
#ifdef _TRACEIMAGE
          fclose(log);/******/
#endif
          sxErr(T("unable to save node"), p);
          break;
      }
      foff += sizeof(NODE);
    }
  }
#ifdef _TRACEIMAGE
  fclose(log);/******/
#endif
  /* write the terminator */
  sxWriteByte(NT_FREE, imgio);
  writeptr((OFFTYPE) 0);

  /* close the output file */
  sxPortClose(imgio);

  /* make the image file executable on UNIX systems, if needed */
  if (header != NULL && header[0] == T('#') && header[1] == T('!'))
    ossetexecflag(fname);

  /* return successfully */
  return TRUE;
}

/* sxImgRestore - restore a saved memory image */
SOBJ sxImgRestore(const tchar_t *fname)
{
  SOBJ thunk;
  tchar_t *cp;
  byte_t *bp;
  OFFTYPE pos;
  vmtag_t type;
  sv_size_t size;
  SOBJ p;
  SOBJ *vp;
  int b;

  /* manually open the input file */
  {
    PORTVPTR vp; PORTDPTR dp;
    if (!fbi_open(fname, &vp, &dp)) return so_nil;
    setvp(imgio, vp);
    setdp(imgio, dp);
    imgio->n_type = NT_PORT;
  }

  /* first of all, skip header and check the SIGNATURE */
  readheader();
  if (readptr() != SIGNATURE) {
    /* OOPS! Close the input file and return so_nil */
    sxPortClose(imgio);
    return so_nil;
  }
  /* free the old memory image (if any) */
  sxMemFree();
  sxDstateFree();
  sxStackFree();

  /* read the stack size */
  size = (sv_size_t)readptr();

  /* allocate memory for the workspace and stack */
  sxMemInit();
  /* sxDstateInit is postponed, dstate is read from the image */
  vmdstate = cviptr(readptr());
  sxStackInit(size);

  /* read the thunk, and system obj tables */
  thunk = cviptr(readptr());

  for (pos = 0; (vp = enum_subr_table(&pos)) != NULL;) 
    *vp = cviptr(readptr());
  for (pos = 0; (vp = enum_csubr_table(&pos)) != NULL;)
    *vp = cviptr(readptr());

  for (pos = 0; (vp = enum_data_table(&pos)) != NULL;)
    *vp = cviptr(readptr());
  for (pos = 0; (vp = enum_var_table(&pos)) != NULL;)
    *vp = cviptr(readptr());

  /* read each node */
#ifdef _TRACEIMAGE
  FILE*log = fopen("inlog", "w+"); /******/
#endif
  for (off = NODESTARTOFF; (b = sxReadByte(imgio)) != EOF;) {
    type = (vmtag_t)b;
#ifdef _TRACEIMAGE
    fprintf(log, "%lu: %d\n", (unsigned long)off, (int)type); /******/
#endif
    switch (type) {
      case NT_FREE:
        if ((off = readptr()) == (OFFTYPE) 0)
          goto done;
        continue;
      case NT_CONS:
      case NT_CLOSURE:
      case NT_WEAKPAIR:
      case NT_BOX:
      case NT_GCELL:
      case NT_PROMISE:
        p = readtagnflags(type, off);
        setcar(p, cviptr(readptr()));
        setcdr(p, cviptr(readptr()));
        break;
      case NT_VECTOR:
      case NT_HASHTABLE:
      case NT_RECORD:
      case NT_CODE:
        p = readtagnflags(type, off);
        p->n_vsize = size = (sv_size_t)readptr();
        p->n_vdata = getvspace(p, size);
        for (vp = getvdata(p); size != 0; --size)
          *vp++ = cviptr(readptr());
        break;
      case NT_SYMBOL:
      case NT_KEYWORD:
      case NT_STRING:
        p = readtagnflags(type, off);
        p->n_strlen = size = (sv_size_t)readptr();
        p->n_str = (tchar_t*)getvspace(p, ttow_size(size));
        for (cp = getstring(p); size != 0; --size) {
          int cnt;
          bp = (byte_t*)cp;
          cnt = sizeof(tchar_t);
          while (cnt--) *bp++ = sxReadByte(imgio);
          cp++;
        }
        break;
      case NT_BYTEVEC:
        p = readtagnflags(type, off);
        p->n_bveclen = size = (sv_size_t)readptr();
        p->n_bvec = (byte_t*)getvspace(p, btow_size(size));
        for (bp = getbytes(p); size != 0; --size) {
          *bp++ = sxReadByte(imgio);
        }
        break;
      case NT_PORT:
        p = readtagnflags(type, off);
        sxPortRestore(p, imgio);
        break;
      case NT_SUBR:
      case NT_CSUBR:
        p = readtagnflags(type, off);
        { /* read id */
          static tchar_t idbuf[SX_MAXSYMBOL+1];
          size = (sv_size_t)readptr();
          for (cp = idbuf; size > 0; --size) {
            int cnt;
            bp = (byte_t*)cp;
            cnt = sizeof(tchar_t);
            if (cp - idbuf < SX_MAXSYMBOL) {
              while (cnt--) *bp++ = sxReadByte(imgio);
              cp++;
            } else { /* name too long? skip the rest */
              while (cnt--) sxReadByte(imgio);
            }
          }
          *cp = 0;
          /* fill subr/csubr object */
          if (type == NT_SUBR) 
            get_subr(idbuf, &p->n_subr, &p->n_id);
          else
            get_csubr(idbuf, (vm_csubr_t*)&p->n_subr, &p->n_id);
        }
        break;
      case NT_HANDLE: /* handles are not persistent! */
        p = readtagnflags(type, off);
        readinfo(p);
        sethandle(p, (OSHANDLE) 0); /* closed */
        /* sethtype(p,0); */
        break;
      case NT_FLONUM:
      case NT_FIXNUM: /* large fixnums */
      case NT_CHAR:   /* large characters */
        p = readtagnflags(type, off);
        readinfo(p);
        break;
      case NT_BOOLEAN: /* never stored in a node! */
      case NT_FROB:    /* never stored in a node! */
      default: /* unknown type?? */
        /* should never happen! */
        assert(0);
#ifdef _TRACEIMAGE
        fclose(log);/******/
#endif
        ospanic(T("corrupted image file"));
        break;
    }
    off += sizeof(NODE);
  }
done:
#ifdef _TRACEIMAGE
  fclose(log);/******/
#endif
  /* close the input file */
  sxPortClose(imgio);

  /* collect to initialize the free space */
  gcLock(thunk);
  sxMemGC();
  /* init dstate using the one restored from the image */
  sxDstateInit();
  gcUnlock(1);
  return thunk;
}

/* setoffset - output a positioning command if nodes have been skipped */
static void setoffset(void)
{
  if (off != foff) {
    sxWriteByte(NT_FREE, imgio);
    writeptr(off);
    foff = off;
  }
}

static void writeheader(const tchar_t *header)
{
  if (header != NULL) { 
    while (*header) sxWriteByte((byte_t)*header++, imgio);
    sxWriteByte('\n', imgio); /* make sure line ends before 0 */
  }
  sxWriteByte('\0', imgio);
}

static void readheader(void)
{
  byte_t c;
  do { c = sxReadByte(imgio); } while (c != '\0');
}


/* writetagnflags - read node header */
static void writetagnflags(SOBJ node)
{
  setoffset();
  sxWriteByte(node->n_type, imgio);
  sxWriteByte((byte_t)(node->n_flags & ~NF_GCFLAGS), imgio);
}

/* writeinfo - write n_info to a file */
static void writeinfo(SOBJ node)
{
  byte_t *p = (byte_t*)&node->n_info;
  int n = sizeof(node->n_info);
  while (--n >= 0)
    sxWriteByte(*p++, imgio);
}

/* readtagnflags - read node header */
static SOBJ readtagnflags(vmtag_t type, OFFTYPE off)
{ 
  SOBJ p = cviptr(off);
  p->n_type = type;
  p->n_flags = sxReadByte(imgio);
  return p;
}

/* readinfo - read node data */
static void readinfo(SOBJ node)
{
  byte_t *p = (byte_t *) &node->n_info;
  int n = sizeof(node->n_info);
  while (--n >= 0)
    *p++ = sxReadByte(imgio);
}


/* writeptr - write a pointer to a file */
static void writeptr(OFFTYPE off)
{
  byte_t *p = (byte_t *) &off;
  int n = sizeof(OFFTYPE);

  while (--n >= 0)
    sxWriteByte(*p++, imgio);
}

/* readptr - read a pointer */
static OFFTYPE readptr(void)
{
  OFFTYPE off;
  byte_t *p = (byte_t *) &off;
  int n = sizeof(OFFTYPE);

  while (--n >= 0)
    *p++ = sxReadByte(imgio);
  return off;
}

/* cviptr - convert a pointer on input */
static SOBJ cviptr(OFFTYPE o)
{
  NSEGMENT *nseg;
  OFFTYPE off = NODESTARTOFF;
  OFFTYPE nextoff;

  /* check for nil and small fixnums */
  if (o == 0)
    return so_nil;
  if (sfixp(o))
    return (SOBJ) o;

  if (CHAROFFP(o))
    return OFF2CHAR(o);
  if (BOOLOFFP(o))
    return OFF2BOOL(o);
  if (FROBOFFP(o))
    return OFF2FROB(o);

  /* compute a NODE pointer for this offset */
  for (nseg = nsegments; nseg != NULL; nseg = nseg->ns_next) {
    nextoff = off + (OFFTYPE) (nseg->ns_size * sizeof(NODE));
    if (o >= off && o < nextoff)
      return ((SOBJ) ((OFFTYPE) & nseg->ns_data[0] + o - off));
    off = nextoff;
  }

  /* create new segments if necessary */
  for (;;) {
    /* create the next segment */
    if ((nseg = newnsegment(SX_NSSIZE)) == NULL)
      ospanic(T("insufficient memory - segment"));

    /* check to see if the offset is in this segment */
    nextoff = off + (OFFTYPE) (nseg->ns_size * sizeof(NODE));
    if (o >= off && o < nextoff)
      return ((SOBJ) ((OFFTYPE) & nseg->ns_data[0] + o - off));
    off = nextoff;
  }
}

/* cvoptr - convert a pointer on output */
static OFFTYPE cvoptr(SOBJ p)
{
  OFFTYPE off = NODESTARTOFF;
  NSEGMENT *nseg;

  /* check for nil and small fixnums */
  if (nullp(p))
    return 0;
  if (!nodep(p))
    return (OFFTYPE) p;

  /* check for static NODE tables */
  switch (_ntype(p)) {
    case NT_CHAR:
      if (scharp(p)) return CHAR2OFF(p);
      break;
    case NT_BOOLEAN:
      return BOOL2OFF(p);
    case NT_FROB:
      return FROB2OFF(p);
  }

  /* compute an offset for this NODE pointer */
  for (nseg = nsegments; nseg != NULL; nseg = nseg->ns_next) {
    if (INSEGMENT(p, nseg))
      return (off + ((OFFTYPE) p - (OFFTYPE) & nseg->ns_data[0]));
    off += (OFFTYPE) (nseg->ns_size * sizeof(NODE));
  }

  /* pointer not within any segment */
  sxErr(T("bad pointer found during image save"), p);
  return 0; /* Impossible */
}
