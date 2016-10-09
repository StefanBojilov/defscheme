/* sxmem.c - memory management routines */

#include "sxm.h"
#include "mem.h"
#include "os.h"
#include "extern.h"
#include "sxio.h"

#define SX_NSMIN        (SX_NSSIZE/8)
#define SX_VSMIN        (SX_VSSIZE)

#define SHOULD_MALLOC_ELTS(n) ((n) + 1 > SX_VSSIZE)

/* booleans, small chars and constant objects */
NODE bool_tab[2];
NODE frob_tab[NFROBS];
NODE small_char_tab[NSCHARS];

/* useless statistics */
FIXTYPE total;      /* total number of bytes of memory in use */
FIXTYPE gccalls;    /* number of calls to the garbage collector */
FIXTYPE vgccalls;   /* ? number of GC by vector space */

/* node space */
NSEGMENT *nsegments;     /* list of node segments */
static NSEGMENT *nslast; /* last of node segments */
size_t nscount;     /* number of node segments */
FIXTYPE nnodes;     /* total number of nodes */
FIXTYPE nfree;      /* number of nodes in free list */

/* small vector (and string) space */
VSEGMENT *vsegments;  /* list of vector segments */
VSEGMENT *vscurrent;  /* current vector segment */
size_t vscount;       /* number of vector segments */
static SOBJ *vfree;   /* next free location in vector space */
static SOBJ *vtop;    /* top of vector space */

/* memory initialization flag */
bool_t sxMem_inited;     /* initialized to FALSE */
bool_t sxStack_inited;   /* initialized to FALSE */
bool_t sxDstate_inited;  /* initialized to FALSE */

/* basic set of GC roots */
#define NLOCKS 10
SOBJ _gclocks[NLOCKS];
gcroot_t _gclroot = { NULL, GCRT_SOBJVEC, 0, (OFFTYPE)&_gclocks };

/* global chain of GC roots */
gcroot_t* gcroots = NULL;


/* forward declarations */
static SOBJ findmemory(void);
static SOBJ allocvector(sv_size_t size, int type);

static SOBJ fnodes; /* (improper) list of free nodes */

#define fn_end          (NULL) /* end of free list 'fnodes' */
#define fn_endp(f)      ((f) == fn_end) /* test end of free list     */

#ifdef _DEBUG_GC
#define GETNODE(nnode, type) \
{ sxMemGC(); \
  if( fn_endp(nnode = fnodes) ) nnode = findmemory(); \
  fnodes = cdr(nnode); nnode->n_type = (type); }
#else
#define GETNODE(nnode, type) \
{ if( fn_endp(nnode = fnodes) ) nnode = findmemory(); \
  fnodes = cdr(nnode); nnode->n_type = (type); }
#endif

#ifdef _DEBUG_GC
SOBJ _checknode(SOBJ s)
{
  assert(s != NULL);
  switch (xntype(s)) {
    default:
    case NT_FREE:
      assert(0);
    case NT_CONS:
    case NT_SYMBOL:
    case NT_FIXNUM:
    case NT_FLONUM:
    case NT_STRING:
    case NT_PORT:
    case NT_VECTOR:
    case NT_CLOSURE:
    case NT_CODE:
    case NT_SUBR:
    case NT_CSUBR:
    case NT_CHAR:
    case NT_BYTEVEC:
    case NT_PROMISE:
    case NT_HASHTABLE:
    case NT_RECORD:
    case NT_WEAKPAIR:
    case NT_KEYWORD:
    case NT_BOX:
    case NT_HANDLE:
    case NT_BOOLEAN:
    case NT_FROB:
    case NT_GCELL:
      break;
  }
  return s;
}

SOBJ* _checkvdata(SOBJ* p)
{
  SOBJ node;
  assert(p != NULL);
  node = p[-1];
  switch (xntype(node)) {
    default:
      assert(0);
    case NT_HASHTABLE:
    case NT_RECORD:
    case NT_VECTOR:
    case NT_CODE:
      assert(node->n_vdata == p);
      break;
    case NT_BYTEVEC:
      assert(node->n_bvec == (byte_t*)p);
      break;
    case NT_STRING:
    case NT_SYMBOL:
    case NT_KEYWORD:
      assert(node->n_str == (tchar_t*)p);
      break;
  }
  return p;
}

tchar_t* _checksdata(tchar_t* p)
{
  _checkvdata((SOBJ*)p);
  return p;
}

byte_t* _checkbdata(byte_t* p)
{
  _checkvdata((SOBJ*)p);
  return p;
}

#endif /* _DEBUG_GC */

/* consa - construct a new cons-like node */
SOBJ consa(SOBJ a, SOBJ d, vmtag_t stype)
{
  register SOBJ nnode;

#ifdef _DEBUG_GC
  gcLock(a);
  gcLock(d);
  sxMemGC();
  gcUnlock(2);
#endif

  /* get a free node */
  if (fn_endp(nnode = fnodes)) {
    /* protect arguments against GC before */
    gcLock(a);
    gcLock(d);
    nnode = findmemory();
    gcUnlock(2);
  }
  /* unlink the node from the free list */
  fnodes = cdr(nnode);

  /* initialize the new node */
  nnode->n_type = stype;
  setcar(nnode, a);
  setcdr(nnode, d);

  /* return the new node */
  return nnode;
}

/* cvlist - convert an SOBJ array to a list */
SOBJ cvlist(SOBJ * data, sv_size_t len, SOBJ tail)
{
  SOBJ lst, p;
  int gc = 0;

#ifdef _DEBUG_GC
  gcLock(tail);
  sxMemGC();
  gcUnlock(1);
#endif

  if (!len)
    return tail;
  if (fn_endp(fnodes)) {
    ++gc;
    gcLock(tail);
    findmemory();
    gcUnlock(1);
  }
  for (lst = p = fnodes;; p = cdr(p)) {
    p->n_type = NT_CONS;
    setcar(p, *data++);
    if (!--len)
      break;
    if (fn_endp(cdr(p))) {
      fnodes = fn_end;
      if (!gc++) {
        gcLock(lst);
        setcdr(p, tail);  /* protect tail and make list proper */
        findmemory();
        gcUnlock(1);
      } else
        sxMemExpand(nscount + 1, vscount);
      setcdr(p, fnodes);
    }
  }
  fnodes = cdr(p);
  setcdr(p, tail);
  return lst;
}

/* cvstringn - convert a static string to a string node */
SOBJ cvstringn(const tchar_t *str, ss_size_t len)
{
  SOBJ val = newstring(len);
  tmemcpy(getstring(val), str, len);
  return val;
}

/* cvstringn - convert a static zero-terminated string to a string node */
SOBJ cvstring(const tchar_t *str)
{
  return cvstringn(str, tcslen(str) + 1);
}

/* cvbytevec - convert a byte buffer to a bytevec node */
SOBJ cvbytevec(const byte_t* buf, ss_size_t len)
{
  SOBJ val = newbytevec(len);
  memcpy(getbytes(val), buf, len);
  return val;
}

SOBJ testmutable(SOBJ x)
{
  return isimmutable(x) ? sxae_immutable(x) : (x);
}


/* cvsym - convert a symbol-like object to another type */
SOBJ cvsym(SOBJ symbolic, vmtag_t stype)
{
  ss_size_t len = getslength(symbolic);
  SOBJ val;
  gcLock(symbolic);
  val = newstring(len);
  gcUnlock(1);
  /* now it is safe to get data pointer from symbolic */
  tmemcpy(getstring(val), getstring(symbolic), len);
  val->n_type = stype;
  return val;
}

/* mksym - make a symbol/keyword from a static c string */
SOBJ mksym(const tchar_t *pname, vmtag_t stype)
{
  SOBJ sym = cvstring(pname);

  sym->n_type = stype;
  return sym;
}

/* cvfixnum - convert an integer to a fixnum node */
SOBJ cvfixnum(FIXTYPE n)
{
  SOBJ val;

  if (n >= SFIXMIN && n <= SFIXMAX)
    return cvsfixnum(n);
  GETNODE(val, NT_FIXNUM);
  val->n_int = n;
  return val;
}

/* cvflonum - convert a floating point number to a flonum node */
SOBJ cvflonum(FLOTYPE n)
{
  SOBJ val;

  GETNODE(val, NT_FLONUM);
  val->n_flonum = n;
  return val;
}

/* cvport - convert a vp/dp pair to a port node */
SOBJ cvport(PORTVPTR vp, PORTDPTR dp)
{
  SOBJ val;

  GETNODE(val, NT_PORT);
  val->n_vp = vp;
  val->n_dp = dp;
  return val;
}

/* makechar - convert a large character to a char node */
SOBJ makechar(tchar_t c)
{
  SOBJ val;

  GETNODE(val, NT_CHAR);
  val->n_char = c;
  return val;
}


SOBJ makesubr(vm_subr_t subr, const tchar_t* id)
{
  SOBJ val;

  GETNODE(val, NT_SUBR);
  val->n_subr = subr;
  val->n_id = id;
  return val;
}

SOBJ makecsubr(vm_csubr_t csubr, const tchar_t* id)
{
  SOBJ val;

  GETNODE(val, NT_CSUBR);
  val->n_subr = (vm_subr_t) csubr;
  val->n_id = id;
  return val;
}

/* cvhandle - convert an oshandle and type to a handle */
SOBJ cvhandle(OSHANDLE h, int htype)
{
  SOBJ val;

  GETNODE(val, NT_HANDLE);
  sethandle(val, h);
  sethtype(val, htype);
  return val;
}

/* newvector - allocate and initialize a new vector */
SOBJ newvector(sv_size_t size, SOBJ fill)
{
  SOBJ vec = allocvector(size, NT_VECTOR);
  if (fill != NV_NO_INIT) {	/* initialize all the elements */
    register SOBJ *p;
    for (p = getvdata(vec); size; --size)
      *p++ = fill;
  }
  return vec;
}

/* newstring - allocate and initialize a new string */
SOBJ newstring(ss_size_t size)
{
  SOBJ val = allocvector(ttow_size(size), NT_STRING);
  getstring(val)[size - 1] = '\0';
  val->n_strlen = size;
  return val;
}

/* newbytevec - allocate and initialize a new string */
SOBJ newbytevec(ss_size_t size)
{
  SOBJ val = allocvector(btow_size(size), NT_BYTEVEC);
  val->n_bveclen = size;
  return val;
}

/* newcode - create a new code object */
SOBJ newcode(sv_size_t nlits)
{
  return allocvector(nlits, NT_CODE);
}

/* newcontinuation - create a new continuation vector */
SOBJ newcontinuation(void)
{
  SOBJ cont;
  sv_size_t size = (sv_size_t) (vmstktop - vmsp);

  cont = allocvector(size + 1, NT_VECTOR);
  setelement(cont, 0, vmdstate);
  sobjcpy(getvdata(cont) + 1, vmsp, size);
  return cont;
}

/* forward decl for inline allocvector */
static void findsvmemory(sv_size_t size, int gcflag);

/* newframe - create a new env frame - MUST BE FAST! */
/* all args are already GC-safe! */
SOBJ newframe(SOBJ parent, int size, SOBJ name)
{
  SOBJ val;
  SOBJ *p;
#if 1 /* inline allocvector/getvspace for speed */
  sv_size_t asize;
  assert(size >= 0);
  /* get a free node */
  GETNODE(val, NT_VECTOR);
  /* get space for content */
  asize = (sv_size_t)size + 1;
  if (vfree + asize >= vtop) {
    /* not enough space in current segment */
    if (SHOULD_MALLOC_ELTS(size)) {
      /* large vector, just use malloc */
      p = (SOBJ*)malloc(size * sizeof(SOBJ));
      if (p == NULL)
        ospanic(T("insufficient large vector space"));
      goto fill;
    }
    findsvmemory(asize, FALSE);	/* no GC! */
  }
  p = vfree;
  vfree += asize;
  *p++ = val; /* backpointer */
fill:
  val->n_vdata = p;
  val->n_vsize = size;
#else /* just call allocvector */
  assert(size >= 0);
  val = allocvector(size, NT_VECTOR);
  p = getvdata(val);
#endif
  *p++ = parent;
  *p++ = name;
  for (size -= FIRSTARG; size > 0; --size)
    *p++ = so_default;
  return val;
}

/* newhashtable - allocate and initialize a new hash table */
SOBJ newhashtable(sv_size_t size)
{
  SOBJ h;
  if (size <= 0) size = 1;
  else if (size > 200) size = 200;
  h = newvector(size, so_nil);
  h->n_type = NT_HASHTABLE;
  return h;
}

/* newrecord - allocate and initialize a new record */
SOBJ newrecord(sv_size_t size)
{
  SOBJ r;
  if (size <= 0) size = 1;
  r = newvector(size, so_false);
  r->n_type = NT_RECORD;
  return r;
}

/***********************************************************************/

/* findmemory - garbage collect, then add more node space if necessary */
static SOBJ findmemory(void)
{
  /* first try simple garbage collecting */
  sxMemGC();

  /* allocate the new segment, if not enough free space */
  if (nfree < SX_NSMIN && newnsegment(SX_NSSIZE) == NULL)
    if (fn_endp(fnodes))
      sxSysErr(T("insufficient node space"));
  return fnodes;
}

/* findsvmemory - find small vector memory (used by getvspace only) */
static void findsvmemory(sv_size_t size, int gcflag)
{
  VSEGMENT *vseg;

  assert(size <= SX_VSSIZE);

  /* uncache current segment fill pointer */
  vscurrent->vs_free = vfree;

  if ((vseg = vscurrent->vs_next) == NULL)
    vseg = vsegments;

  while (1) {
    /* look for a vector segment with enough space */
    for (; vseg != NULL; vseg = vseg->vs_next)
      if (vseg->vs_free + size <= vseg->vs_top)
        goto provide;

    if (!gcflag) { /* GC is not allowed or already done */
      /* allocate a new vector segment and make it current */
      if ((vseg = newvsegment(SX_VSSIZE)) != NULL)
        break;
      else
        ospanic(T("insufficient small vector space"));
    }
    gcflag = FALSE;
    /* try garbage collecting if not in a restore mode */
    sxMemGC();
    vseg = vsegments;
  }

provide: /* choose it as current one (cache fill pointer) */
  vscurrent = vseg;
  vfree = vseg->vs_free;
  vtop = vseg->vs_top;
}

/* getvspace - allocate vector space (used above and by 'sximage.c') */
SOBJ *getvspace(SOBJ node, sv_size_t size)
{
  SOBJ *p;
  /*  try to use small vector storage first */
  sv_size_t asize = (sv_size_t)size + 1;
  if (vfree + asize >= vtop) {
    /* not enough space in current segment */
    if (SHOULD_MALLOC_ELTS(size)) {
      /* large vector, just use malloc */
      p = (SOBJ*)malloc(size * sizeof(SOBJ));
      if (p == NULL)
        ospanic(T("insufficient large vector space"));
      return p;
    }
    findsvmemory(asize, FALSE);	/* no GC! */
  }
  p = vfree;
  vfree += asize;
  *p++ = node;
  return p;
}

/* allocvector - allocate and initialize a new vector node */
static SOBJ allocvector(sv_size_t size, int type)
{
  SOBJ val;

  /* get a free node */
  GETNODE(val, type);

  /* get space for content */
  val->n_vdata = getvspace(val, size);

  /* put size there for NT_VECTOR and other look-alikes */
  val->n_vsize = size; /* rewritten for strings and byte vectors */

  return val;
}


/* newnsegment - create a new node segment */
NSEGMENT *newnsegment(sv_size_t n)
{
  NSEGMENT *newseg;
  SOBJ p;

  /* allocate the new segment */
  if ((newseg = (NSEGMENT *) malloc(nsegsize(n))) == NULL)
    return NULL;

  /* initialize the new segment */
  newseg->ns_size = n;
  newseg->ns_next = NULL;
  if (nslast != NULL)
    nslast->ns_next = newseg;
  nslast = newseg;

  /* update the statistics */
  total += nsegsize(n);
  nnodes += n;
  nfree += n;
  ++nscount;

  /* add each new node to the free list */
  for (p = newseg->ns_data; n; --n, ++p) {
    p->n_type = NT_FREE;
    p->n_flags = NF_FREE;
#ifdef _DEBUG
    /* clear node type inside to catch errors */
    p->n_int = 1000;
#endif
    setcdr(p, fnodes);
    fnodes = p;
  }

  /* return the new segment */
  return newseg;
}

/* newvsegment - create a new vector segment */
VSEGMENT *newvsegment(sv_size_t n)
{
  VSEGMENT *newseg;

  /* allocate the new segment */
  if ((newseg = (VSEGMENT *) malloc(vsegsize(n))) == NULL)
    return NULL;

  /* initialize the new segment */
  newseg->vs_free = newseg->vs_data;
  newseg->vs_top = newseg->vs_free + n;
  newseg->vs_next = vsegments;
  vsegments = newseg;

  /* update the statistics */
  total += vsegsize(n);
  ++vscount;

  /* return the new segment */
  return newseg;
}

/* sxMemExpand - make ns node segments and vs vector segments */
void sxMemExpand(size_t ns, size_t vs)
{
  while (nscount < ns)
    if (newnsegment(SX_NSSIZE) == NULL)
      sxSysErr(T("insufficient node space"));

  while (vscount < vs)
    if (newvsegment(SX_VSSIZE) == NULL)
      sxSysErr(T("insufficient vector space"));
}

/************************ Garbage Collector ******************************/
/* GC note: automatic GC must not remove dead weak pairs from anywhere! */

static void markvector(SOBJ vect);
static void compact(void);
static void shake(void);
static sv_size_t compact_vector(VSEGMENT * vseg);
static void sweep(void);
static sv_size_t sweep_segment(NSEGMENT * nseg);
static void _mark(SOBJ ptr);


#define mark(n) { if (nodep(n)) _mark(n); }

/* sxMemGC - garbage collect */
void sxMemGC(void)
{
  gcroot_t *proot;

  /* call GC hook */
  osgchook(TRUE);
  if (!fn_endp(fnodes))
    ++vgccalls;

  /* mark all roots */
  for (proot = gcroots; proot != NULL; proot = proot->next) {
    switch (proot->rtype) {
      case GCRT_SOBJ: {
        SOBJ p = (SOBJ)proot->data1;
        mark(p);
      } break;
      case GCRT_SOBJ2: {
        SOBJ p1 = (SOBJ)proot->data1;
        SOBJ p2 = (SOBJ)proot->data2;
        mark(p1); mark(p2);
      } break;
      case GCRT_SOBJVAR: {
        SOBJ p = *(SOBJ*)proot->data1;
        mark(p);
      } break;
      case GCRT_SOBJVEC: {
        size_t objc = (size_t)proot->data1;
        SOBJ* objv = (SOBJ*)proot->data2;
        while (objc-- > 0) { mark(*objv); ++objv; }
      } break;
      case GCRT_SOBJRANGE: {
        SOBJ* from = *(SOBJ**)proot->data1;
        SOBJ* to = *(SOBJ**)proot->data2;
        while (from < to) { mark(*from); ++from; }
      } break;  
      case GCRT_ENUMPROC: {
        SOBJ* (*pe)(OFFTYPE*);
        OFFTYPE pos; SOBJ *p;
        pe = (SOBJ*(*)(OFFTYPE*))proot->data1;
        for (pos = 0; (p = (*pe)(&pos)) != NULL;) mark(*p);
      } break;
      default: assert(FALSE);
    }
  }

  shake();         /* process GC-aware objects */
  compact();       /* compact vector space */
  sweep();         /* sweep memory collecting all unmarked nodes */
  ++gccalls;       /* count the GC call */
  vmInterruptRequest(VMI_GC); /* notify VM about this GC */
  osgchook(FALSE); /* uncall GC hook */
}

/* this hook is used from virtual mark method */
void sxMemMark(SOBJ vp)
{
  mark(vp);
}

#ifdef NONREC_GC
/* mark - mark all accessible nodes (nonrecursive for both car & cdr) */
static void _mark(SOBJ ptr)  /* ptr points to real NODE here! */
{
  register SOBJ thisp, prev, tmp;

  /* initialize */
  prev = NULL; /* yes, NULL!! */
  thisp = ptr;

  /* mark thisp node */
  for (;;) {

    /* descend as far as we can */
    while (!(thisp->n_flags & NF_MARK)) {

      /* GC flags: only NF_MARK flag should be set here! */
      thisp->n_flags &= ~NF_GCFLAGS; /* should be already so? */
      thisp->n_flags |= NF_MARK;

      /* trace its children */
      switch (thisp->n_type) {
        case NT_CONS:   /* mark cons-like nodes */
        case NT_GCELL:
        case NT_BOX:
        case NT_CLOSURE:
        case NT_PROMISE:
          if (tmp = car(thisp), nodep(tmp)) {
            thisp->n_flags |= NF_LEFT;
            car(thisp) = prev;
            prev = thisp;
            thisp = tmp;
          } else if (tmp = cdr(thisp), nodep(tmp)) {
            cdr(thisp) = prev;
            prev = thisp;
            thisp = tmp;
          } else
            goto skip_share;
          break;
        case NT_WEAKPAIR:
          /* don't mark car! */
          if (tmp = cdr(thisp), nodep(tmp)) {
            cdr(thisp) = prev;
            prev = thisp;
            thisp = tmp;
          } else
            goto skip_share;
          break;
        case NT_VECTOR: /* mark vector-like nodes */
        case NT_HASHTABLE:
        case NT_RECORD:
        case NT_CODE:
          markvector(thisp);
          goto skip_share;
        case NT_PORT:
          getvp(thisp)->v_mark(getdp(thisp));
          goto skip_share;
        default: /* other types of nodes have no children */
          goto skip_share;
      }
    }
    /* thisp was marked before! */
    thisp->n_flags |= NF_SHARED;
skip_share:

    /* backup to a point where we can continue descending */
    for (;;)
      /* make sure there is a previous node */
      if (prev != NULL) {
        if (prev->n_flags & NF_LEFT) {  /* came from left side */
          prev->n_flags &= ~NF_LEFT;
          tmp = car(prev);
          car(prev) = thisp;
          if (thisp = cdr(prev), nodep(thisp)) {
            cdr(prev) = tmp;
            break;
          }
        } else {    /* came from right side */
          tmp = cdr(prev);
          cdr(prev) = thisp;
        }
        thisp = prev;   /* step back up the branch */
        prev = tmp;
      }
    /* no previous node, must be done */
      else
        return;
  }
}

#else /* not def NONREC_GC */
/* mark - mark all accessible nodes (nonrecursive for cdr only) */
static void _mark(SOBJ ptr) /* ptr points to real NODE here! */
{
  /* NOTE: it's crucial not to have local variables here! */
  /* mark this node and cdr chain nodes if any */
  while (!(ptr->n_flags & NF_MARK)) {
    /* only NF_MARK flag should be set! */
    ptr->n_flags &= ~NF_GCFLAGS; /* should be already so? */
    ptr->n_flags |= NF_MARK;
    /* all other flags (if any) remain intact! */
    switch (ptr->n_type) {
      case NT_CONS: /* mark cons-like nodes */
      case NT_GCELL:
      case NT_BOX:
      case NT_CLOSURE:
      case NT_PROMISE:
        /* mark car & cdr */
        if (nodep(cdr(ptr))) {
          mark(car(ptr));
          ptr = cdr(ptr);
          break;
        }
        if (!nodep(car(ptr)))
          return;
        ptr = car(ptr);
        break;
      case NT_WEAKPAIR:
        /* mark only cdr */
        if (!nodep(cdr(ptr)))
          return;
        ptr = cdr(ptr);
        break;
      case NT_VECTOR: /* mark vector-like nodes */
      case NT_HASHTABLE:
      case NT_RECORD:
      case NT_CODE:
        markvector(ptr);
        return;
      case NT_PORT:
        getvp(ptr)->v_mark(getdp(ptr));
        return;
      default:			/* other types of nodes have no children */
        return;
    }
  }
  ptr->n_flags |= NF_SHARED;
}

#endif /* def NONREC_GC */

/* markvector - mark a vector-like node */
static void markvector(SOBJ vect)
{
  SOBJ *p; sv_size_t n;
  for (p = getvdata(vect), n = getvsize(vect); n; ++p, --n) mark(*p);
}

/* shake - process all GC-aware objects */
static void shake(void)
{
  NSEGMENT *nseg;
  /* process each node segment */
  for (nseg = nsegments; nseg != NULL; nseg = nseg->ns_next) {
    sv_size_t n; SOBJ p;
    /* process marked nodes only */
    for (p = nseg->ns_data, n = nseg->ns_size; n != 0; ++p, --n) {
      if (p->n_flags & NF_MARK) {
        switch (p->n_type) {
          case NT_WEAKPAIR: {
            /* if car pointer is weak, break it */
            SOBJ tmp = car(p);
            if (nodep(tmp) && !(tmp->n_flags & NF_MARK))
              setcar(p, so_bwp);
          } break;
        }
      }
    }
  }
}

#ifdef _DEBUG_GC
static void permute_vector(VSEGMENT * vseg)
{
  static vbuf[SX_VSSIZE+1];
  SOBJ *vprev, *vcur, *vfree;
  vprev = NULL;
  vcur = vseg->vs_data;
  vfree = vseg->vs_free;
  while (vcur < vfree) {
    SOBJ curvec = *vcur;
    sv_size_t vcursize;
    switch (curvec->n_type) {
      case NT_STRING:
      case NT_SYMBOL:
      case NT_KEYWORD:
        vcursize = ttow_size(curvec->n_vsize) + 1;
        break;
      case NT_BYTEVEC:
        vcursize = btow_size(curvec->n_vsize) + 1;
        break;
      default:
        vcursize = curvec->n_vsize + 1;
    }
    if (vprev != NULL) {
      /* swap prev & cur and fix backpointers */
      SOBJ prevvec = *vprev;
      sv_size_t vprevsize = vcur - vprev;
      sobjcpy(vbuf, vprev, vprevsize);
      sobjmove(vprev, vcur, vcursize);
      sobjcpy(vprev + vcursize, vbuf, vprevsize);
      *vprev = curvec;
      curvec->n_vdata = vprev + 1;
      *(vprev + vcursize) = prevvec;
      prevvec->n_vdata = vprev + vcursize + 1;
      vcur = vprev + vcursize + vprevsize; 
      vprev = vprev + vcursize;
    } else {
      vprev = vcur;
      vcur += vcursize;
    }
  }
}
#endif 

/* compact - compact small vector space */
static void compact(void)
{
  VSEGMENT *vseg;
  long freeptrs = 0;

  /* uncache the current vector segment fill pointer */
  vscurrent->vs_free = vfree;

  /* compact each vector segment */
  for (vseg = vsegments; vseg != NULL; vseg = vseg->vs_next) {
    freeptrs += compact_vector(vseg);
#ifdef _DEBUG_GC
    permute_vector(vseg);
#endif
  }

  /* try add one more segment if memory low */
  if (freeptrs < SX_VSMIN)
    newvsegment(SX_VSSIZE);

  /* make the first vector segment current (cache pointers) */
  if ((vscurrent = vsegments) != NULL) {
    vfree = vscurrent->vs_free;
    vtop = vscurrent->vs_top;
  }
}

/* compact_vector - compact a small vector segment */
static sv_size_t compact_vector(VSEGMENT * vseg)
{
  SOBJ *vdata, *vnext, *vfree;

  vdata = vnext = vseg->vs_data;
  vfree = vseg->vs_free;
  while (vdata < vfree) {
    SOBJ vector = *vdata;	/* get backpointer to vector node */
    sv_size_t vsize;

    switch (vector->n_type) {
      case NT_STRING:
      case NT_SYMBOL:
      case NT_KEYWORD:
        vsize = ttow_size(vector->n_vsize) + 1;
        break;
      case NT_BYTEVEC:
        vsize = btow_size(vector->n_vsize) + 1;
        break;
      default:
        vsize = vector->n_vsize + 1;
    }
    if (vector->n_flags & NF_MARK) {
      if (vdata != vnext) {
        vector->n_vdata = vnext + 1;  /* update Vpointer */
        sobjmvup(vnext, vdata, vsize);
      }
      vnext += vsize;
    }
    vdata += vsize;
  }

  vseg->vs_free = vnext;
  return (sv_size_t) (vseg->vs_top - vnext);
}

/* sweep - sweep all unmarked nodes and add them to the free list */
static void sweep(void)
{
  NSEGMENT *nseg;

  fnodes = fn_end;		/* empty the free list */
  nfree = 0L;			/* reset free nodes counter */

  /* sweep each node segment */
  for (nseg = nsegments; nseg != NULL; nseg = nseg->ns_next)
    nfree += sweep_segment(nseg);
}

/* sweep_segment - sweep a node segment */
static sv_size_t sweep_segment(NSEGMENT * nseg)
{
  sv_size_t n, nfree;
  SOBJ p;

  /* add all unmarked nodes */
  for (nfree = 0, p = nseg->ns_data, n = nseg->ns_size; n != 0; ++p, --n) {
    if (!(p->n_flags & NF_MARK)) {
      sv_size_t vsize;
      /* finalize garbage objects and add them to free list */
      switch (p->n_type) {
        case NT_STRING:
        case NT_SYMBOL:
        case NT_KEYWORD:
          vsize = ttow_size(p->n_strlen);
          goto finalize_vdata;
        case NT_BYTEVEC:
          vsize = btow_size(p->n_bveclen);
          goto finalize_vdata;
        case NT_CODE:
        case NT_HASHTABLE:
        case NT_RECORD:
        case NT_VECTOR:
          vsize = p->n_vsize;
        finalize_vdata:
          if (SHOULD_MALLOC_ELTS(vsize)) {
            /* large vector allocated by malloc: free it here */
            assert(p->n_vdata != NULL);
            free(p->n_vdata);
          } /* else: small vector; taken care of by compact phase */
          break;
        case NT_PORT:
          sxPortClose(p);
          break;
        case NT_HANDLE:
          osfreehandle(gethandle(p), gethtype(p));
          break;
#ifdef _DEBUG
        /* to catch errors earlier make this list exaustive */
        case NT_CONS:
        case NT_FIXNUM:
        case NT_FLONUM:
        case NT_SUBR:
        case NT_CSUBR:
        case NT_CHAR:
        case NT_PROMISE:
        case NT_BOX:
        case NT_BOOLEAN:
        case NT_CLOSURE:
        case NT_FROB:
        case NT_GCELL:
          break; /* no finalization needed */
        case NT_WEAKPAIR:
          break; /* shake phase did it all: no more finalization needed */
        case NT_FREE:
          break; /* part of the previous free chain */
        default:
          /* check p->n_type: free node or garbage? */
          assert(0);  
#endif      
      }
#ifdef _DEBUG
      /* save node type inside to catch errors */
      if (p->n_type != NT_FREE) /* not a part of the previous free chain */
        p->n_int = p->n_type;
#endif
      p->n_type = NT_FREE;
      p->n_flags = NF_FREE;
      setcdr(p, fnodes);
      fnodes = p;
      ++nfree;
    } else {
      /* prepare live objects for future mark phase */
      p->n_flags &= ~NF_GCFLAGS;
    }
  }

  return nfree;
}

static void lowMemory(void)
{
  ospanic(T("insufficient memory"));
}


/* sxMemInit - initialize the memory image */
void sxMemInit()
{
  if (sxMem_inited) return; /* already inited */

  /* initialize our internal variables */
  gccalls = 0;
  total = 0L;

  /* initialize node space */
  nsegments = nslast = NULL;
  nscount = 0;
  nnodes = nfree = 0L;
  fnodes = fn_end;

  /* initialize vector space */
  vsegments = vscurrent = NULL;
  vscount = 0;
  vfree = vtop = NULL;

  /* initialize structures that are marked by the collector */
  vmfun = vmenv = vmdstate = so_nil; /* clear dstate too! */
  if ((newnsegment(SX_NSSIZE) == NULL)) lowMemory();
  if ((newvsegment(SX_VSSIZE) == NULL)) lowMemory();
  nsegments = nslast;
  vscurrent = vsegments;
  vfree = vscurrent->vs_free;
  vtop = vscurrent->vs_top;

  /* [?] initialize the dynamic state: (#f)! */
  vmdstate = cons(so_false, so_nil);

  /* initialize basic set of roots */
  gcroots = NULL;
  { /* locks */
    gcUnlockAll();
    pushroot(_gclroot);
  }
  { /* system tables */
    static gcroot_t _subrtblroot = { NULL, GCRT_ENUMPROC, (OFFTYPE)&enum_subr_table, 0 };
    static gcroot_t _csubrtblroot = { NULL, GCRT_ENUMPROC, (OFFTYPE)&enum_csubr_table, 0 };
    static gcroot_t _datatblroot = { NULL, GCRT_ENUMPROC, (OFFTYPE)&enum_data_table, 0 };
    static gcroot_t _vartblroot = { NULL, GCRT_ENUMPROC, (OFFTYPE)&enum_var_table, 0 };
    pushroot(_subrtblroot);
    pushroot(_csubrtblroot);
    pushroot(_datatblroot);
    pushroot(_vartblroot);
  }
  { /* VM registers and stack */
    static gcroot_t _vmfunroot = { NULL, GCRT_SOBJVAR, (OFFTYPE)&vmfun, 0 };
    static gcroot_t _vmenvroot = { NULL, GCRT_SOBJVAR, (OFFTYPE)&vmenv, 0 };
    static gcroot_t _vmdsroot = { NULL, GCRT_SOBJVAR, (OFFTYPE)&vmdstate, 0 };
    static gcroot_t _vmstackroot = { NULL, GCRT_SOBJRANGE, (OFFTYPE)&vmsp, (OFFTYPE)&vmstktop };
    pushroot(_vmfunroot);
    pushroot(_vmenvroot);
    pushroot(_vmdsroot);
    pushroot(_vmstackroot);
  }

  /* set init flag */
  sxMem_inited = TRUE;
}


/* sxMemFree - free the current memory image (if any) */
void sxMemFree(void)
{
  SOBJ p;
  int n;

  if (!sxMem_inited)
    return;			/* nothing to free */

  /* drop all the roots */
  gcroots = NULL;

  /* reset registers, including dstate (SIC!) */
  vmfun = vmenv = vmdstate = so_nil;
  fnodes = fn_end;

  /* close all open ports & handles and free each node segment */
  while (nsegments != NULL) {
    NSEGMENT *nseg = nsegments->ns_next;

    p = nsegments->ns_data;
    n = nsegments->ns_size;
    for (; --n >= 0; ++p)
      switch (p->n_type) {
        case NT_PORT:
          sxPortClose(p);
          break;
        case NT_HANDLE:
          osfreehandle(gethandle(p), gethtype(p));
          break;
      }
    free(nsegments);
    nsegments = nseg;
  }

  /* free all vector segments */
  while (vsegments != NULL) {
    VSEGMENT *vseg = vsegments->vs_next;

    free(vsegments);
    vsegments = vseg;
  }

  /* clear init flag */
  sxMem_inited = FALSE;
}

/* sxDstateInit - initialize dynamic state */
void sxDstateInit(void)
{
  if (sxDstate_inited) return; /* already inited */

  /* initialize dynamic state to (#f) if needed */
  if (vmdstate == so_nil)
    vmdstate = cons(so_false, so_nil);

  sxDstate_inited = TRUE;
}

/* sxDstateFree - free the dynamic state */
void sxDstateFree(void)
{
  if (!sxDstate_inited)
    return;			/* nothing to free */

  /* set to nil, which is not a legal dstate, but
     still good for GC purposes */
  vmdstate = so_nil;

  /* clear init flag */
  sxDstate_inited = FALSE;
}

/* sxStackInit - initialize the (new) stack */
void sxStackInit(sv_size_t ssize)
{
  if (sxStack_inited) return; /* already inited */

  /* allocate the value stack */
  if ((vmstkbase = (SOBJ *) malloc(ssize * sizeof(SOBJ))) == NULL)
    lowMemory();
  total += ssize * sizeof(SOBJ);

  /* initialize stack registers */
  vmsp = vmstktop = vmstkbase + ssize;

  /* set init flag */
  sxStack_inited = TRUE;
}

/* sxStackFree - free the (active) stack */
void sxStackFree(void)
{
  if (!sxStack_inited)
    return;			/* nothing to free */

  /* free the stack memory */
  free(vmstkbase);

  /* reset registers so GC can still run */
  vmsp = vmstktop = vmstkbase = NULL;

  /* set init flag */
  sxStack_inited = FALSE;
}


extern jmp_buf vm_dispatch;



SOBJ sxExecuteInNestedVM(SOBJ fun, SOBJ arglist, sv_size_t ssize)
{
  SOBJ result, gcsafe[NLOCKS+2];
  gcroot_t root1 = {NULL, GCRT_SOBJVEC, 0, 0};
  gcroot_t root2 = {NULL, GCRT_SOBJVEC, 0, 0};
  bool_t old_sxStack_inited = sxStack_inited;
  SOBJ* old_vmsp = vmsp; 
  SOBJ* old_vmstkbase = vmstkbase; 
  SOBJ* old_vmstktop = vmstktop;
  SOBJ old_vmfun = vmfun;
  SOBJ old_vmenv = vmenv;
  int old_vmargc = vmargc;
  jmp_buf old_dispatch;
  memcpy(old_dispatch, vm_dispatch, sizeof(jmp_buf)); 

  root1.data1 = vmstktop - vmsp;
  root1.data2 = (OFFTYPE)vmsp;
  pushroot(root1);
  gcsafe[0] = old_vmfun;
  gcsafe[1] = old_vmenv;
  sobjcpy(&gcsafe[2], (SOBJ*)_gclroot.data2, (size_t)_gclroot.data1);
  root2.data1 = 2 + _gclroot.data1;
  root2.data2 = (OFFTYPE)gcsafe;
  pushroot(root2);

  sxStack_inited = FALSE;
  sxStackInit(ssize);
  result = sxExecute(fun, arglist);
  sxStackFree();
  sxStack_inited = old_sxStack_inited;

  poproot();
  _gclroot.data1 = root2.data1 - 2;
  sobjcpy((SOBJ*)_gclroot.data2, &gcsafe[2], (size_t)_gclroot.data1);
  poproot();

  memcpy(vm_dispatch, old_dispatch, sizeof(jmp_buf)); 
  vmargc = old_vmargc;
  vmenv = old_vmenv;
  vmfun = old_vmfun;
  vmsp = old_vmsp; 
  vmstkbase = old_vmstkbase; 
  vmstktop = old_vmstktop;
  return result;
}

