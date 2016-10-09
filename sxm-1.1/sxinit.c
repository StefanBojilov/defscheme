/* sxinit.c - initialization routines */

#include "sxm.h"
#include "vmop.h"
#include "os.h"
#include "sxintern.h"
#include "sxlocale.h"
#include "sximage.h"
#include "sxio.h"
#include "extern.h"

#ifdef CPPTABLES /* C++ : tables are collected by static constructors */
#ifndef __cplusplus
#error this is C++!!
#endif
#include "define.h"
#endif /* def CPPTABLES */

/******************** enode table *************************/

static void setup_node_tabs(void)
{
  int i;
  /* booleans */
  for (i=0; i < 2; i++) {
    bool_tab[i].n_type  = NT_BOOLEAN;
    bool_tab[i].n_flags = NF_IMMUTABLE;
  }
  /* characters */
  for (i=0; i < NSCHARS; i++) {
    small_char_tab[i].n_type  = NT_CHAR;
    small_char_tab[i].n_flags = NF_IMMUTABLE;
    small_char_tab[i].n_char = (tchar_t)i;
  }
  /* frobs */
  for (i=0; i < NFROBS; i++) {
    frob_tab[i].n_type  = NT_FROB;
    frob_tab[i].n_flags = NF_IMMUTABLE;
  }
}

/******************** subr table *************************/

#ifdef CPPTABLES /* C++ : tables are collected by static constructors */

_subr_link* _subr_link::root = NULL;

static bool_t do_enum_subr_table(OFFTYPE* rpos, SOBJ** pvp, vm_subr_t* psp,
                              const tchar_t** pid)
{
  _subr_link** lpp = (_subr_link**)*rpos;
  if (lpp == 0) lpp = &_subr_link::root;
  if (*lpp == NULL) return FALSE;
  *rpos = (OFFTYPE)(&((*lpp)->m_next));
  *pvp = (*lpp)->m_vp; *psp = (*lpp)->m_sp; *pid = (*lpp)->m_id;
  return TRUE;
}

#else /* plain C: tables are collected by mktab utility */

/* subr declarations */
#define PROCEDURE(name) extern SOBJ name; extern SOBJ p_##name (void);
#define PROCEDURE_VARACCESS(name,var,type) extern SOBJ name; extern SOBJ p_##name (void);
#include "all.pt"
#undef PROCEDURE
#undef PROCEDURE_VARACCESS

/* subr table */
#define PROCEDURE(name) &name,
#define PROCEDURE_VARACCESS(name,var,type) &name,
static SOBJ* _subr_tab[] = {
#include "all.pt"
};
#undef PROCEDURE
#undef PROCEDURE_VARACCESS

#define NUMSUBRS (sizeof(_subr_tab)/sizeof(_subr_tab[0]))

/* subr proc table */
#define PROCEDURE(name) p_##name,
#define PROCEDURE_VARACCESS(name,var,type) p_##name,
static vm_subr_t _subr_proc_tab [] = {
#include "all.pt"
};
#undef PROCEDURE
#undef PROCEDURE_VARACCESS

/* subr id table */
#define PROCEDURE(name) T(#name),
#define PROCEDURE_VARACCESS(name,var,type) T(#name),
static const tchar_t* _subr_id_tab [] = {
#include "all.pt"
};
#undef PROCEDURE
#undef PROCEDURE_VARACCESS

static bool_t do_enum_subr_table(OFFTYPE* rpos, SOBJ** pvp, vm_subr_t* psp,
				 const tchar_t** pid)
{
  int idx = (int)*rpos;
  if (idx < 0 || idx >= NUMSUBRS) return FALSE;
  *rpos = (OFFTYPE)(idx + 1);
  *pvp = _subr_tab[idx];
  *psp = _subr_proc_tab[idx];
  *pid = _subr_id_tab[idx];
  return TRUE;
}

#endif /* def CPPTABLES */

/*************** subr table setup (C/C++) *********************/

static void setup_subr_tab(void)
{
  /* init table once to #f's that are not allocated in the image */
  SOBJ* vp; vm_subr_t sp; const tchar_t* id;
  OFFTYPE pos;
  for (pos = 0; do_enum_subr_table(&pos, &vp, &sp, &id);)
    *vp = so_false;
}

static void install_subr_tab(void)
{
  SOBJ* vp; vm_subr_t sp; const tchar_t* id;
  OFFTYPE pos;
  for (pos = 0; do_enum_subr_table(&pos, &vp, &sp, &id);)
    *vp = makesubr(sp, id);
}

/* public enumerator for gc & image */
SOBJ* enum_subr_table(OFFTYPE* rpos)
{
  SOBJ* vp; vm_subr_t sp; const tchar_t* id;
  if (do_enum_subr_table(rpos, &vp, &sp, &id)) return vp;
  else return NULL;
}

static SOBJ _subr_stub(void)
{
  sxFail(T("primitive is not implemented in this runtime"));
  never_returns(so_false);
}

/* public accessor for image restore */
void get_subr(const tchar_t* name, vm_subr_t* ps, const tchar_t** pid)
{
  SOBJ* vp; vm_subr_t sp; const tchar_t* id;
  OFFTYPE pos;
  tchar_t buf[SX_MAXSYMBOL + 100];

  for (pos = 0; do_enum_subr_table(&pos, &vp, &sp, &id);) {
    if (tcscmp(name, id) == 0) {
      *ps = sp; *pid = id;
      return;
    }
  }

#if 1
  /* subr is not found; substitute subr_stub
     to delay error reporting until it is actually called */
  *ps = _subr_stub;
  /* this will cause memory leaks, but I guess it's OK here */
  *pid = tcscat((tchar_t*)malloc(sizeof(tchar_t)*(tcslen(name)+1)), name);
  stprintf(buf, T("Warning: Subr not supported: %s"), name);
  osmsg(buf);
#else
  ospanic(T("incompatible image file"));
  never_returns(NULL);
#endif
}

/**************** csubr table *******************/

#ifdef CPPTABLES /* C++ : tables are collected by static constructors */

_csubr_link* _csubr_link::root = NULL;

static bool_t do_enum_csubr_table(OFFTYPE* rpos, SOBJ** pvp, vm_csubr_t* psp,
                               const tchar_t** pid)
{
  _csubr_link** lpp = (_csubr_link**)*rpos;
  if (lpp == 0) lpp = &_csubr_link::root;
  if (*lpp == NULL) return FALSE;
  *rpos = (OFFTYPE)(&((*lpp)->m_next));
  *pvp = (*lpp)->m_vp; *psp = (*lpp)->m_sp; *pid = (*lpp)->m_id;
  return TRUE;
}

#else /* plain C: tables are collected by mktab utility */

/* csubr declarations */
#define CONTINUATION(name,a) extern SOBJ name; extern SOBJ cp_##name(SOBJ);
#include "all.kt"
#undef CONTINUATION

/* csubr table */
#define CONTINUATION(name,a) &name,
static SOBJ* _csubr_tab[] = {
#include "all.kt"
};
#undef CONTINUATION

/* number of csubrs */
#define NUMCSUBRS (sizeof(_csubr_tab)/sizeof(_csubr_tab[0]))

/* csubr proc table */
#define CONTINUATION(name,a) cp_##name,
static vm_csubr_t _csubr_proc_tab [] = {
#include "all.kt"
};
#undef CONTINUATION

/* csubr id table */
#define CONTINUATION(name,a) T(#name),
static const tchar_t* _csubr_id_tab [] = {
#include "all.kt"
};
#undef CONTINUATION

static bool_t do_enum_csubr_table(OFFTYPE* rpos, SOBJ** pvp, vm_csubr_t* psp,
				  const tchar_t** pid)
{
  int idx = (int)*rpos;
  if (idx < 0 || idx >= NUMCSUBRS) return FALSE;
  *rpos = (OFFTYPE)(idx + 1);
  *pvp = _csubr_tab[idx];
  *psp = _csubr_proc_tab[idx];
  *pid = _csubr_id_tab[idx];
  return TRUE;
}

#endif /* def CPPTABLES */

/*************** csubr table setup (C/C++) *********************/

static void setup_csubr_tab(void)
{
  /* init table once to #f's that are not allocated in the image */
  SOBJ* vp; vm_csubr_t sp; const tchar_t* id;
  OFFTYPE pos;
  for (pos = 0; do_enum_csubr_table(&pos, &vp, &sp, &id);)
    *vp = so_false;
}

static void install_csubr_tab(void)
{
  SOBJ* vp; vm_csubr_t sp; const tchar_t* id;
  OFFTYPE pos;
  for (pos = 0; do_enum_csubr_table(&pos, &vp, &sp, &id);)
    *vp = makecsubr(sp, id);
}

/* public enumerator for gc & image */
SOBJ* enum_csubr_table(OFFTYPE* rpos)
{
  SOBJ* vp; vm_csubr_t sp; const tchar_t* id;
  if (do_enum_csubr_table(rpos, &vp, &sp, &id)) return vp;
  else return NULL;
}

static SOBJ _csubr_stub(SOBJ result)
{
  sxFail(T("primitive continuation is not implemented in this runtime"));
  CONT_RETURN(result);
}

/* public accessor for image restore */
void get_csubr(const tchar_t* name, vm_csubr_t* ps, const tchar_t** pid)
{
  SOBJ* vp; vm_csubr_t sp; const tchar_t* id;
  OFFTYPE pos;
  tchar_t buf[SX_MAXSYMBOL + 100];

  for (pos = 0; do_enum_csubr_table(&pos, &vp, &sp, &id);) {
    if (tcscmp(name, id) == 0) {
      *ps = sp; *pid = id;
      return;
    }
  }

#if 1
  /* csubr is not found; substitute csubr_stub
     to delay error reporting until it is actually called */
  *ps = _csubr_stub;
  /* this will cause memory leaks, but I guess it's OK here */
  *pid = tcscat((tchar_t*)malloc(sizeof(tchar_t)*(tcslen(name)+1)), name);
  stprintf(buf, T("Warning: CSubr not supported: %s"), name);
  osmsg(buf);
#else
  ospanic(T("incompatible image file"));
  never_returns(NULL);
#endif
}


/****************** data table ***********************/

#ifdef CPPTABLES /* C++ : tables are collected by static constructors */

_data_link* _data_link::root = NULL;

static bool_t do_enum_data_table(OFFTYPE* rpos, SOBJ** pvp, vm_subr_t* psp)
{
  _data_link** lpp = (_data_link**)*rpos;
  if (lpp == 0) lpp = &_data_link::root;
  if (*lpp == NULL) return FALSE;
  *rpos = (OFFTYPE)(&((*lpp)->m_next));
  *pvp = (*lpp)->m_vp; *psp = (*lpp)->m_sp;
  return TRUE;
}

#else /* plain C: tables are collected by mktab utility */

/* data declarations */
#define DATUM_INIT(name) extern SOBJ name; extern SOBJ ip_##name (void);
#define DATUM_SYMBOL(name,str) extern SOBJ name; extern SOBJ ip_##name (void);
#define DATUM_KEYWORD(name,str) extern SOBJ name; extern SOBJ ip_##name (void);
#define DATUM_STRING(name,str) extern SOBJ name; extern SOBJ ip_##name (void);
#define DATUM_DISPATCHER(name,str) extern SOBJ name; extern SOBJ ip_##name (void);
#include "all.dt"
#undef DATUM_INIT
#undef DATUM_SYMBOL
#undef DATUM_KEYWORD
#undef DATUM_STRING
#undef DATUM_DISPATCHER

/* data table */
#define DATUM_INIT(name) &name,
#define DATUM_SYMBOL(name,str) &name,
#define DATUM_KEYWORD(name,str) &name,
#define DATUM_STRING(name,str) &name,
#define DATUM_DISPATCHER(name,str) &name,
static SOBJ* _data_tab[] = {
#include "all.dt"
};
#undef DATUM_INIT
#undef DATUM_SYMBOL
#undef DATUM_KEYWORD
#undef DATUM_STRING
#undef DATUM_DISPATCHER

/* number of data entries */
#define NUMDATA (sizeof(_data_tab)/sizeof(_data_tab[0]))

/* init proc table */
#define DATUM_INIT(name) ip_##name,
#define DATUM_SYMBOL(name,str) ip_##name,
#define DATUM_KEYWORD(name,str) ip_##name,
#define DATUM_STRING(name,str) ip_##name,
#define DATUM_DISPATCHER(name,str) ip_##name,
static vm_subr_t _data_init_tab [] = {
#include "all.dt"
};
#undef DATUM_INIT
#undef DATUM_SYMBOL
#undef DATUM_KEYWORD
#undef DATUM_STRING
#undef DATUM_DISPATCHER

static bool_t do_enum_data_table(OFFTYPE* rpos, SOBJ** pvp, vm_subr_t* psp)
{
  int idx = (int)*rpos;
  if (idx < 0 || idx >= NUMDATA) return FALSE;
  *rpos = (OFFTYPE)(idx + 1);
  *pvp = _data_tab[idx];
  *psp = _data_init_tab[idx];
  return TRUE;
}

#endif /* def CPPTABLES */

/*************** data table setup (C/C++) *********************/

static void setup_data_tab(void)
{
  /* init table once to #f's that are not allocated in the image */
  SOBJ* vp; vm_subr_t sp;
  OFFTYPE pos;
  for (pos = 0; do_enum_data_table(&pos, &vp, &sp);)
    *vp = so_false;
}

static void install_data_tab(void)
{
  SOBJ* vp; vm_subr_t sp;
  OFFTYPE pos;
  for (pos = 0; do_enum_data_table(&pos, &vp, &sp);)
    *vp = (*sp)();
}

/* public enumerator for gc & image */
SOBJ* enum_data_table(OFFTYPE* rpos)
{
  SOBJ* vp; vm_subr_t sp;
  if (do_enum_data_table(rpos, &vp, &sp)) return vp;
  else return NULL;
}

/**************** initial bindings table *****************/

#ifdef CPPTABLES /* C++ : tables are collected by static constructors */

_ibinding_link* _ibinding_link::root = NULL;

static bool_t do_enum_ibindings_table(OFFTYPE* rpos, SOBJ** pvp, const tchar_t** psp)
{
  _ibinding_link** lpp = (_ibinding_link**)*rpos;
  if (lpp == 0) lpp = &_ibinding_link::root;
  if (*lpp == NULL) return FALSE;
  *rpos = (OFFTYPE)(&((*lpp)->m_next));
  *pvp = (*lpp)->m_vp; *psp = (*lpp)->m_sp;
  return TRUE;
}

#else /* plain C: tables are collected by mktab utility */

/* initial bindings table entry structure */
typedef struct {
  const tchar_t* str;
  SOBJ* sop;
} IBINDDEF, *IBINDDEFPTR;

/* initial bindings table */
/* NOTE: all relevant global variables were declared in tables above */
#define INITIAL_BINDING(str, name) {T(str), &name},
static IBINDDEF _ibindings_tab [] = {
#include "all.ibt"
};
#undef INITIAL_BINDING

/* number of ibindings */
#define NUMIBINDINGS (sizeof(_ibindings_tab)/sizeof(_ibindings_tab[0]))

static bool_t do_enum_ibindings_table(OFFTYPE* rpos, SOBJ** pvp, const tchar_t** psp)
{
  int idx = (int)*rpos;
  if (idx < 0 || idx >= NUMIBINDINGS) return FALSE;
  *rpos = (OFFTYPE)(idx + 1);
  *pvp = _ibindings_tab[idx].sop;
  *psp = _ibindings_tab[idx].str;
  return TRUE;
}

#endif /* def CPPTABLES */

/************** bindings table setup (C/C++) *********************/

static void setup_ibindings_tab(void)
{
}

static void install_ibindings_tab(void)
{
  SOBJ* vp; const tchar_t* sp;
  OFFTYPE pos;
  for (pos = 0; do_enum_ibindings_table(&pos, &vp, &sp);) {
    SOBJ sym = sxSymEnter(sp);
    gcLock(sym);
    setsymval(sym, *vp);
    gcUnlock(1);
  }
}

/****************** variable table ***********************/

#ifdef CPPTABLES /* C++ : tables are collected by static constructors */

_var_link* _var_link::root = NULL;

static bool_t do_enum_var_table(OFFTYPE* rpos, SOBJ** pvp, vm_subr_t* psp)
{
  _var_link** lpp = (_var_link**)*rpos;
  if (lpp == 0) lpp = &_var_link::root;
  if (*lpp == NULL) return FALSE;
  *rpos = (OFFTYPE)(&((*lpp)->m_next));
  *pvp = (*lpp)->m_vp; *psp = (*lpp)->m_sp;
  return TRUE;
}

#else /* plain C: tables are collected by mktab utility */

/* variables declarations */
#define VARIABLE(name) extern SOBJ name;
#define VARIABLE_INIT(name) extern SOBJ name; extern SOBJ rp_##name (void);
#define VARIABLE_VARINIT(name, val) extern SOBJ name; extern SOBJ rp_##name (void);
#include "all.vt"
#undef VARIABLE
#undef VARIABLE_INIT
#undef VARIABLE_VARINIT

/* variable table */
#define VARIABLE(name) &name,
#define VARIABLE_INIT(name) &name,
#define VARIABLE_VARINIT(name, val) &name,
static SOBJ* _var_tab[] = {
#include "all.vt"
};
#undef VARIABLE
#undef VARIABLE_INIT
#undef VARIABLE_VARINIT

/* number of variables */
#define NUMVARS (sizeof(_var_tab)/sizeof(_var_tab[0]))

/* init proc table */
#define VARIABLE(name) NULL,
#define VARIABLE_INIT(name) rp_##name,
#define VARIABLE_VARINIT(name, val) rp_##name,
static vm_subr_t _var_init_tab [] = {
#include "all.vt"
};
#undef VARIABLE
#undef VARIABLE_INIT
#undef VARIABLE_VARINIT

static bool_t do_enum_var_table(OFFTYPE* rpos, SOBJ** pvp, vm_subr_t* psp)
{
  int idx = (int)*rpos;
  if (idx < 0 || idx >= NUMVARS) return FALSE;
  *rpos = (OFFTYPE)(idx + 1);
  *pvp = _var_tab[idx];
  *psp = _var_init_tab[idx];
  return TRUE;
}

#endif /* def CPPTABLES */

/************** variables table setup (C/C++) *********************/

static void setup_var_tab(void)
{
  /* init table once to frobs that are not allocated in the image */
  SOBJ* vp; vm_subr_t sp;
  OFFTYPE pos;
  for (pos = 0; do_enum_var_table(&pos, &vp, &sp);)
    *vp = so_unbound;
}

static void install_var_tab(void)
{
  SOBJ* vp; vm_subr_t sp;
  OFFTYPE pos;
  for (pos = 0; do_enum_var_table(&pos, &vp, &sp);)
    if (sp != NULL) *vp = (*sp)();
}


/* public enumerator for gc & image */
SOBJ* enum_var_table(OFFTYPE* rpos)
{
  SOBJ* vp; vm_subr_t sp;
  if (do_enum_var_table(rpos, &vp, &sp)) return vp;
  else return NULL;
}


/*********************************************************/

/* sxSetup - first-time static initialization */
void sxSetup(void)
{
  /* clear sxMem initialization flag */
  extern bool_t sxMem_inited;
  extern bool_t sxStack_inited;
  sxMem_inited = FALSE;
  sxStack_inited = FALSE;
  setup_node_tabs();     /* here */
  setup_subr_tab();      /* here */
  setup_csubr_tab();     /* here */
  setup_port_tab();      /* in sxio.c */
  setup_data_tab();      /* here */
  setup_var_tab();       /* here */
  setup_ibindings_tab(); /* here */
}

void sxCleanup(void)
{
  sxMemFree();
  sxStackFree();
}


EXTERN_VARIABLE(sv_symtable)
EXTERN_VARIABLE(sv_keytable)
EXTERN_VARIABLE(sv_curloc)
EXTERN_VARIABLE(sv_curstart)


/* sxImgInit - create an initial image */
SOBJ sxImgInit(sv_size_t stacksize)
{
  extern jmp_buf vm_dispatch;

  /* catch an initialization errors */
  if (setjmp(vm_dispatch))
    ospanic(T("Initialization code fails"));

  /* free old image (if any) */
  sxMemFree();
  sxDstateFree();
  sxStackFree();

  /* allocate memory for new image */
  sxMemInit();
  sxDstateInit();
  sxStackInit(stacksize);

  /* initialize global resources first */
  sv_symtable = newvector(SX_STSIZE, so_nil);
  /* now sxSymEnter is legal */
  sv_keytable = newvector(SX_KTSIZE, so_nil);
  /* now sxKeyEnter is legal */
  sv_curloc = newlocale(300);
  setlname(sv_curloc, sxSymEnter(T("sxm")));
  setlexport(sv_curloc, so_nil);
  /* now sxLocEnter is legal */

  /* install the built-in functions */
  install_subr_tab();

  /* install the built-in continuations */
  install_csubr_tab();

  /* install the required data */
  install_data_tab();

  /* install ports & port functions (make error printing possible!) */
  install_port_tab();

  /* install the initial values of internal variables */
  install_var_tab();

  /* install the initial set of bindings */
  install_ibindings_tab();

  /* return startup proc */
  return sv_curstart;
}

