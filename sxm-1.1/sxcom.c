
/********************************************************************
 * sxcom.c - backend of the FROBIT bytecode compiler for SXM (v.A)  *
 * (c) 1993-1999 Sergei Egorov, Alexander Osipenko                  *
 ********************************************************************/

/* => inline lambda: arg.count, rest, optional, arg matching */
/* optimizations: jump, arguments, closure, tail-call-args */
/* display closures! */


#include "sxm.h"
#include "vmop.h"
#include "sxintern.h"
#include "sxdisasm.h"
#include "sxlocale.h"
#include "sxhtab.h"
#include "define.h"
#include "extern.h"

/* compiler symbols */
DEFINE_DATUM_SYMBOL(sd_snlambda, "named-lambda")
DEFINE_DATUM_SYMBOL(sd_sslambda, "stack-lambda")
DEFINE_DATUM_SYMBOL(sd_salambda, "ac/stack-lambda")
DEFINE_DATUM_SYMBOL(sd_sset, "set!")
DEFINE_DATUM_SYMBOL(sd_sif, "if")
DEFINE_DATUM_SYMBOL(sd_sbegin, "begin")
DEFINE_DATUM_SYMBOL(sd_sfuncall, "funcall")
EXTERN_DATUM(sd_squote) /* "quote": see sxread.c */
EXTERN_DATUM(sd_sprim) /* "#primitive": see sxread.c */

/* compiler keywords : they are named constants (frobs) now
 * DEFINE_DATUM_KEYWORD(sc_optional, "optional")
 * DEFINE_DATUM_KEYWORD(sc_rest, "rest")
 * DEFINE_DATUM_KEYWORD(sc_aux, "aux")
 * DEFINE_DATUM_KEYWORD(sc_ac, "current-value")
 * DEFINE_DATUM_KEYWORD(sc_void, "void")
 * DEFINE_DATUM_KEYWORD(sc_default, "default")
 * DEFINE_DATUM_KEYWORD(sc_nonspec, "nonspecified")
 * DEFINE_DATUM_KEYWORD(sc_tmany, "too-many")
 * DEFINE_DATUM_KEYWORD(sc_tfew, "too-few")
 */

/* compiler parameters and tables */

/*#| (integrable-procedures [htable]) |#*/
DEFINE_INITIAL_BINDING("integrable-procedures", sp_curintprocs)
DEFINE_VARIABLE(sv_curintprocs) /* initially unbound */
DEFINE_PROCEDURE_VARACCESS(sp_curintprocs, sv_curintprocs, NT_HASHTABLE)

/*#| (optimize-level [int]) |#*/
DEFINE_INITIAL_BINDING("optimize-level", sp_curoptlevel)
DEFINE_VARIABLE(sv_curoptlevel) /* initially unbound */
DEFINE_PROCEDURE_VARACCESS(sp_curoptlevel, sv_curoptlevel, NT__BYTE)

/* some specific inline-definitions */
/* variables is extended to be SYMBOLs and GCELLs */
static bool_t varp(SOBJ x)
{
  return symbolp(x) || gcellp(x);
}

#define syntaxError(msg, arg)   sxErr(msg, arg)
#define compilerError(msg)      sxSysErr(msg)

#define ARGSMAX         255
#define MINLITB         -128
#define MAXLITB         +128
#define LITBMASK        0xFF
#define LITBASE         (FIRSTLIT-1)
#define MAXLITID        0xFF
#define SAVEFRAME       3

/* LAMBDA generation types */
typedef enum {
  LA_STACK, LA_AC, LA_HEAP, LA_NOT_INLINE
} lam_t;

/* continuation types */
typedef enum {
  C_VALUE, C_RETURN, C_PUSH, C_BRT, C_NEXT, C_CALL
} cont_t;

typedef int cint;       /* code (signed!) integer (vmpc_t) */

#define MKCONT(type, par)   ((cont_t)((type) | ((par) << 3)))
#define CONTTYPE(cont)      ((cont_t)((cont) & 0x7))
#define CONTPARAM(cont)     ((cont_t)((cont) >> 3))

/* NB: no fancy stuff here: this struct is copied as binary */
typedef struct _cinfo {     /* compiler context info list   */
  struct _cinfo* prev;      /* previous info block          */
  SOBJ*  argv;     /* arguments in the stack       */
  SOBJ*  litv;     /* literals in the stack        */
  cint   cbase;    /* base for current function    */
  cint   stack;    /* stack level                  */
  cint   argc;     /* true arguments count         */
  bool_t f_eref;   /* outer heap frames referenced */
  bool_t f_eframe; /* heap frame present           */
  bool_t f_expand; /* inline expanding lambda      */
  bool_t f_inline; /* inline lambda code/literals  */
  bool_t f_rest;   /* #!rest argument present      */
  bool_t f_acarg;  /* first argument is in AC      */
  SOBJ   display;  /* display list or so_nil       */
} cinfo_t;

static cinfo_t cinfo; /* keeps current context */

#define NLITS(cinfo)    ((cint)((cinfo).litv - vmsp))
#define NARGS(cinfo)    ((cint)((cinfo).argv - (cinfo).litv))
static SOBJ ctloc;    /* working locale      */
static cint fixloc;   /* last fixup location */

/* forward declarations */
static void do_expr(SOBJ expr, cont_t cont);
static cint findliteral(SOBJ lit);
static void box_stack_vars(void);
static void cd_nary(SOBJ fun, SOBJ args, cont_t cont);
static void cd_cont(cont_t cont);
static SOBJ cd_fundef(SOBJ func, lam_t ltype, cont_t cont);
static void cd_var(bool_t set, SOBJ sym, cont_t cont);
static void cd_literal(SOBJ lit, cont_t cont);
static void cd_integrable(SOBJ opp, cint narg, cont_t cont);
static void cd_dclose(SOBJ display, cint codelit, cont_t cont);

static vmop_t emit_error(void)
{
  compilerError(T("insufficient code space"));
  return 0;
}

#define    emit(b)              (cptr>=CMAX?emit_error():(cbuff[cptr++]=(vmop_t)(b)))
#define    emit2(b1,b2)         (emit(b1), emit(b2))
static cint fixup(cint ref, cint loc);

#define    FIXUP_HERE           (cptr)
#define    FIXUP_BIND(x)        ((cint)(-(x)))

static struct {
  unsigned
  o_nodebug: 1,   /* remove debug info    */
  o_eframe:  1,   /* remove EFRAME        */
  o_iprocs:  1;   /* use integrable procs */
} opt, no_opt;

/* code buffer */
#define CMAX  8192
static vmop_t cbuff[CMAX];

/* code buffer pointer */
static cint cptr;

EXTERN_VARIABLE(sv_stdout)

/* sxCompile - compile core expression */
SOBJ sxCompile(SOBJ expr, SOBJ locale)
{
  SOBJ v;
  int iol;
  SOBJ ctenv = so_nil;    /* initial environment */

  if (consp(expr) && closurep(car(expr)) && nullp(cdr(expr)))
    return car(expr);
  /* initialize the compile time environment */
  cpush(locale);
  ctloc = locale;
  cpush(expr);
  cpush(ctenv);     /* GC protect */
  cinfo.litv = cinfo.argv = vmsp;
  cinfo.prev = NULL;
  cinfo.f_expand = cinfo.f_inline = FALSE;
  /* setup the base of the code for this function */
  cinfo.cbase = cptr = 1; /* cptr == 0 is reserved */
  cinfo.display = so_nil;

  opt = no_opt;     /* clear optimizer flags */
  v = sv_curoptlevel;
  iol = fixp(v) ? (int) getfixnum(v) : 0x7FFF;
  if (iol >= 2)
    opt.o_iprocs = 1;
  if (iol >= 3)
    opt.o_eframe = 1;
  if (iol >= 5)
    opt.o_nodebug = 1;

  /* no table - no integration */
  v = sv_curintprocs;
  if (!hashtablep(v)) opt.o_iprocs = 0;

  /* compile the expression */
  settop(cd_fundef(expr, LA_HEAP, C_NEXT));
  assert(codep(top())); /* should be a code */
  settop(cvclosure(top(), ctenv));

  v = top();
  drop(3);
  return v;
}

/******************************* OPCODES ******************************/

static void i_setpc(cint pc)
{
  cptr = pc;
}
static void i_push(void)
{
  emit(OP_PUSH);
}
static void i_pop(void)
{
  emit(OP_POP);
}
static void i_drop(cint n)
{
  emit2(OP_ADROP, n);
}
static void i_edrop(void)
{
  emit(OP_EDROP);
}
static void i_return(cint drop)
{
  if (drop == 0)
    emit(OP_RETURN);
  else
    emit2(OP_SRETURN, drop);
}
static void i_acnt(cint narg)
{
  emit2(OP_ACNT, narg);
}
static void i_atest(cint narg)
{
  if (narg == 0)
    emit(OP_ALAST);
  else
    emit2(OP_ATEST, narg);
}
static void i_close(cint codelit)
{
  emit2(OP_CLOSE, codelit);
}
static void i_argmore(void)
{
  emit2(OP_AERROR, 1);
}
static void i_argless(void)
{
  emit2(OP_AERROR, 0);
}

/* stack variables */
static void i_svref(cint slotn)
{
  if (slotn <= 7)
    emit(OP_SREF0 + slotn);
  else
    emit2(OP_SREF, slotn);
}
static void i_svpush(cint slotn)
{
  if (slotn <= 7)
    emit(OP_PUSHSREF0 + slotn);
  else
    emit2(OP_PUSHSREF, slotn);
}

/* boxed stack variables */
static void i_svbox(cint slotn)
{
  emit2(OP_SBOX, slotn);
}
static void i_svrefi(cint slotn)
{
  emit2(OP_SREFI, slotn);
}
static void i_svseti(cint slotn)
{
  emit2(OP_SSETI, slotn);
}


/* display variables (boxed and not) */
static void i_dvref(cint slotn)
{
  emit2(OP_DREF, slotn);
}
static void i_dvrefi(cint slotn)
{
  emit2(OP_DREFI, slotn);
}
static void i_dvseti(cint slotn)
{
  emit2(OP_DSETI, slotn);
}
static void i_dclose(cint nvars, cint codelit)
{
  emit(OP_DCLOSE);
  emit(nvars);
  emit(codelit);
}


/* environment variables */
static cint i_evframe(void)
{
  emit(OP_EFRAME);
  emit2(0, 0);
  return cptr - 2;
}
static cint i_evframepc(cint ffix)
{
  return ffix - 1;
}
static void i_evframefixup(cint ffix, cint fsize, cint vindex)
{
  cbuff[ffix + 0] = (vmop_t)fsize;
  cbuff[ffix + 1] = (vmop_t)vindex; /* fixup lambda vars index */
}
static void i_evref(cint lev, cint off)
{
  emit(OP_EREF);
  emit2(lev, off);
}
static void i_evset(cint lev, cint off)
{
  emit(OP_ESET);
  emit2(lev, off);
}
static void i_evpush(cint lev, cint off)
{
  emit(OP_PUSHEREF);
  emit2(lev, off);
}

/* global variables */
static void i_gvref(cint gvlit)
{
  emit2(OP_GREF, gvlit);
}
static void i_gvset(cint gvlit)
{
  emit2(OP_GSET, gvlit);
}

static void i_mvreqargs(cint narg)
{
  emit2(OP_MVARGS, narg);
}
static void i_mvoptarg(cint slotn)
{
  emit2(OP_MVOARG, slotn);
}
static void i_mvrest(cint slotn)
{
  emit2(OP_MVRARG, slotn);
}
static void i_shoptarg(cint slotn)
{
  emit2(OP_SHOARG, slotn);
}
static void i_shrest(cint slotn)
{
  emit2(OP_SHRARG, slotn);
}
static void i_shaux(void)
{
  emit(OP_SHAUXARG);
}

/* generic branch instruction */
static cint ii_branch(vmop_t op)
{
  emit(op);
  emit2(0, 0);
  return cptr - 2;
}

static void i_call(cint narg, cint drop)
{
  if (drop == 0)
    emit2(OP_CALL, narg);
  else {
    emit2(OP_SCALL, narg);
    emit(drop);
  }
}

static cint opti_bsbaddr(cint narg, cint addr)
{
  if (addr > 0) { /* already expanded. optimize call point */
    if (narg && cbuff[addr + 0] == OP_ATEST && cbuff[addr + 1] == narg)
      return addr + 2;
    else if (!narg && cbuff[addr + 0] == OP_ALAST)
      return addr + 1;
  }
  return addr;
}

static cint i_sbsb(cint narg, cint drop, cint addr)
{
  emit2(OP_SBSB, narg);
  /* [?]cast: MSVC reports int size mismatch */
  return fixup(ii_branch((vmop_t) drop), opti_bsbaddr(narg, addr));
}

static cint i_bsb(cint narg, cint addr)
{
  if ((addr = opti_bsbaddr(narg, addr)) > 0)  /* already expanded */
    return fixup(ii_branch(OP_BR), addr);
  /* [?]cast: MSVC reports int size mismatch */
  else {
    emit(OP_BSB);
    return fixup(ii_branch((vmop_t) narg), addr);
  }
}

/*******************************  FORMS  ******************************/
static lam_t lambda_type(SOBJ fun)
{
  if (fun == sd_snlambda) return LA_HEAP;
  if (fun == sd_sslambda) return LA_STACK;
  if (fun == sd_salambda) return LA_AC;
  return LA_NOT_INLINE;
}

/* #form: (XXX-LAMBDA (name args ...) expr ... ) */
static void do_lambda(SOBJ form, lam_t ltype, cont_t cont)
{
  if (!consp(form) || !consp(car(form)))
    syntaxError(T("expecting nonempty argument list"), form);
  switch (CONTTYPE(cont)) {
    case C_BRT:
      fixloc = ii_branch(OP_BR);
      return;
    case C_NEXT: /* never!! cd_fundef() */
      return;
    case C_CALL:
      /* patch lambda type in nested calls */
      form = cd_fundef(form, ltype != LA_AC ? ltype : LA_STACK, cont);
      assert(form == so_false); /* inline */
      return;
    default: /* compile code to create a closure */
      form = cd_fundef(form, ltype, C_VALUE);
      assert(form != so_false); /* not inline */
      if (closurep(form)) { /* no environment required */
        cd_literal(form, cont);
      } else if (consp(form)) { /* display required */
        gcLock(form);
        assert(consp(car(form))); /* display */
        assert(codep(cdr(form))); /* code */
        cd_dclose(car(form), findliteral(cdr(form)), cont);
        gcUnlock(1);
      } else { /* close in current env */
        assert(codep(form));
        i_close(findliteral(form));
        cd_cont(cont);
      }
      break;
  }
}

static bool_t literalp(SOBJ form)
{
  if (consp(form)) return car(form) == sd_squote;
  if (varp(form)) return FALSE;
  return TRUE;
}

/* #form: (IF test-expr then-expr [else-expr] ) */
/* special care: (if e1 e1 e3) and (if e1 e2 e1) */
static void do_if(SOBJ form, cont_t cont)
{
  cint nxt, end = 0;
  cint stack;

  if (!consp(form) || !consp(cdr(form)))
    syntaxError(T("if: syntax error"), form);

  /* TEST: */
  do_expr(car(form), C_BRT);
  nxt = fixloc; /* save fixup location          */

  /* ELSE: */
  stack = cinfo.stack;
  if (consp(cdr(cdr(form))))
    do_expr(car(cdr(cdr(form))), cont);
  else
    cd_literal(so_false, cont);
  if (nxt == 0)
    return; /* test == false -> skip THEN   */

  if (CONTTYPE(cont) == C_BRT && fixloc == 0 && cbuff[nxt - 1] == OP_BRT) {
    cbuff[nxt - 1] = OP_BRNOT;
    end = nxt;
  } else {
    if (CONTTYPE(cont) != C_RETURN)
      end = ii_branch(OP_BR);
    fixup(nxt, FIXUP_HERE);
  }
  nxt = fixloc;

  /* THEN: */
  cinfo.stack = stack;  /* restore stack level */
  if (car(form) != car(cdr(form)) || literalp(car(form)))
    do_expr(car(cdr(form)), cont);
  else
    cd_cont(cont);

#if 0
  /* ... magic patch: ((if c (lambda...) (lambda ...)) arg) */
  if (CONTTYPE(cont) == C_CALL)
    cinfo.stack -= SAVEFRAME;
#endif

  /* END: */
  if (end)
    fixup(end, FIXUP_HERE);
  if (CONTTYPE(cont) == C_BRT && nxt != 0) {
    if (fixloc)
      fixup(fixloc, FIXUP_BIND(nxt));
    else
      fixloc = nxt;
  }
}

/* #form: (BEGIN expr ...); (BEGIN) == (void) */
static void do_begin(SOBJ form, cont_t cont)
{
  if (nullp(form)) {
    cd_cont(cont);
    return;
  } else if (!consp(form))
    syntaxError(T("begin: syntax error"), form);
  for (; consp(cdr(form)); form = cdr(form))
    do_expr(car(form), C_NEXT);
  do_expr(car(form), cont);
}

/**********************************  **********************************/

typedef enum {
  FT_OTHER, FT_QUOTE, FT_FUNCALL, FT_IF, FT_BEGIN, FT_SET,
  FT_NLAMBDA, FT_SLAMBDA, FT_ALAMBDA
} formid_t;

static struct {
  SOBJ *sop;
  formid_t id;
} form_tab[] = {
  {&sd_squote,   FT_QUOTE},
  {&sd_sfuncall, FT_FUNCALL},
  {&sd_sif,      FT_IF},
  {&sd_sbegin,   FT_BEGIN},
  {&sd_snlambda, FT_NLAMBDA},
  {&sd_sslambda, FT_SLAMBDA},
  {&sd_salambda, FT_ALAMBDA},
  {&sd_sset,     FT_SET}
};

/* is_form - check for a symbol is a core-0 form */
static formid_t is_form(SOBJ form)
{
  int i;

  if (!symbolp(form))
    return FT_OTHER;
  for (i = 0; i < (int)(sizeof(form_tab) / sizeof(form_tab[0])); i++)
    if (form == *(form_tab[i].sop))
      return form_tab[i].id;
  return FT_OTHER;
}

/* do_expr - compile an expression */
static void do_expr(SOBJ expr, cont_t cont)
{
  static SOBJ errexpr;

  if (consp(expr)) { /* forms and combinations */
    errexpr = expr;
    switch (is_form(car(expr))) {
      case FT_QUOTE:
        expr = cdr(expr);
        if (!consp(expr) || !nullp(cdr(expr)))
          syntaxError(T("expecting quoted expression"), errexpr);
        cd_literal(car(expr), cont);
        break;

      case FT_IF:
        do_if(cdr(expr), cont);
        break;

      case FT_BEGIN:
        do_begin(cdr(expr), cont);
        break;

      case FT_NLAMBDA:
        do_lambda(cdr(expr), LA_HEAP, cont);
        break;
      case FT_SLAMBDA:
        do_lambda(cdr(expr), LA_STACK, cont);
        break;
      case FT_ALAMBDA:
        do_lambda(cdr(expr), LA_AC, cont);
        break;

      case FT_SET:
        expr = cdr(expr);
        if (!consp(expr) || !varp(car(expr)) ||
            !consp(cdr(expr)) || !nullp(cdr(cdr(expr))))
          syntaxError(T("set! syntax error"), errexpr);
        do_expr(car(cdr(expr)), C_VALUE);
        cd_var(TRUE, car(expr), cont);
        break;

      case FT_FUNCALL:
        expr = cdr(expr);
        if (!consp(expr))
          syntaxError(T("improper `funcall' syntax"), errexpr);

      case FT_OTHER:
        cd_nary(car(expr), cdr(expr), cont);
        break;

      default:
        assert(FALSE);
    }
  }
  /* atomic expressions */
  else if (expr == sd_sprim)
    syntaxError(T("reserved identifier used as variable"), expr);
  else if (varp(expr))
    cd_var(FALSE, expr, cont);
  else if (vectorp(expr))
    syntaxError(T("vector syntax error"), expr);
  else if (keywordp(expr)) {
    /* keywords with special meaning... */
    if (expr == sc_ac)
      cd_cont(cont);
    else if (expr == sc_void)
      cd_cont(cont);
    else if (expr == sc_default)
      cd_literal(so_default, cont);
    else if (expr == sc_nonspec)
      cd_literal(so_void, cont);
    else if (expr == sc_tmany) {
      i_argmore();
      cd_cont(cont);
    } else if (expr == sc_tfew) {
      i_argless();
      cd_cont(cont);
    } else
      syntaxError(T("unknown syntax"), expr);
  } else
    cd_literal(expr, cont);
}

/**********************************  **********************************/
static cint todrop(cint nargs)
{
  cinfo_t *ci = &cinfo;

  for (nargs = -nargs; ci != NULL; ci = ci->prev) {
    nargs += ci->stack;
    if (!ci->f_expand)
      break;
  }
  return nargs;
}

/* push_args - compile the arguments for a function  / inline call    */
static void push_args(SOBJ args)
{
  if (consp(args)) {
    push_args(cdr(args));
    do_expr(car(args), C_PUSH);
  }
}

/* integrables hash table */
static SOBJ nptr; /* (argn (op-value ...) (op-push ...) (op-brt...)) */
typedef enum {
  IN_UNKNOWN, IN_FIXED_ARITY, IN_VAR_ARITY
} in_t;

/* in_ntab - check for integrable function (use gcell as unique name) */
static in_t in_ntab(SOBJ fun, int narg)
{
  FIXTYPE na;

  /* convert symbol to correspondent gcell */
  if (symbolp(fun))
    fun = sxLocFind(ctloc, fun, FALSE);
  if (!gcellp(fun))
    return IN_UNKNOWN;
  nptr = sxHashTableGet(sv_curintprocs, fun, NULL);
  if (consp(nptr) && sfixp(car(nptr)) && consp(cdr(nptr))) {
    na = getsfixnum(car(nptr));
    if (na == -1) return IN_VAR_ARITY;
    if (na == narg) return IN_FIXED_ARITY;
  }
  return IN_UNKNOWN;
}

/* @ cd_nary - compile nary operator expressions */
static void cd_nary(SOBJ fun, SOBJ args, cont_t cont)
{
  lam_t ltype; 
  int narg = length(args);
  in_t in = IN_UNKNOWN;

  /* check for #primitive (works at any optimize level) */
  if (consp(fun) && car(fun) == sd_sprim) {
    if (!consp(cdr(fun)) || !nullp(cdr(cdr(fun)))) 
      syntaxError(T("invalid \\#primitive form"), fun);
    nptr = car(cdr(fun));
    if (consp(nptr) && sfixp(car(nptr)) && consp(cdr(nptr))) {
      FIXTYPE na = getsfixnum(car(nptr));
      if (na == -1) in = IN_VAR_ARITY;
      else if (na == narg) in = IN_FIXED_ARITY;
    }
    if (in == IN_UNKNOWN) { /* nptr is not inline list or arg mismatch */
      if (!symbolp(nptr)) /* arg number mismatch in inlining */
        /* syntaxError(T("invalid \\#primitive call"), cons(fun, args)); */
        syntaxError(T("invalid \\#primitive data"), nptr);
      else { /* Chez-like global ref? */
        cd_nary(nptr, args, cont);
        return;
      }
    }
    fun = cdr(nptr); /* keep iproc list */
    if (in == IN_VAR_ARITY) {
      push_args(args); /* non-AC args */
    } else if (narg) { /* AC-args */
      if (narg > 1)
        push_args(cdr(args)); /* push all but the first */
      do_expr(car(args), C_VALUE); /* place first arg in AC */
      narg--;
    }
    cd_integrable(fun, (cint)narg, cont);
    return;
  }
  /* check for integrable function */
  if (opt.o_iprocs && (in = in_ntab(fun, narg)) != IN_UNKNOWN) {
    fun = cdr(nptr); /* keep iproc list */
    if (in == IN_VAR_ARITY) {
      push_args(args); /* non-AC args */
    } else if (narg) { /* AC-args */
      if (narg > 1)
        push_args(cdr(args)); /* push all but the first */
      do_expr(car(args), C_VALUE); /* place first arg in AC */
      narg--;
    }
    cd_integrable(fun, (cint)narg, cont);
    return;
  }
  /* check for inline functions */
  if (consp(fun) && (ltype = lambda_type(car(fun))) != LA_NOT_INLINE) {
    /* push arguments */
    if (ltype == LA_AC) {
      if (narg) {
        if (narg > 1)
          push_args(cdr(args));
        do_expr(car(args), C_VALUE);
        narg--;
      } else
        ltype = LA_STACK; /* don't pop first argument */
    } else
      push_args(args);
    /* generate inline function application */
    if (CONTTYPE(cont) == C_RETURN) {
      SOBJ c = cd_fundef(cdr(fun), ltype, MKCONT(C_CALL, MKCONT(C_RETURN, narg)));
      assert(c == so_false); /* inline */
    } else {
      SOBJ c = cd_fundef(cdr(fun), ltype, MKCONT(C_CALL, MKCONT(C_VALUE, narg)));
      assert(c == so_false); /* inline */
      cd_cont(cont);
    }
    return;
  }
  /* ordinary function call */
  if (CONTTYPE(cont) == C_RETURN) {
    push_args(args);
    do_expr(fun, MKCONT(C_CALL, MKCONT(C_RETURN, narg)));
  } else {
    cint nxt = ii_branch(OP_SAVE);

    cinfo.stack += SAVEFRAME;
    push_args(args);
    do_expr(fun, MKCONT(C_CALL, MKCONT(C_CALL, narg)));
    fixup(nxt, FIXUP_HERE);
    cd_cont(cont);
  }
}

/* cd_cont - compile a continuation */
static void cd_cont(cont_t cont)
{
  switch (CONTTYPE(cont)) {
    case C_BRT:
      fixloc = ii_branch(OP_BRT);
      break;
    case C_PUSH:
      i_push();
      ++cinfo.stack;
      break;
    case C_NEXT:
      break;
    case C_VALUE:
      break;
    case C_RETURN:
      i_return(todrop(0));
      break;
    case C_CALL: {
      cint a;
      cont = CONTPARAM(cont);
      a = CONTPARAM(cont);
      if (CONTTYPE(cont) == C_RETURN)
        i_call(a, todrop(a));
      else {
        i_call(a, 0);
        cinfo.stack -= a + SAVEFRAME;
      }
    } break;
  }
}

/********************* function definition stuff **********************/

/* addarg - check one argument          */
/* called by: parse_lambda_list() ONLY  */
static SOBJ addarg(SOBJ* fargs, bool_t aux)
{
  SOBJ arg = *fargs;

  /* see if we are done */
  if (!consp(arg))
    return nullp(arg) ? so_true : so_false; /* so_false means error! */
  /* pop next list element */
  *fargs = cdr(arg); arg = car(arg);
  /* check argument syntax */
  if (symbolp(arg)) { /* argument name */
    if (aux) { /* aux args */
      /* aux with no initializer: do some checks */
      if (!cinfo.f_eframe) {
        /* ac/stack? -- too complicated for frobit */
        if (cinfo.f_acarg) 
          syntaxError(T("#!aux with no init in ac/stack-lambda?"), arg);
        /* make sure we didn't start pushing labels yet */
        if (vmsp < cinfo.argv && consp(top())) /* last push was second-class? */
          syntaxError(T("#!aux with no init follows one with init?"), top());
      } /* else: kosher in named-lambda */
      /* this aux is first-class; count it as such */
    } 
    /* push argument name and count as first-class var */
    ++cinfo.argc; /* these are real vars, not second-class stuff */
    cpush(arg);
    return so_nil;
  } else if (boxp(arg)) { /* mutable var */
    /* box arg: must be doing {ac/}stack-lambda! */
    if (cinfo.f_eframe) syntaxError(T("boxed arg in named-lambda?"), arg);
    /* stack-lambda aux checks */
    if (aux) {
      /* ac/stack? -- too complicated for frobit */
      if (cinfo.f_acarg) 
        syntaxError(T("#!aux with no init in ac/stack-lambda?"), arg);
      /* make sure we didn't start pushing labels yet */
      if (vmsp < cinfo.argv && consp(top())) /* last push was second-class? */
        syntaxError(T("#!aux with no init follows one with init?"), top());
    }
    /* make sure that the box contains a symbol */
    if (!symbolp(getboxval(arg))) syntaxError(T("illegal formal arg"), arg);
    /* push argument in a box and count as first-class var */
    ++cinfo.argc; /* these are real vars, not second-class stuff */
    cpush(arg);
    return so_nil;
  } else if (consp(arg)) { /* aux with initializer? */
    SOBJ init;
    if (!aux) /* no list args before #!aux! */
      syntaxError(T("argument must be a variable"), arg);
    /* list arg in aux mode: must be doing {ac/}stack-lambda! */
    if (cinfo.f_eframe)
      syntaxError(T("#!aux with init in named-lambda?"), arg);
    /* aux arg: (name . atom) | (name . 'data) | (name . (stack-lambda...)) */
    init = cdr(arg);
    /* push initializer or 0 for labels first */
    if (!symbolp(car(arg)))
      syntaxError(T("#!aux name must be a variable"), arg);
    if (!consp(init)) /* esl: originally treated as quote! cpush(def);*/
      syntaxError(T("illegal initializer in #!aux arg"), arg);
    else if (car(init) == sd_sslambda && consp(cdr(init)))
      cpush(so_fix0); /* to be patched in expand_aux */
    else if (car(init) == sd_salambda && consp(cdr(init)))
      cpush(so_fix0); /* to be patched in expand_aux */
    /* named-lambda is not allowed in #!aux */
    else if (car(init) == sd_squote && consp(cdr(init)))
      cpush(car(cdr(init)));
    else
      syntaxError(T("bad #!aux usage"), arg);
    /* push argument name or (name . initializer) but don't count */
    cpush(arg);
    return so_nil;
  } else if (arg == sc_optional || arg == sc_rest || arg == sc_aux) {
    return arg;
  } else {
    syntaxError(T("unexpected lambda argument format"), arg);
  }
  return so_nil; /* never gets here */
}

/* parse_lambda_list - parse the formal argument list */
/* called by: cd_fundef() ONLY */
/* NB: the resulting data structure will be used by findcevar() */
static void parse_lambda_list(SOBJ finfo, SOBJ fargs)
{
  SOBJ key;
  cint frame = 0, slotn = 0;

  cinfo.argv = vmsp; /* save arguments position */
  cinfo.argc = 0; /* first-class vars count (incremented in addarg) */
  /* setup the entry code */
  if (cinfo.f_eframe)
    frame = i_evframe();

  /* handle each required argument */
  while (so_nil == (key = addarg(&fargs, FALSE)))
    slotn++;
  /* adjust for calling convention */
  if (cinfo.f_expand) { /* inline let */
    if (cinfo.f_acarg) /* ac/stack call convention */
      --slotn;
    if (cinfo.stack < slotn) /* mismatch in inline let? */
      syntaxError(T("not enough arguments to function"), finfo);
  }
  if (slotn > 0 && cinfo.f_eframe)
    i_mvreqargs(slotn);

  /* check for the '#!optional' argument(s) */
  if (key == sc_optional) {
    /* handle each optional argument */
    for (; so_nil == (key = addarg(&fargs, FALSE)); slotn++)
      if (cinfo.f_eframe)
        i_mvoptarg(slotn + FIRSTARG);
      else
        i_shoptarg(slotn);
  }

  /* check for the '#!rest' argument */
  if (key == sc_rest) {
    /* handle at most one rest argument */
    cinfo.f_rest = TRUE;
    if (so_nil == (key = addarg(&fargs, FALSE))) {
      /* got ourselves a rest var! */
      if (cinfo.f_eframe)
        i_mvrest(slotn + FIRSTARG);
      else { /* in stack mode, rest goes on top */
        i_shrest(slotn); /* this op builds rest at run time */
        /* rotate compiler vars by one to get proper placement */
        check(1);
        sobjmvup(vmsp - 1, vmsp, slotn + 1);
        vmsp[slotn] = vmsp[-1];
        /* now findcevar() will calc correct stack offsets */
      }
      ++slotn;
      key = addarg(&fargs, FALSE);
    }
  }

  /* check for the '#!aux' argument(s) */
  if (key == sc_aux) {
    /* collect first-class auxes (frame mode) or second-class
     * auxes with const/label initializers (stack mode)
     * TRUE means that addarg is called in 'aux' mode */
    while (so_nil == (key = addarg(&fargs, TRUE))) {
      if (!cinfo.f_eframe && !consp(top())) {
        /* first-class aux in stack-lambda: put on top */
        i_shaux(); /* pushes at run time */
        /* rotate compiler vars by one to get proper placement */
        check(1);
        sobjmvup(vmsp - 1, vmsp, slotn + 1);
        vmsp[slotn] = vmsp[-1];
        /* now findcevar() will calc correct stack offsets */
        ++slotn;
      }
    }
  }

  /* check for the correct end of the argument list */
  if (key != so_true)
    syntaxError(T("bad argument list"), fargs);

  cinfo.litv = vmsp;    /* save literals position */

  /* mode-dependent finalization */
  if (cinfo.f_eframe) { /* heap mode */
    /* frame mode: put info and finalize */
    if (opt.o_eframe && cinfo.argc == 0) {
      i_setpc(i_evframepc(frame)); /* remove frame instruction <sip> */
      cinfo.f_eframe = FALSE;
    } else { /* fixup the frame instruction */
      cint k;
      /* store DEBUG_INFO (!strip #!aux constants) */
      if (consp(finfo) && (opt.o_nodebug || !cinfo.f_eframe || cinfo.argc > 5))
        finfo = car(finfo); /* to save space, drop lambda-list */
      k = findliteral(finfo); /* register info as literal */
      /* fixup the frame instruction */
      i_evframefixup(frame, cinfo.argc + FIRSTARG, k);
    }
    slotn = cinfo.stack = 0; /* at run time, nothing remains on the stack */
  } else { /* stack mode */
    /* check for display info */
    assert(consp(finfo)); /* (info . arglist) */
    if (consp(car(finfo))) { /* display! */
      /* syntaxError(T("display lists not yet supported"), car(finfo)); */
      assert(nullp(cinfo.display));
      cinfo.display = car(finfo); /* CG-protected as part of org expr */
    }
    /* stack mode: finalize depending on calling convention */
    cinfo.stack = slotn = cinfo.argc; /* first-class vars are on stack */
    if (cinfo.f_acarg && cinfo.argc > 0) {
      /* ac/stack calling convention with at least one arg */
      if (!cinfo.f_expand) /* real procedure */
        i_pop(); /* stdcall -> accall adjustment */
      else /* inline let: caller doesn't push first arg */
        --slotn; /* no run-time adjustment, stack will be smaller */
      /* prolog ends with first var in ac, others on stack */
      --cinfo.stack;
    }
    /* check for boxed vars and generate code to box them */
    box_stack_vars();
  }

  /* check arguments count, if required */
  if (!cinfo.f_rest)
    i_atest(slotn);
}

static void cd_auxdef(SOBJ fun)
{
  lam_t ltype = lambda_type(car(fun));
  if (ltype == LA_NOT_INLINE || ltype == LA_HEAP) {
    compilerError(T("bad #!aux lambda"));
  } else {
    SOBJ c = cd_fundef(cdr(fun), ltype, MKCONT(C_CALL, MKCONT(C_CALL, ARGSMAX)));
    assert(c == so_false); /* inline */
  }
}

static void expand_aux(bool_t over)
{
  cint tobr = over ? 0 : -1;
  SOBJ *a = cinfo.litv;
  /* go through second-class vars in reverse order */
  while (a != cinfo.argv && consp(*a)) {
    /* looking at (name . init) : second-class const or label */
    SOBJ init = cdr(*a);
    ++a; /* skip (name . init) */
    /* constant/label goes next */
    if (car(init) != sd_squote) { /* skip constants */
      cint ref;
      assert(sfixp(*a));
      ref = (cint) getsfixshort(*a);
      if (ref <= 0) { /* label is not yet anchored */
        /* anchor the label and expand the code here */
        if (!tobr) /* prepare for branch over the code */
          tobr = ii_branch(OP_BR);
        /* we will anchor the label here */
        *a = cvsfixnum(fixup(-ref, FIXUP_HERE));
        /* expand lambda code here */
        cd_auxdef(cdr(a[-1]));
      }
    }
    ++a; /* skip constant/label */
  }
  /* seen all second-class vars */
  if (tobr > 0) /* fix the branch if needed */
    fixup(tobr, FIXUP_HERE);
}

static void box_stack_vars(void)
{
  SOBJ *a = cinfo.argv;
  cint argc = cinfo.argc;
  cint slotn = 0;
  assert(!cinfo.f_eframe);
  if (cinfo.f_acarg && argc > 0) { --a; --argc; }
  /* go through first-class vars in direct order */
  while (argc > 0) {
    SOBJ x = *--a;
    assert(!consp(x)); /* second-class const or label? */
    if (boxp(x)) /* boxed var: generate sbox opcode */
      i_svbox(slotn);
    ++slotn;
    --argc;
  }     
}

/* variable storage classes */
typedef enum {
  VAR_ENV0 = 0, 
  VAR_GLOBAL = -1, 
  VAR_AC = -2, 
  VAR_CONST = -3, 
  VAR_STACK = -4, 
  VAR_STACK_BOXED = -5, 
  VAR_DISPLAY = -6, 
  VAR_DISPLAY_BOXED = -7
} var_t;
#define VAR_ENV(el) ((var_t)el) /* >= 0 -> VAR_ENV(environment_level) */


static cint dispvaroff(SOBJ sym, SOBJ display)
{
  cint index = 0;
  while (consp(display))
    if (car(display) == sym) return index;
    else { ++index; display = cdr(display); }
  syntaxError(T("var not found in nearest display"), sym);
  never_returns(-1);    
}

/*
 * @ findcevar - find an environment variable IN REVERSE ORDER, though #!aux
 * definitions can shadow variables. NB: we assume that parse_lambda_list()
 * always generates correct data
 */
static var_t findcevar(SOBJ sym, cint* offset)
{
  cinfo_t *ci = &cinfo; /* start lookup from the current context */
  bool_t inl = TRUE; /* all labels are accessible unless proven otherwise */
  cint elev, slev;  /* heap and stack level counters */
  SOBJ display = so_nil; /* first display list if we met one or more */

  /* walk outwards thru context chain */
  for (elev = slev = 0; ci != NULL; ci = ci->prev) {
    SOBJ *a = ci->litv;  /* arg pointer (first/second class local vars) */
    cint argc = ci->argc;  /* arg counter */
    slev += ci->stack; /* bump stack frame base offset */
    /* look in local vars (from second-class to first-class) */
    while (a != ci->argv) {
      if (consp(*a)) { /* second-class constant/label: (var . init) */
        if (sym == car(*a)) {  /* found! */
          /* check: must be inlining to access labels */
          if (!consp(cdr(*a)) || car(cdr(*a)) != sd_squote) { /* label? */
            if (!inl) syntaxError(T("inaccessible label"), sym);
          }
          return (*offset = (cint)(a - vmsp), VAR_CONST);
        }
        a += 2; /* skip this second-class entry */
      } else if (boxp(*a)) { /* first-class stack var */
        --argc;
        if (sym == getboxval(*a)) { /* found! */
          /* must be a stack frame */
          if (ci->f_eframe) syntaxError(T("boxed var in named-lambda?"), sym);
          if (argc == 0 && ci->f_acarg)
            /* accumulator var cannot be boxed */
            syntaxError(T("boxed ac-based variable?"), sym);
          else  { /* boxed stack var */
            if (consp(display)) /* alias in display */
              return (*offset = dispvaroff(sym, display), VAR_DISPLAY_BOXED);
            else /* in stack */
              return (*offset = argc + slev - ci->argc, VAR_STACK_BOXED);
          }
        }
        a += 1; /* skip this first-class entry */
      } else {  /* first-class ac/stack/heap var */
        --argc;
        if (sym == *a) { /* found! */
          if (ci->f_eframe) { /* this is a heap frame */
            /* mark functions that reference parent scopes */
            if (ci != &cinfo) {
              cinfo_t *cp = &cinfo;
              for (; cp != ci; cp = cp->prev)
                cp->f_eref = TRUE;
            }
            /* return env var info */
            return (*offset = argc + FIRSTARG, VAR_ENV(elev));
          } else { /* stack frame */
            if (argc == 0 && ci->f_acarg && !consp(display)) 
              /* this var is in accumulator */
              return (*offset = 1, VAR_AC);
            else {
              if (consp(display)) /* alias in display */
                return (*offset = dispvaroff(sym, display), VAR_DISPLAY);
              else /* ordinary stack var */
                return (*offset = argc + slev - ci->argc, VAR_STACK);
            }
          }
        }
        a += 1; /* skip this first-class entry */
      }
    }
    /* look for display before going to the next frame */
    if (consp(ci->display) && !consp(display))
      display = ci->display; /* checked before returning stack vars info */
    /* not found in the current frame: try the enclosing one */
    if (ci->f_eframe) ++elev; /* bump level for heap frame access */
    /* if we are not inlining code, labels are not accessible */
    if (!ci->f_inline) inl = FALSE;
  }
  /* not a lexical var: must be global */
  return (*offset = 0, VAR_GLOBAL); /* not found in local frames */
}

/* make_code_object - build a code object */
static SOBJ make_code_object(void)
{
  SOBJ code;
  sv_size_t i;

  /* create a code object */
  i = LITBASE + NLITS(cinfo);
  code = newcode(i);
  for (; i-- > LITBASE;)
    setelement(code, i, pop());
  do
    setelement(code, i, so_nil);/* init remaining slots */
  while (i--);
  gcLock(code);
  setbcode(code, cvbytevec((byte_t*)cbuff + cinfo.cbase, cptr - cinfo.cbase));
  gcUnlock(1);
  vmsp = cinfo.argv;
  return code;
}

static void set_acnt(SOBJ fun, cint narg)
{
  cint farg = -1;

  if (!cinfo.f_eframe && cinfo.f_expand)
    for (farg = cinfo.f_acarg ? -1 : 0; consp(fun); ++farg, fun = cdr(fun))
      if (keywordp(car(fun))) {
        if (car(fun) == sc_rest
            && (nullp(cdr(fun))
            || (consp(cdr(fun)) && car(cdr(fun)) == sc_aux)))
          break;
        else {
          farg = -1;
          break;
        }
      }
  if (farg != narg)
    i_acnt(narg); /* set argument count */
  else
    cinfo.f_rest = TRUE; /* disable arg. check */
}

/* @ cd_fundef - compile the function definition: ((name args ...) expr ...) */
/* returns code, closure, pair (display . code). or so_false (inline) */
static SOBJ cd_fundef(SOBJ func, lam_t ltype, cont_t cont)
{
  cinfo_t oldcinfo = cinfo;

  /* compile the lambda list and the function body */
  cinfo.prev = &oldcinfo;
  cinfo.stack = 0;
  cinfo.f_eref = FALSE;
  cinfo.f_eframe = (ltype == LA_HEAP);
  cinfo.f_expand = (CONTTYPE(cont) == C_CALL);
  cinfo.f_inline = (CONTTYPE(cont) == C_CALL);
  cinfo.f_rest = FALSE;
  cinfo.f_acarg = (ltype == LA_AC);
  cinfo.display = so_nil;
  if (CONTTYPE(cont) == C_CALL) {
    cint stack = CONTPARAM(CONTPARAM(cont));
    cont = CONTTYPE(CONTPARAM(cont));
    if (cont == C_CALL) {
      cont = C_RETURN;
      cinfo.f_expand = FALSE;
    }
    if (stack != ARGSMAX) { /* special case - #!aux expand */
      /* remove call frame from caller info */
      oldcinfo.stack -= (cinfo.stack = stack);
      set_acnt(cdr(car(func)), stack);
    }
    parse_lambda_list(car(func), cdr(car(func)));
    if (cinfo.display != so_nil)
      syntaxError(T("no display expected (inline)"), cinfo.display);
    do_begin(cdr(func), cont);
    expand_aux(cinfo.f_expand && CONTTYPE(cont) == C_VALUE);
    /* merge literals, throw away variables */
    memmove(vmsp + NARGS(cinfo), vmsp, NLITS(cinfo) * sizeof(SOBJ));
    vmsp += NARGS(cinfo);
    /* `return' from inline function -- drop frames */
    if (CONTTYPE(cont) == C_VALUE) {
      if (cinfo.f_eframe)
        i_edrop();
      else if (cinfo.stack > 0)
        i_drop(cinfo.stack);
    }
    func = so_false;
  } else if (CONTTYPE(cont) == C_NEXT) {        /* top-level expression */
    cinfo.cbase = cptr;
    cinfo.f_eframe = TRUE;
    parse_lambda_list(so_false, so_nil);
    if (cinfo.display != so_nil)
      syntaxError(T("no display expected (top)"), cinfo.display);
    do_expr(func, C_RETURN);
    func = make_code_object();
    cptr = cinfo.cbase;
  } else {  /* first-class lambda */
    cinfo.cbase = cptr;
    parse_lambda_list(car(func), cdr(car(func)));
    do_begin(cdr(func), C_RETURN);
    expand_aux(FALSE);
    /* build the code object */
    func = make_code_object();  /* restores stack */
    cptr = cinfo.cbase;
    if (cinfo.display != so_nil) /* needs display */
      func = cons(cinfo.display, func);
    else if (!cinfo.f_eref) /* does not reference anything */
      func = cvclosure(func, so_nil);
  }
  /* restore the previous environment */
  cinfo = oldcinfo;
  return func;
}

/* findliteral - find a literal in the literal frame */
static cint findliteral(SOBJ lit)
{
  SOBJ *ep;
  cinfo_t *ci;
  cint o, ro;

  for (ep = vmsp, o = ro = 0, ci = &cinfo; ci != NULL; ep = ci->argv, ci = ci->prev) {
    SOBJ *p;
    for (p = ci->litv; p != ep; ++o, ++ep) {
      if (ro || (equal(lit, *ep) ? (ro = o + 1) : 0)) {
        o += (cint) (p - ep);
        break;
      }
    }
    if (!ci->f_inline)
      break;
  }
  o += LITBASE - ro;
  if (!ro) { /* add a literal */
    cpush(lit);
    if (o > MAXLITID)
      syntaxError(T("too many literals"), lit);
  }
  return o;
}

/**********************************  **********************************/

/*
 * cd_var - compile a variable reference; variables can be either symbols or
 * gcells
 */
static void cd_var(bool_t set, SOBJ sym, cont_t cont)
{
  var_t vt; 
  cint off;

  /* first thing, let's try to optimize it out */
  if (CONTTYPE(cont) == C_NEXT && !set) return;  /* no effect */
  /* gcell: jump to the global var processing */
  if (!symbolp(sym)) goto gvar;
  /* lookup the var in the cenv */
  vt = findcevar(sym, &off);
  switch (vt) {

    default: { /* VAR_ENV: env frame var */
      /* level is encoded in vt; decode it */
      cint lev = (cint)vt - (cint)VAR_ENV(0);
      assert(lev >= 0);
      /* emit the opcodes */
      if (set) {
        i_evset(lev, off);
      } else { /* ref */
        if (CONTTYPE(cont) == C_PUSH) { /* special code for push */
          i_evpush(lev, off); 
          cinfo.stack++;
          return;
        }
        i_evref(lev, off);
      }
      cd_cont(cont);
      return;
    }

    case VAR_GLOBAL:  /* global var */
      sym = sxLocEnter(ctloc, sym);
    gvar:
      if (set)
        i_gvset(findliteral(sym));
      else
        i_gvref(findliteral(sym));
      cd_cont(cont);
      return;

    case VAR_AC:  /* accumulator var */
      if (set) syntaxError(T("set! to AC-based variable"), sym);
      /* else: no code is required to put it in ac! */
      cd_cont(cont);
      return;

    case VAR_STACK:  /* stack variable */
      if (set) syntaxError(T("set! to stack-based variable"), sym);
      else { /* ref */
        if (CONTTYPE(cont) == C_PUSH) { /* special code for push */
          i_svpush(off); 
          cinfo.stack++;
          return;
        }
        i_svref(off);
      }
      cd_cont(cont);
      return;

    case VAR_STACK_BOXED: /* boxed stack variable */
      if (set)
        i_svseti(off);
      else /* ref: no special code for push yet */
        i_svrefi(off);
      cd_cont(cont);
      return;

    case VAR_DISPLAY:  /* displayed alias to stack variable */
      if (set) syntaxError(T("set! to display variable"), sym);
      else /* ref: no special code for push yet */
        i_dvref(off);
      cd_cont(cont);
      return;

    case VAR_DISPLAY_BOXED: /* boxed display variable */
      if (set)
        i_dvseti(off);
      else /* ref: no special code for push yet */
        i_dvrefi(off);
      cd_cont(cont);
      return;

    case VAR_CONST: { /* second-class constant/label */
      /* off is offset to (var . init) */
      SOBJ init = cdr(vmsp[off]);
      /* make sure we are not trying to change it... */
      if (set) syntaxError(T("set! to constant"), sym);
      if (car(init) == sd_squote) {
        /* constant: make sure we are not compiling a call */
        if (CONTTYPE(cont) == C_CALL) syntaxError(T("calling quoted const"), sym);
        /* integrate as literal */
        cd_literal(vmsp[off + 1], cont); /* datum is at next offset */
        return;
      } else {
        /* label: make sure we are compiling a call */
        if (CONTTYPE(cont) != C_CALL) syntaxError(T("leaking label"), sym);
        else { /* cook the correct call opcodes */
          cint narg, ref, lev;
          /* decode call continuation */
          cont = CONTPARAM(cont); /* callee continuation */
          narg = CONTPARAM(cont); /* callee arg count */
          /* bump off to point to the label data in compiler stack */
          ++off;
          ref = (cint)getsfixshort(vmsp[off]); /* label address */
          /* generate opcodes for different callee continuations */
          if (CONTTYPE(cont) == C_RETURN && (lev = todrop(narg)) > 0) {
            /* spec case #1: tail call with stack adjustment */
            /* generate SBSB op and update address fixup chain */
            ref = i_sbsb(narg, lev, ref);
            vmsp[off] = cvsfixnum(ref);
          } else if (ref <= 0 && off < (int) (cinfo.argv - vmsp)) {  
            /* label is from this frame and not yet expanded */
            /* spec case #2: 'drop into' compilation */
            i_acnt(narg); /* setup the arg count; fun is expanded at call point */
          } else {
            /* normal case: not a tail call */
            ref = i_bsb(narg, ref);
            /* generate BSB op and update address fixup chain */
            vmsp[off] = cvsfixnum(ref);
          }
          /* check, if need to expand here */
          ref = (cint)getsfixshort(vmsp[off]); /* label address now */
          if (ref <= 0 && off < (int) (cinfo.argv - vmsp)) { 
            /* label is from this frame and not yet expanded */
            /* expand at call point */
            ref = fixup(-ref, FIXUP_HERE);
            vmsp[off] = cvsfixnum(ref);
            cinfo.stack -= narg;
            cd_auxdef(init);
            cinfo.stack -= SAVEFRAME;
          } else {
            /* don't expand */
            cinfo.stack -= narg + (CONTTYPE(cont) != C_RETURN ? SAVEFRAME : 0);
          }
        }
      }
      return;
    } 
  }
}


/*
 * push_display_vars - display building code helper
 */
static void push_display_vars(SOBJ display)
{
  if (consp(display)) { /* compile last var first */
    push_display_vars(cdr(display));
    { cint off;
      SOBJ sym = car(display);
      assert(symbolp(sym));
      switch (findcevar(sym, &off)) {
        case VAR_AC:  /* accumulator var : */
          i_push();
          break;
        case VAR_STACK:  /* stack variable */
        case VAR_STACK_BOXED: /* boxed stack variable: don't unbox! */
          i_svpush(off); 
          break;
        case VAR_DISPLAY:  /* displayed alias to stack variable */
        case VAR_DISPLAY_BOXED: /* boxed display variable: don't unbox! */
          i_dvref(off);
          i_push();
          break;
        default:
          syntaxError(T("unexpected display var"), sym);
      }
      ++cinfo.stack;
    }
  }
}

/*
 * cd_dclose - compile a display closure building code
 */
static void cd_dclose(SOBJ display, cint codelit, cont_t cont)
{
  cint stack = cinfo.stack;
  push_display_vars(display);
  i_dclose(cinfo.stack - stack, codelit);
  /* restore stack to pre-dclose level */
  cinfo.stack = stack;
  cd_cont(cont);
}

/* cd_literal - compile a literal reference */
static void cd_literal(SOBJ lit, cont_t cont)
{
  int id;
  FIXTYPE pos;
  static struct compiler_littab_t {
    SOBJ lit; int op_val; int op_push;
  } i_littab[] = {
    { NULL,       OP_LITB,    OP_PUSHLITB }, /* [0] - small int */
    { NULL,       OP_LIT,     OP_PUSHLIT  }, /* [1] - literals  */
    { so_default, OP_DEFAULT, OP_PUSHDEF  },
    { so_void,    -1,         OP_PUSH     },
    { so_nil,     OP_NIL,     OP_PUSHNIL  },
    { so_true,    OP_T,       OP_PUSHT    },
    { so_false,   OP_F,       OP_PUSHF    }
  };
  /* check if we can optimize out the instruction */
  switch (CONTTYPE(cont)) {
    case C_NEXT:
      return;
    case C_BRT:
      fixloc = falsep(lit) ? 0 : ii_branch(OP_BR);
      return;
    default: 
      /* no luck: instruction is needed */
      break;
  }
  /* calc and emit the required instruction */
  pos = -1;
  if (sfixp(lit) && (pos = getsfixnum(lit)) >= MINLITB && pos < MAXLITB) {
    pos &= LITBMASK;
    id = 0;
  } else {
    for (id = sizeof(i_littab) / sizeof(i_littab[0]) - 1;; id--) {
      if (id == 1) {
        pos = findliteral(lit);
        break;
      }
      if (i_littab[id].lit == lit)
        break;
    }
  }
  if (CONTTYPE(cont) == C_PUSH) {
    emit(i_littab[id].op_push);
    if (pos != -1)
      emit((vmop_t) pos);
    cinfo.stack++;
  } else {
    if ((id = i_littab[id].op_val) != -1)
      emit((vmop_t) id);
    if (pos != -1)
      emit((vmop_t) pos);
    cd_cont(cont);
  }
}

/* cd_integrable - compile an integrable procedure */
static void cd_integrable(SOBJ inentry, cint narg, cont_t cont)
{
  /* pick the corresponding op list */
  SOBJ ops;
  switch (CONTTYPE(cont)) {
    default:     /* pick first element */
      ops = car(inentry);
      break;
    case C_PUSH: /* pick second element */
      ops = car(cdr(inentry));
      break;
    case C_BRT:  /* pick third element */
      ops = car(cdr(cdr(inentry)));
      break;
  }
  /* emit the codes */
  for (; consp(ops); ops = cdr(ops)) {
    FIXTYPE b;
    if (!sfixp(car(ops)) || (b = getsfixnum(car(ops))) > 255 || b < -1)
      syntaxError(T("bad opcode for integrable procedure"), car(ops));
    else {
      emit(b >= 0 ? (cint) b : narg);
    }
  }
  /* adjust stack */
  cinfo.stack -= narg;
  /* finalize integration */
  switch (CONTTYPE(cont)) {
    case C_PUSH:
      ++cinfo.stack;
      break;
    case C_BRT:
      fixloc = ii_branch(cbuff[--cptr]);
      break;
    default:
      cd_cont(cont);
      break;
  }
}

/* fixup - fixup a reference chain */
static cint fixup(cint ref, cint loc)
{

  if (ref == 0)
    return loc;
  else if (ref < 0)
    compilerError(T("bad fixup"));
  /* store the value into each location in the chain */
  if (loc <= 0) { /* bind next fixup to the chain */
    loc = -loc;
    cbuff[ref + 0] = (vmop_t)loc;
    cbuff[ref + 1] = (vmop_t)(loc >> 8);
    return -ref;
  }
  loc -= cinfo.cbase; /* fixup the chain */
  while (ref > 0) {
    cint nxt = (cbuff[ref + 1] << 8) | (cbuff[ref + 0]);
    cbuff[ref + 0] = (vmop_t)loc;
    cbuff[ref + 1] = (vmop_t)(loc >> 8);
    ref = nxt;
  }
  return loc += cinfo.cbase;
}

/**********************************  **********************************/
