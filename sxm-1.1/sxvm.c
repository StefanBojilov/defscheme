/* sxvm.c - the Virtual Machine */

#include "sxm.h"
#include "vmop.h"
#include "os.h"
#include "sxio.h"
#include "sxintern.h"
#include "sxlocale.h"
#include "sxdisasm.h"
#include "sxmath.h"
#include "define.h"
#include "extern.h"

#define FULLCASE
/*#define NOVCHECK*/
/*#define INDEXPC*/

/* forward declarations */
extern SOBJ sk_callmv;
static SOBJ do_reroot(SOBJ val, SOBJ newds);
static SOBJ do_reroot_step(int argc);

EXTERN_VARIABLE(sv_curexnhandler)
EXTERN_VARIABLE(sv_curerrorhandler)
EXTERN_VARIABLE(sv_curerrorcont)
EXTERN_VARIABLE(sv_curtimhandler)
EXTERN_VARIABLE(sv_curresethandler)
EXTERN_VARIABLE(sv_curexithandler)
EXTERN_VARIABLE(sv_curkbinthandler)

DEFINE_DATUM_INIT(sd_brestore)
{
  static byte_t bcode[] = {(byte_t)OP_RESTORE/*, (byte_t)0*/};
  SOBJ code = newcode(FIRSTLIT);

  gcLock(code);
  setcname(code, so_nil);
  setbcode(code, so_nil);
  setcname(code, sxSymEnter(T("%continuation%")));
  setbcode(code, cvbytevec(bcode, sizeof(bcode)));
  gcUnlock(1);
  return code;
}

DEFINE_CONTINUATION(sk_reset, ignored)
{
  CONT_GOTO(sv_curresethandler, 0);
}

DEFINE_CONTINUATION(sk_retval, ignored)
{
  CONT_GOTO(sv_curresethandler, 0);
}

/* virtual machine registers */
SOBJ vmfun;                     /* current function */
SOBJ vmenv;                     /* current environment */
SOBJ vmdstate;                  /* current dynamic state */
int vmargc;                     /* argument count */
int vmint = 0;                  /* VM interrupt before instruction fetch */
SOBJ *vmsp;                     /* value stack pointer */
SOBJ *vmstkbase;                /* base of value stack */
SOBJ *vmstktop;                 /* top of value stack (actually, one beyond) */

jmp_buf vm_dispatch;            /* VM dispatcher */
static SOBJ vm_exval;           /* variable to pass a value trough an
                                 * exception */

/* macros to get the address of the code string for a code object */
#define getlitvec(fun)      getvdata(fun)
#define getcodestr(litv)    ((vmop_t*)getstring((litv)[0]))

/* local variables */
static long ticks = 0L;         /* timer ticks left before interrupt */

/* local routines */
static SOBJ restore_continuation(SOBJ cont, int argc);
static SOBJ escape_continuation(SOBJ cont, int argc);
static SOBJ *vm__vref(SOBJ vector, SOBJ index);
static tchar_t *vm__strref(SOBJ string, SOBJ index);

long vm_gettimer(void)
{
  return ticks / 20L;
}

void vm_settimer(long val)
{
  if ((ticks = val * 20L) != 0)
    vmint |= VMI_TIMER;
  else
    vmint &= ~VMI_TIMER;
}

/* vm_restart - restart VM using new startup procedure */
void vm_restart(SOBJ proc)
{
  vm_exval = proc;
  longjmp(vm_dispatch, 2);
}

/* vm_error0 - call the error handler */
void vm_error0(const tchar_t* fstr)
{
  PUSH_CONT(sk_reset, so_void);
  PUSH_ARG(cvstring(fstr));
  vm_exval = so_fix1;
  longjmp(vm_dispatch, 1);
}

/* vm_error1 - call the error handler */
void vm_error1(const tchar_t* fstr, SOBJ arg)
{
  PUSH_CONT(sk_reset, so_void);
  PUSH_ARG(arg);
  PUSH_ARG(cvstring(fstr));
  vm_exval = so_fix2;
  longjmp(vm_dispatch, 1);
}

/* vm_reset - stop VM execution and return so_nil */
void vm_reset(void)
{
  longjmp(vm_dispatch, 3);
}

/* vm_exit - exit VM (and caller loop) */
void vm_exit(SOBJ retcode)
{
  vm_exval = retcode; /* exit must return integer */
  longjmp(vm_dispatch, 4);
}


EXTERN_VARIABLE(sv_conout)

/* sxExecute - execute byte codes */
SOBJ sxExecute(SOBJ fun, SOBJ tmp/*arglist*/)
{
  register SOBJ ac;
  register int i;

#ifdef INDEXPC  /* pc is index (good if local pointer cannot
                 * be in register) */
  register int pc;
  vmop_t *base;

#define getop() (base[pc++])
#define peekop() (base[pc])
#define peekopat(n) (base[pc+n])
#define skipops(n) (pc += (n))
#define pcoffset() (pc)
#define pcset(n) (pc = (n))
#ifdef le2bint_t
#define getpc(x) (x = *((le2bint_t*)&(base[pc])), pc += 2)
#define peekpc() (*((le2bint_t*)&(base[pc])))
#else
#define getpc(x) (x = base[pc++], x |= (base[pc++] << 8))
#define peekpc() (base[pc]|(base[pc+1] << 8))
#endif
#else   /* !INDEXPC (pc is pointer) */
  vmop_t *pc;
  vmop_t *base;

#define getop() (*pc++)
#define peekop() (*pc)
#define peekopat(n) (pc[n])
#define skipops(n) (pc += (n))
#define pcoffset() ((int)(pc - base))
#define pcset(n) (pc = base + (n))
#ifdef le2bint_t
#define getpc(x) (x = (*((le2bint_t*)pc)), pc+=2)
#define peekpc() ((*((le2bint_t*)pc)))
#else
#define getpc(x) (x = *pc++, x |= (*pc++ << 8))
#define peekpc() (pc[0] | (pc[1] << 8))
#endif
#endif  /* !INDEXPC */

#ifdef NOVCHECK  /* speed-up kluge for large stack: disable
                  * overflow check */
#define ispush(v) push(v)
#define isfree(n)
#else
#define ispush(v) cpush(v)
#define isfree(n) check(n)
#endif


#define JCOND(cnd) { if (cnd) goto jump; else skipops(2); }
#define CONSARG(x) if (!consp(x)) sxae_type(x,NT_CONS);
#define unwind(d,args) { register int _i=(args); \
                         while (_i--) vmsp[_i+(d)] = vmsp[_i]; drop(d); }

#define recover_vm_regs_after_gc_int(i) do { \
          vmlitv = getlitvec(vmfun); i = pcoffset(); \
          base = getcodestr(vmlitv); pcset(i); \
          vmint &= ~VMI_GC; \
        } while (0)


  SOBJ *vmlitv = NULL;          /* literal vector pointer */

restart:                        /* longjumping via vm_restart */

  gcUnlockAll();                /* all locks are invalid at this point */

  /* initialize the registers for apply: */
  vmfun = so_nil;               /* not a 'valid' value ? */
  vmenv = so_nil;
  base = NULL;                  /* not a 'valid' value ? */
  pcset(0);

  /* setup the 'exit' continuation */
  check(2);
  push(sk_endexec);             /* handled specially in restore: */
  push(vmenv);

  /* push sxExecute args and setup the initial argument count */
  i = length(tmp); check(i); vmsp -= i; vmargc = i;
  for (i = 0; consp(tmp); tmp = cdr(tmp)) vmsp[i++] = car(tmp);

  /* setup a target for the error handler & apply fun */
  switch (setjmp(vm_dispatch)) {
    case 0:                     /* going thru with first-time setjmp() */
      ac = fun;                 /* function to apply in AC  */
      goto apply_ac;            
    case 1:                     /* longjumping via new vm_error0/vm_error1 */
      gcUnlockAll();            /* free GC-locks before entering errorHandler */
      ac = sv_curerrorhandler;
      if (procedurep(ac)) {
        /* check(1); tmp = cvsfixnum(pcoffset()); push(cons(vmfun, tmp)); */
        cpush(vmfun);
        vmargc = getfixnum(vm_exval) + 1;
        goto apply_ac;
      }
      /* no error handler: reset */
      goto reset;
    case 2:                     /* longjumping via vm_restart */
      gcUnlockAll();            /* free GC-locks */
      fun = vm_exval;
      tmp = so_nil;
      goto restart;
    case 3:                     /* longjumping via vm_reset */
      gcUnlockAll();            /* free GC-locks */
reset:
      ac = sv_curresethandler;
      if (procedurep(ac)) {
        vmargc = 0;
        goto apply_ac;
      }
      /* no reset handler: exit with retcode 1 */
      return so_fix1;
    case 4:                     /* longjumping via vm_exit */
      gcUnlockAll();            /* free GC-locks */
      return vm_exval;          
    default:
      assert(FALSE);
      ospanic(T("Unexpected longjmp parameter!"));
  }

  /*
   * The function should be in AC and the arguments should be on the stack.
   * The number of arguments should be in vmargc.
   */
apply_ac:                       /* apply the function in AC */
  if (nodep(ac))                /* check for valid node */
    switch (_ntype(ac)) {       /* dispatch on function type */
      case NT_CLOSURE:
        vmenv = getcenv(ac);
        vmfun = getcode(ac);
        vmlitv = getlitvec(vmfun);
        base = getcodestr(vmlitv);
        pcset(0);
        goto execute;
      case NT_SUBR:
        /* for proper error handling inside primitives */
        vmfun = ac; /* vmenv = so_fix0; */
        ac = (*getsubr(ac)) ();
        goto restore_ac;
    }
  xlreqproc(ac);

jump:                           /* set pc and continue */
  pcset(peekpc());

execute:                        /* execute the code */
  for (;;) {
    if (vmint) {
      /* check for GC interrupt */
      if (vmint & VMI_GC)
        recover_vm_regs_after_gc_int(i);
      /* check for timer interrupts */
      if ((vmint & VMI_TIMER) && --ticks == 0L) {
        vmint &= ~VMI_TIMER;
        check(6); vmsp -= 6;
        vmsp[0] = vmenv;
        vmsp[1] = sk_popregs;
        vmsp[2] = vmfun;
        vmsp[3] = cvsfixnum(pcoffset());
        vmsp[4] = cvsfixnum(vmargc);
        vmsp[5] = ac;
        vmargc = 0;
        ac = sv_curtimhandler;
        goto apply_ac;
      }
      /* check for keyboard interrupts */
      if (vmint & VMI_KBINT) {
        vmint &= ~VMI_KBINT;
        check(6); vmsp -= 6;
        vmsp[0] = vmenv;
        vmsp[1] = sk_popregs;
        vmsp[2] = vmfun;
        vmsp[3] = cvsfixnum(pcoffset());
        vmsp[4] = cvsfixnum(vmargc);
        vmsp[5] = ac;
        vmargc = 0;
        ac = sv_curkbinthandler;
        goto apply_ac;
      }
      /* print trace information
      if (vmint & VMI_TRACE)
        sxDisInst(sv_conout, vmfun, (int) pcoffset(),
                  vmenv, (int) pcoffset()); */
    }

    /* execute the next bytecode instruction */
    switch (getop()) {

      case OP_SREF0 + 0:        ac = vmsp[0];   break;
      case OP_SREF0 + 1:        ac = vmsp[1];   break;
      case OP_SREF0 + 2:        ac = vmsp[2];   break;
      case OP_SREF0 + 3:        ac = vmsp[3];   break;
      case OP_SREF0 + 4:        ac = vmsp[4];   break;
      case OP_SREF0 + 5:        ac = vmsp[5];   break;
      case OP_SREF0 + 6:        ac = vmsp[6];   break;
      case OP_SREF0 + 7:        ac = vmsp[7];   break;

      case OP_PUSHSREF0 + 0:    isfree(1);      pushsref(0);    break;
      case OP_PUSHSREF0 + 1:    isfree(1);      pushsref(1);    break;
      case OP_PUSHSREF0 + 2:    isfree(1);      pushsref(2);    break;
      case OP_PUSHSREF0 + 3:    isfree(1);      pushsref(3);    break;
      case OP_PUSHSREF0 + 4:    isfree(1);      pushsref(4);    break;
      case OP_PUSHSREF0 + 5:    isfree(1);      pushsref(5);    break;
      case OP_PUSHSREF0 + 6:    isfree(1);      pushsref(6);    break;
      case OP_PUSHSREF0 + 7:    isfree(1);      pushsref(7);    break;

      case OP_EFRAME:           /* create an environment frame */
        i = getop();            /* get the frame size */
        vmenv = newframe(vmenv, i, vmlitv[getop()]);
        break;
      case OP_MVARGS:           /* * move required arguments to frame slot */
        if ((vmargc -= (i = getop())) < 0)
          sxae_toofew();
        /* fill the slots */
        sobjcpy(&envar(vmenv, FIRSTARG), vmsp, i);
        drop(i);
        break;
      case OP_MVOARG:           /* move optional argument to frame slot */
        i = getop();            /* get the slot number */
        /* newframe() fills the frame by the so_default */
        if (vmargc > 0) {
          envar(vmenv, i) = pop();
          --vmargc;
        }
        break;
      case OP_MVRARG:           /* build rest argument and move to frame slot */
        i = getop();            /* get the slot number */
        envar(vmenv, i) = cvlist(vmsp, vmargc, so_nil);
        drop(vmargc);
        break;
      case OP_SHOARG:           /* shift optional argument on the stack */
        if ((i = getop()) > vmargc)
          sxae_toofew();
        else if (i == vmargc) { /* shift up 1 top i stack elements */
          isfree(1);
          sobjmvup(vmsp - 1, vmsp, i);
          vmsp--;
          vmsp[i] = so_default;
          vmargc++;
        }
        break;
      case OP_SHRARG:           /* build rest argument and push it */
        if ((i = getop()) > vmargc)
          sxae_toofew();
        else {
          int k = vmargc - i;   /* frame size */
          ac = cvlist(vmsp + i, k, so_nil);
          unwind(k, i);
        }
        push(ac);
        break;
      case OP_ATEST:            /* test number of actual arguments */
        if ((i = getop() - vmargc) != 0) {
          if (i < 0) sxae_toomany(); else sxae_toofew();
        }
        break;
      case OP_ADROP:            /* drop arguments */
        drop(getop());
        break;
      case OP_ALAST:            /* make sure there are no more arguments */
        if (vmargc > 0)  sxae_toomany();
        break;

      case OP_GREF:             /* gcell reference */
        ac = vmlitv[i = getop()];
        if ((ac = getgcellval(ac)) == so_unbound)
          sxErr(T("unbound variable"), getgcellname(vmlitv[i]));
        break;
      case OP_GSET:             /* gcell set (check mutability of top-level
                                 * binding!) */
        tmp = vmlitv[getop()];
        if (ismutable(tmp))
          setgcellval(tmp, ac);
        else if (!equal(getgcellval(tmp), ac))
          sxae_immutable(tmp);  /* error!!! */
        /* else warn user: try to mute immutable! */
        break;

      case OP_EREF:             /* env var reference */
#define _EREF(x)  envar(x,getop())
        switch ((i = getop())) {
          case 3: ac = _EREF(env1(env1(env1(vmenv)))); break;
          case 2: ac = _EREF(env1(env1(vmenv)));       break;
          case 1: ac = _EREF(env1(vmenv));             break;
          case 0: ac = _EREF(vmenv);                   break;
          default:
            for (ac = vmenv; --i >= 0; ac = env1(ac)) {}
            ac = _EREF(ac);
        }
        break;
      case OP_PUSHEREF: /* env var reference */
        switch ((i = getop())) {
          case 3: ispush(_EREF(env1(env1(env1(vmenv)))));    break;
          case 2: ispush(_EREF(env1(env1(vmenv))));          break;
          case 1: ispush(_EREF(env1(vmenv)));                break;
          case 0: ispush(_EREF(vmenv));                      break;
          default:
            for (tmp = vmenv; --i >= 0; tmp = env1(tmp)) {}
            ispush(_EREF(tmp));
        }
        break;
      case OP_ESET:             /* env var set */
#define _ESET(x)  (envar(x,getop()) = ac)
        switch ((i = getop())) {
          case 3: _ESET(env1(env1(env1(vmenv))));   break;
          case 2: _ESET(env1(env1(vmenv)));         break;
          case 1: _ESET(env1(vmenv));               break;
          case 0: _ESET(vmenv);                     break;
          default:
            for (tmp = vmenv; --i >= 0; tmp = env1(tmp)) {}
            _ESET(tmp);
        }
        break;

      case OP_SAVE:             /* save a continuation */
        isfree(3);
        getpc(i);
        vmsp -= 3;
        vmsp[0] = vmenv;
        vmsp[1] = vmfun;
        vmsp[2] = cvsfixnum(i);
        break;

      case OP_CALL:             /* jump to a procedure */
        vmargc = getop();
        goto apply_ac;
      case OP_SCALL:            /* unwind stack and jump to a procedure */
        vmargc = getop();       /* get argument count */
        i = getop();            /* get frame size     */
        unwind(i, vmargc);
        goto apply_ac;

      case OP_SRETURN:          /* unwind stack and restore saved
                                 * continuation */
        drop(getop());          /* drop stack argument frame */
      case OP_RETURN:           /* restore the enviroment and the
                                 * continuation */
    restore_ac:
        vmenv = vmsp[0];
        tmp = vmsp[1];
        /* dispatch on the function type */
        if (nodep(tmp))
          switch (_ntype(tmp)) {
            case NT_CODE:
              vmfun = tmp;
              vmlitv = getlitvec(tmp);
              base = getcodestr(vmlitv);
              pcset(getsfixshort(vmsp[2]));
              vmsp += 3;
              goto execute;
            case NT_CSUBR:
              vmsp += 2;
              ac = (*(vm_csubr_t) getsubr(tmp)) (ac);
              if (vmargc == -1)
                goto restore_ac;
              else
                goto apply_ac;
          }
        /* process primitive continuations */
        if (tmp == sk_endexec) { /* 'exit' continuation */
          return ac;
        } else if (tmp == sk_popregs) { /* restore vm regs */
          vmfun = vmsp[2];
          vmlitv = getlitvec(vmfun);
          base = getcodestr(vmlitv);
          pcset(getsfixshort(vmsp[3]));
          vmargc = getsfixshort(vmsp[4]);
          ac = vmsp[5];
          vmsp += 6;
          goto execute;
        } else
          sxErr(T("bad continuation"), tmp);
        break;

      case OP_CLOSE:            /* make a closure object */
        ac = cvclosure(vmlitv[getop()], vmenv); /* GC-safe */
        break;
      case OP_MKPROMISE:        /* make a promise object */
        ac = cvpromise(ac);     /* force will check the type */
        break;

      case OP_AERROR:           /* bad number of actual arguments */
        sxFail(getop() ? T("too many arguments") : T("too few arguments"));
        break;
      case OP_EDROP:            /* drop topmost env frame */
        vmenv = env1(vmenv);
        break;
      case OP_ACNT:             /* load argument counter */
        vmargc = getop();
        break;
      case OP_BSB:              /* branch to an inline procedure */
        vmargc = getop();       /* get argument count */
        goto jump;
      case OP_SBSB:             /* unwind stack and branch to an inline
                                 * procedure */
        vmargc = getop();       /* get argument count */
        i = getop();            /* get frame size     */
        unwind(i, vmargc);
        goto jump;

        /* !!! new opcodes for display-closure support !!! */
        /* in display closures vmenv is a vector of display vars */
      case OP_DCLOSE:           /* [n] [i] create display closure */
        i = getop();            /* get the number of vars */
        ac = newvector(i, NV_NO_INIT);
        sobjcpy(getvdata(ac), vmsp, i);
        drop(i);
        if (vmint & VMI_GC) recover_vm_regs_after_gc_int(i);
        ac = cvclosure(vmlitv[getop()], ac);    /* GC-safe */
        break;
      case OP_DREF:             /* access display variable */
        /* index follows opcode; */
        ac = getelement(vmenv, getop());
        break;
      case OP_SHIFT:            /* [n] [f] unwind stack */
        i = getop();            /* get argument count */
        unwind(getop(), i);
        break;
        /* !!! opcodes to support boxed variables !!! */
      case OP_SBOX:             /* [off] box stack location */
        i = getop();
        vmsp[i] = cvbox(vmsp[i], so_nil);       /* GC-safe */
        break;
      case OP_DROP:             /* [n] generic stack unwind */
        drop(getop());
        break;
      case OP_SREFI:            /* [off] ac = unbox(sp[off]) */
        ac = getboxval(vmsp[getop()]);
        break;
      case OP_SSETI:            /* [off] set-box(sp[off],ac) */
        setboxval(vmsp[getop()], ac);
        break;
      case OP_DREFI:            /* [off] ac = unbox(env[off]) */
        ac = getboxval(getelement(vmenv, getop()));
        break;
      case OP_DSETI:            /* [off] set-box(env[off],ac) */
        setboxval(getelement(vmenv, getop()), ac);
        break;

        /*
         * standard inline functions support: OP_xxx       - leave result in
         * AC OP_PUSHxxx   - leave result on stack (AC could be spoiled)
         * OP_BRxxx     - jump to label if result != #f
         */

      case OP_MEMQ:
        for (tmp = pop();; tmp = cdr(tmp)) {
          if (!consp(tmp)) { ac = so_false; break; }
          if (ac == car(tmp)) { ac = tmp; break; }
        }
        break;
      case OP_PUSHMEMQ:
        for (tmp = top(), settop(so_false); consp(tmp); tmp = cdr(tmp))
          if (ac == car(tmp)) { settop(tmp); break; }
        break;
      case OP_BRMEMQ:
        for (tmp = pop(); consp(tmp); tmp = cdr(tmp))
          if (ac == car(tmp)) goto jump;
        skipops(2);
        break;
      case OP_MEMV:
        for (tmp = pop();; tmp = cdr(tmp)) {
          if (!consp(tmp)) { ac = so_false; break; }
          if (eqv(ac, car(tmp))) { ac = tmp; break; }
        }
        break;
      case OP_PUSHMEMV:
        for (tmp = top(), settop(so_false); consp(tmp); tmp = cdr(tmp))
          if (eqv(ac, car(tmp))) { settop(tmp); break; }
        break;
      case OP_BRMEMV:
        for (tmp = pop(); consp(tmp); tmp = cdr(tmp))
          if (eqv(ac, car(tmp))) goto jump;
        skipops(2);
        break;

      case OP_T:
        ac = so_true;
        break;
      case OP_F:
        ac = so_false;
        break;
      case OP_NIL:
        ac = so_nil;
        break;
      case OP_DEFAULT:
        ac = so_default;
        break;
      case OP_CONS:
        ac = cons(ac, pop());
        break;
      case OP_ATOM:
        ac = cvbool(!consp(ac));
        break;
      case OP_PAIR:
        ac = cvbool(consp(ac));
        break;
      case OP_NULL:
        ac = cvbool(nullp(ac));
        break;
      case OP_EQ:
        ac = cvbool(ac == pop());
        break;
      case OP_NOT:
        ac = cvbool(falsep(ac));
        break;
      case OP_OMITTED:
        ac = cvbool(ac == so_default);
        break;
      case OP_EQLIT:
        ac = cvbool(ac == vmlitv[getop()]);
        break;
      case OP_SREF:
        ac = vmsp[getop()];
        break;
      case OP_LITB:
        ac = cvsfixnum(((int)(sbyte_t)getop()));
        break;
      case OP_LIT:
        ac = vmlitv[getop()];
        break;
      case OP_POP:
        ac = pop();
        break;
      case OP_PUSH:
        ispush(ac);
        break;
      case OP_PUSHT:
        ispush(so_true);
        break;
      case OP_PUSHF:
        ispush(so_false);
        break;
      case OP_PUSHNIL:
        ispush(so_nil);
        break;
      case OP_PUSHDEF:
        ispush(so_default);
        break;
      case OP_PUSHLIT:
        ispush(vmlitv[getop()]);
        break;
      case OP_PUSHSREF:
        tmp = vmsp[getop()];
        ispush(tmp);
        break;
      case OP_PUSHCONS:
        settop(cons(ac, top()));
        break;
      case OP_PUSHLITB:
        ispush(cvsfixnum((sbyte_t)getop()));
        break;

      case OP_PUSHCAR:
        CONSARG(ac);
        ispush(car(ac));
        break;
      case OP_PUSHCDR:
        CONSARG(ac);
        ispush(cdr(ac));
        break;
      case OP_CAR:
        CONSARG(ac);
        ac = car(ac);
        break;
      case OP_CDR:
        CONSARG(ac);
        ac = cdr(ac);
        break;
      case OP_SETCAR:
        CONSARG(ac);
        setcar(testmutable(ac), pop());
        break;
      case OP_SETCDR:
        CONSARG(ac);
        setcdr(testmutable(ac), pop());
        break;

      case OP_ADD:
        if (sfix2p(ac, top()))
          ac = cvfixnum(getsfixnum(ac) + getsfixnum(pop()));
        else
          ac = numBinarySOBJ(numAdd, ac, pop());
        break;
      case OP_SUB:
        if (sfix2p(ac, top()))
          ac = cvfixnum(getsfixnum(ac) - getsfixnum(pop()));
        else
          ac = numBinarySOBJ(numSubtract, ac, pop());
        break;
      case OP_MUL:
        if (sfix2p(ac, top()))
          ac = cvfixnum(getsfixnum(ac) * getsfixnum(pop()));
        else
          ac = numBinarySOBJ(numMultiply, ac, pop());
        break;
      case OP_QUO:
        ac = numBinarySOBJ(numQuotient, ac, pop());
        break;
      case OP_INC:
        if (sfixp(ac) && sfixcmp(ac, <, cvsfixnum(SFIXMAX)))
          ac = (SOBJ)((FIXTYPE)ac + SFIX1DELTA);
        else
          ac = numUnarySOBJ(numAdd1, ac);
        break;
      case OP_DEC:
        if (sfixp(ac) && sfixcmp(ac, >, cvsfixnum(SFIXMIN)))
          ac = (SOBJ)((FIXTYPE)ac - SFIX1DELTA);
        else
          ac = numUnarySOBJ(numSubtract1, ac);
        break;
      case OP_LT:
        if (sfix2p(ac, top()))
          ac = cvbool(sfixcmp(ac, <, pop()));
        else
          ac = numCompareSOBJ(numLessP, ac, pop());
        break;
      case OP_EQL:
        if (sfix2p(ac, top()))
          ac = cvbool(ac == pop());
        else
          ac = numCompareSOBJ(numEqualP, ac, pop());
        break;
      case OP_GT:
        if (sfix2p(ac, top()))
          ac = cvbool(sfixcmp(ac, >, pop()));
        else
          ac = numCompareSOBJ(numGreaterP, ac, pop());
        break;

      case OP_PUSHADD:
        if (sfix2p(ac, top()))
          settop(cvfixnum(getsfixnum(ac) + getsfixnum(top())));
        else
          settop(numBinarySOBJ(numAdd, ac, top()));
        break;
      case OP_PUSHSUB:
        if (sfix2p(ac, top()))
          settop(cvfixnum(getsfixnum(ac) - getsfixnum(top())));
        else
          settop(numBinarySOBJ(numSubtract, ac, top()));
        break;
      case OP_PUSHMUL:
        if (sfix2p(ac, top()))
          settop(cvfixnum(getsfixnum(ac) * getsfixnum(top())));
        else
          settop(numBinarySOBJ(numMultiply, ac, top()));
        break;
      case OP_PUSHQUO:
        settop(numBinarySOBJ(numQuotient, ac, top()));
        break;
      case OP_PUSHINC:
        if (sfixp(ac) && sfixcmp(ac, <, cvsfixnum(SFIXMAX)))
          ispush((SOBJ)((FIXTYPE)ac + SFIX1DELTA));
        else
          ispush(numUnarySOBJ(numAdd1, ac));
        break;
      case OP_PUSHDEC:
        if (sfixp(ac) && sfixcmp(ac, >, cvsfixnum(SFIXMIN)))
          ispush((SOBJ)((FIXTYPE)ac - SFIX1DELTA));
        else
          ispush(numUnarySOBJ(numSubtract1, ac));
        break;

      case OP_PUSHMKPROM:
        ispush(cvpromise(ac));
        break;

      case OP_BRT:
        JCOND(!falsep(ac));
        break;
      case OP_BRNOT:
        JCOND(falsep(ac));
        break;
      case OP_BR:
        goto jump;
      case OP_BRSREF:
        JCOND(!falsep(vmsp[getop()]));
        break;
      case OP_BRATOM:
        JCOND(!consp(ac));
        break;
      case OP_BRPAIR:
        JCOND(consp(ac));
        break;
      case OP_BREQ:
        JCOND(ac == pop());
        break;
      case OP_BRNULL:
        JCOND(nullp(ac));
        break;
      case OP_BROMITTED:
        JCOND(ac == so_default);
        break;
      case OP_BREQLIT:
        JCOND(ac == vmlitv[getop()]);
        break;

      case OP_BRLT:
        if (sfix2p(ac, top())) {
          JCOND(sfixcmp(ac, <, pop()));
        } else {
          JCOND(numCompareSOBJ(numLessP, ac, pop()) == so_true);
        }
        break;
      case OP_BREQL:
        if (sfix2p(ac, top())) {
          JCOND(ac == pop());
        } else {
          JCOND(numCompareSOBJ(numEqualP, ac, pop()) == so_true);
        }
        break;
      case OP_BRGT:
        if (sfix2p(ac, top())) {
          JCOND(sfixcmp(ac, >, pop()));
        } else {
          JCOND(numCompareSOBJ(numGreaterP, ac, pop()) == so_true);
        }
        break;

      case OP_LIST:
        i = getop();            /* get the list length */
        ac = cvlist(vmsp, i, so_nil);
        drop(i);
        break;
      case OP_PUSHLIST:
        i = getop();            /* get the list length */
        ac = cvlist(vmsp, i, so_nil);
        drop(i);
        push(ac);
        break;
      case OP_ILIST:
        i = getop();            /* get the list length */
        ac = cvlist(vmsp, i - 1, vmsp[i - 1]);
        drop(i);
        break;
      case OP_PUSHILIST:
        i = getop();            /* get the list length */
        ac = cvlist(vmsp, i - 1, vmsp[i - 1]);
        drop(i);
        push(ac);
        break;


        /* misc. instructions */

      case OP_MVC:              /* call/mv specific */
        isfree(2);
        vmsp -= 2;
        vmsp[0] = ac;
        vmsp[1] = sk_callmv;
        break;

      case OP_SMVC:             /* call/mv specific */
        isfree(2);
        vmsp -= 2;
        i = getop();            /* get frame size */
        sobjmvup(vmsp, vmsp + 2, i);    /* lift frame */
        vmsp[i + 0] = ac;
        vmsp[i + 1] = sk_callmv;
        break;

      case OP_PORTOP:   /* obsolete */
        sxFail(T("PORTOP: obsolete"));
        break;

      case OP_RESTORE:
        tmp = restore_continuation(vmenv, vmargc); /* stack is switched */
        /* now stack contains saved continuation (maybe from call/mv) */
        /* build VALUES continuation */
        ac = vm_values(vmargc);
        /* now build dynamic space tree rerooting cont */
        ac = do_reroot(ac, tmp);
        if (vmargc == -1)
          goto restore_ac;
        else
          goto apply_ac;

      case OP_TYPEP:
        ac = cvbool(xntype(ac) == getop());
        break;
      case OP_BRTYPEP:
        JCOND(xntype(ac) == getop());
        break;

      case OP_VREF:
        ac = *vm__vref(ac, pop());
        break;
      case OP_PUSHVREF:
        settop(*vm__vref(ac, top()));
        break;
      case OP_VSET:
        *vm__vref(ac, vmsp[0]) = vmsp[1];
        drop(2);
        break;
      case OP_VSIZE:
        if (!vectorp(ac))
          sxae_type(ac, NT_VECTOR);
        else
          ac = cvfixnum(getvsize(ac));
        break;

      case OP_CURRY: /* cheap closures! */
        /*
         * now stack contains N extra arguments; vmargc = N; env is a list
         * (fun argk .. arg0), where fun is a function to call and arg[k-0]
         * are extra left args (in reverse order)
         */
        ac = car(vmenv);
        for (tmp = cdr(vmenv); consp(tmp); tmp = cdr(tmp)) {
          ispush(car(tmp));
          vmargc++;
        }
        /* all arguments are in place and counted. Jump to fun! */
        goto apply_ac;

      case OP_ZERO: /* new in Sxm'99 */
        if (sfixp(ac)) ac = cvbool(ac == so_fix0);
        else ac = numPredicateSOBJ(numZeroP, ac);
        break;
      case OP_BRZERO: /* new in Sxm'99 */
        if (sfixp(ac)) {
          JCOND(ac == so_fix0);
        } else {
          JCOND(numPredicateSOBJ(numZeroP, ac) == so_true);
        }
        break;

      case OP_STRREF: {
        tchar_t c = *vm__strref(ac, pop());
        ac = cvchar(c);
      } break;
      case OP_PUSHSTRREF: {
        tchar_t c = *vm__strref(ac, top());
        settop(cvchar(c));
      } break;
      case OP_STRSET:
        if (!charp(vmsp[1]))
          sxae_type(vmsp[1], NT_CHAR);
        else 
          *vm__strref(ac, vmsp[0]) = getchcode(vmsp[1]);
        drop(2);
        break;
      case OP_STRSIZE:
        if (!stringp(ac))
          sxae_type(ac, NT_STRING);
        else
          ac = cvsfixnum(getslength(ac)-1);
        break;

      case OP_SHAUXARG:   /* push aux arg */
        push(so_default); /* so_default is here for no reason... */
        vmargc++;
        break;


#ifdef FULLCASE

     /*
      * kluge to get faster code on some compilers: all possible cases
      * should be defined
      */
                      case 0x01:      case 0x02:      case 0x03:
      case 0x04:      case 0x05:      case 0x06:      case 0x07:
      case 0x08:      case 0x09:      case 0x0A:      case 0x0B:
      case 0x0C:      case 0x0D:      case 0x0E:      case 0x0F:

                                      
                                                      case 0x9B:
      case 0x9C:      case 0x9D:      case 0x9E:      case 0x9F:

      case 0xA0:      case 0xA1:      case 0xA2:      case 0xA3:
      case 0xA4:      case 0xA5:      case 0xA6:      case 0xA7:
      case 0xA8:      case 0xA9:      case 0xAA:      case 0xAB:
      case 0xAC:      case 0xAD:      case 0xAE:      case 0xAF:

      case 0xB0:      case 0xB1:      case 0xB2:      case 0xB3:
      case 0xB4:      case 0xB5:      case 0xB6:      case 0xB7:
      case 0xB8:      case 0xB9:      case 0xBA:      case 0xBB:
      case 0xBC:      case 0xBD:      case 0xBE:      case 0xBF:

      case 0xC0:      case 0xC1:      case 0xC2:      case 0xC3:
      case 0xC4:      case 0xC5:      case 0xC6:      case 0xC7:
      case 0xC8:      case 0xC9:      case 0xCA:      case 0xCB:
      case 0xCC:      case 0xCD:      case 0xCE:      case 0xCF:

      case 0xD0:      case 0xD1:      case 0xD2:      case 0xD3:
      case 0xD4:      case 0xD5:      case 0xD6:      case 0xD7:
      case 0xD8:      case 0xD9:      case 0xDA:      case 0xDB:
      case 0xDC:      case 0xDD:      case 0xDE:      case 0xDF:

      case 0xE0:      case 0xE1:      case 0xE2:      case 0xE3:
      case 0xE4:      case 0xE5:      case 0xE6:      case 0xE7:
      case 0xE8:      case 0xE9:      case 0xEA:      case 0xEB:
      case 0xEC:      case 0xED:      case 0xEE:      case 0xEF:

      case 0xF0:      case 0xF1:      case 0xF2:      case 0xF3:
      case 0xF4:      case 0xF5:      case 0xF6:      case 0xF7:
      case 0xF8:      case 0xF9:      case 0xFA:      case 0xFB:
      case 0xFC:      case 0xFD:      case 0xFE:      case 0xFF:
#else
      default:
#endif  /* FULLCASE */
        /* no default: - switch is exaustive! */
        sxErr(T("bad opcode"), cvsfixnum(peekopat(-1)));
      case 0x00:
        sxErr(T("opcode 0"), vmfun);
    }
  }
}


/************** Common Utils ****************/

DEFINE_CONTINUATION(sk_apply, result)
{
  /* get argc and invoke procedure */
  CONT_GOTO(result, getsfixshort(CONT_ENV()));
}

/* vm_apply - pushes simple continuation to deliver argc args to proc */
/* <= stack: arg1..argn, cur.cont; argc = n                           */
/* => arg for new continuation on stack; vmargc: extra info for cont; */
/* USAGE in VM: ac = return value; goto restore_ac;                   */
SOBJ vm_apply(SOBJ proc, int argc)
{
  PUSH_CONT(sk_apply, cvsfixnum(argc));
  return proc;
}

DEFINE_CONTINUATION(sk_ret_value, result)
{
  result = CONT_ENV(); /* ignore result */
  CONT_RETURN(result);
}

/* push continuation that ignores its arg and returns value */
void vm_push_value_cont(SOBJ value)
{
  PUSH_CONT(sk_ret_value, value);
}


/********** Multiple Values Utils ***********/

DEFINE_CONTINUATION(sk_callmv, result)
{
  /* get the procedure */
  SOBJ proc = vmenv;

  /* call it with result */
  cpush(result);
  /* invoke procedure with single result */
  CONT_GOTO(proc, 1);
}

/* push CALL-WITH-VALUES continuation */
void vm_push_cmv_cont(SOBJ receiver)
{
  PUSH_CONT(sk_callmv, receiver);
}

/* looks for sk_callmv under argc arguments; is found,
 * shifts args down, puts receiver into pval and returns TRUE;
 * else discards extra args, puts single val into pval and
 * returns FALSE */
static bool_t dig_up_cmv_cont(int argc, SOBJ *pval)
{
  /* current continuation resides under arguments */
  if (vmsp[argc + 1] == sk_callmv) {
    /* continuation was created by call/mv and can accept all values */
    *pval = vmsp[argc];  /* receiver in env position */
    /* shift arguments down (we don't need env/sk_callmv anymore) */
    switch (argc) {
      case 7: vmsp[6 + 2] = vmsp[6];
      case 6: vmsp[5 + 2] = vmsp[5];
      case 5: vmsp[4 + 2] = vmsp[4];
      case 4: vmsp[3 + 2] = vmsp[3];
      case 3: vmsp[2 + 2] = vmsp[2];
      case 2: vmsp[1 + 2] = vmsp[1];
      case 1: vmsp[0 + 2] = vmsp[0];
      case 0: break;
      default:
        sobjmvdn(vmsp + 2, vmsp, argc);
    }
    drop(2);
    return TRUE;
  } else {
    /* other continuations can accept just one value */
    *pval = (argc == 1) ? top() : so_mv_mismatch;
    drop(argc);
    return FALSE;
  }
}

/* return argc values from procedure */
SOBJ vm_values(int argc)
{
  SOBJ val;
  if (dig_up_cmv_cont(argc, &val)) {
    /* sk_callmv was found and args arranged for receiver call */
    PROC_GOTO(val, argc);
  } else {
    /* sk_callmv wasn't found and single arg is left */
    PROC_RETURN(val);
  }
}

DEFINE_CONTINUATION(sk_ret_values, result)
{
  int argc = (int)getsfixshort(CONT_ENV());
  if (dig_up_cmv_cont(argc, &result)) {
    /* sk_callmv was found and args arranged for receiver call */
    CONT_GOTO(result, argc);
  } else {
    /* sk_callmv wasn't found and single arg is left */
    CONT_RETURN(result);
  }
}

/* push continuation that ignores its arg and returns argc values */
void vm_push_values_cont(int argc)
{
  PUSH_CONT(sk_ret_values, cvsfixnum(argc));
}


/************** Dynamic State Utils ***************/

/* continuation for dynamic state tree navigation */
DEFINE_CONTINUATION(sk_reroot, result)
{
  /* result is ignored */
  /* stack: dscnt, ds1..dsn, val, cont */
  SOBJ ln = pop();
  int n = getsfixshort(ln);

  if (n < 0)
    sxErr(T("bad reroot nodes cnt"), ln);
  return do_reroot_step(n);
}

/* USAGE in VM: ac = return value; goto restore_ac; */
SOBJ vm_reroot(SOBJ newds)
{
  int n;
  SOBJ lst;

  /* if newds is the same as current, do nothing */
  if (newds == vmdstate)
    return so_void;

  /* prepare data for rerooting... */
  cpush(so_void); /* sk_reroot will pass val to its own continuation */

  /* push to stack all intermediate ds nodes starting from newds */
  for (n = 0, lst = newds; lst != vmdstate; lst = cdr(lst)) {
    if (!consp(lst))
      sxErr(T("bad new dstate"), newds);
    cpush(lst);
    n++;
  }

  /* prepare new continuation */
  /* stack: dscnt, ds1..dsn, val, cont */
  check(3);
  push(cvsfixnum(n));
  push(sk_reroot);              /* stack: dscnt, ds1..dsn, val, cont */
  push(so_nil);                 /* => env */
  return so_void;               /* ignored by sk_reroot cont */
}

/* do_reroot - builds continuation to navigate in dynamic space tree    */
/* <= stack: cur.cont; val = cur.cont argument                          */
/* USAGE in VM: if(vmargc == -1) return retval;else apply(retval,args); */
static SOBJ do_reroot(SOBJ val, SOBJ newds)
{
  int n;
  SOBJ lst;

  if (!consp(vmdstate) || car(vmdstate) != so_false)
    sxErr(T("bad dstate"), vmdstate);

  /* if newds is the same as current, restore current cont */
  if (newds == vmdstate) {
    vmargc = -1;
    return val;
  }
  /* prepare data for rerooting... */
  cpush(val); /* sk_reroot will pass val to its own continuation */

  /* push to stack all intermediate ds nodes starting from newds */
  for (n = 0, lst = newds; lst != vmdstate; lst = cdr(lst)) {
    if (!consp(lst))
      sxErr(T("bad new dstate"), newds);
    cpush(lst);
    n++;
  }
  /* prepare first thunk to call and continuation for it */
  return do_reroot_step(n);
}

/*
 * prepare cont; vmargc = 0; return before_thunk => apply/return | vmargc =
 * -1; return saved val => return to cont under it
 */
static SOBJ do_reroot_step(int argc)
{
  SOBJ before, after, ba, ds;

  /* check end condition */
  if (argc == 0) {
    /* return saved val to original continuation under it */
    vmargc = -1;                /* VM: return */
    return pop();               /* => ac */
  }
  /* get next target dstate */
  ds = pop();
  argc--;
  /* do as many checks as possible */
  if (!consp(ds))
    sxErr(T("bad reroot ds"), ds);
  if (!consp(vmdstate) || car(vmdstate) != so_false)
    sxErr(T("bad dstate in reroot"), vmdstate);
  if (cdr(ds) != vmdstate)
    sxErr(T("reroot ds is not next"), ds);
  ba = car(ds);
  if (!consp(ba))
    sxErr(T("bad ba pair in ds"), ds);
  /* get before & after thunks */
  before = car(ba);
  after = cdr(ba);
  /* reroot to ds */
  setcdr(vmdstate, ds);
  setcar(vmdstate, ba);
  setcar(ba, after);
  setcdr(ba, before);
  setcdr(ds, so_nil);
  setcar(ds, so_false);
  vmdstate = ds;
  /* ok: dstate tree is in next state */
  /* arrange everything to call 'before' thunk and continue rerooting */
  /* 1. prepare new continuation */
  check(3);
  push(cvsfixnum(argc));
  push(sk_reroot);              /* stack: dscnt, ds1..dsn, val, cont */
  push(so_nil);                 /* => env */
  /* 2. apply before thunk */
  vmargc = 0;                   /* VM: apply to 0 args */
  return before;                /* => ac */
}

/* high level utils */

/* sp_dynwind_aux - return to the old ds; return "during" thunk values */
DEFINE_PROCEDURE(sp_dynwind_aux)
{
  /* get oldds from the top */
  SOBJ oldds = xlgetarg();
  /* push continuation that will return other arguments */
  PUSH_VALUES_CONT(vmargc);
  /* reroot back to oldds */
  return vm_reroot(oldds);
}

/* sk_dynwind - we're in new dynamic state here; call "during" thunk */
DEFINE_CONTINUATION(sk_dynwind, result)
{ 
  SOBJ during = CONT_ENV();
  /* oldds is on top of stack */
  /* create receiver for its values */
  SOBJ receiver = CURRY(sp_dynwind_aux, 1);
  /* push CALL/MV continuation and call 'during' thunk */
  PUSH_CALLMV_CONT(receiver);
  PROC_GOTO(during, 0);
  /* resulds will be delivered to sp_dynwind_aux closure (see above) */
}

/* vm_dynwind : prepare for dynamic-wind return from procedure */
SOBJ vm_dynwind(SOBJ before, SOBJ during, SOBJ after)
{
  SOBJ newds;
  /* make new dstate */
  gcLock(during);
  newds = cons(cons(before, after), vmdstate);
  gcUnlock(1);
  /* reroot and continue into sk_dynwind (see above) */
  PUSH_ARG(vmdstate);
  PUSH_CONT(sk_dynwind, during);
  return vm_reroot(newds);
}

/* vm_fluidbind : prepare for fluid-let jump from procedure */
SOBJ vm_fluidbind(SOBJ setproc, SOBJ newval, SOBJ thunk, SOBJ oldval)
{
  SOBJ before, after;
  /* make before & after thunks */
  gcLock(thunk);
  gcLock(setproc);
  PUSH_ARG(oldval);
  PUSH_ARG(newval);
  before = CURRY(setproc, 1);
  gcLock(before);
  after = CURRY(setproc, 1);
  gcUnlock(3);
  PROC_DYNAMIC_WIND(before, thunk, after);
}

/************ Simple Closure Utils ***********/

DEFINE_DATUM_INIT(sd_bcurry)
{
  static byte_t bcode[] = {(byte_t)OP_CURRY/*, (byte_t)0*/};
  SOBJ code = newcode(FIRSTLIT);

  gcLock(code);
  setcname(code, so_nil);
  setbcode(code, so_nil);
  setcname(code, sxSymEnter(T("%simple-closure%")));
  setbcode(code, cvbytevec(bcode, sizeof(bcode)));
  gcUnlock(1);
  return code;
}

/* vm_curry - creates simple closure that adds argc args to proc args */
/* <= proc stack: arg1..argn, callerarg..., cur.cont; vmargc += n     */
SOBJ vm_curry(SOBJ proc, int argc)
{
  SOBJ lst = cons(proc, so_nil);

  gcLock(lst);
  /* collect arguments in a list (in reverse order!) */
  for (; argc > 0; argc--) {
    SOBJ p = cons(pop(), cdr(lst));

    setcdr(lst, p);
  }
  /* create a closure with curry code and lst as environment */
  proc = cvclosure(sd_bcurry, lst);
  gcUnlock(1);
  return proc;
}

/******** Exception System Utils *********/


DEFINE_CONTINUATION(sk_raise, result)
{
  result = CONT_ENV(); /* get exn and make compiler happy */
  sxErr(T("attempt to return from RAISE"), result);
  never_returns(so_void);
}

/* vm_prepare_handler - prepares exception handler and arguments */
SOBJ vm_prepare_handler(SOBJ exn)
{
  PUSH_CONT(sk_raise, exn);
  PUSH_ARG(exn);
  return sv_curexnhandler;
}

/*********** Continuation Helpers ************/

/* restore_continuation - restore a continuation to the stack */
static SOBJ restore_continuation(SOBJ cont, int argc)
{
  sv_size_t size;
  SOBJ arg1 = so_nil;           /* init to make g++ happy */
  SOBJ *oldsp = vmsp;

  if (sfixp(cont))
    return escape_continuation(cont, argc);
  size = getvsize(cont);
  vmsp = vmstktop - (size - 1) - argc;
  check(1);
  /* move arguments to new location, possible overlap */
  if (argc <= 1)
    arg1 = *oldsp;              /* most frequent case */
  else
    sobjmove(vmsp, oldsp, argc);
  /* move saved stack */
  sobjcpy(vmsp + argc, getvdata(cont) + 1, size - 1);
  if (argc == 1)
    *vmsp = arg1;
  return getvdata(cont)[0];     /* DSTATE */
}

/* escape_continuation - restore a continuation to the stack */
static SOBJ escape_continuation(SOBJ cont, int argc)
{
  sv_size_t size;
  SOBJ arg1 = so_nil;           /* init to make g++ happy */
  SOBJ *oldsp = vmsp;

  size = (sv_size_t) getsfixnum(cont);
  if (oldsp > vmstktop - size)
    sxFail(T("Escape point lost"));
  vmsp = vmstktop - size - argc;
  /* move arguments to new location, possible overlap */
  if (argc <= 1)
    arg1 = *oldsp;              /* most frequent case */
  else
    sobjmove(vmsp, oldsp, argc);
  /* move saved stack */
  if (argc == 1)
    *vmsp = arg1;
  return vmdstate;
}

/* vm_stkover - value stack overflow */
SOBJ vm_stkover(void)
{
  sxSysErr(T("value stack overflow"));
  never_returns(so_false);
}

/*********** Arg List Helpers ************/

/* vm_flatten_star_args: vmargc+stack ->
 * new vmargc+stack for (arg... arglist) */

int vm_flatten_star_args(int argc)
{
  SOBJ *p;
  SOBJ arglist, args;
  int len;

  if (argc == 0)
    sxae_toofew();

  /* case 1: [(argn...)] */
  if (argc == 1) {
    args = arglist = pop();
    len = length(args);

    /* copy the arguments onto the stack */
    check(len);
    for (p = vmsp - len; consp(args); args = cdr(args))
      *p++ = car(args);
    if (!nullp(args))
      xlreqlist(arglist);

    /* set the new sp, argument count and return */
    vmsp -= len;
    return len;
  }
  args = arglist = vmsp[argc - 1];

  /* case 2: [arg1 arg2... ()] */
  if (!consp(args)) {
    if (!nullp(args))
      xlreqlist(arglist);

    /* move down arguments on stack */
    sobjmvdn(vmsp + 1, vmsp, argc - 1);

    /* set the new sp, argument count and return */
    vmsp++;
    return argc - 1;
  }
  /* case 3: [arg1 arg2... (argn argn+1...)] */
  len = length(args);

  /* move up arguments on stack and set new sp */
  if (len > 1) {
    check(len - 1);
    sobjmvup(vmsp - (len - 1), vmsp, argc - 1);
    vmsp -= len - 1;
  }
  /* copy the arguments onto the stack */
  for (p = vmsp + argc - 1; consp(args); args = cdr(args))
    *p++ = car(args);
  if (!nullp(args))
    xlreqlist(arglist);

  /* set argument count and return */
  return argc + len - 1;
}

static SOBJ *vm__vref(SOBJ vector, SOBJ index)
{
  FIXTYPE i = 0;                /* init to make g++ happy */

  if (!vectorp(vector))
    sxae_type(vector, NT_VECTOR);
  if (!sfixp(index)) {          /* => nodep(index) == TRUE */
    if (_ntype(index) == NT_FIXNUM)
      i = getfixnum(index);
    else
      sxae_type(index, NT_FIXNUM);
  } else
    i = getsfixnum(index);
  if (i < 0 || i >= (FIXTYPE)getvsize(vector))
    sxae_range(index, cvsfixnum(getvsize(vector)));
  return getvdata(vector) + (sv_size_t) i;
}

static tchar_t *vm__strref(SOBJ string, SOBJ index)
{
  FIXTYPE i = 0;                /* init to make g++ happy */

  if (!stringp(string))
    sxae_type(string, NT_STRING);
  if (!sfixp(index)) {          /* => nodep(index) == TRUE */
    if (_ntype(index) == NT_FIXNUM)
      i = getfixnum(index);
    else
      sxae_type(index, NT_FIXNUM);
  } else
    i = getsfixnum(index);
  if (i < 0 || i >= (FIXTYPE)getslength(string)-1)
    sxae_range(index, cvsfixnum(getslength(string)-1));
  return getstring(string) + (ss_size_t) i;
}
