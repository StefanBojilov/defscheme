/* sxdisasm.c - VM disassembler */

#include "sxm.h"
#include "sxwrite.h"
#include "sxdisasm.h"
#include "vmop.h"
#include "define.h"
#include "extern.h"

/* instruction output formatters forward decls */
typedef int (*formatter_t) (tchar_t *buf, tchar_t *name, vmop_t *cp);
#define OPSIG(sig) static int format_##sig (tchar_t *buf, tchar_t *name, vmop_t *cp);
#define OPCODE(name, num, sig) /* ignore opcodes */
#include "vmop.t"
#undef OPSIG
#undef OPCODE

/* format table entry structure */
typedef struct {
  vmop_t ot_code;
  tchar_t* ot_name;
  formatter_t ot_formatter;
} OTDEF;

/* format table */
#define OPSIG(sig) /* ignore opcodes */
#define OPCODE(name, num, sig) {num, T(#name), format_##sig},
static OTDEF otab[] = {
#include "vmop.t"
};
#undef OPSIG
#undef OPCODE
#define OTAB_SIZE (sizeof(otab)/sizeof(otab[0]))

/* sxDisProc - decode the instructions in a code object */
void sxDisProc(SOBJ port, SOBJ fun, int pc)
{
  ss_size_t len, lc;
  SOBJ code, env;

  if (closurep(fun)) {
    code = getcode(fun);
    env = getcenv(fun);
  } else {
    code = fun;
    env = so_nil;
    pc = -1;
  }
  len = getslength(getbcode(code)) - 1; /* exclude zero terminator */
  for (lc = 0; lc < len;) {
    if ((int)lc == pc) {
      sxWriteString(T(";; [SAVED PC] ==>"), port);
      sxNewline(port);
    }
    lc += sxDisInst(port, code, lc, env, pc);
  }
}



/* sxDisInst - decode a single bytecode instruction   */
/* if pc != -1 (second arg provided) assume run-time object    */
/* Returns: next instruction PC */
static SOBJ theport;
static SOBJ thecode;

ss_size_t sxDisInst(SOBJ port, SOBJ code, ss_size_t lc, SOBJ env, int pc)
{
  vmop_t *cp;
  tchar_t buf[120];
  SOBJ tmp;
  int n = 1;

  /* get a pointer to the bytecodes for this instruction */
  cp = (vmop_t *) getstring(getbcode(code)) + lc;

  /* show the address and opcode */
  tmp = getcname(code);
  if (consp(tmp))
    tmp = car(tmp);
  if (symbolp(tmp))
    stprintf(buf, T("%s:%04lx %02x "),
             getpstring(tmp), (unsigned long) lc, (unsigned) *cp);
  else
    stprintf(buf, T("%lx:%04lx %02x "),
             (unsigned long) code, (unsigned long) lc, (unsigned) *cp);
  sxWriteString(buf, port);

  { /* find & call operand formatter */
    OTDEF *op;
    int i;
    formatter_t formatter = NULL;
    tchar_t *name = NULL;
    for (op = otab, i = OTAB_SIZE; i > 0; --i, ++op) {
      if (op->ot_code == *cp) {
        formatter = op->ot_formatter;
        name = op->ot_name;
        break;
      }
    }
    if (formatter == NULL) {
      /* special cases */
      vmop_t op = *cp;
      if (op > OP_SREF0 && op <= OP_SREF0 + 7) {
        name = T("SREF0"); formatter = format_RANGE07;
      } else if (op > OP_PUSHSREF0 && op <= OP_PUSHSREF0 + 7) {
        name = T("PUSHSREF0"); formatter = format_RANGE07;
      } else {
        name = T("<UNKNOWN>"); formatter = format_NONE;
      }
    }
    theport = port;
    thecode = code;
    n += (formatter)(buf, name, cp);
  }
  if (*buf != 0) sxWriteString(buf, port);
  sxNewline(port);
  return n;
}


/* no parameters */
static int format_NONE (tchar_t *buf, tchar_t *name, vmop_t *cp)
{
  stprintf(buf, T("            (%s)"), name);
  return 0;
}

/* 1-byte LITB parameter */
static int format_LITB (tchar_t *buf, tchar_t *name, vmop_t *cp)
{
  stprintf(buf, T("%02x          (%-10s %d)"), cp[1], name, (int)(sbyte_t)cp[1]);
  return 1;
}

/* 1-byte parameter */
static int format_BYTE (tchar_t *buf, tchar_t *name, vmop_t *cp)
{
  stprintf(buf, T("%02x          (%-10s %d)"), cp[1], name, cp[1]);
  return 1;
}

/* 1-byte litvec index */
static int format_LITREF (tchar_t *buf, tchar_t *name, vmop_t *cp)
{
  stprintf(buf, T("%02x          (%-10s "), cp[1], name);
  sxWriteString(buf, theport);
  sxWrite(getelement(thecode, cp[1]), theport);
  stprintf(buf, T(")"));
  return 1;
}

/* 1-byte parameter and 1-byte litvec index of data */
static int format_BYTE_LITREF (tchar_t *buf, tchar_t *name, vmop_t *cp)
{
  stprintf(buf, T("%02x %02x       (%-10s %d "),
           cp[1], cp[2], name, cp[1]);
  sxWriteString(buf, theport);
  sxWrite(getelement(thecode, cp[2]), theport);
  stprintf(buf, T(")"));
  return 2;
}

/* 1-byte parameter and 1-byte litvec index of code */
static int format_BYTE_CODEREF (tchar_t *buf, tchar_t *name, vmop_t *cp)
{
  return format_BYTE_LITREF(buf, name, cp);
}

/* 2-byte address */
static int format_ADDR (tchar_t *buf, tchar_t *name, vmop_t *cp)
{
  stprintf(buf, T("%02x %02x       (%-10s %02x%02x)"), cp[1], cp[2],
           name, cp[2], cp[1]);
  return 2;
}

/* 1-byte parameter and 2-byte address */
static int format_BYTE_ADDR (tchar_t *buf, tchar_t *name, vmop_t *cp)
{
  stprintf(buf, T("%02x %02x %02x    (%-10s %d %02x%02x)"),
           cp[1], cp[2], cp[3], name, cp[1], cp[3], cp[2]);
  return 3;
}

/* 2 1-byte parameters */
static int format_BYTE_BYTE (tchar_t *buf, tchar_t *name, vmop_t *cp)
{
  stprintf(buf, T("%02x %02x       (%-10s %d %d)"), cp[1], cp[2],
           name, cp[1], cp[2]);
  return 2;
}

/* 2 1-byte parameters and 2-byte address */
static int format_BYTE_BYTE_ADDR (tchar_t *buf, tchar_t *name, vmop_t *cp)
{
  stprintf(buf, T("%02x %02x %02x %02x (%-10s %d %d %02x%02x)"),
           cp[1], cp[2], cp[3], cp[4], name,
           cp[1], cp[2], cp[4], cp[3]);
  return 4;
}

/* 1-byte litvec index and 2-byte address */
static int format_LITREF_ADDR (tchar_t *buf, tchar_t *name, vmop_t *cp)
{
  stprintf(buf, T("%02x %02x %02x    (%-10s "), 
           cp[1], cp[2], cp[3], name);
  sxWriteString(buf, theport);
  sxWrite(getelement(thecode, cp[1]), theport);
  stprintf(buf, T(" %02x%02x)"), cp[3], cp[2]);
  return 3;
}

/* 1-byte DVAR index */
static int format_DVAR (tchar_t *buf, tchar_t *name, vmop_t *cp)
{
  return format_BYTE(buf, name, cp);
}

/* 1-byte litvec index of gcell */
static int format_GCELLREF (tchar_t *buf, tchar_t *name, vmop_t *cp)
{
  stprintf(buf, T("%02x          (%-10s "), cp[1], name);
  sxWriteString(buf, theport);
  sxDisplay(getelement(thecode, cp[1]), theport);
  stprintf(buf, T(")"));
  return 1;
}

/* 1-byte litvec index of code */
static int format_CODEREF (tchar_t *buf, tchar_t *name, vmop_t *cp)
{
  return format_LITREF(buf, name, cp);
}

/* 1-byte frame index */
static int format_INDEX (tchar_t *buf, tchar_t *name, vmop_t *cp)
{
  return format_BYTE(buf, name, cp);
}

/* 1-byte node tag */
static int format_TAG (tchar_t *buf, tchar_t *name, vmop_t *cp)
{
  return format_BYTE(buf, name, cp);
}

/* 1-byte node tag and and 2-byte address */
static int format_TAG_ADDR (tchar_t *buf, tchar_t *name, vmop_t *cp)
{
  return format_BYTE_ADDR(buf, name, cp);
}

/* no parameters (CURRY) */
static int format_STOP (tchar_t *buf, tchar_t *name, vmop_t *cp)
{
  return format_NONE(buf, name, cp);
}

/* 1-byte frame size and 1-byte litvec index of vars */
static int format_EFRAME_VARSREF (tchar_t *buf, tchar_t *name, vmop_t *cp)
{
  return format_BYTE_LITREF(buf, name, cp);
}

/* 1-byte frame number and 1-byte frame index */
static int format_FRAME_INDEX (tchar_t *buf, tchar_t *name, vmop_t *cp)
{
  return format_BYTE_BYTE(buf, name, cp);
}

/* no parameters (SREF0, PUSHSREF0)*/
static int format_RANGE07 (tchar_t *buf, tchar_t *name, vmop_t *cp)
{
  vmop_t op = *cp;
  int n = 0;
  if (op >= OP_SREF0 && op <= OP_SREF0 + 7) {
    n = op - OP_SREF0;
  } else if (op >= OP_PUSHSREF0 && op <= OP_PUSHSREF0 + 7) {
    n = op - OP_PUSHSREF0;
  }
  stprintf(buf, T("            (%-10s %d)"), name, n);
  return 0;
}

