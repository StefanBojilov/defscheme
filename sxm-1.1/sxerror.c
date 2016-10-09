/* sxerror.c - error functions */

#include "sxm.h"
#include "os.h"
#include "sxwrite.h"
#include "extern.h"

/*
 * slow type checker
 */
 
bool_t sx_typep(SOBJ val, int nt)
{
  switch (nt) {
    case NT_FIXNUM:     return fixp(val);
    case NT_CONS:
    case NT_SYMBOL:
    case NT_KEYWORD:
    case NT_HASHTABLE:
    case NT_RECORD:
    case NT_BOX:
    case NT_STRING:
    case NT_CHAR:
    case NT_VECTOR:
    case NT_PORT:
    case NT_CLOSURE:
    case NT_HANDLE:
    case NT_GCELL:      return typep(val, nt);
    case NT__LIST:      return consornullp(val);
    case NT__NUMBER:    return numberp(val);
    case NT__PROC:      return procedurep(val);
    case NT__BYTE:      return bytep(val);
    case NT__IPORT:     return iportp(val);
    case NT__OPORT:     return oportp(val);
    case NT__ANY:       return TRUE;
  }
  assert(FALSE);
  ospanic(T("sx_typep called with illegal type number"));
  return FALSE;  
}
  

/*
 *  common argument parsing errors
 */

void sxae_argc(int narg)
{
  sxErr(T("wrong number of arguments"),
        cons(cvsfixnum(narg), cvsfixnum(vmargc)));
}

/* sxae_toofew - too few arguments to this function */
SOBJ sxae_toofew(void)
{
  return sxFail(T("too few arguments"));
}

/* sxae_toomany - too many arguments to this function */
SOBJ sxae_toomany(void)
{
  return sxFail(T("too many arguments"));
}

/* sxae_justone - just one argument required to this function */
SOBJ sxae_justone(void)
{
  return sxFail(T("just one argument required"));
}

/* sxae_type - incorrect argument type */
SOBJ sxae_type(SOBJ val, int t)
{
  tchar_t *s;
  static tchar_t buf[80];

  switch (t) {
    case NT_CONS:       s = T("a pair");           break;
    case NT_SYMBOL:     s = T("a symbol");         break;
    case NT_KEYWORD:    s = T("a keyword");        break;
    case NT_HASHTABLE:  s = T("a hash table");     break;
    case NT_BOX:        s = T("a box");            break;
    case NT_RECORD:     s = T("a record");         break;
    case NT_STRING:     s = T("a string");         break;
    case NT_FIXNUM:     s = T("an exact integer"); break;
    case NT_CHAR:       s = T("a character");      break;
    case NT_VECTOR:     s = T("a vector");         break;
    case NT_BOOLEAN:    s = T("a boolean");        break;
    case NT_PORT:       s = T("a port");           break;
    case NT_CLOSURE:    s = T("a closure");        break;
    case NT_HANDLE:     s = T("a handle");         break;
    case NT_GCELL:      s = T("a gcell");          break;
    case NT__LIST:      s = T("a list");           break;
    case NT__NUMBER:    s = T("a number");         break;
    case NT__PROC:      s = T("a procedure");      break;
    case NT__BYTE:      s = T("an exact integer in a range 0-255"); break;
    case NT__IPORT:     s = T("an input port");    break;
    case NT__OPORT:     s = T("an output port");   break;
    case 0:
    default:            s = T("of an expected type");  break;
  }
  stprintf(buf, T("argument is not %s"), s);
  sxErr(buf, val);
  return so_unbound; /* Impossible */
}

/* sxae_range - incorrect argument range */
SOBJ sxae_range(SOBJ index, SOBJ val)
{
  sxErr(T("index out of range"), nullp(val) ? index : cons(index, val));
  return so_unbound; /* Impossible */
}

/* sxae_immutable - immutable argument to mutation function */
SOBJ sxae_immutable(SOBJ val)
{
  sxErr((symbolp(val) ? T("immutable top-level binding")
                        : T("immutable argument")), val);
  return so_unbound; /* Impossible */
}


/*
 * Basic error handlers
 */

/* what we eventually need is to emulate error call; this
 * means that we have to put "reset" continuation on top
 * of the existing stack (it is in an unknown state now, so
 * we cannot safely continue by returning to it and reset
 * continuation makes sure we won't return to it). after 
 * that we have to push args (last arg is pushed first), 
 * format string and "where" object, longjump to reset
 * C stack down to main VM loop, and call the error
 * handler from there. 
 */

void sxError0(const tchar_t* fstr)
{
  vm_error0(fstr);
}

void sxError1(const tchar_t* fstr, SOBJ arg)
{
  vm_error1(fstr, arg);
}

/* sxErr - report an error and call error handler */
SOBJ sxErr(const tchar_t *msg, SOBJ arg)
{
  if (arg != so_unbound) {
    static tchar_t buf[121];
    tcsncpy(buf, msg, 120);
    buf[120] = 0;
    if (tcslen(buf) + 6 <= 120) tcscat(buf, T(":~% ~s"));
    vm_error1(buf, arg);
  } else {
    vm_error0(msg);
  }
  never_returns(so_void);
}

SOBJ sxFail(const tchar_t *msg)
{
  vm_error0(msg);
  never_returns(so_void);
}


EXTERN_VARIABLE(sv_conout)

/* sxSysErr - print an error message and jump out to toplevel */
void sxSysErr(const tchar_t *msg)
{
  /* display the error message */
  sxWriteString(T("System Error: "), sv_conout);
  sxWriteString(msg, sv_conout);
  sxNewline(sv_conout);

  /* print the function where the error occurred */
  sxWriteString(T("happened in: "), sv_conout);
  sxDisplay(vmfun, sv_conout); 
  sxNewline(sv_conout);

  /* reset back to the top level */
  vm_reset();
}

