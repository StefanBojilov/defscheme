/* pairs.c - standard procedures 6.3 p.1 */

#include "sxm.h"
#include "define.h"

/*#| (pair? obj) |#*/
DEFINE_INITIAL_BINDING("pair?", sp_pairp)
DEFINE_PROCEDURE(sp_pairp)
{
  SOBJ arg = xlonearg();
  return cvbool(consp(arg));
}

/*#| (atom? obj) |#*/
DEFINE_INITIAL_BINDING("atom?", sp_atomp)
DEFINE_PROCEDURE(sp_atomp)
{
  SOBJ arg = xlonearg();
  return cvbool(!consp(arg));
}

/*#| (cons car cdr) |#*/
DEFINE_INITIAL_BINDING("cons", sp_cons)
DEFINE_PROCEDURE(sp_cons)
{
  SOBJ carval,cdrval;
  carval = xlgetarg();
  cdrval = xlgetarg();
  xllastarg();
  return cons(carval, cdrval);
}

/*#| (car pair) |#*/
DEFINE_INITIAL_BINDING("car", sp_car)
DEFINE_PROCEDURE(sp_car)
{
  SOBJ co = xlgacons();
  xllastarg();
  return car(co);
}

/*#| (%car pair) |#*/
DEFINE_INITIAL_BINDING("%car", sp_icar)
DEFINE_PROCEDURE(sp_icar)
{
  SOBJ co = xlonearg();
  return car(co);
}

/*#| (cdr pair) |#*/
DEFINE_INITIAL_BINDING("cdr", sp_cdr)
DEFINE_PROCEDURE(sp_cdr)
{
  SOBJ co = xlgacons();
  xllastarg();
  return cdr(co);
}

/*#| (%cdr pair) |#*/
DEFINE_INITIAL_BINDING("%cdr", sp_icdr)
DEFINE_PROCEDURE(sp_icdr)
{
  SOBJ co = xlonearg();
  return cdr(co);
}

/*#| (set-car! pair obj) |#*/
DEFINE_INITIAL_BINDING("set-car!", sp_setcar)
DEFINE_PROCEDURE(sp_setcar)
{
  SOBJ pair = xlgacons();
  SOBJ arg = xlgetarg();
  xllastarg();
  setcar(testmutable(pair), arg);
  return so_void;
}

/*#| (%set-car! pair-like obj) |#*/
DEFINE_INITIAL_BINDING("%set-car!", sp_isetcar)
DEFINE_PROCEDURE(sp_isetcar)
{
  SOBJ pair = xlgetarg();
  SOBJ arg = xlgetarg();
  xllastarg();
  setcar(pair, arg);
  return pair;
}

/*#| (set-cdr! pair obj) |#*/
DEFINE_INITIAL_BINDING("set-cdr!", sp_setcdr)
DEFINE_PROCEDURE(sp_setcdr)
{
  SOBJ pair = xlgacons();
  SOBJ arg = xlgetarg();
  xllastarg();
  setcdr(testmutable(pair), arg);
  return so_void;
}

/*#| (%set-cdr! pair-like obj) |#*/
DEFINE_INITIAL_BINDING("%set-cdr!", sp_isetcdr)
DEFINE_PROCEDURE(sp_isetcdr)
{
  SOBJ pair = xlgetarg();
  SOBJ arg = xlgetarg();
  xllastarg();
  setcdr(pair, arg);
  return pair;
}


/* cxr - common ca(.)r/cd(.)r routine */
static SOBJ cxr(int adr)
{
  SOBJ co = xlonearg();
  do {
    if (!consp(co)) sxae_type(co, NT_CONS);
    co = (adr & 1) ? cdr(co) : car(co);
  } while ((adr >>= 1) & 0x100);
  return co;
}

/*#| (cxxr lst) |#*/

DEFINE_INITIAL_BINDING("caar", sp_caar)
DEFINE_PROCEDURE(sp_caar)
{
  return cxr(0x300);
}

DEFINE_INITIAL_BINDING("cadr", sp_cadr)
DEFINE_PROCEDURE(sp_cadr)
{
  return cxr(0x301);
}

DEFINE_INITIAL_BINDING("cdar", sp_cdar)
DEFINE_PROCEDURE(sp_cdar)
{
  return cxr(0x302);
}

DEFINE_INITIAL_BINDING("cddr", sp_cddr)
DEFINE_PROCEDURE(sp_cddr)
{
  return cxr(0x303);
}


/*#| (cxxxr lst) |#*/

DEFINE_INITIAL_BINDING("caaar", sp_caaar)
DEFINE_PROCEDURE(sp_caaar)
{
  return cxr(0x700);
}

DEFINE_INITIAL_BINDING("caadr", sp_caadr)
DEFINE_PROCEDURE(sp_caadr)
{
  return cxr(0x701);
}

DEFINE_INITIAL_BINDING("cadar", sp_cadar)
DEFINE_PROCEDURE(sp_cadar)
{
  return cxr(0x702);
}

DEFINE_INITIAL_BINDING("caddr", sp_caddr)
DEFINE_PROCEDURE(sp_caddr)
{
  return cxr(0x703);
}

DEFINE_INITIAL_BINDING("cdaar", sp_cdaar)
DEFINE_PROCEDURE(sp_cdaar)
{
  return cxr(0x704);
}

DEFINE_INITIAL_BINDING("cdadr", sp_cdadr)
DEFINE_PROCEDURE(sp_cdadr)
{
  return cxr(0x705);
}

DEFINE_INITIAL_BINDING("cddar", sp_cddar)
DEFINE_PROCEDURE(sp_cddar)
{
  return cxr(0x706);
}

DEFINE_INITIAL_BINDING("cdddr", sp_cdddr)
DEFINE_PROCEDURE(sp_cdddr)
{
  return cxr(0x707);
}


/*#| (cxxxxr lst) |#*/

DEFINE_INITIAL_BINDING("caaaar", sp_caaaar)
DEFINE_PROCEDURE(sp_caaaar)
{
  return cxr(0xF00);
}

DEFINE_INITIAL_BINDING("caaadr", sp_caaadr)
DEFINE_PROCEDURE(sp_caaadr)
{
  return cxr(0xF01);
}

DEFINE_INITIAL_BINDING("caadar", sp_caadar)
DEFINE_PROCEDURE(sp_caadar)
{
  return cxr(0xF02);
}

DEFINE_INITIAL_BINDING("caaddr", sp_caaddr)
DEFINE_PROCEDURE(sp_caaddr)
{
  return cxr(0xF03);
}

DEFINE_INITIAL_BINDING("cadaar", sp_cadaar)
DEFINE_PROCEDURE(sp_cadaar)
{
  return cxr(0xF04);
}

DEFINE_INITIAL_BINDING("cadadr", sp_cadadr)
DEFINE_PROCEDURE(sp_cadadr)
{
  return cxr(0xF05);
}

DEFINE_INITIAL_BINDING("caddar", sp_caddar)
DEFINE_PROCEDURE(sp_caddar)
{
  return cxr(0xF06);
}

DEFINE_INITIAL_BINDING("cadddr", sp_cadddr)
DEFINE_PROCEDURE(sp_cadddr)
{
  return cxr(0xF07);
}

DEFINE_INITIAL_BINDING("cdaaar", sp_cdaaar)
DEFINE_PROCEDURE(sp_cdaaar)
{
  return cxr(0xF08);
}

DEFINE_INITIAL_BINDING("cdaadr", sp_cdaadr)
DEFINE_PROCEDURE(sp_cdaadr)
{
  return cxr(0xF09);
}

DEFINE_INITIAL_BINDING("cdadar", sp_cdadar)
DEFINE_PROCEDURE(sp_cdadar)
{
  return cxr(0xF0A);
}

DEFINE_INITIAL_BINDING("cdaddr", sp_cdaddr)
DEFINE_PROCEDURE(sp_cdaddr)
{
  return cxr(0xF0B);
}

DEFINE_INITIAL_BINDING("cddaar", sp_cddaar)
DEFINE_PROCEDURE(sp_cddaar)
{
  return cxr(0xF0C);
}

DEFINE_INITIAL_BINDING("cddadr", sp_cddadr)
DEFINE_PROCEDURE(sp_cddadr)
{
  return cxr(0xF0D);
}

DEFINE_INITIAL_BINDING("cdddar", sp_cdddar)
DEFINE_PROCEDURE(sp_cdddar)
{
  return cxr(0xF0E);
}

DEFINE_INITIAL_BINDING("cddddr", sp_cddddr)
DEFINE_PROCEDURE(sp_cddddr)
{
  return cxr(0xF0F);
}

/* weak pairs */

/*#| (bwp-object? obj) |#*/
DEFINE_INITIAL_BINDING("bwp-object?", sp_bwpp)
DEFINE_PROCEDURE(sp_bwpp)
{
  SOBJ arg = xlonearg();
  return cvbool(bwpp(arg));
}

/*#| (weak-cons car cdr) |#*/
DEFINE_INITIAL_BINDING("weak-cons", sp_weakcons)
DEFINE_PROCEDURE(sp_weakcons)
{
  SOBJ carval,cdrval;
  carval = xlgetarg();
  cdrval = xlgetarg();
  xllastarg();
  return consa(carval, cdrval, NT_WEAKPAIR);
}

