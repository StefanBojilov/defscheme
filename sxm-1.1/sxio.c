/* sxio.c - I/O support functions */

#include "sxm.h"
#include "vmop.h"
#include "os.h"
#include "io.h"
#include "sxio.h"
#include "sxintern.h"
#include "sxlocale.h"
#include "sxwrite.h"
#include "sxread.h"
#include "sximage.h"
#include "define.h"
#include "extern.h"

#ifdef CPPTABLES /* C++ : tables are collected by static constructors */
#ifndef __cplusplus
#error this is C++!!
#endif
#endif /* def CPPTABLES */


/********** Port class table for init/save/restore ************/

#ifdef CPPTABLES /* C++ : tables are collected by static constructors */

_portclass_link* _portclass_link::root = NULL;

static bool_t enum_port_table(OFFTYPE* rpos, PORTVPTR* pvp)
{
  _portclass_link** lpp = (_portclass_link**)*rpos;
  if (lpp == 0) lpp = &_portclass_link::root;
  if (*lpp == NULL) return FALSE;
  *rpos = (OFFTYPE)(&((*lpp)->m_next));
  *pvp = (*lpp)->m_vp;
  return TRUE;
}

#else /* plain C: tables are collected by mktab utility */

/* port class declarations */
#define PORT_CLASS(name,type) extern PORTVTAB name[];
#include "all.pct"
#undef PORT_CLASS

/* port class table */
#define PORT_CLASS(name,type) name,
static PORTVPTR _port_tab[] = {
#include "all.pct"
};
#undef PORT_CLASS

#define _PORT_TAB_SIZE (sizeof(_port_tab)/sizeof(_port_tab[0]))

static bool_t enum_port_table(OFFTYPE* rpos, PORTVPTR* pvp)
{
  int idx = (int)*rpos;
  if (idx < 0 || idx >= _PORT_TAB_SIZE) return FALSE;
  *rpos = (OFFTYPE)(idx + 1);
  *pvp = _port_tab[idx];
  return TRUE;
}

#endif /* def CPPTABLES */

/*************** port table setup *********************/

/* constant ports used by the system (set up in install_port_tab) */
DEFINE_VARIABLE(sv_conin)
DEFINE_VARIABLE(sv_conout)
DEFINE_VARIABLE(sv_stdin)
DEFINE_VARIABLE(sv_stdout)
DEFINE_VARIABLE(sv_stderr)
/* fluid port vars */
EXTERN_VARIABLE(sv_curin)
EXTERN_VARIABLE(sv_curout)

/* called from sxSetup */
void setup_port_tab(void)
{
  PORTVPTR vp; OFFTYPE pos; int idx;
  for (idx = 0, pos = 0; enum_port_table(&pos, &vp); idx++)
    vp->v_id = idx;
}

/* install_port_tab(void) - open standard ports
 * (called from sxImgInit _before_ standard vars init)
 */
void install_port_tab(void)
{
  PORTVPTR vp; PORTDPTR dp;

  /* open std ports */
  if (std_open(0, &vp, &dp)) sv_stdin  = cvport(vp, dp); else goto panic;
  if (std_open(1, &vp, &dp)) sv_stdout = cvport(vp, dp); else goto panic;
  if (std_open(2, &vp, &dp)) sv_stderr = cvport(vp, dp); else goto panic;

  /* if -t option is given and tty can be opened, use it as "console" */
  if (sx_use_tty && tty_open(&vp, &dp)) sv_conin = sv_conout = cvport(vp, dp);
  /* otherwise, use stdin and stdout */
  else { sv_conin = sv_stdin; sv_conout = sv_stdout; }
  return;

panic:
  ospanic(T("Failed to open one of standard ports"));
}


/* port class <-> index converters */

short sxPortClassToPcid(PORTVPTR vp)
{
  if (vp != NULL) return vp->v_id;
  else return 0;
}

PORTVPTR sxPcidToPortClass(short pcid)
{
  PORTVPTR vp; OFFTYPE pos;

  for (pos = 0; enum_port_table(&pos, &vp);)
    if (vp->v_id == pcid) return vp;

  ospanic(T("corrupted image file"));
  never_returns(NULL);
}



/******************* standard ports *********************/

/* fluid port variables */
EXTERN_VARIABLE(sv_curin)
EXTERN_VARIABLE(sv_curout)


/***** default implementations for common port methods *****/

void sxp_print(PORTDPTR dp, SOBJ ovp)
{
  tchar_t buf[40];
  stprintf(buf, T("port @%lx"), (unsigned long)dp);
  sxWriteString(buf, ovp);
}

int sxp_unsupp(PORTDPTR dp)
{
  sxFail(T("Operation not supported"));
  never_returns(EOF);
}

void sxp_mark(PORTDPTR dp)
{
  /* nothing to mark by default */
}

void sxp_save(PORTDPTR dp, SOBJ stream)
{
  /* the average port is not serializable : nothing to write */
}

bool_t sxp_restore(PORTDPTR* pdp, SOBJ stream)
{
  /* the average port is not serializable: nothing to read; 
     restore as closed */
  return FALSE;
}

bool_t sxp_listen(PORTDPTR dp)
{
  /* the average port does not block (?) */
  return TRUE;
}

void sxp_noop(PORTDPTR dp)
{
  /* do nothing, return nothing; good for dummy clear/flush */
}


/*********************************************************/

/* sxPortSave - save port nodes to image stream */
void sxPortSave(CSOBJ port, SOBJ stream)
{
  /* all port images are prefixed with the port class #: */
  short n = sxPortClassToPcid(getvp(port));
  sxWriteWord((short)n, stream);
  /* now vp is saved; ask port class to save dp */
  getvp(port)->v_save(getdp((SOBJ)port), stream);
}

/* sxPortRestore - restore port nodes from image stream */
void sxPortRestore(SOBJ port, SOBJ stream)
{
  /* all port images are prefixed with the port class #: */
  short n = sxReadWord(stream);
  PORTVPTR vp = sxPcidToPortClass(n);
  setvp(port, vp);
  /* now vp is restored; ask port class to restore dp */
  if (!vp->v_restore(&getdp((SOBJ)port), stream)) {
    /* restore failed: change to closed port */
    PORTVPTR vp; PORTDPTR dp;
    clo_open(&vp, &dp);
    setvp(port, vp); 
    setdp(port, dp);
  }
}

void sxPortClose(SOBJ port)
{
  if (portp(port)) {
    PORTVPTR vp; PORTDPTR dp;
    getvp(port)->v_close(getdp(port));
    clo_open(&vp, &dp);
    setvp(port, vp);
    setdp(port, dp);
  }
}
