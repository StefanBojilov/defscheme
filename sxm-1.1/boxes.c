/* boxes.c - box procedures */

#include "sxm.h"
#include "define.h"

/*#| (box? obj) |#*/
DEFINE_INITIAL_BINDING("box?", sp_boxp)
DEFINE_PROCEDURE(sp_boxp)
{
  SOBJ arg = xlonearg();
  return cvbool(boxp(arg));
}

/*#| (box obj [tag]) |#*/
DEFINE_INITIAL_BINDING("box", sp_box)
DEFINE_PROCEDURE(sp_box)
{
  SOBJ val = xlgetarg();
  SOBJ tag = optarg() ? nextarg() : so_nil;
  xllastarg();
  return cvbox(val,tag);
}

/*#| (unbox box) |#*/
DEFINE_INITIAL_BINDING("unbox", sp_unbox)
DEFINE_PROCEDURE(sp_unbox)
{
  SOBJ box = xlgabox();
  xllastarg();
  return getboxval(box);
}

/*#| (box-tag box) |#*/
DEFINE_INITIAL_BINDING("box-tag", sp_boxtag)
DEFINE_PROCEDURE(sp_boxtag)
{
  SOBJ box = xlgabox();
  xllastarg();
  return getboxtag(box);
}

/*#| (set-box! box obj) |#*/
DEFINE_INITIAL_BINDING("set-box!", sp_setbox)
DEFINE_PROCEDURE(sp_setbox)
{
  SOBJ box = xlgabox();
  SOBJ newval = xlgetarg();
  xllastarg();
  setboxval(box,newval);
  return so_void;
}

/*#| (set-box-tag! box tag) |#*/
DEFINE_INITIAL_BINDING("set-box-tag!", sp_setboxtag)
DEFINE_PROCEDURE(sp_setboxtag)
{
  SOBJ box = xlgabox();
  SOBJ newtag = xlgetarg();
  xllastarg();
  setboxtag(box,newtag);
  return box;
}


