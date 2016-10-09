/* iottycgi.c - tty ports (Standard C version for CGIs) */

#include "../sxm.h"

/*
 *  No port classes defined: tty is not present, transcript
 *  is not supported
 */


bool_t tty_open(PORTVPTR* pvp, PORTDPTR* pdp)
{
  /* console will be attached to stdin/stdout */
  return FALSE;
}


