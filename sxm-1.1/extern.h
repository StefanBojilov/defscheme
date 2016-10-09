/* extern.h - entities import */

#ifndef __EXTERN_H
#define __EXTERN_H

#include "sxm.h"

#define EXTERN_PROCEDURE(name) extern SOBJ name;
#define EXTERN_CONTINUATION(name) extern SOBJ name;
#define EXTERN_DATUM(name) extern SOBJ name;
#define EXTERN_VARIABLE(name) extern SOBJ name;
#define EXTERN_PORT_CLASS(name) /*extern PORTVTAB name[];*/

#endif /* ndef __EXTERN_H */
