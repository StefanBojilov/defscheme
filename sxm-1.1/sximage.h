/* sximage.h - image save/restore procedures  */

#ifndef __SXIMAGE_H
#define __SXIMAGE_H

#include "sxm.h"

extern bool_t sxImgSave(const tchar_t* fname, 
                        SOBJ thunk, bool_t strip,
                        const tchar_t* header); /* #! line for UNIX */

extern SOBJ sxImgRestore(const tchar_t* fname);

#endif /* ndef __SXIMAGE_H */
