/* vmop.h - VM opcode definitions */

#ifndef __VMOP_H
#define __VMOP_H

/* OP_xxx constants */
#define OPSIG(sig) /* ignore signatures */
#define OPCODE(name, num, sig) OP_##name = num,
enum opcode_t {
#include "vmop.t"
OP_MAX
};
#undef OPSIG
#undef OPCODE

#endif /* ndef __VMOP_H */

