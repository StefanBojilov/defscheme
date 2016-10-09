/* define.h - entities definition */

#ifndef __DEFINE_H
#define __DEFINE_H

#include "sxm.h"

#ifdef CPPTABLES /* C++ : tables are collected by static constructors */

#ifndef __cplusplus
#error this is C++!!
#endif

struct _subr_link {
  SOBJ* m_vp; vm_subr_t m_sp; 
  const tchar_t* m_id; _subr_link* m_next;
  static _subr_link* root;
  _subr_link(SOBJ* vp, vm_subr_t sp, const tchar_t* id) {
     m_vp = vp; m_sp = sp; m_id = id;
     m_next = root; root = this;
  }
};

#define DEFINE_PROCEDURE(name)\
        SOBJ name;\
        SOBJ p_##name(void);\
        static _subr_link _pl##name(&name, p_##name, T(#name));\
        SOBJ p_##name(void)

#define DEFINE_PROCEDURE_VARACCESS(name,var,type)\
        SOBJ name;\
        SOBJ p_##name(void);\
        static _subr_link _pl##name(&name, p_##name, T(#name));\
        SOBJ p_##name(void) {\
          if (moreargs()) { SOBJ newval = xlgetarg();\
            if (!sx_typep(newval, type)) sxae_type(newval, type);\
            xllastarg(); var = newval; return so_void;\
          } else { xllastarg(); return var; }\
        }

struct _csubr_link {
  SOBJ* m_vp; vm_csubr_t m_sp; 
  const tchar_t* m_id; _csubr_link* m_next;
  static _csubr_link* root;
  _csubr_link(SOBJ* vp, vm_csubr_t sp, const tchar_t* id) {
     m_vp = vp; m_sp = sp; m_id = id;
     m_next = root; root = this;
  }
};

#define DEFINE_CONTINUATION(name, a)\
        SOBJ name;\
        SOBJ cp_##name(SOBJ);\
        static _csubr_link _cl##name(&name, cp_##name, T(#name));\
        SOBJ cp_##name(SOBJ a)

struct _data_link {
  SOBJ* m_vp; vm_subr_t m_sp; _data_link* m_next;
  static _data_link* root;
  _data_link(SOBJ* vp, vm_subr_t sp) {
     m_vp = vp; m_sp = sp; m_next = root; root = this;
  }
};

#define DEFINE_DATUM_INIT(name)\
        SOBJ name;\
        SOBJ ip_##name(void);\
        static _data_link _dl##name(&name, ip_##name);\
        SOBJ ip_##name(void)

#define DEFINE_DATUM_SYMBOL(name, str)\
        SOBJ name;\
        SOBJ ip_##name(void);\
        static _data_link _dl##name(&name, ip_##name);\
        SOBJ ip_##name(void)\
        { return sxSymEnter(T(str)); }

#define DEFINE_DATUM_KEYWORD(name, str)\
        SOBJ name;\
        SOBJ ip_##name(void);\
        static _data_link _dl##name(&name, ip_##name);\
        SOBJ ip_##name(void)\
        { return sxKeyEnter(T(str)); }

#define DEFINE_DATUM_STRING(name, str)\
        SOBJ name;\
        SOBJ ip_##name(void);\
        static _data_link _dl##name(&name, ip_##name);\
        SOBJ ip_##name(void)\
        { return cvstring(T(str)); }

struct _ibinding_link {
  SOBJ* m_vp; const tchar_t* m_sp; _ibinding_link* m_next;
  static _ibinding_link* root;
  _ibinding_link(SOBJ* vp, const tchar_t* sp) {
     m_vp = vp; m_sp = sp; m_next = root; root = this;
  }
};

// this hack allows us to generate almost-unique names for links
#define __MKIBLNAME(n, l) _ibl ## n ## l
#define ___MKIBLNAME(n, l) __MKIBLNAME(n, l)
#define DEFINE_INITIAL_BINDING(str, name)\
        extern SOBJ name;\
        static _ibinding_link ___MKIBLNAME(name, __LINE__) (&name, T(str));

struct _var_link {
  SOBJ* m_vp; vm_subr_t m_sp; _var_link* m_next;
  static _var_link* root;
  _var_link(SOBJ* vp, vm_subr_t sp) {
     m_vp = vp; m_sp = sp; m_next = root; root = this;
  }
};


#define DEFINE_VARIABLE(name)\
        SOBJ name;\
        static _var_link _vl##name(&name, NULL);

#define DEFINE_VARIABLE_INIT(name)\
        SOBJ name;\
        SOBJ rp_##name(void);\
        static _var_link _vl##name(&name, rp_##name);\
        SOBJ rp_##name(void)

#define DEFINE_VARIABLE_VARINIT(name, val)\
        SOBJ name;\
        SOBJ rp_##name(void);\
        static _var_link _vl##name(&name, rp_##name);\
        SOBJ rp_##name(void)\
        { return val; }

struct _portclass_link {
  PORTVPTR m_vp; _portclass_link* m_next;
  static _portclass_link* root;
  _portclass_link(PORTVPTR vp) {
     m_vp = vp; m_next = root; root = this;
  }
};

#define DEFINE_PORT_CLASS(name, type)\
        extern PORTVTAB name[];\
        static _portclass_link _pcl##name(name);\
        /*static*/ PORTVTAB name[1] = {{ type, 0, 

#define ENDDEF_PORT_CLASS }};

#else /* plain C: tables are collected by mktab utility */

#define DEFINE_INITIAL_BINDING(str, name)

#define DEFINE_PROCEDURE(name)\
        SOBJ name;\
        SOBJ p_##name(void)

#define DEFINE_PROCEDURE_VARACCESS(name,var,type)\
        SOBJ name;\
        SOBJ p_##name(void) {\
          if (moreargs()) { SOBJ newval = xlgetarg();\
            if (!sx_typep(newval, type)) sxae_type(newval, type);\
            xllastarg(); var = newval; return so_void;\
          } else { xllastarg(); return var; }\
        }

#define DEFINE_CONTINUATION(name, a)\
        SOBJ name;\
        SOBJ cp_##name(SOBJ a)

#define DEFINE_DATUM_INIT(name)\
        SOBJ name;\
        SOBJ ip_##name(void)

#define DEFINE_DATUM_SYMBOL(name, str)\
        SOBJ name;\
        SOBJ ip_##name(void)\
        { return sxSymEnter(T(str)); }

#define DEFINE_DATUM_KEYWORD(name, str)\
        SOBJ name;\
        SOBJ ip_##name(void)\
        { return sxKeyEnter(T(str)); }

#define DEFINE_DATUM_STRING(name, str)\
        SOBJ name;\
        SOBJ ip_##name(void)\
        { return cvstring(T(str)); }

#define DEFINE_VARIABLE(name)\
        SOBJ name;

#define DEFINE_VARIABLE_INIT(name)\
        SOBJ name;\
        SOBJ rp_##name(void)

#define DEFINE_VARIABLE_VARINIT(name, val)\
        SOBJ name;\
        SOBJ rp_##name(void)\
        { return val; }


#define DEFINE_PORT_CLASS(name, type)\
        PORTVTAB name[1] = {{ type, 0, 

#define ENDDEF_PORT_CLASS }};

#endif /* def CPPTABLES ***************************************/

#endif /* ndef __DEFINE_H */
