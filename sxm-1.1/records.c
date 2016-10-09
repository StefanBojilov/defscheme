/* records.c - record procedures */

#include "sxm.h"
#include "define.h"
#include "sxhtab.h"
#include "sxintern.h"

/* meta-rtd object */
DEFINE_INITIAL_BINDING("#rtd", sd_rtdrtd)
DEFINE_DATUM_INIT(sd_rtdrtd)
{
  SOBJ rtdrtd, name, flist;
  name = cvstring(T("rtd"));
  gcLock(name);
  flist = cons(sxSymEnter(T("fields")), so_nil);
  flist = cons(sxSymEnter(T("name")), flist);
  gcLock(flist);
  rtdrtd = newrecord(3);
  setrelement(rtdrtd, 0, rtdrtd);
  setrelement(rtdrtd, 1, name);
  setrelement(rtdrtd, 2, flist);
  gcUnlock(2);
  return rtdrtd;
}

/*#| (%make-record rtd arg ...) |#*/
DEFINE_INITIAL_BINDING("%make-record", sp_imakerecord)
DEFINE_PROCEDURE(sp_imakerecord)
{
  /* get the rtd and number of fields */
  SOBJ rtd = xlgarecord();
  if (getrsize(rtd) != 3 || getrelement(rtd, 0) != sd_rtdrtd) {
    return sxErr(T("invalid record type"), rtd);
  } else { /* create and initialize the record */
    size_t rlen = length(getrelement(rtd, 2)) + 1;
    size_t i = 0; SOBJ rec;
    gcLock(rtd);
    rec = newrecord((sv_size_t)rlen);
    setrelement(rec, i++, rtd);
    while (i < rlen) {
      SOBJ arg = xlgetarg();
      setrelement(rec, i++, arg);
    } 
    xllastarg();
    gcUnlock(1);
    return rec;
  }
}

/*#| (record? obj [rtd]) |#*/
DEFINE_INITIAL_BINDING("record?", sp_recordp)
DEFINE_PROCEDURE(sp_recordp)
{
  SOBJ arg = xlgetarg();
  if (moreargs()) {
    SOBJ rtd = xlgarecord();
    xllastarg();
    return cvbool(recordp(arg) && getrelement(arg, 0) == rtd);
  } else {
    xllastarg();
    return cvbool(recordp(arg));
  }
}

/*#| (record-type-descriptor rec) |#*/
DEFINE_INITIAL_BINDING("record-type-descriptor", sp_rtd)
DEFINE_PROCEDURE(sp_rtd)
{
  SOBJ rec = xlgarecord();
  xllastarg();
  return getrelement(rec, 0);
}

/*#| (%record-ref rec k) |#*/
DEFINE_INITIAL_BINDING("%record-ref", sp_recref)
DEFINE_PROCEDURE(sp_recref)
{
  SOBJ rec = xlgarecord();
  SOBJ index = xlgafixnum();
  FIXTYPE i = getfixnum(index) + 1;
  xllastarg();
  if (i < 0 || i >= (FIXTYPE)getrsize(rec))
    sxae_range(index, cvsfixnum(getvsize(rec)));
  return getrelement(rec, i);
}

/*#| (%record-set! rec k obj) |#*/
DEFINE_INITIAL_BINDING("%record-set!", sp_recset)
DEFINE_PROCEDURE(sp_recset)
{
  SOBJ rec = xlgarecord();
  SOBJ index = xlgafixnum();
  FIXTYPE i = getfixnum(index) + 1;
  SOBJ val = xlgetarg();
  xllastarg();
  if (i < 0 || i >= (FIXTYPE)getrsize(rec))
    sxae_range(index, cvsfixnum(getvsize(rec)));
  setrelement(rec, i, val);
  return so_void;
}

/* hash-table mapping names to registered rtds */
DEFINE_DATUM_INIT(sv_recreaders_ht)
{
  return sxMakeHashTable(HTT_STRING, 51, so_false);
}

/*#| (record-reader name [rtd]) |#*/
DEFINE_INITIAL_BINDING("record-reader", sp_recreader)
DEFINE_PROCEDURE(sp_recreader)
{
  SOBJ name = xlgastring();
  if (moreargs()) { /* update */
    SOBJ rtd = xlgetarg();
    xllastarg();
    if (falsep(rtd)) {
      sxHashTableRemove(sv_recreaders_ht, name);
    } else if (!recordp(rtd) || getrsize(rtd) != 3 
               || getrelement(rtd, 0) != sd_rtdrtd) {
      sxErr(T("invalid record type"), rtd);
    } else {
      gcLock(name); gcLock(rtd);
      sxHashTablePut(sv_recreaders_ht, name, rtd, TRUE);
      gcUnlock(2);
    }
    return so_void;
  } else { /* lookup */
    xllastarg();
    return sxHashTableGet(sv_recreaders_ht, name, NULL);
  }
}
