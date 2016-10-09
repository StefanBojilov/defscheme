/* chqueue.c - character queue adt */

#include "sxm.h"
#include "chqueue.h"

/* opaque cq_tag structure */
struct cq_tag {
  tint_t cq_hold;
  size_t cq_bsize;
  size_t cq_level;
  size_t cq_get;
  short  cq_inc;
  tchar_t *cq_str;
}; /* typedef'd to tchar_queue_t */

#define INC_STATIC  (-1)
#define CQ_GRANULAR_SIZE(x) (((x)+(CQ_GRANULARITY-1))&~(size_t)(CQ_GRANULARITY-1))
#define CQ_GRANULARITY      16

tchar_queue_t *cq_alloc(size_t cqlen, const tchar_t *str, size_t strsize)
{
  tchar_queue_t *cqp;

  cqp = (tchar_queue_t *)malloc(sizeof(*cqp));
  if (cqp == NULL)
    return NULL;

  cqlen = CQ_GRANULAR_SIZE(cqlen);
  cqp->cq_get = 0;
  cqp->cq_hold = TEOF;
  if (cqlen == 0) {
    cqp->cq_str = (tchar_t*)str; /* guaranteed to remain CONST */
    cqp->cq_bsize = cqp->cq_level = (str == NULL) ? 0 : strsize;
    cqp->cq_inc = INC_STATIC;
    return cqp;
  }
  cqp->cq_inc = CQ_INCREMENT;
  cqp->cq_bsize = cqlen;
  cqp->cq_str = (tchar_t *)malloc(cqlen * sizeof(tchar_t));
  if (cqp->cq_str == NULL) {
    free(cqp);
    return NULL;
  }
  if (str != NULL) {
    if (strsize > cqlen)
      strsize = cqlen;
    tmemcpy(cqp->cq_str, str, strsize);
    cqp->cq_level = strsize;
  } else
    cqp->cq_level = 0;
  return cqp;
}

size_t cq_realloc(tchar_queue_t * cqp, size_t cqlen)
{
  tchar_t *str;
  tint_t hold;

  if (cqlen <= cqp->cq_bsize)
    return cqp->cq_bsize;
  cqlen = CQ_GRANULAR_SIZE(cqlen);
  str = (tchar_t*)malloc(cqlen * sizeof(tchar_t));
  if (str == NULL)
    return cqp->cq_bsize;
  hold = cqp->cq_hold;
  cqp->cq_hold = TEOF;
  cq_gets(cqp, str);
  free(cqp->cq_str);
  cqp->cq_str = str;
  cqp->cq_bsize = cqlen;
  cqp->cq_hold = hold;
  cqp->cq_get = 0;
  return cqlen;
}


tchar_queue_t *cq_free(tchar_queue_t * cqp)
{
  if (cqp->cq_str != NULL && cqp->cq_inc != INC_STATIC)
    free(cqp->cq_str);
  free(cqp);
  return NULL;
}


tint_t cq_getc(tchar_queue_t * cqp)
{
  tint_t c;

  if ((c = cqp->cq_hold) != TEOF) {
    cqp->cq_hold = TEOF;
    return c;
  } else if (cqp->cq_level--) {
    if (cqp->cq_get >= cqp->cq_bsize)
      cqp->cq_get = 0;
    return cqp->cq_str[cqp->cq_get++];
  } else {
    cqp->cq_level = 0;
    return TEOF;
  }
}

tint_t cq_ungetc(tchar_queue_t * cqp, tint_t c)
{
  return cqp->cq_hold = c;
}

tint_t cq_putc(tchar_queue_t * cqp, tchar_t c)
{
  size_t _bsize = cqp->cq_bsize;

  if (_bsize > cqp->cq_level ||
      _bsize < cq_realloc(cqp, _bsize + cqp->cq_inc)) {
    /* after realloc all cq_xxx fields are changed!! */
    size_t p = cqp->cq_level++ + cqp->cq_get;

    if (p >= cqp->cq_bsize)
      p -= cqp->cq_bsize;
    cqp->cq_str[p] = c;
  }
  return c;
}

size_t cq_size(const tchar_queue_t * cqp)
{
  return cqp->cq_bsize;
}

size_t cq_count(const tchar_queue_t * cqp)
{
  return cqp->cq_level + (cqp->cq_hold == TEOF ? 0 : 1);
}

void cq_empty(tchar_queue_t * cqp)
{
  cqp->cq_level = cqp->cq_get = 0;
  cqp->cq_hold = TEOF;
}

void cq_gets(tchar_queue_t * cqp, tchar_t *cp)
{
  size_t t;

  if (cqp->cq_hold != TEOF)
    *cp++ = cqp->cq_hold;
  if ((t = cqp->cq_level) == 0)
    return;
  if (cqp->cq_get + t >= cqp->cq_bsize)
    t = cqp->cq_bsize - cqp->cq_get;
  if (t)
    tmemcpy(cp, cqp->cq_str + cqp->cq_get, t);
  if (cqp->cq_level != t)
    tmemcpy(cp + t, cqp->cq_str, cqp->cq_level - t);
}

