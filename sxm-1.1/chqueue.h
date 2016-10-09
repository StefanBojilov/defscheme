/* chqueue.h - char queue adt */

#ifndef __CHQUEUE_H
#define __CHQUEUE_H

/* definitions from chqueue.c */

/* default grow increment on realloc during putc */
#define CQ_INCREMENT        64

typedef struct cq_tag tchar_queue_t;

tchar_queue_t* cq_alloc(size_t cqlen, const tchar_t* str, size_t strsize);
size_t         cq_realloc(tchar_queue_t* cqp, size_t newsize);
tchar_queue_t* cq_free(tchar_queue_t* cqp);
tint_t         cq_ungetc(tchar_queue_t* cqp, tint_t c);
tint_t         cq_getc(tchar_queue_t* cqp);
tint_t         cq_putc(tchar_queue_t* cqp, tchar_t c);
size_t         cq_size(const tchar_queue_t* cqp);
size_t         cq_count(const tchar_queue_t* cqp);
void           cq_empty(tchar_queue_t* cqp);
void           cq_gets(tchar_queue_t* cqp, tchar_t* cp);

#endif /* ndef __CHQUEUE_H */

