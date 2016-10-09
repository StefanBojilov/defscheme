
;;
;; Support for selected SRFIs (0, 6, 8, 9, 11, 16)

;SRFI-0:  Feature-based conditional expansion construct
#| (cond-expand clause ...) |#
(define-syntax cond-expand
  (syntax-rules 
    (and or not else srfi-0 srfi-6 srfi-8 srfi-9 srfi-11 srfi-16)
    [(_ [else  . body]) (begin . body)]
    [(_ [(and) . body] . clauses) (begin . body)]
    [(_ [(and req1 req2 ...) . body] . clauses)
     (cond-expand
       [req1 (cond-expand [(and req2 ...) . body] . clauses)]
       . clauses)]
    [(_ [(or) . body] . clauses) (cond-expand . clauses)]
    [(_ [(or req . reqs) . body] . clauses)
     (cond-expand
       [req . body]
       [else (cond-expand [(or . reqs) . body] . clauses)])]
    [(_ [(not req) . body] . clauses)
     (cond-expand
       [req (cond-expand . clauses)]
       [else . body])]
    [(_ (srfi-0 . body) . clauses) (begin . body)] ;cond-expand
    [(_ (srfi-6 . body) . clauses) (begin . body)] ;string ports
    [(_ (srfi-8 . body) . clauses) (begin . body)] ;receive
    [(_ (srfi-9 . body) . clauses) (begin . body)] ;define-record-type
    [(_ (srfi-11 . body) . clauses) (begin . body)] ;let-values
    [(_ (srfi-16 . body) . clauses) (begin . body)] ;case-lambda
    [(_ (srfi-? . body) . clauses) (cond-expand  . clauses)]))

;SRFI-6: Basic String Ports
;built-in

;SRFI-8: receive: Binding to multiple values
#| (receive formals mvexp exp ...) |#
(define-syntax receive
  (syntax-rules ()
    [(_ formals exp . body)
     (call-with-values (lambda () exp)
       (lambda formals . body))]))

;SRFI-9: Defining Record Types
#| (define-record-type rtname (consname fld ...) predname (fld accname [modname]) ...) |#
(define-syntax define-record-type
  (lambda (orig-x)
    (syntax-case orig-x ()
      [(_ rtd
        (constructor constructor-tag ...)
        predicate
        (field-tag accessor . more) ...)
       (andmap identifier?
        #'(rtd constructor constructor-tag ... predicate field-tag ... accessor ...))
       (with-syntax 
         ([rtd-name (symbol->string (syntax-object->datum #'rtd))]
          [(noncons-tag ...)
           (let loop ([lst #'(field-tag ...)] [res '()])
             (cond [(null? lst) res]
                   [(ormap (lambda (i) (free-identifier=? i (car lst))) 
                           #'(constructor-tag ...)) 
                    (loop (cdr lst) res)]
                   [else (loop (cdr lst) (cons (car lst) res))]))]
          [(accessor/modifier ...)
           (map (lambda (field-spec)
                  (syntax-case field-spec ()
                    [(field-tag accessor)
                     #'(define accessor (record-field-accessor rtd 'field-tag))]
                    [(field-tag accessor modifier)
                     (identifier? #'modifier)
                     #'(begin
                         (define accessor (record-field-accessor rtd 'field-tag))
                         (define modifier (record-field-mutator rtd 'field-tag)))]
                    [_ (syntax-error orig-x)]))
                #'([field-tag accessor . more] ...))])
         #'(begin
             (define rtd
               (let ([d (make-record-type rtd-name '(field-tag ...))])
                 (record-reader rtd-name d)
                 d))
             (define constructor
               (lambda (constructor-tag ...)
                 (let ([noncons-tag '?] ...)
                   (%make-record rtd field-tag ...))))
             (define predicate
               (lambda (x)
                 (record? x rtd)))
             accessor/modifier ...))]))) 

;SRFI-11: Syntax for receiving multiple values
#| (let-values ((formals mvexp) ...) exp ...) |#
(define-syntax let-values
  (syntax-rules ()
    [(_ ((formals exp)) . body)
     (call-with-values (lambda () exp) (lambda formals . body))]
    [(_ (binding ...) . body)
     (let-values "bind" (binding ...) () (begin . body))]
    [(_ "bind" () temps exp)
     (let temps exp)]
    [(_ "bind" ((b0 e0) . bindings) temps exp)
     (let-values "mktmp" b0 e0 () bindings temps exp)]
    [(_ "mktmp" () e0 args bindings temps exp)
     (call-with-values 
       (lambda () e0)
       (lambda args (let-values "bind" bindings temps exp)))]
    [(_ "mktmp" (a . b) e0 (arg ...) bindings (tmp ...) exp)
     (let-values "mktmp" b e0 (arg ... x) bindings (tmp ... (a x)) exp)]
    [(_ "mktmp" a e0 (arg ...) bindings (tmp ...) exp)
     (call-with-values
       (lambda () e0)
       (lambda (arg ... . x)
         (let-values "bind" bindings (tmp ... (a x)) exp)))]))

#| (let*-values ((formals mvexp) ...) exp ...) |#
(define-syntax let*-values
  (syntax-rules ()
    [(_ () . body)
     (begin . body)]
    [(_ (binding . bindings) . body)
     (let-values (binding)
       (let*-values bindings . body))]))

;SRFI 16: Syntax for procedures of variable arity
;implemented in complib.ss

