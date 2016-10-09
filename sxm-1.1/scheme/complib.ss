
;;
;; Chez/MIT compatibility -- esl

;;
;; I. Procedures

#| (list-copy list) |# ;Chez
(define (list-copy lst) 
  (append lst '()))

#| (list-set! list n val) |#
(define (list-set! lst n val)
  (set-car! (list-tail lst n) val))

#| (make-list size [value]) |# ;Chez
(define (make-list size #!optional value) 
  (let loop ([n size] [list '()])
    (if (positive? n)
        (loop (-1+ n) (cons value list))
        list)))
      
#| (list-head list index) |# ;MIT
(define (list-head list index)
  (let loop ([list list] [index index])
    (if (zero? index)
        '()
        (cons (car list) (loop (cdr list) (-1+ index))))))

#| (sublist list start end) |#
(define (sublist list start end)
  (list-head (list-tail list start) (- end start)))

#| (except-last-pair list) |# ;MIT
(define (except-last-pair l)
  (if (pair? l)
      (let loop ([l l])
        (if (pair? (cdr l)) (cons (car l) (loop (cdr l))) '()))
      (error 'except-last-pair "argument is not a nonempty list ~s" l)))

#| (except-last-pair! list) |# ;MIT
(define (except-last-pair! l)
  (if (pair? l)
      (if (pair? (cdr l))
          (begin
            (let loop ([l l])
              (if (pair? (cddr l)) (loop (cdr l)) (set-cdr! l '())))
            l)
          '())
      (error 'except-last-pair! "argument is not a nonempty list ~s" l)))

#| (merge! less? list1 list2) |# ;Chez
(define (merge! less? lst1 lst2) 
  (cond
    [(atom? lst1) lst2]
    [(atom? lst2) lst1]
    [(less? (car lst2) (car lst1))
     (set-cdr! lst2 (merge! less? lst1 (cdr lst2)))
     lst2]
    [else (set-cdr! lst1 (merge! less? (cdr lst1) lst2)) lst1]))

#| (split! list) |#
(define (split! lst)
  (if (atom? lst)
      lst
      (let loop ([hd lst] [tl (cdr lst)])
        (if (or (atom? tl) (atom? (cdr tl)))
            (let ([x (cdr hd)])
              (set-cdr! hd '())
              x)
            (loop (cdr hd) (cddr tl))))))

#| (sort! less? list) |# ;Chez
(define (sort! less? lst) 
  (cond
     [(null? lst) '()]
     [(null? (cdr lst)) lst]
     [else
      (let ([lst2 (split! lst)])
        (merge! less? (sort! less? lst) (sort! less? lst2)))]))

#| (sort less? list) |# ;Chez
(define (sort less? lst) 
  (sort! less? (list-copy lst)))

#| (merge less? list1 list2) |#
(define (merge less? lst1 lst2)
  (merge! less? (list-copy lst1) (list-copy lst2)))


#| (make-remove-procedure predicate) |#
(define (make-remove-procedure predicate)
  (define (proc element list)
     (cond
        [(atom? list) list]
        [(predicate (car list) element) (proc element (cdr list))]
        [else (cons (car list) (proc element (cdr list)))]))
  proc)

#| (make-remove!-procedure predicate) |#
(define (make-remove!-procedure predicate)
  (define (proc element list)
     (let loop ([first #f] [last #f] [rest list])
        (cond
          [(atom? rest)
           (if last (begin (set-cdr! last rest) first) rest)]
          [(predicate (car rest) element)
           (loop first last (cdr rest))]
          [else
           (if last
               (begin (set-cdr! last rest) (loop first rest (cdr rest)))
               (loop rest rest (cdr rest)))])))
  proc)

#| (remq element list) |# ;Chez
(define remq (make-remove-procedure eq?)) 
#| (remv element list) |# ;Chez
(define remv (make-remove-procedure eqv?)) 
#| (remove element list) |# ;Chez
(define remove (make-remove-procedure equal?)) 

;remq! remv! remove! : built-in

#| (remove-if predicate list) |#
(define (remove-if predicate list)
  (define (proc pred list)
    (cond
      [(atom? list) list]
      [(pred (car list)) (proc pred (cdr list))]
      [else (cons (car list) (proc pred (cdr list)))]))
  (proc predicate list))

#| (remove-if! predicate list) |#
(define (remove-if! predicate list)
  (let loop ([first #f] [last #f] [rest list])
    (cond
      [(atom? rest)
       (if last (begin (set-cdr! last rest) first) rest)]
      [(predicate (car rest))
       (loop first last (cdr rest))]
      [else
       (if last
           (begin (set-cdr! last rest) (loop first rest (cdr rest)))
           (loop rest rest (cdr rest)))])))


#| (make-subst-procedure predicate) |#
(define (make-subst-procedure predicate)
  (define (proc new old tree)
     (if (predicate tree old)
         new
         (if (pair? tree)
             (cons (proc new old (car tree))
                   (proc new old (cdr tree)))
             tree)))
  proc)

#| (make-subst!-procedure predicate new-value) |#
(define (make-subst!-procedure predicate new-value)
  (define (proc new old tree)
     (if (predicate tree old)
         new
         (if (pair? tree)
             (begin
               (set-car! tree (proc new old (car tree)))
               (set-cdr! tree (proc new old (cdr tree)))
               tree)
             tree)))
  proc)

#| (substq new old tree) |# ;Chez
(define substq (make-subst-procedure eq?))
#| (substv new old tree) |# ;Chez
(define substv (make-subst-procedure eqv?))
#| (subst new old tree) |# ;Chez
(define subst (make-subst-procedure equal?))

;substq!, substv!, subst! : built-in

#| (subst-if predicate new tree) |#
(define (subst-if predicate new tree)
  (let loop ([pred predicate] [new new] [tree tree])
    (if (pred tree)
        new
        (if (pair? tree)
            (cons (loop pred new (car tree))
                  (loop pred new (cdr tree)))
            tree))))

#| (subst-if! predicate new tree) |#
(define (subst-if! predicate new tree)
  (let loop ([pred predicate] [new new] [tree tree])
    (if (pred tree)
        new
        (if (pair? tree)
            (begin
              (set-car! tree (loop pred new (car tree)))
              (set-cdr! tree (loop pred new (cdr tree)))
              tree)
            tree))))


#| (append! list ...) |# ;Chez
(define (append! . lists) 
  (if (null? lists)
      '()
      (let loop ([head (car lists)] [tail (cdr lists)])
        (cond 
          [(null? tail) head]
          [(null? head) (loop (car tail) (cdr tail))]
          [(pair? head)
           (set-cdr! (last-pair head) (loop (car tail) (cdr tail)))
           head]
          [else (error 'append! "argument is not a list ~s" head)]))))

;; property list procedures
(define getprop)
(define putprop)
(define remprop)
(define property-list)

(let ([prop-table (make-hash-table eq? #f 30)])

  #| (getprop symbol eqkey [default]) |# ;Chez
  (set! getprop
    (lambda (sym key #!optional default) 
      (cond [(hash-table-get prop-table sym) =>
             (lambda (alist)
               (cond [(assq key alist) => cdr]
                     [(default-object? default) #f]
                     [else default]))]
            [(default-object? default) #f]
            [else default])))

  #| (putprop symbol eqkey val) |# ;Chez
  (set! putprop 
    (lambda (sym key val) 
      (cond [(hash-table-get prop-table sym) =>
             (lambda (alist)
               (cond [(assq key alist) =>
                      (lambda (pair) (set-cdr! pair val))]
                     [else 
                      (hash-table-put! prop-table sym 
                         (cons (cons key val) alist))]))]
            [else
             (hash-table-put! prop-table sym 
                (list (cons key val)))])))

  #| (remprop symbol eqkey) |# ;Chez
  (set! remprop 
    (lambda (sym key) 
      (cond [(hash-table-get prop-table sym) =>
             (lambda (alist)
               (cond [(assq key alist) =>
                      (lambda (pair) 
                        (hash-table-put! prop-table sym 
                           (remq! pair alist)))]))])))

  #| (property-list symbol) |# ;Chez
  (set! property-list
    (lambda (sym) 
      (cond [(hash-table-get prop-table sym) =>
             (lambda (alist)
               (let loop ([alist alist] [plist '()])
                 (if (null? alist) 
                     (reverse! plist)
                     (loop (cdr alist) 
                       (list* (cdar alist) (caar alist) plist)))))]
            [else '()])))
)

#| (add1 n) |# ;Chez
(define add1 1+) 
#| (sub1 n) |# ;Chez
(define sub1 -1+) 

;; Chez records

#| (make-record-type name fields) |# ;Chez
(define (make-record-type name fields)
  (%make-record \#rtd name fields))

#| (record-type-descriptor? obj) |# ;Chez
(define (record-type-descriptor? obj)
  (record? obj \#rtd))

#| (record-type-name rtd) |# ;Chez
(define (record-type-name rtd)
  (if (record-type-descriptor? rtd)
      (%record-ref rtd 0)
      (error 'record-type-name 
        "~s is not a record type descriptor" rtd)))

#| (record-type-field-names rtd) |# ;Chez
(define (record-type-field-names rtd)
  (if (record-type-descriptor? rtd)
      (%record-ref rtd 1)
      (error 'record-type-field-names 
        "~s is not a record type descriptor" rtd)))

#| (record-constructor rtd) |# ;Chez
(define (record-constructor rtd)
  (case (length (record-type-field-names rtd))
    [(0)  (lambda () (%make-record rtd))]
    [(1)  (lambda (a) (%make-record rtd a))]
    [(2)  (lambda (a b) (%make-record rtd a b))]
    [(3)  (lambda (a b c) (%make-record rtd a b c))]
    [(4)  (lambda (a b c d) (%make-record rtd a b c d))]
    [(5)  (lambda (a b c d e) (%make-record rtd a b c d e))]
    [else (lambda args (apply %make-record rtd args))]))

#| (record-predicate rtd) |# ;Chez
(define (record-predicate rtd)
  (lambda (obj) (record? obj rtd)))

#| (record-field-accessor rtd field) |# ;Chez
(define (record-field-accessor rtd field)
  (let ([i (posq field (record-type-field-names rtd))])
    (if i 
        (lambda (rec) (%record-ref rec i))
        (error 'record-field-accessor 
          "unrecognized field name ~s" field))))

#| (record-field-accessible? rtd field) |# ;Chez
(define (record-field-accessible? rtd field)
  (let ([i (posq field (record-type-field-names rtd))])
    (if i
        #t ;all fields are accessible in SXM
        (error 'record-field-accessible? 
          "unrecognized field name ~s" field)))) 

#| (record-field-mutator rtd field) |# ;Chez
(define (record-field-mutator rtd field)
  (let ([i (posq field (record-type-field-names rtd))])
    (if i
        (lambda (rec val) (%record-set! rec i val))
        (error 'record-field-mutator 
          "unrecognized field name ~s" field))))

#| (record-field-mutable? rtd field) |# ;Chez
(define (record-field-mutable? rtd field)
  (let ([i (posq field (record-type-field-names rtd))])
    (if i
        #t ;all fields are mutable in SXM
        (error 'record-field-mutable? 
          "unrecognized field name ~s" field))))


;; i/o

#| (clear-input-port [iport]) |# ;Chez
(define clear-input-port clear-input) 
#| (clear-output-port [oport]) |# ;Chez : no-op in sxm?
(define clear-output-port clear-output) 
#| (flush-output-port [oport]) |# ;Chez
(define flush-output-port force-output) 

#| (block-read port string count) |# ;Chez
(define (block-read port string count) 
  (let ([nread (read-substring! string 0 count port)])
    (if (zero? nread) 
        (if (zero? count) 0 (read-char port)) ;read-char must return eof!
        nread)))

#| (block-write port string count) |# ;Chez
(define (block-write port string count) 
  (write-substring string 0 count port))

#| (sprintf format arg ...) |#
(define (sprintf format . objects)
  (let ([port (open-output-string)])
    (apply fprintf (cons port (cons format objects)))
    (get-output-string port)))

#| (format [port] format arg ...) |# ;Chez & others
(define (format p/f . args)
  (cond [(string? p/f) (apply sprintf p/f args)]
        [(port? p/f) (apply fprintf p/f args)]
        [else (error 'format "argument is not a port or format string ~s" p/f)]))

#| (procedure-parameter-filter where) |#
(define (procedure-parameter-filter where)
  (lambda (x)
    (if (procedure? x)
        x
        (error where "parameter value ~s is not a procedure" x))))

;;
;; II. Syntax

#| (case-lambda (formals exp ...) ...) |#
;; chez/srfi-16 -- implemented via sxm's #!optional
(define-syntax case-lambda 
   (lambda (x)
      (define (generate-n-temporaries n lst)
         (if (<= n 0) 
             lst 
             (generate-n-temporaries 
                (- n 1) (cons (car (generate-temporaries '(foo))) lst))))
      (define (first-n-ids lst n res)
         (if (or (<= n 0) (null? lst)) 
             (reverse! res)
             (first-n-ids (cdr lst) (- n 1) (cons (car lst) res))))
      (define (process formals-list bodies-list)
         (let loop ([flist formals-list] 
                    [minreq 1000] [maxreq 0] [anyrest? #f] [req&rest?-list '()])
            (if (null? flist)
                (if anyrest?
                    (process/rest formals-list (reverse! req&rest?-list) bodies-list)
                    (process/optionals minreq maxreq formals-list (reverse! req&rest?-list) bodies-list))
                (let loop1 ([formals (car flist)] [req 0])
                   (syntax-case formals ()
                      [() 
                       (loop (cdr flist) (min req minreq) (max req maxreq) anyrest?
                             (cons (cons req #f) req&rest?-list))] 
                      [i (identifier? #'i) 
                       (loop (cdr flist) (min req minreq) (max req maxreq) #t
                             (cons (cons req #t) req&rest?-list))]
                      [(i . more) (identifier? #'i)
                       (loop1 #'more (+ req 1))])))))
      (define (process/rest formals-list req&rest?-list bodies-list)
         (let ([restid (car (generate-n-temporaries 1 '()))]
               [rlenid (car (generate-n-temporaries 1 '()))])
            (with-syntax ([r restid] [rl rlenid])
               (with-syntax 
                  ([llist #'(#!rest r)]
                   [(formals ...) formals-list]
                   [(body ...) bodies-list]
                   [(test ...) (map (lambda (req&rest?)
                                       (gen-test/rest 
                                          (car req&rest?) (cdr req&rest?) rlenid))
                                    req&rest?-list)])
                #'(lambda llist
                     (let ([rl (length r)])
                        (cond [test (apply (lambda formals . body) r)] ...
                              [else (error 'case-lambda 
                                       "unexpected number of arguments")])))))))
      (define (gen-test/rest req rest? rlenid)
         (if rest?
             ;; there should be req or more args
             (with-syntax ([q req] [rl rlenid])
                #'(>= rl q))
             ;; there should be exactly req args
             (with-syntax ([q req] [rl rlenid])
                #'(= rl q))))
      (define (process/optionals minreq maxreq formals-list req&rest?-list bodies-list)
         (let* ([reqids (generate-n-temporaries minreq '())]
                [optids (generate-n-temporaries (- maxreq minreq) '())]
                [ids (append reqids optids)])
            (with-syntax ([(r ...) reqids] [(o ...) optids])
               (with-syntax 
                  ([llist (if (null? optids) #'(r ...) #'(r ... #!optional o ...))]
                   [(formals ...) formals-list]
                   [(body ...) bodies-list]
                   [(test ...) (map (lambda (req&rest?)
                                       (gen-test/optionals 
                                          minreq maxreq (car req&rest?) ids))
                                    req&rest?-list)]
                   [(args ...) (map (lambda (req&rest?)
                                       (first-n-ids ids (car req&rest?) '()))
                                    req&rest?-list)])
                #'(lambda llist
                     (cond [test ((lambda formals . body) . args)] ...
                           [else (error 'case-lambda 
                                   "unexpected number of arguments")]))))))
      (define (gen-test/optionals minreq maxreq req ids) 
         ;=> #'test
         ;; gen test that exactly req args were given and actual args
         (cond
            [(= req minreq) 
             ;; at least minreq args are always there; do not allow
             ;; more than that
             (if (> maxreq minreq) ;; optional args follow req'd args
                 ;; test that first opt has default value
                 (with-syntax ([i (list-ref ids req)])
                    #'(default-object? i))
                 ;; no optionals: we don't need any tests
                 #'#t)]
            ;; now we know that minreq < req <= maxreq and that optionals do exist!
            [(< req maxreq)
             ;; the last of req is optional followed by more optionals;
             ;; test that id doesn't have default value, but the next one does
             (with-syntax ([i (list-ref ids (- req 1))]
                           [inext (list-ref ids req)]) 
                #'(and (not (default-object? i)) (default-object? inext)))]
            ;; now we know that last of req is the last optional arg
            [else
             ;; test that id doesn't have default value
             (with-syntax ([i (list-ref ids (- req 1))])
                #'(not (default-object? i)))]))
      (syntax-case x ()
         [(_)
          (syntax-error x "no clauses in CASE-LAMBDA form")]
         [(_ [formals . body] ...)
          (process #'(formals ...) #'(body ...))])))

#| (rec var exp) |# ;Chez
(define-syntax rec 
  (syntax-rules ()
    [(_ x e) (letrec ([x e]) x)])) 

#| (fluid-let ((var val) ...) exp ...) |# ;Chez
(define-syntax fluid-let 
  (lambda (x)
    (syntax-case x ()
      [(_ () e1 e2 ...) #'(let () e1 e2 ...)]
      [(_ ([x v] ...) e1 e2 ...)
       (andmap identifier? #'(x ...))
       (with-syntax ([(y ...) (generate-temporaries #'(x ...))])
       #'(let ([y v] ...)
           (let ([swap (lambda ()
                         (let ([t x]) (set! x y) (set! y t)) ...)])
             (dynamic-wind swap (lambda () e1 e2 ...) swap))))]))) 

;record-case ?

#| (when test exp ...) |# ;Chez
(define-syntax when 
  (lambda (x)
    (syntax-case x ()
      [(_ cond expr ...) #'(if cond (begin expr ...))])))

#| (unless test exp ...) |# ;Chez
(define-syntax unless 
  (lambda (x)
    (syntax-case x ()
      [(_ cond expr ...) #'(if cond (void) (begin expr ...))])))



#| (define-record name (fld1 ...) ((fld2 val) ...) ((reader-name expr))) |# ;Chez
(define-syntax define-record 
  (lambda (x)
    (define gen-id
      (lambda (template-id . args)
        (datum->syntax-object template-id
          (string->symbol
            (apply string-append
                   (map (lambda (x)
                          (if (string? x)
                              x
                              (symbol->string
                                (syntax-object->datum x))))
                        args))))))
    (syntax-case x (reader-name)
      ((_ name fields)
       (syntax (define-record name fields () ((reader-name #f)))))
      ((_ name fields inits)
       (syntax (define-record name fields inits ((reader-name #f)))))
      ((_ name (field1 ...) ((field2 init) ...) ((reader-name str)))
       (andmap identifier? (syntax (name field1 ... field2 ...)))
       (with-syntax
         ((rtd-name (symbol->string (syntax-object->datum (syntax name))))
          (rtd (gen-id (syntax name) "#" (syntax name)))
          (constructor (gen-id (syntax name) "make-" (syntax name)))
          (predicate (gen-id (syntax name) (syntax name) "?"))
          ((access ...)
           (map (lambda (x) (gen-id x (syntax name) "-" x))
                (syntax (field1 ... field2 ...))))
          ((assign ...)
           (map (lambda (x) (gen-id x "set-" (syntax name) "-" x "!"))
                (syntax (field1 ... field2 ...))))
          ((index ...)
           (let f ((i 0) (ids (syntax (field1 ... field2 ...))))
              (if (null? ids)
                  '()
                  (cons i (f (+ i 1) (cdr ids)))))))
         (syntax (begin
                   (define rtd
                     (let ((d (make-record-type rtd-name 
                                '(field1 ... field2 ...)))
                           (s str))
                       (if s (record-reader s d))
                       d))
                   (define constructor
                     (lambda (field1 ...)
                       (let* ((field2 init) ...)
                         (%make-record rtd field1 ... field2 ...))))
                   (define predicate
                     (lambda (x)
                       (record? x rtd)))
                   (define access
                     (lambda (x)
                       (unless (record? x rtd)
                         (error 'access "~s is not of type ~s" x rtd))
                       (%record-ref x index)))
                   ...
                   (define assign
                     (lambda (x update)
                       (unless (record? x rtd)
                         (error 'assign "~s is not of type ~s" x rtd))
                       (%record-set! x index update)))
                   ...))))))) 


#| (define-structure (name id1 ...) ((id2 val) ...)) |# ;Chez
(define-syntax define-structure 
  (lambda (x)
    (define gen-id
      (lambda (template-id . args)
        (datum->syntax-object template-id
          (string->symbol
            (apply string-append
                   (map (lambda (x)
                          (if (string? x)
                              x
                              (symbol->string
                                (syntax-object->datum x))))
                        args))))))
    (syntax-case x ()
      ((_ (name field1 ...))
       (andmap identifier? (syntax (name field1 ...)))
       (syntax (define-structure (name field1 ...) ())))
      ((_ (name field1 ...) ((field2 init) ...))
       (andmap identifier? (syntax (name field1 ... field2 ...)))
       (with-syntax
         ((constructor (gen-id (syntax name) "make-" (syntax name)))
          (predicate (gen-id (syntax name) (syntax name) "?"))
          ((access ...)
           (map (lambda (x) (gen-id x (syntax name) "-" x))
                (syntax (field1 ... field2 ...))))
          ((assign ...)
           (map (lambda (x) (gen-id x "set-" (syntax name) "-" x "!"))
                (syntax (field1 ... field2 ...))))
          (structure-length
           (+ (length (syntax (field1 ... field2 ...))) 1))
          ((index ...)
           (let f ((i 1) (ids (syntax (field1 ... field2 ...))))
              (if (null? ids)
                  '()
                  (cons i (f (+ i 1) (cdr ids)))))))
         (syntax (begin
                   (define constructor
                     (lambda (field1 ...)
                       (let* ((field2 init) ...)
                         (vector 'name field1 ... field2 ...))))
                   (define predicate
                     (lambda (x)
                       (and (vector? x)
                            (= (vector-length x) structure-length)
                            (eq? (vector-ref x 0) 'name))))
                   (define access
                     (lambda (x)
                       (vector-ref x index)))
                   ...
                   (define assign
                     (lambda (x update)
                       (vector-set! x index update)))
                   ...))))))) 

#| (parameterize ((param val) ...) exp ...) |# ;Chez
(define-syntax parameterize
  (lambda (x)
    (syntax-case x ()
      [(_ () e1 e2 ...) (syntax (begin e1 e2 ...))]
      [(_ ([x v] ...) e1 e2 ...)
       (andmap identifier? (syntax (x ...)))
       (with-syntax ([(p ...) (generate-temporaries (syntax (x ...)))]
                     [(y ...) (generate-temporaries (syntax (x ...)))])
         (syntax
           (let ([p x] ... [y v] ...)
             (let ([swap (lambda ()
                           (let ([t (p)]) (p y) (set! y t)) ...)])
               (dynamic-wind swap (lambda () e1 e2 ...) swap)))))])))

#| (record-case exp clause ...) where clause is ((key ...) formals exp ...) or (ELSE exp ...) |#
(define-syntax record-case ;chez -> sxm
  (lambda (x)
    (syntax-case x (else)
      ((_ var) (syntax (error 'record-case "no clause matches ~s" var)))
      ((_ var (else exp1 exp2 ...)) (syntax (begin exp1 exp2 ...)))
      ((_ exp clause ...)
       (not (identifier? (syntax exp)))
       (syntax (let ((var exp)) (_ var clause ...))))
      ((_ var ((key ...) formals exp1 exp2 ...) clause ...)
       (syntax
         (if (memv-macro (car var) '(key ...))
             (apply (lambda formals exp1 exp2 ...) (cdr var))
             (_ var clause ...)))))))

#| (define-integrable name (lambda formals exp ...)) |# ;Chez
(define-syntax define-integrable
  (lambda (x)
    (define make-residual-name
      (lambda (name)
        (datum->syntax-object name
          (string->symbol
            (string-append "residual-"
              (symbol->string (syntax-object->datum name)))))))
    (syntax-case x (lambda)
      ((_ name (lambda formals form1 form2 ...))
       (identifier? (syntax name))
       (with-syntax ((xname (make-residual-name (syntax name))))
         (syntax
           (begin
             (define-syntax name
               (lambda (x)
                 (syntax-case x ()
                   (_ (identifier? x) (syntax xname))
                   ((_ arg (... ...))
                    (syntax
                      ((fluid-let-syntax
                         ((name (identifier-syntax xname)))
                         (lambda formals form1 form2 ...))
                       arg (... ...)))))))
             (define xname
               (fluid-let-syntax ((name (identifier-syntax xname)))
                 (lambda formals form1 form2 ...)))))))))) 

#| (include filename) |# ;Chez
(define-syntax include
  (lambda (x)
    (define read-file
      (lambda (fn k)
        (let ((p (open-input-file fn)))
          (let f ((x (read p)))
            (if (eof-object? x)
                (begin (close-input-port p) '())
                (cons (datum->syntax-object k x)
                      (f (read p))))))))
    (syntax-case x ()
      ((k filename)
       (let ((fn (syntax-object->datum (syntax filename))))
         (with-syntax (((exp ...) (read-file fn (syntax k))))
           (syntax (begin exp ...))))))))

#| (time exp) |# ;Chez
;; chez-like time (uses SXM's right-to-left order of arg evaluation)
(define-syntax time
  (syntax-rules ()
    [(time x)
     ((lambda (gcc1 gt1 t1 v t2 gt2 gcc2) 
        (fprintf (console-output-port) 
           "~s~%    ~d collection~p~%    ~d ms elapsed gc time~%    ~d ms elapsed real time~%" 
           'x
           (- (abs (- gcc1 gcc2)) 1) (- (abs (- gcc1 gcc2)) 1)
           (* 1000 (/ (abs (- gt1 gt2)) (internal-time-units-per-second))) 
           (* 1000 (/ (abs (- t1 t2)) (internal-time-units-per-second))))
        v)
      (call-with-values collect (lambda (gccnt a b c d e f) gccnt))
      (get-internal-gc-run-time)
      (get-internal-run-time) 
      x 
      (get-internal-run-time)
      (get-internal-gc-run-time)
      (call-with-values collect (lambda (gccnt a b c d e f) gccnt)))]))


;;;
;;; III. Miscellanea

#| (do-list (var list [result]) exp ...) |#
(define-syntax do-list
  (lambda (x)
    (syntax-case x ()
      [(_ (var lst) . body)
       #'(_ (var lst (void)) . body)]
      [(_ (var lst res) ex1 ex2 ...)
       (identifier? #'var)
       #'(let loop ([tail lst])
            (let ([var (if (pair? tail) (car tail) #f)])
               (if (pair? tail)
                   (begin ex1 ex2 ... (loop (cdr tail)))
                   res)))])))

#| (do-times (var count [result]) exp ...) |#
(define-syntax do-times
  (lambda (x)
    (syntax-case x ()
      [(_ (var cnt) . body)
       #'(_ (var cnt (void)) . body)]
      [(_ (var cnt res) ex1 ex2 ...)
       (identifier? #'var)
       #'(let loop ([var 0] [top cnt])
           (if (< var top)
               (begin ex1 ex2 ... (loop (1+ var) top))
               res))])))


#| (set-values! (var ...) exp) |#
(define-syntax set-values!
  (lambda (x)
    (syntax-case x ()
      [(_ (x ...) e)
       (with-syntax ([(t ...) (generate-temporaries #'(x ...))])
          #'(call-with-values
              (lambda () e)
              (lambda (t ...) (set! x t) ...)))])))


#| (with-handlers ((predicate handler-proc) ...) exp ...) |#
(define-syntax with-handlers
  (syntax-rules ()
    [(_ ([predicate handler-procedure] ...) b1 b2 ...)
     ((call-with-current-continuation
        (lambda (k)
          (let ([rh (current-exception-handler)]
                [preds (list predicate ...)]
                [handlers (list handler-procedure ...)])
            (with-exception-handler                
              (lambda (exn) 
                (with-exception-handler rh
                  (lambda ()
                    (let f ([preds preds] [handlers handlers])
                      (if (not (null? preds))
                          (if ((car preds) exn)
                              (k (lambda () ((car handlers) exn)))
                              (f (cdr preds) (cdr handlers)))
                          (rh exn))))))
              (lambda ()
                (call-with-values
                  (lambda () b1 b2 ...)
                  (lambda args
                    (k (lambda () (apply values args)))))))))))]))


#| (values-case exp (formals exp ...) ...) |#
(define-syntax values-case
  (lambda (x)
    (syntax-case x ()
       [(_ expr [formals . body] ...)
        #'(call-with-values 
            (lambda () expr)
            (case-lambda [formals . body] ...))])))


