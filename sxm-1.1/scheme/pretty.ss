
;;
;; pretty-printer -- esl

;; number of columns to print within
#| (pretty-line-length [n]) |#
(define pretty-line-length (make-parameter 70))  

;; initial indent (first line assumption)
#| (pretty-initial-indent [n]) |#
(define pretty-initial-indent (make-parameter 0))  

;; indentation within special forms
#| (pretty-standard-indent [n]) |#
(define pretty-standard-indent (make-parameter 1)) 

;; unindentation op length limit
#| (pretty-alt-indent-limit [n]) |# ;non-chez!
(define pretty-alt-indent-limit (make-parameter 3)) 

#| (pretty-use-tabs [bool]) |# ;non-chez!
(define pretty-use-tabs (make-parameter #f))

;; print brackets around some subforms
#| (print-brackets [bool]) |#
(define print-brackets (make-parameter #t))

;; list of pp patterns (initialized below)
#| (pretty-printer-patterns [list]) |# ;non-chez!
(define pretty-printer-patterns (make-parameter '()))

#| (pretty-print obj [oport]) |#
(define pretty-print 
  (let () ;; value is at the end of the let

    ;; counters and measurments

    (define (make-cnt depth) (list (- (pretty-line-length) depth)))
    (define cnt-val car)
    (define cnt-set! set-car!)

    (define (cnt-zero? cnt) (<= (cnt-val cnt) 0))
    (define (cnt-sub cnt val) 
      (>= (begin (cnt-set! cnt (- (cnt-val cnt) val)) (cnt-val cnt)) 0))

    (define (abbrev? exp)
      (and (pair? exp) (pair? (cdr exp)) (null? (cddr exp))
           (memq (car exp) 
                '(quote quasiquote unquote unquote-splicing syntax \#primitive))))

    (define (fits? exp cnt) ;=> #t if exp fits into space provided by cnt
      (cond
        [(abbrev? exp)
         (and (cnt-sub cnt 
                (case (car exp) [(syntax \#primitive unquote-splicing) 2] [else 1]))
              (fits? (cadr exp) cnt))]
        [(pair? exp)
         (let ([h (car exp)] [t (cdr exp)])
           (cond [(null? t)
                  (and (cnt-sub cnt 2) (fits? h cnt))]
                 [(pair? t)
                  (and (cnt-sub cnt 1) (fits? h cnt) (fits? t cnt))]
                 [else
                  (and (cnt-sub cnt 5) (fits? h cnt) (fits? t cnt))]))]
        [(vector? exp)
         (let ([vlen (- (vector-length exp) 1)])
           (let loop ([i 0])
             (if (>= i vlen)
                 (cnt-sub cnt 3)
                 (and (fits? (vector-ref exp i) cnt)
                      (loop (+ i 1))))))]
        [else
         (cnt-sub cnt (external-representation-length exp))]))
    
    ;; output primitives

    (define (lpar port) (write-char #\( port))
    (define (rpar port) (write-char #\) port))
    (define (space port) (write-char #\space port))
    (define (lbra port) (write-char (if (print-brackets) #\[ #\() port))
    (define (rbra port) (write-char (if (print-brackets) #\] #\)) port))

    (define (break inline? depth port)
      (if inline?
          (space port)
          (let ([q (quotient depth 8)] [r (remainder depth 8)])
            (newline port)
            (do ([i 0 (+ i 1)]) [(>= i q)]
              (if (pretty-use-tabs) 
                  (write-char #\tab port) 
                  (display "        " port)))
            (do ([i 0 (+ i 1)]) [(>= i r)] (space port)))))


    ;; printers

    (define (print-pair exp depth port)
      (let ([inline? (fits? exp (make-cnt depth))])
        (lpar port)
        (do ([depth (+ depth 1)] [first? #t #f] [exp exp (cdr exp)])
          [(not (pair? exp))
           (unless (null? exp) 
             (display " . " port)
             (print-datum exp (+ depth 3) port))]
          (unless first? (break inline? depth port))
          (print-datum (car exp) depth port))
        (rpar port)))

    (define (print-vector exp depth port)
      (let ([inline? (fits? exp (make-cnt depth))]
            [vlen (vector-length exp)])
        (write-char #\# port)
        (lpar port)
        (do ([depth (+ depth 2)] [idx 0 (+ idx 1)])
          [(= idx vlen)]
          (unless (zero? idx) (break inline? depth port))
          (print-datum (vector-ref exp idx) depth port))
        (rpar port)))

    (define (print-datum exp depth port)
      (cond 
        [(pair? exp) (print-pair exp depth port)]
        [(vector? exp) (print-vector exp depth port)]
        [else (write exp port)]))

    (define (print-exp exp depth port)
      (cond 
        [(abbrev? exp)
         (print-abbrev exp depth port)]
        [(pair? exp)
         (cond
           [(not (symbol? (car exp)))
            (print/pat '(e ...) exp depth port)]
           [(and (eq? (car exp) 'let) (pair? (cdr exp)) (symbol? (cadr exp)))
            (print/pat '(i i (#&(d e ...) ...) e ...) exp depth port)] ;hack #1
           [(assq (car exp) (pretty-printer-patterns)) =>
            (lambda (pair) (print/pat (cons 'i (cdr pair)) exp depth port))]
           [else (print/pat '(i e ...) exp depth port)])]
        [else
         (print-datum exp depth port)]))

    (define (print-abbrev exp depth port)
      (let ([abr (car exp)] [arg (cadr exp)])
        (case abr
          [(quote) (write-char #\' port)]
          [(quasiquote) (write-char #\` port)]
          [(unquote) (write-char #\, port)]
          [(unquote-splicing) (display ",@" port)]
          [(syntax) (display "#'" port)]
          [(\#primitive) (display "#%" port)])
        (let ([depth (+ depth (case abr [(syntax \#primitive unquote-splicing) 2] [else 1]))])
          (if (and (not (eq? abr 'quote)) (pair? arg))
              (print-exp arg depth port)
              (print-datum arg depth port)))))

    (define (print/pat pat exp depth port)
      (cond
        [(eq? pat 'e) 
         (print-exp exp depth port)]
        [(memq pat '(d i)) 
         (print-datum exp depth port)]
        [(and (list? pat) (list? exp))
         (lpar port)
         (print*/pat pat exp (+ 1 depth) port #t)
         (rpar port)]
        [(and (box? pat) (list? (unbox pat)) (list? exp))
         (lbra port)
         (if (and (pair? exp) (eq? (car exp) 'else)) ;hack #2
             (print*/pat '(i e ...) exp (+ 1 depth) port #f)
             (print*/pat (unbox pat) exp (+ 1 depth) port #f))
         (rbra port)]
        [(or (box? pat) (pair? pat))
         (print-datum exp depth port)]
        [else 
         (error 'pretty-print "illegal pretty-print pattern: ~s" pat)]))

    (define (print*/pat pat* exp* depth port indent-body?)
      (let ([etc? (memq '... pat*)] [plen (length pat*)] [elen (length exp*)])
        (cond
          [(and (not etc?) (= plen elen))
           ;; sequential pattern
           (let ([inline? (fits? exp* (make-cnt (+ 2 depth)))])
             (let loop ([first? #t] [pat* pat*] [exp* exp*])
               (unless (null? exp*)
                 (unless first? (break inline? depth port))
                 (print/pat (car pat*) (car exp*) depth port)
                 (loop #f (cdr pat*) (cdr exp*)))))]
          [(and etc? (= plen 2))
           ;; etc pattern
           (let ([inline? (fits? exp* (make-cnt (+ 2 depth)))])
             (let loop ([first? #t] [exp* exp*])
               (unless (null? exp*)
                 (unless first? (break inline? depth port))
                 (print/pat (car pat*) (car exp*) depth port)
                 (loop #f (cdr exp*)))))]
          [(and etc? indent-body? (= plen 3) (eq? (car pat*) 'i) 
             (>= elen 3) (symbol? (car exp*)))
           ;; id pattern followed by etc pattern
           (let ([op-len (external-representation-length (car exp*))])
             (if (<= op-len (pretty-alt-indent-limit))
                 (begin
                   (write (car exp*) port)
                   (space port)
                   (print*/pat (cdr pat*) (cdr exp*) (+ depth 1 op-len) port #t))
                 (print*/pat (cons 'd (cdr pat*)) exp* depth port indent-body?)))]
          [(and etc? (> plen 2) (<= (- plen 2) elen))
           ;; sequential pattern followed by etc
           (let ([all-inline? (fits? exp* (make-cnt (+ 2 depth)))]
                 [head-exp* (list-head exp* (- plen 2))]
                 [tail-exp* (list-tail exp* (- plen 2))])
             (print*/pat 
               (list-head pat* (- plen 2)) 
               head-exp* depth port #f)
             (unless (null? tail-exp*)
               (let ([depth (if indent-body? (+ depth (pretty-standard-indent)) depth)])
                 (break all-inline? depth port)
                 (print*/pat 
                   (list-tail pat* (- plen 2)) 
                   tail-exp* depth port #f))))]
          [else
           ;; pattern doesn't match
           (print*/pat '(e ...) exp* depth port indent-body?)])))
    
    (pretty-printer-patterns '(
      (lambda d e ...)
      (define d e ...)
      (define-syntax d e)
      (let (#&(d e ...) ...) e ...)
      (letrec (#&(d e ...) ...) e ...)
      (let* (#&(d e ...) ...) e ...)
      (let-syntax (#&(d e ...) ...) e ...)
      (letrec-syntax (#&(d e ...) ...) e ...)
      (syntax-rules d #&(d e) ...)
      (syntax-case e d #&(d e ...) ...)
      (with-syntax (#&(d e ...) ...) e ...)
      (do (#&(d e ...) ...) #&(e e ...) e ...)
      (begin e ...)
      (if e ...)
      (cond #&(e e ...) ...)
      (case e #&(d e ...) ...)
      (case-lambda #&(d e ...) ...)
      (rec d e)
      (fluid-let (#&(d e ...) ...) e ...)
      (when e e ...)
      (unless e e ...)
      (define-structure (i i ...) (#&(i e) ...))
      (parameterize (#&(e e) ...) e ...)
      (record-case e #&(i d e ...) ...)
      (define-integrable i e)
      (cond-expand #&(d e ...) ...)
      (receive d e e ...)
      (let-values (#&(d e ...) ...) e ...)
      (let*-values (#&(d e ...) ...) e ...)
      (do-list #&(i e ...) e ...)
      (do-times #&(i e ...) e ...)
      (set-values! d e)
      (with-handlers (#&(i e) ...) e ...)
      (values-case e #&(d e ...) ...)
    ))

    ;; value of pretty-print
    (lambda (exp #!optional port)
      (let ([port (if (default-object? port) (current-output-port) port)])
        (print-exp exp (pretty-initial-indent) port)
        (newline port)))))

#| (pretty-file ifilename ofilename) |#
(define (pretty-file ifn ofn)
  (call-with-input-file ifn
    (lambda (ip)
      (call-with-output-file ofn
        (lambda (op)
          (let loop ([obj (read ip)])
            (unless (eof-object? obj)
              (newline op)
              (pretty-print obj op)
              (loop (read ip)))))))))

