
;;
;; engines.ss -- esl after R. Kent Dybvig

;;
;; a simple implementation of engines from the book by R. Kent Dybvig
;; "The SCHEME Programming Language", based on SXM/Chez timer interrupts 
;; handling.

;;
;; engine-return, engine-block, and make-engine are given values
;; inside the body of the let below.

(define engine-return)
(define engine-block)
(define make-engine)


(let () ;begin package

  (define active? #f) ;true when an engine is running.

  (define escape #f)  ;holds the continuation to the engine invoker

  (define (sanitize)  ;clean up the state, return time left
    (set! active? #f)
    ;; error handlers reset code goes here
    )

  (define (stop)  ;disable engine and return the continuation
    (unless active? (error 'engine-block "no engine active"))
    (sanitize)
    (call/cc
      (lambda (k)
        (escape (lambda () (k (void)))))))

  (define (block)
    (set-timer 0)
    (unless active? (error 'engine-block "no engine active"))
    (sanitize)
    (call/cc
      (lambda (k)
        (escape (lambda () (k #f))))))

  ;; disable engine and return list (ticks result ...)
  (define (return . results) 
    (let ([n (set-timer 0)])
      (unless active? (error 'engine-return "no engine active"))
      (sanitize)
      (escape (cons n results))))

  ;; here goes the code to redefine captured interrupt handlers -
  ;; we intentionally omit it

  (define (run-engine thunk ticks)
    (call/cc
      (lambda (k)
        (set-timer 0)
        (when active? (error 'run-engine "cannot nest engines"))
        (set! escape k)
        (set! active? #t)
        (timer-interrupt-handler stop)
        (set-timer ticks)
        (call-with-values thunk return))))

  (define (engine thunk)
    (lambda (ticks complete expire)
      (let ([x (run-engine thunk ticks)])
        (if (procedure? x)
            (expire (engine x))
            (apply complete x)))))

  (set! engine-return return)
  (set! engine-block block)
  (set! make-engine engine)

) ;end package

;; tests and examples

;(define fibonacci
;  (lambda (n)
;    (let fib ([i n])
;      (cond
;        [(= i 0) 0]
;        [(= i 1) 1]
;        [else (+ (fib (- i 1)) (fib (- i 2)))])))) 

;(define eng
;  (make-engine
;    (lambda ()
;      (fibonacci 10)))) 

;(eng 50 list (lambda (new-eng) (set! eng new-eng) "expired")) -- repeat

;(define mileage
;  (lambda (thunk)
;    (let loop ([eng (make-engine thunk)] [total-ticks 0])
;      (eng 50
;           (lambda (ticks . values)
;             (+ total-ticks (- 50 ticks)))
;           (lambda (new-eng)
;             (loop new-eng
;                   (+ total-ticks 50))))))) 

;(mileage (lambda () (fibonacci 10))) => 117 (may vary)

;(define snapshot
;  (lambda (thunk)
;    (let again ([eng (make-engine thunk)])
;      (cons eng
;            (eng 1 (lambda (t . v) '()) again))))) 

;(snapshot (lambda () (fibonacci 10))) => long list of closures

;(define round-robin
;  (lambda (engs)
;    (if (null? engs)
;        '()
;        ((car engs)
;         1
;         (lambda (ticks value)
;           (cons value (round-robin (cdr engs))))
;         (lambda (eng)
;           (round-robin
;             (append (cdr engs) (list eng)))))))) 

;(round-robin
;  (map (lambda (x)
;         (make-engine
;           (lambda ()
;             (fibonacci x))))
;       '(4 5 2 8 3 7 6 2)))     => (1 1 2 3 5 8 13 21)

;(define first-true
;  (let ([pick
;         (lambda (ls)
;           (list-ref ls (random (length ls))))])
;    (lambda (engs)
;      (if (null? engs)
;          #f
;          (let ([eng (pick engs)])
;            (eng 1
;                 (lambda (ticks value)
;                   (or value
;                       (first-true
;                         (remq eng engs))))
;                 (lambda (new-eng)
;                   (first-true
;                     (cons new-eng
;                           (remq eng engs)))))))))) 

;(define-syntax por
;  (syntax-rules ()
;    [(_ x ...)
;     (first-true
;       (list (make-engine (lambda () x)) ...))])) 

;(por 1 2 3)  => 1 2 or 3 (nondeterministic)

;(por (let loop () (loop)) 2)  => 2 (always finishes)


;(define eng1
;  (make-engine
;    (lambda ()
;      (engine-block)
;      "completed"))) 

;(eng1 100 (lambda (ticks value) value) (lambda (x) (set! eng1 x) "expired")) 
; => "expired" first time, then "completed"

;(define eng2
;  (make-engine
;   (lambda ()
;      (reverse (engine-return 'a 'b 'c))))) 

;(eng2 100 (lambda (ticks . values) values) (lambda (new-eng) "expired")) 
; => (a b c)

