
;;
;; Chez-like tracer -- esl

;; bookkeeping

(define tracer-wrap)    ;forward
(define tracer-add!)    ;forward
(define tracer-remove!) ;forward

(let ([tracer-list '()]) ; ((var org-val wrapped-val) ...)

  (define tracer-cleanup
    (lambda (alst)
      (let loop ([alst alst] [res '()])
        (if (null? alst)
            (reverse! res)
            (if (eq? (top-level-value (caar alst)) (caddar alst))
                (loop (cdr alst) (cons (car alst) res))
                (loop (cdr alst) res))))))

  (set! tracer-add! 
    (lambda vars
      (define alst (tracer-cleanup tracer-list)) 
      (for-each
        (lambda (var)
          (unless (assq var alst)
            (unless (top-level-bound? var)
              (error 'trace "~s is not bound" var))
            (let ([p (top-level-value var)])
              (unless (procedure? p)
                (error 'trace "the top-level value of ~s is not a procedure" var))
              (let ([wrapped-p (tracer-wrap var p)])
                (set-top-level-value! var wrapped-p)
                (set! alst (cons (list var p wrapped-p) alst))))))
        vars)
      (set! tracer-list alst)
      (map car alst)))

  (set! tracer-remove!
    (lambda vars
      (define alst (tracer-cleanup tracer-list)) 
      (for-each
        (lambda (var)
          (let ([vow (assq var alst)])
            (when vow
              (let ([p (cadr vow)])
                (set! alst (remq! vow alst))
                (set-top-level-value! var p)))))
        vars)
      (set! tracer-list alst)
      (map car alst)))
) 

;; tracer output

(define trace-output-port
  (make-parameter
    (console-output-port)
    (lambda (x)
      (if (output-port? x)
          x
          (error 'trace-output-port 
            "parameter value ~s is not an output port" x)))))
    
(define trace-print
  (make-parameter
    pretty-print
    (procedure-parameter-filter 'trace-print)))


;; tracer core

(define tracer-wrap
  (let ([tracer-parent-context (make-parameter (cons -1 -1))])
    (define (tracer-indent-string n)
      (if (< n 10)
          (vector-ref 
            '#("|" "| " "| |" "| | " "| | |" "| | | " 
               "| | | |" "| | | | " "| | | | |" "| | | | | ")
            n)
          (sprintf "| | | |[~s]" n)))
    (lambda (var p)
      (lambda args
        (call/esc
          (lambda (esc)
            (let ([l&sp (tracer-parent-context)]
                  [port (trace-output-port)])
              (cond 
                [(= (cdr l&sp) (%cdr esc))
                 ;; this call shares caller's continuation
                 (let* ([istr (tracer-indent-string (car l&sp))]
                        [indent (string-length istr)])
                   ;; print call args on the parent's level
                   (display istr port)
                   (parameterize ([pretty-initial-indent indent])
                     ((trace-print) (cons var args) port))
                   ;; proceed; parent will print the result(s)
                   (apply p args))]
                [else  
                 ;; this call is nested; increase level
                 (let* ([new-l&sp (cons (+ 1 (car l&sp)) #f)]
                        [istr (tracer-indent-string (car new-l&sp))]
                        [indent (string-length istr)])
                   ;; print call args on the new level
                   (display istr port)
                   (parameterize ([pretty-initial-indent indent])
                     ((trace-print) (cons var args) port))
                   ;; receive results
                   (call-with-values
                     (lambda ()
                       (parameterize ([tracer-parent-context new-l&sp]) 
                         (call/esc
                           (lambda (esc)
                             (set-cdr! new-l&sp (%cdr esc))
                             (apply p args)))))
                     (lambda results
                       (cond 
                         [(null? results)
                          (display istr port) 
                          (newline port)]
                         [(null? (cdr results))
                          (display istr port)
                          (parameterize ([pretty-initial-indent indent])
                            ((trace-print) (car results) port))]
                         [else
                          (let ([istr2 (make-string indent #\space)])
                            (let loop ([results results] [str istr])
                              (unless (null? results)
                                (display str port)
                                ((trace-print) (car results) port)
                                (loop (cdr results) istr2))))])
                       (apply values results))))]))))))))

;; syntax sugar

(define-syntax trace
  (syntax-rules ()
    [(_ var ...) (tracer-add! 'var ...)]))

(define-syntax untrace
  (syntax-rules ()
    [(_ var ...) (tracer-remove! 'var ...)]))

(define-syntax trace-lambda
  (syntax-rules ()
    [(_ var args exp1 exp2 ...)
     (tracer-wrap 'var (lambda args exp1 exp2 ...))]))

(define-syntax trace-define
  (syntax-rules ()
    [(_ (var . idspec) exp1 exp2 ...) 
     (define var (trace-lambda var idspec exp1 exp2 ...))]
    [(_ var exp)
     (define var
       (let ([x exp])
         (trace-lambda var args
           (apply x args))))]))  

(define-syntax trace-let
  (syntax-rules ()
    [(_ name ([var val] ...) exp1 exp2 ...)
     ((letrec ([name (trace-lambda name (var ...) exp1 exp2 ...)])
        name)
      val ...)]))      

(define-syntax trace-define-syntax
  (syntax-rules ()
    [(_ name transformer)
     (define-syntax name (tracer-wrap 'name transformer))]))

