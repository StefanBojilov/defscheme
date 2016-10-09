
;;
;; cafes and waiters

(current-exception-handler
  (lambda (exn)
    (cond
      [(open-error? exn)
       (error #f "error opening ~s" (open-error-filename exn))]
      [(read-error? exn)
       (let ([p (read-error-port exn)])
         (let ([loc (if (and (port? p) (source-file-port? p))
                        (sprintf " (line ~d)" (+ (lines-read p) 1)) "")])
           (case (read-error-tag exn)
             [(id2long)   (error 'read "identifier too long~a" loc)]  
             [(mdl)       (error 'read "missing delimiter~a" loc)] 
             [(zidsyn)    (error 'read "zero-length symbol~a" loc)]
             [(num2long)  (error 'read "number too long~a" loc)]
             [(inumsyn)   (error 'read "invalid number syntax~a" loc)]
             [(znumsyn)   (error 'read "zero-length number~a" loc)]
             [(iuvec)     (error 'read "invalid u8 vector syntax~a" loc)]
             [(ueof)      (error 'read "unexpected end-of-file~a" loc)]
             [(iscspec)   (error 'read "invalid string char spec~a" loc)]
             [(ccover)    (error 'read "character code out of range~a" loc)]
             [(cn2long)   (error 'read "char name too long~a" loc)]
             [(ucname)    (error 'read "unknown character name~a" loc)]
             [(unknconst) (error 'read "unknown symbolic constant~a" loc)]
             [(unxpdot)   (error 'read "unexpected dot~a" loc)]
             [(iu8int)    (error 'read "invalid u8 integer~a" loc)]
             [(badrpar)   (error 'read "mismatching close parenthesis~a" loc)]
             [(mcpar)     (error 'read "mismatching close parenthesis~a" loc)]
             [(unxprpar)  (error 'read "unexpected right parenthesis~a" loc)]
             [(ich)       (error 'read "illegal char: ~s~a" (read-error-arg exn) loc)]
             [(ueofch)    (error 'read "end-of-file in comment: ~s~a" (read-error-arg exn) loc)]
             [(ihch)      (error 'read "invalid char after #: ~c~a" (read-error-arg exn) loc)]
             [(mrecnam)   (error 'read "missing record name~a" loc)]
             [(recnamnr)  (error 'read "record name ~s is not registered witn an rtd~a"
                            (read-error-arg exn) loc)]
             [(mrecter)   (error 'read "record terminator ] expected~a" loc)]
             [else        (error 'read "unidentified read error~a" loc)])))]
      [else 
       (error #f "exception not handled: ~s" exn)])))

(define (string-parameter-filter where)
  (lambda (x)
     (if (string? x)
         x
         (error where "parameter value ~s is not a procedure" x))))

(define waiter-prompt-string
  (make-parameter 
    ">"
    (string-parameter-filter 'waiter-prompt-string)))

(define waiter-prompt-and-read
  (make-parameter
    (lambda (n)
      (unless (and (integer? n) (> n 0))
        (error 'default-waiter-prompt-and-read
               "~s is not a positive integer"
               n))
      (do ([n n (- n 1)])
          [(= n 0)
           (write-char #\space (console-output-port))
           (flush-output-port (console-output-port))]
          (display (waiter-prompt-string) (console-output-port)))
      (let ([x (read (console-input-port))])
        (when (eof-object? x)
          (newline (console-output-port))
          (flush-output-port (console-output-port)))
        x))
    (procedure-parameter-filter 'waiter-prompt-and-read))) 

(define waiter-write
  (make-parameter
    (lambda (x)
      (unless (eq? x (apply void '()))
        (pretty-print x (console-output-port)))
      (flush-output-port (console-output-port)))
    (procedure-parameter-filter 'waiter-write))) 
  
(define cafe-nesting-level
  (make-parameter 0))

(define (new-cafe #!optional eval-proc)
  (define (cafe exit-proc)
    (define gorepl #f)
    (define new-level (+ 1 (cafe-nesting-level)))
    (parameterize 
      ([exit-handler exit-proc]
       [reset-handler (lambda () (gorepl))]
       [cafe-nesting-level new-level])
      (call/cc (lambda (k) (set! gorepl k)))
      (let repl ()
        (let ([x ((waiter-prompt-and-read) new-level)])
          (if (eof-object? x)
              (void)
              (call-with-values
                (lambda () (eval x))
                (lambda vals
                  (for-each (waiter-write) vals) 
                  (repl))))))))
  (if (default-object? eval-proc)
      (call/cc cafe)
      (parameterize ([current-eval eval-proc])
        (call/cc cafe))))

;; redefine interactive startup parameter

(scheme-start
  (lambda args
    (call/cc
      (lambda (k)
        (reset-handler (lambda () (k void)))
        (for-each load args)))
    (new-cafe)))
