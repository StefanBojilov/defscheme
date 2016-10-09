
;;
;; debugger and inspector

;; noninteractive inspector

(define (inspect k)
   (if (and (closure? k) (vector? (%cdr k)))
       (let loop ([lst (%vector->list (%cdr k))])
          (if (null? lst)
              (void) ;(printf "K: ~s~%" (%cdr k))
              (begin
                 (if (and (>= (length lst) 3) 
                          (or (null? (car lst)) (vector? (car lst)))
                          (code? (cadr lst)) 
                          (integer? (caddr lst)))
                     (begin
                       (printf "=== Frame: ~s ~s ===~%" (cadr lst) (caddr lst))
                       (disassemble (%set-tag! (cons (cadr lst) (car lst)) 8) (caddr lst))))
                 (loop (cdr lst)))))
       (pretty-print k)))

;; install debugger

(debug-handler
  (lambda ()
    (define outer-k #f)
    (define (repl)
      (cprintf "debug> ~!")
      (let ([cmd (read (console-input-port))])
        (if (eof-object? cmd)
            (apply void '()) ;same as typing 'e'
            (case cmd
               [(?)
                (cprintf "
Type e to exit interrupt handler and continue
     r to reset scheme
     a to abort scheme
     n to enter new cafe
     i to inspect current continuation
     s to display statistics

~!") (repl)]
               [(e) (apply void '())]
               [(r) (reset)]
               [(a) (abort)]
               [(n) (new-cafe) (repl)]
               [(i) (inspect outer-k) (repl)]
               [(s) (display-statistics) (repl)]
               [else
                (cprintf "Invalid command.  Type ? for options.~%")
                (repl)]))))
    (call/cc
      (lambda (k)
        (set! outer-k k)
        (repl)))))
