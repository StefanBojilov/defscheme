
;;
;; sxc.ss -- "compiler" for sxm

(define (compiler)
  (define outfile "i.out")
  (define infiles '())
  (define cexps '())
  (define (usage)
    (printf "Use 'sxc -o foo in1.ss in2.ss ...' to build a foo image~%")
    (printf " or 'sxc in1.ss in2.ss ...' to build the i.out image~%")
    (exit))
  (define (fail msg . args)
    (printf "error: ")
    (apply printf msg args)
    (newline))
  (define (ffail file line msg . args)
    (printf "~a:~a: error: " file line)
    (apply printf msg args)
    (newline))

  ;; preocess arguments
  (let ([files (program-arguments)])
    (cond 
      [(null? files) (usage)]
      [(string=? (car files) "-o")
       (when (< (length files) 3) (usage))
       (set! outfile (cadr files))
       (set! infiles (cddr files))]
      [else
       (set! infiles files)]))

  ;; read/compile input expressions
  (for-each
    (lambda (infile)
      (printf "Compiling ~a " infile)
      (let ([in #f])
        (with-handlers 
          ([open-error? 
            (lambda (e) (fail "cannot open ~a" infile))]
           [read-error? 
            (lambda (e) (ffail infile (lines-read in) "syntax (~a)" (vector-ref e 1)))]
           [true?
            (lambda (e) (ffail infile (lines-read in) "~s" e))])
          (set! in (open-source-file infile))
          (let loop ([exp (read in)])
            (unless (eof-object? exp)
              (set! cexps
                (cons (frobit/sc->core (expand exp)) cexps))
              (write-char #\.)
              (loop (read in)))))
        (when (port? in) (close-port in)))
      (printf "~%"))
    infiles)

  ;; compile-core and save image
  (printf "Linking ")   
  (set! thunk (compile-core (cons 'begin (reverse! cexps))))
  (printf ".~%")
  (printf "Writing image ~a" outfile)
  ;; writing the image will terminate current process!
  ;; Since save-program strips interpreter code,
  ;; make sure that resulting program does not call it
  (current-eval (lambda args (internal-error 'eval "EVAL is disabled")))
  (printf ".~%")
  (save-program outfile thunk))

;; load this and save this to image file (do not strip eval!):
;; (save-image "sxc" compiler)

