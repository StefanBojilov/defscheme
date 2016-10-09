
;;
;; locales.ss -- esl


;;
;; forwards (exported from the package)

(define scheme-report-environment)
(define null-environment)
(define interaction-environment)
(define ieee-environment)
(define named-environment)

(let* ( ;start package

  ;;
  ;; IEEE-1178
  ;;

  [ieee-syntax-ids '(

    ;; 4.1. Primitive expression types
    quote           lambda          if              set!

    ;; 4.2. Derived expression types
    cond            case            else            =>
    and             or
    let             let*            letrec          begin
    do
    quasiquote      unquote         unquote-splicing

    ;; 5.2. Definitions
    define

  )]


  [ieee-variable-ids '(

    ;; 6.1. Booleans
    not             boolean?

    ;; 6.2. Equivalence predicates
    eqv?            eq?             equal?

    ;; 6.3. Pairs and lists
    pair?           cons            
    car             cdr             set-car!        set-cdr!
    caar            cadr            cdar            cddr            
    caaar           caadr           cadar           caddr           
    cdaar           cdadr           cddar           cdddr
    caaaar          caaadr          caadar          caaddr
    cadaar          cadadr          caddar          cadddr
    cdaaar          cdaadr          cdadar          cdaddr
    cddaar          cddadr          cdddar          cddddr
    null?           list?           list            length
    append          reverse         list-ref
    memq            memv            member
    assq            assv            assoc

    ;; 6.4. Symbols
    symbol?         symbol->string  string->symbol

    ;; 6.5. Numbers
    number?         complex?        real?           rational?
    integer?        exact?          inexact?
    =               >               <               >=
    <=              zero?           positive?       negative?
    odd?            even?           max             min
    +               *               -               /
    abs             quotient        remainder       modulo
    gcd             lcm
    floor           ceiling         truncate        round
    rationalize     
    exp             log             sin             cos
    tan             asin            acos            atan
    sqrt            expt
    make-rectangular make-polar     real-part       imag-part       
    magnitude       angle
    exact->inexact  inexact->exact
    number->string  string->number

    ;; 6.6. Characters
    char?           
    char=?          char<?          char>?          char<=?         char>=?
    char-ci=?       char-ci<?       char-ci>?       char-ci<=?      char-ci>=?
    char-alphabetic? char-numeric?  char-whitespace?
    char-upper-case? char-lower-case?
    char->integer   integer->char
    char-upcase     char-downcase

    ;; 6.7. Strings
    string?         make-string     string
    string-length   string-ref      string-set!
    string=?        string>?        string<?        string>=?       string<=?
    string-ci=?     string-ci>?     string-ci<?     string-ci>=?    string-ci<=?
    substring       string-append

    ;; 6.8. Vectors
    vector?         make-vector     vector
    vector-length   vector-ref      vector-set!

    ;; 6.9. Control features
    procedure?      apply           map             for-each
    call-with-current-continuation

    ;; 6.10. Input and output
    ;; 6.10.1. Ports
    call-with-input-file            call-with-output-file
    input-port?                     output-port?
    current-input-port              current-output-port
    with-input-from-file            with-output-to-file
    open-input-file                 open-output-file
    close-input-port                close-output-port
    ;; 6.10.2. Input
    eof-object?     read            read-char       peek-char
    ;; 6.10.3. Output
    write           display         newline         write-char

  )]


  ;;
  ;; R4RS - superset of IEEE
  ;;

  [r4rs-syntax-ids (append '(

    ;; 4.2. Derived expression types
    delay

    ;; Appendix: Macros
    let-syntax      letrec-syntax   syntax-rules    ...
    define-syntax

  ) ieee-syntax-ids)]


  [r4rs-variable-ids (append '(

    ;; 6.3. Pairs and lists
    list-tail

    ;; 6.7. Strings
    string->list    list->string
    string-copy     string-fill!

    ;; 6.8. Vectors
    vector->list    list->vector    vector-fill!

    ;; 6.9. Control features
    force

    ;; 6.10.2. Input
    char-ready?

    ;; 6.10.4. System interface
    load            transcript-on   transcript-off

  ) ieee-variable-ids)]


  ;;
  ;; R5RS - superset of both IEEE and R4RS
  ;;

  [r5rs-syntax-ids (append '(

    ;; 4.3. Macros
    let-syntax      letrec-syntax   syntax-rules    ...

    ;; 5.3. Syntax definitions
    define-syntax

  ) r4rs-syntax-ids)]


  [r5rs-variable-ids (append '(

    ;; 6.4. Control features
    values          call-with-values  
    dynamic-wind

    ;; 6.5. Eval
    eval            
    scheme-report-environment
    null-environment
    interaction-environment

  ) r4rs-variable-ids)]


  ;;
  ;; Syntax-case transformer namespace - superset of R5RS (?)
  ;;

  [transformer-syntax-ids (append '(

    ;; Macros
    syntax-case                      syntax 
    with-syntax                      fluid-let-syntax
    identifier-syntax

  ) r5rs-syntax-ids)]


  [transformer-variable-ids (append '(

    ;; general procedures
    andmap                          ormap        

    ;; syntax object manipulation procedures
    bound-identifier=?              free-identifier=?
    datum->syntax-object            syntax-object->datum
    generate-temporaries            identifier?
    syntax-dispatch                 syntax-error                    

  ) r5rs-variable-ids)]

  ) ;end of let* bindings

  ;;
  ;; locale utils

  (define locales-table (make-hash-table 15))

  (define (enter-locale! loc)
    (hash-table-put! locales-table (locale-name loc) loc))

  (define (find-locale name) ;=> locale | #f
     (hash-table-get locales-table name))

  (define (symbol->locale name)
     (or (hash-table-get locales-table name)
         (error 'find-locale
            "undefined locale: ~s" name)))

  (define (import-id-to-locale from-loc to-loc sym protect?)
    (let ([gcell (find-gcell sym from-loc)])
       (when gcell
          (when protect? (%set-mutability! gcell #f))
          (enter-private-gcell! gcell sym to-loc))))

  (define (copy-id-to-locale from-loc to-loc sym protect?)
    (let ([gcell (find-gcell sym from-loc)])
       (when gcell
          (let ([new-gcell (make-gcell sym to-loc (gcell-value gcell))]) 
             (when protect?
                (%set-mutability! new-gcell #f))
             (enter-private-gcell! new-gcell sym to-loc)))))

  (define (import/copy-ids-to-locale from-loc to-loc imports copies protect?)
    (for-each 
      (lambda (sym) (import-id-to-locale from-loc to-loc sym protect?))
      imports)
    (for-each 
      (lambda (sym) (copy-id-to-locale from-loc to-loc sym protect?))
      copies))

  ;;
  ;; create locales we need

  (enter-locale! (current-locale))

  (let ([transf    (make-locale 'transformer)]
        [r5rs      (make-locale 'r5rs)]
        [r5rs-null (make-locale 'r5rs-null)]
        ;[r4rs      (make-locale 'r4rs)]
        ;[r4rs-null (make-locale 'r4rs-null)]
        [ieee      (make-locale 'ieee)]
        [ieee-null (make-locale 'ieee-null)]
        ;[user      (make-locale 'user)]
       )

    (define (setup-protected-loc loc syn-ids var-ids)
      (import/copy-ids-to-locale transf loc syn-ids '() #t)
      (import/copy-ids-to-locale transf loc var-ids '() #t)
      (%set-mutability! loc #f) ;not extendable
      (enter-locale! loc))

    (define (setup-unprotected-loc loc syn-ids var-ids)
      (import/copy-ids-to-locale transf loc '() syn-ids #f)
      (import/copy-ids-to-locale transf loc '() var-ids #f)
      (%set-mutability! loc #t) ;extendable
      (enter-locale! loc))

    ;; fill largest locale (transf) first
    (import/copy-ids-to-locale (current-locale) transf
       transformer-syntax-ids transformer-variable-ids #t)
    (%set-mutability! transf #f) ;not extendable
    (enter-locale! transf)

    ;; import subsets of transf into other locales
    (setup-protected-loc r5rs r5rs-syntax-ids r5rs-variable-ids)
    (setup-protected-loc r5rs-null r5rs-syntax-ids '())
    ;(setup-protected-loc r4rs r4rs-syntax-ids r4rs-variable-ids)
    ;(setup-protected-loc r4rs-null r4rs-syntax-ids '())
    (setup-protected-loc ieee ieee-syntax-ids ieee-variable-ids)
    (setup-protected-loc ieee-null ieee-syntax-ids '())
    ;(setup-unprotected-loc user r5rs-syntax-ids r5rs-variable-ids)
  )  

  ;;
  ;; export visible bindings

  (set! named-environment
    symbol->locale)

  (set! scheme-report-environment 
    (lambda (version)
      (case version
        [(4)    (symbol->locale 'r4rs)]
        [(5)    (symbol->locale 'r5rs)]
        [else   (error 'scheme-report-environment 
                  "invalid report specifier ~s" version)])))

  (set! null-environment 
    (lambda (version)
      (case version
        [(4)    (symbol->locale 'r4rs-null)]
        [(5)    (symbol->locale 'r5rs-null)]
        [else   (error 'null-environment 
                  "invalid report specifier ~s" version)])))

  (set! interaction-environment
    (lambda ()
      (symbol->locale 'init)))

  (set! ieee-environment
    (lambda ()
      (symbol->locale 'ieee)))

) ;end package

