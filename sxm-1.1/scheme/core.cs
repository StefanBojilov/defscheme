
;;;
;;; Core.cs -- esl
;;; 

;;;
;;; this file is loaded in the "core sxm" mode, i.e. it should
;;; be the first file loaded into the image-less interpreter

;;;
;;; Note: stack- and ac/stack- forms were used throughout this
;;; file to increase speed and save bytecode space. If you are
;;; going to modify the code, named-lambda/let/define are safe
;;; choices

;;;
;;; I. Integrable functions for internal compiler

(optimize-level 4)
(integrable-procedures (make-hash-table 41))

(set! install-integrable-procedure! 
   (ac/stack-lambda (install-integrable-procedure! sym idef) 
      ((stack-lambda (#f proc #!rest) 
          (begin 
             (%set-mutability! proc #f) 
             (hash-table-put! (integrable-procedures) proc idef))) 
       (symbol->gcell sym))))

(set! integrable-procedure? 
   (stack-lambda (integrable-procedure? s #!optional loc) 
      (if (symbol? s) 
          (if (> (optimize-level) 1) 
              (hash-table-get (integrable-procedures)
                        (symbol->gcell s loc)) 
              #f) 
          #f)))

(for-each 
   (stack-lambda (#f pdef) 
      (install-integrable-procedure! (car pdef) (cdr pdef))) 
   (quote ((identity-procedure 1 () (60) (43)) 
           (void 0 () (60) (43)) 
           (make-promise 1 (65) (109) (65 43)) 
           (eq? 2 (67) (67 60) (113)) 
           (char=? 2 (67) (67 60) (113)) 
           (null? 1 (68) (68 60) (114)) 
           (not 1 (69) (69 60) (44)) 
           (cons 2 (70) (99) (45)) 
           (set-car! 2 (73) (73 60) (73 43)) 
           (set-cdr! 2 (74) (74 60) (74 43)) 
           (default-object? 1 (85) (85 60) (119)) 
           (memq 2 (94) (108) (118)) 
           (memv 2 (53) (132) (54)) 
           (car 1 (71) (100) (71 43)) 
           (cdr 1 (72) (101) (72 43)) 
           (caar 1 (71 71) (71 100) (71 71 43)) 
           (cadr 1 (72 71) (72 100) (72 71 43)) 
           (cdar 1 (71 72) (71 101) (71 72 43)) 
           (cddr 1 (72 72) (72 101) (72 72 43)) 
           (caaar 1 (71 71 71) (71 71 100) (71 71 71 43)) 
           (caadr 1 (72 71 71) (72 71 100) (72 71 71 43)) 
           (cadar 1 (71 72 71) (71 72 100) (71 72 71 43)) 
           (caddr 1 (72 72 71) (72 72 100) (72 72 71 43)) 
           (cdaar 1 (71 71 72) (71 71 101) (71 71 72 43)) 
           (cdadr 1 (72 71 72) (72 71 101) (72 71 72 43)) 
           (cddar 1 (71 72 72) (71 72 101) (71 72 72 43)) 
           (cdddr 1 (72 72 72) (72 72 101) (72 72 72 43)) 
           (caaaar 1 (71 71 71 71) (71 71 71 100) (71 71 71 71 43)) 
           (caaadr 1 (72 71 71 71) (72 71 71 100) (72 71 71 71 43)) 
           (caadar 1 (71 72 71 71) (71 72 71 100) (71 72 71 71 43)) 
           (caaddr 1 (72 72 71 71) (72 72 71 100) (72 72 71 71 43)) 
           (cadaar 1 (71 71 72 71) (71 71 72 100) (71 71 72 71 43)) 
           (cadadr 1 (72 71 72 71) (72 71 72 100) (72 71 72 71 43)) 
           (caddar 1 (71 72 72 71) (71 72 72 100) (71 72 72 71 43)) 
           (cadddr 1 (72 72 72 71) (72 72 72 100) (72 72 72 71 43)) 
           (cdaaar 1 (71 71 71 72) (71 71 71 101) (71 71 71 72 43)) 
           (cdaadr 1 (72 71 71 72) (72 71 71 101) (72 71 71 72 43)) 
           (cdadar 1 (71 72 71 72) (71 72 71 101) (71 72 71 72 43)) 
           (cdaddr 1 (72 72 71 72) (72 72 71 101) (72 72 71 72 43)) 
           (cddaar 1 (71 71 72 72) (71 71 72 101) (71 71 72 72 43)) 
           (cddadr 1 (72 71 72 72) (72 71 72 101) (72 71 72 72 43)) 
           (cdddar 1 (71 72 72 72) (71 72 72 101) (71 72 72 72 43)) 
           (cddddr 1 (72 72 72 72) (72 72 72 101) (72 72 72 72 43)) 
           (atom? 1 (133 1 69) (133 1 69 60) (133 1 44)) 
           (pair? 1 (133 1) (133 1 60) (134 1)) 
           (symbol? 1 (133 2) (133 2 60) (134 2)) 
           (fixnum? 1 (133 3) (133 3 60) (134 3))
           (flonum? 1 (133 4) (133 4 60) (134 4))
           ;add integer?
           (string? 1 (133 5) (133 5 60) (134 5)) 
           (port? 1 (133 6) (133 6 60) (134 6)) 
           (vector? 1 (133 7) (133 7 60) (134 7)) 
           (closure? 1 (133 8) (133 8 60) (134 8)) 
           (code? 1 (133 9) (133 9 60) (134 9)) 
           (built-in-procedure? 1 (133 10) (133 10 60) (134 10)) 
           (char? 1 (133 14) (133 14 60) (134 14)) 
           ;15 reserved
           (promise? 1 (133 16) (133 16 60) (134 16)) 
           (hash-table? 1 (133 17) (133 17 60) (134 17)) 
           (weak-pair? 1 (133 18) (133 18 60) (134 18)) 
           (keyword? 1 (133 19) (133 19 60) (134 19)) 
           (box? 1 (133 20) (133 20 60) (134 20)) 
           (handle? 1 (133 25) (133 25 60) (134 25)) 
           (boolean? 1 (133 26) (133 26 60) (134 26)) 
           (gcell? 1 (133 28) (133 28 60) (134 28)) 
           (vector-ref 2 (135) (136) (135 43)) 
           (vector-set! 3 (137) (137 60) (137 43)) 
           (vector-length 1 (138) (138 60) (138 43)) 
           (+ 2 (75) (102) (75 43)) 
           (- 2 (76) (103) (76 43)) 
           (* 2 (77) (104) (77 43)) 
           (quotient 2 (78) (105) (78 43)) 
           (1+ 1 (79) (106) (79 43)) 
           (-1+ 1 (80) (107) (80 43)) 
           (< 2 (81) (81 60) (115)) 
           (= 2 (82) (82 60) (116)) 
           (> 2 (83) (83 60) (117)) 
           (list -1 (124 -1) (125 -1) (124 -1 43)) 
           (list* -1 (139 -1) (140 -1) (139 -1 43))
           (>= 2 (81 69) (81 69 60) (81 44)) 
           (<= 2 (83 69) (83 69 60) (83 44)) 
           (zero? 1 (142) (142 60) (143)) 
           (string-ref 2 (150) (151) (150 43)) 
           (string-set! 3 (152) (152 60) (152 43)) 
           (string-length 1 (153) (153 60) (153 43)) 
)))

;;
;; now the table is filled; internal compiler will use it to
;; inline function calls in all subsequent expressions...

;;;
;;; II. Syntax extensions (Part 1)

;;;
;;; a general syntax facility employing Expansion Passing Style technique
;;; (see R. Kent Dybvig, Daniel P. Friedman, and Cristopher T. Haynes
;;; "Expansion-Passing Style: A General Macro Mechanism", Lisp and
;;; Symbolic Computation, 1(1), June 1988).

;;
;; this file is loaded while SXM compiler (0.16@0.08e3.99 or later) understands
;; core forms only; all language extensions are added step-by-step so that the
;; previously added extension could affect the compilation of the next one.
;;
;;  The following grammar summarizes the core syntactic forms.
;;
;;         <core>   -->    <identifier>
;;                      |  <constant>
;;                      |  (if <core> <core> <core>)
;;                      |  (set! <identifier> <core>)
;;                      |  (begin <core> <core> ...)
;;                      |  (quote <object>)
;;                      |  (named-lambda <parameters> <core>)
;;                      |  (stack-lambda <parameters> <core>)
;;                      |  (ac/stack-lambda <parameters> <core>)
;;                      |  (funcall <core> <core> ...)
;;                      |  (<core> <core> ...)
;;
;;   <parameters>   -->    ( <name>
;;                           <identifier> ...
;;                           [#!optional <identifier> ...]
;;                           [#!rest <identifier>]
;;                           [#!aux <identifier> ...] )
;;

;; NOTE: Top-level defines could be replaced by set! forms because of shallow
;; binding mechanism being used to implement top level environment.

;;
;; procedure to report syntactic errors


(set! display-error-message
   (stack-lambda (display-error-message message)
      (begin
        (display message (console-output-port))
        (write-char '#\space (console-output-port)))))

(set! syntax-error
   (stack-lambda (syntax-error object #!rest messages)
      (begin
         (newline (console-output-port))
         (display-error-message "Syntax Error:")
         (map display-error-message messages)
         (display-error-message "
happened in:")
         (write object (console-output-port))
         (newline (console-output-port))
         (reset))))


;;
;; expander database: New version uses top-level-value slots!

(set! *expander-tag* (string->uninterned-symbol '"Macro->"))

(set! install-expander! 
   (stack-lambda (install-expander! k e) 
      (set-top-level-value! k (cons *expander-tag* e))))

(set! %expander-function
   (stack-lambda (%expander-function k)
      (if (pair? (top-level-value k)) ;in current locale?
          (if (eq? (car (top-level-value k)) *expander-tag*)
              (cdr (top-level-value k))
              '#f)
          '#f)))

(set! expander? %expander-function)

(set! expander-function
   (stack-lambda (expander-function k)
     ((stack-lambda (#f x)
         (if (procedure? x)
             x
             (internal-error '"bad expander for ~s ~s" k x)))
      (%expander-function k))))

;;
;; expansion functions (loc argument is reserved for locales)

(set! eps-expand 
   (stack-lambda (eps-expand x #!optional loc)
      (initial-expander x initial-expander)))

(set! eps-expand-once 
   (named-lambda (eps-expand-once x #!optional loc)
      (initial-expander x (ac/stack-lambda (#f x e) x))))

(set! initial-expander
   (named-lambda (initial-expander x e)
      (if (symbol? x)
          (if (expander? x)
              (syntax-error x '"reserved keyword used as variable")
              x)
          (if (atom? x)
              (if (keyword? x)
                  (e (list 'quote x) e)
                  x)
              (if (symbol? (car x))
                  (if (expander? (car x))
                      ((expander-function (car x)) x e)
                      (map (stack-lambda (#f x) (e x e)) x))
                  (map (stack-lambda (#f x) (e x e)) x))))))

;;
;; expanders with more traditional protocol: transformers

(set! install-transformer!
   (named-lambda (install-transformer! k t)
      (install-expander! k (stack-lambda (#f x e) (e (t x) e)))))

;;
;; expanders for canonical representation of core forms

(install-expander! 'quote
   (ac/stack-lambda (quote==> x e)
      x))

(install-expander! 'named-lambda
   (stack-lambda (named-lambda==> x e)
      (list 'named-lambda (cadr x) (e (caddr x) e))))

(install-expander! 'stack-lambda
   (stack-lambda (stack-lambda==> x e)
      (list 'stack-lambda (cadr x) (e (caddr x) e))))

(install-expander! 'ac/stack-lambda
   (stack-lambda (ac/stack-lambda==> x e)
      (list 'ac/stack-lambda (cadr x) (e (caddr x) e))))

(install-expander! 'if
   (stack-lambda (if==> x e)
      (list 'if
            (e (cadr x) e)
            (e (caddr x) e)
            (e (if (pair? (cdddr x))
                   (cadddr x)
                   '(void)) e))))

(install-expander! 'set!
   (stack-lambda (set!==> x e)
      (list 'set! (cadr x) (e (caddr x) e))))

(install-expander! 'begin
   (named-lambda (begin==> x e)
      (cons 'begin (map (stack-lambda (#f x) (e x e)) (cdr x)))))

(install-expander! 'funcall
   (named-lambda (funcall==> x e)
      (cons 'funcall (map (stack-lambda (#f x) (e x e)) (cdr x)))))

;;
;; time to make eps-expand current expander and redefine
;; current evaluator to use it...

(current-expand eps-expand)

(current-eval
  (stack-lambda (eval x #!optional loc)
    ((compile-core (expand x loc) loc))))

;;
;; redefined evaluator compiles core language correctly because all core form
;; expanders are already installed; now we can install expanders to add new
;; forms (or enhance old ones) and use them just after installation; some
;; expanders are installed in simplified form and/or without syntax checks
;; to bootstrap the following correct ones

;;
;; `define' is the same as `set!' but for inessential lambda - definition
;; syntax; internal definitions are detected by `define' keyword

(install-transformer! 'define
   ;; standard definition
   (stack-lambda (define=> x)
      (if (pair? (cadr x))
          (list 'set! (caadr x) (cons 'named-lambda (cdr x)))
          (if (null? (cddr x)) 
              '(void) ;forward definition: leave value unbound
              (list 'set! (cadr x) (caddr x))))))

;;
;; expander for `named-lambda' with internal definitions, standard arguments,
;; and implicit `begin'

((named-lambda (#f #!aux scan-internal-definitions convert-lambda-list)
   (begin
     (set! scan-internal-definitions
        (stack-lambda (scan-internal-definitions body vars)
           (if (null? body)
               vars
               (if (atom? (car body))
                   vars
                   (if (not (eq? (caar body) 'define))
                       vars
                       (scan-internal-definitions 
                         (cdr body)
                         (cons (if (pair? (cadar body)) 
                                   (caadar body)
                                   (cadar body))
                               vars)))))))
     (set! convert-lambda-list
        (stack-lambda (convert-lambda-list ll xx)
           (if (null? ll)
               (if (null? xx) '() (cons '#!aux xx))
               (if (atom? ll)
                   (if (null? xx)
                       (list '#!rest ll) 
                       (cons '#!rest (cons ll (cons '#!aux xx))))
                   (if (eq? (car ll) '#!aux)
                       (cons (car ll) 
                             (convert-lambda-list (append xx (cdr ll)) '()))
                       (cons (car ll) 
                             (convert-lambda-list (cdr ll) xx)))))))

     ;; named-lambda with internal definitions and implicit begin
     (install-expander! 'named-lambda
        (stack-lambda (named-lambda==> x e)
           (list 'named-lambda
                 (convert-lambda-list 
                    (cadr x)
                    (scan-internal-definitions (cddr x) '()))
                 (e (if (null? (cdddr x)) 
                        (caddr x) 
                        (cons 'begin (cddr x))) e))))

   )))

;;
;; transformer for standard `lambda'

(install-transformer! 'lambda
   (stack-lambda (lambda=> x)
      (cons 'named-lambda (cons (cons #f (cadr x)) (cddr x)))))

;;
;; transformer for simplified `let' - named `let' is not supported here

(install-transformer! 'let
   (stack-lambda (let=> x)
      (cons (cons 'named-lambda (cons (cons #f (map car (cadr x))) (cddr x)))
            (map cadr (cadr x)))))

;;
;; hack: transformers for internal usage only (be careful with them!)

(install-transformer! 'stack-let
   (stack-lambda (stack-let=> x)
      (cons (list 'stack-lambda
                  (cons #f (append (map car (cadr x)) '(#!rest)))
                  (cons 'begin (cddr x)))
            (map cadr (cadr x)))))

(install-transformer! 'stack-define
   ;; top-level only!
   (stack-lambda (stack-define=> x)
      (if (pair? (cadr x))
          (list 'set! 
                (caadr x) 
                (list 'stack-lambda (cadr x) (cons 'begin (cddr x))))
          (syntax-error x "STACK-DEFINE"))))
          
(install-transformer! 'ac/stack-define
   ;; top-level only!
   (stack-lambda (ac/stack-define=> x)
      (if (pair? (cadr x))
          (list 'set! 
                (caadr x) 
                (list 'ac/stack-lambda (cadr x) (cons 'begin (cddr x))))
          (syntax-error x "AC/STACK-DEFINE"))))

;;
;; now we have R^3.##RS `define', `lambda', and simplified `let' forms; the 
;; next step is to add common conditional constructs such as essential forms 
;; `and', `or', and simplified `cond' to form Scheme subset that is rich 
;; enough for more complex extensions

(install-transformer! 'and
   (stack-lambda (and=> x)
      (if (null? (cdr x))
          #t
          (if (null? (cddr x))
              (cadr x)
              (list 'if (cadr x) (cons 'and (cddr x)) #f)))))


(install-transformer! 'or
   (stack-lambda (or=> x)
      (if (null? (cdr x))
          #f
          (if (null? (cddr x))
              (cadr x)
              (let ((var (gensym))) ;we have let!
                 (list (list 'ac/stack-lambda (list #f var)
                           (list 'if var var (cons 'or (cddr x))))
                       (cadr x)))))))

(install-transformer! 'cond
   ;; simplified cond without => and single-predicate clauses
   (stack-lambda (cond=> x)
      (if (null? (cdr x))
          #f
          (if (eq? (caadr x) 'else)
              (cons 'begin (cdadr x))
              (list 'if (caadr x) 
                    (cons 'begin (cdadr x)) (cons 'cond (cddr x)))))))


;;;
;;; III. Syntax extensions (Part 2: Quasiquote)
;;; -- John Armstrong

;;;
;;; expanders for standard `quasiquote', `unquote', and `unquote-splicing'
;;; forms

(define *check-qq-expansion* #t) ;do checking after expansion

(install-expander! 'quasiquote
 (named-lambda (quasiquote==> x e)
   (define append-me-sym
      ;; must be a gensym to avoid capture in pathological situations
      (gensym))
   (define qq-lev 0) ;always >= 0
   (define (qq-car-cdr exp)
      (let ([qq-car (qq (car exp))]
            [qq-cdr (qq (cdr exp))])
         (if (and (pair? qq-car) (eq? (car qq-car) append-me-sym))
             (list 'append (cdr qq-car) qq-cdr)
             (list 'cons qq-car qq-cdr))))
   (define (qq exp)
      (cond 
         [(symbol? exp) (list 'quote exp)]
         [(vector? exp) (list 'list->vector (qq (vector->list exp)))]
         [(atom? exp) (list 'quote exp)] ;other atomic constants
         [(eq? (car exp) 'quasiquote)
          (set! qq-lev (1+ qq-lev))
          (let ([qq-val
                 (if (= qq-lev 1) ;min val after inc
                     ;; --> outermost level
                     (qq (cadr exp))
                     (qq-car-cdr exp))])
             (set! qq-lev (-1+ qq-lev))
             qq-val)]
         [(or (eq? (car exp) 'unquote) (eq? (car exp) 'unquote-splicing))
          (set! qq-lev (-1+ qq-lev))
          (let ([qq-val
                 (if (= qq-lev 0) ; min val
                     ;; --> outermost level
                     (if (eq? (car exp) 'unquote-splicing)
                         (cons append-me-sym (e (cadr exp) e))
                         (e (cadr exp) e))
                     (qq-car-cdr exp))])
             (set! qq-lev (1+ qq-lev))
             qq-val)]
         [else (qq-car-cdr exp)]))
   (define (check-qq-expansion exp)
      (cond
         [(vector? exp) (check-qq-expansion (vector->list exp))]
         [(atom? exp) #f]
         [else
          (if (eq? (car exp) append-me-sym)
              (syntax-error (list 'unquote-splicing (cdr exp))
                            "UNQUOTE-SPLICING in unspliceable position")
              (or (check-qq-expansion (car exp))
                  (check-qq-expansion (cdr exp))))]))
   (let ([expansion (qq x)])
      (if *check-qq-expansion*
          (check-qq-expansion expansion)) ;error on failure
      expansion)))

(install-expander! 'unquote
   (stack-lambda (unquote==> x e)
      (syntax-error x "UNQUOTE outside QUASIQUOTE")))

(install-expander! 'unquote-splicing
   (stack-lambda (unquote-splicing=> x e)
      (syntax-error x "UNQUOTE-SPLICING outside QUASIQUOTE")))


;;;
;;; IV. Syntax extensions (Part 3: more R^4RS expanders)

;;;
;;; expanders for derived expression types as specified in R^4RS with 
;;; reasonable syntactic checking (expanded version of rewrite rules);
;;; syntactic closures and lexical scope for macros are not supported

;;
;; this part should be loaded when simple expanders for basic syntax
;; forms `lambda', `define', `set!', `begin', `if', `quote', and
;; `quasiquote' are already defined. Current expanders for `cond', 
;; `let', `and', `or' do not support full ^4 syntax; exact versions 
;; are installed here

;;
;; syntax-match? procedure is used to choose among clauses and to check for 
;; syntactic errors (it is a part of extend-syntax package by R. Kent Dybvig)

(define (syntax-match? pat exp)
    (or (eq? pat '*)
        (eq? exp pat)
        (and (pair? pat)
             (cond
              [(and (eq? (car pat) '$)
                    (pair? (cdr pat))
                    (null? (cddr pat)))
               (eq? exp (cadr pat))]
              [(and (pair? (cdr pat))
                    (eq? (cadr pat) '...)
                    (null? (cddr pat)))
               (let ([pat (car pat)])
                 (define (f lst)
                   (or (null? lst)
                       (and (pair? lst)
                            (syntax-match? pat (car lst))
                            (f (cdr lst)))))  
                 (f exp))]
              [else
               (and (pair? exp)
                    (syntax-match? (car pat) (car exp))
                    (syntax-match? (cdr pat) (cdr exp)))]))))


;;
;; now we can define transformers for all R^4RS derived syntax forms;
;; reasonable syntax checks are provided

(install-transformer! 'letrec
   (named-lambda (letrec=> x)
      (cond 
         [(syntax-match? '(letrec ([* *] ...) * * ...) x)
          `((lambda (#!aux ,@(map car (cadr x)))
              ,@(map (stack-lambda (#f b)
                        `(set! ,(car b) ,(cadr b)))
                     (cadr x))
              ,@(cddr x)))]
         [else (syntax-error x "LETREC")])))

(install-transformer! 'let
   (stack-lambda (let=> x)
      (cond 
         [(syntax-match? '(let ([* *] ...) * * ...) x)
          `((lambda ,(map car (cadr x))
               ,@(cddr x))
            ,@(map cadr (cadr x)))]
         [(syntax-match? '(let * ([* *] ...) * * ...) x)
          `((letrec ([,(cadr x)
                      (named-lambda ,(cons (cadr x) (map car (caddr x)))
                         ,@(cdddr x))])
               ,(cadr x)) ,@(map cadr (caddr x)))]
         [else (syntax-error x "LET")])))

(install-transformer! 'let*
   (stack-lambda (let*=> x)
      (cond 
         [(syntax-match? '(let* () * * ...) x)
          `(let ()
              ,@(cddr x))]
         [(syntax-match? '(let* ([* *] * ...) * * ...) x)
          `(let ([,(caaadr x) ,(car (cdaadr x))])
              (let* (,@(cdadr x))
                 ,@(cddr x)))]
         [else (syntax-error x "LET*")])))

(install-transformer! 'and
   (stack-lambda (and=> x)
      (cond 
         [(syntax-match? '(and) x) '#t]
         [(syntax-match? '(and *) x) (cadr x)]
         [(syntax-match? '(and * * ...) x)
          `(if ,(cadr x) (and ,@(cddr x)) #f)]
         [else (syntax-error x "AND")])))

(install-transformer! 'or
   (stack-lambda (or=> x)
      (cond 
         [(syntax-match? '(or) x) '#f]
         [(syntax-match? '(or *) x) (cadr x)]
         [(syntax-match? '(or * * ...) x)
          (stack-let ([v (gensym)])
             ;; `(let ([,v ,(cadr x)]) (if ,v ,v (or ,@(cddr x))))
             ;; hand-optimized version:
             `((ac/stack-lambda (or=> ,v)
                 (if ,v ,v (or ,@(cddr x))))
               ,(cadr x)))]
         [else (syntax-error x "OR")])))

;; non-standard but useful (used by `cond' expander)
(install-transformer! 'apply-if
   (stack-lambda (apply-if=> x)
      (cond 
         [(syntax-match? '(apply-if * * *) x)
          (stack-let ([v (gensym)])
             ;;`(let ([,v ,(cadr x)])
             ;;    (if ,v (,(caddr x) ,v) ,(cadddr x)))
             ;; hand-optimized version:
             `((ac/stack-lambda (apply-if=> ,v)
                 (if ,v (,(caddr x) ,v) ,(cadddr x)))
               ,(cadr x)))]
         [else (syntax-error x "APPLY-IF")])))

(install-transformer! 'cond
   (stack-lambda (cond=> x)
      (cond 
         [(syntax-match? '(cond) x) '(void)]
         [(syntax-match? '(cond [else * * ...]) x)
          `(begin
              ,(cadadr x)
              ,@(cddadr x))]
         [(syntax-match? '(cond [*] * ...) x)
          `(or ,(caadr x) (cond ,@(cddr x)))]
         [(syntax-match? '(cond [* => *] * ...) x)
          `(apply-if ,(caadr x) ,(car (cddadr x)) (cond ,@(cddr x)))]
         [(syntax-match? '(cond [* * * ...] * ...) x)
          `(if ,(caadr x)
               (begin ,@(cdadr x))
               (cond ,@(cddr x)))]
         [else (syntax-error x "COND")])))

(install-transformer! 'case
   (stack-lambda (case=> x)
      (cond 
         [(and (syntax-match? '(case * * ...) x) (pair? (cadr x)))
          (stack-let ([y (gensym)])
             `(let ([,y ,(cadr x)])
                 (case ,y ,@(cddr x))))]
         [(syntax-match? '(case *) x) 
          '(void)]
         [(syntax-match? '(case * [else * * ...]) x)
          `(begin ,@(cdaddr x))]
         [(syntax-match? '(case * [* * * ...] * ...) x)
          (stack-let ([z (cond [(symbol? (caaddr x)) `eq?]
                               [(atom? (caaddr x)) `eqv?]
                               [else `memv])])
             `(if (,z ,(cadr x) (quote ,(caaddr x)))
                  (begin
                     ,@(cdaddr x))
                  (case ,(cadr x) ,@(cdddr x))))]
         [else (syntax-error x "CASE")])))

(install-transformer! 'do
   (named-lambda (do=> x)
      (cond 
         [(syntax-match? '(do ([* * . *] ...) (* * ...) * ...) x)
          (stack-let ([loop (gensym)]
                      [step (map (stack-lambda (#f v s)
                                    (if (null? s) v (car s)))
                                 (map car (cadr x))
                                 (map cddr (cadr x)))])
             (if (not (syntax-match? '(* ...) step))
                 (syntax-error step "DO step"))
             `(letrec ([,loop
                        (lambda ,(map car (cadr x))
                           (if ,(caaddr x)
                               (begin
                                  ,@(if (null? (cdaddr x)) 
                                        '((void))
                                        (cdaddr x)))
                               (begin
                                  ,@(cdddr x)
                                  (,loop ,@step))))])
                 (,loop ,@(map cadr (cadr x)))))]
         [else (syntax-error x "DO")])))


(install-transformer! 'delay
   (stack-lambda (delay=> x)
      (cond 
         [(syntax-match? '(delay *) x)
          `(make-promise (lambda () ,(cadr x)))]
         [else (syntax-error x "DELAY")])))


;;;
;;; V. Syntax extensions (Part 4: extend-syntax)

;;;
;;; Copyright (C) 1987 R. Kent Dybvig
;;; Permission to copy this software, in whole or in part, to use this
;;; software for any lawful purpose, and to redistribute this software
;;; is granted subject to the restriction that all copies made of this
;;; software must include this copyright notice in full.
;;;
;;; The basic design of extend-syntax is due to Eugene Kohlbecker.  See
;;; "E. Kohlbecker: Syntactic Extensions in the Programming Language Lisp",
;;; Ph.D.  Dissertation, Indiana University, 1986."  The structure of "with"
;;; pattern/value clauses, the method for compiling extend-syntax into
;;; Scheme code, and the actual implementation are due to Kent Dybvig.

;;
;; extend-syntax transformer is defined in the end of this file

;;
;; the transformer for `rewrite-rules' is defined in the body of the `let'

(let ()

   (define (id name access control)
      (list name access control))
   (define id-name car)
   (define id-access cadr)
   (define id-control caddr)

   (define (loop)
      (box '()))
   (define loop-ids unbox)
   (define loop-ids! set-box!)

   (define c...rs
      '((car caar . cdar)
        (cdr cadr . cddr)
        (caar caaar . cdaar)
        (cadr caadr . cdadr)
        (cdar cadar . cddar)
        (cddr caddr . cdddr)
        (caaar caaaar . cdaaar)
        (caadr caaadr . cdaadr)
        (cadar caadar . cdadar)
        (caddr caaddr . cdaddr)
        (cdaar cadaar . cddaar)
        (cdadr cadadr . cddadr)
        (cddar caddar . cdddar)
        (cdddr cadddr . cddddr)))

   (define (add-car access)
      (let ([x (and (pair? access) (assq (car access) c...rs))])
         (if x `(,(cadr x) ,@(cdr access)) `(car ,access))))

   (define (add-cdr access)
      (let ([x (and (pair? access) (assq (car access) c...rs))])
         (if x `(,(cddr x) ,@(cdr access)) `(cdr ,access))))

   (define (duplicates vars)
      (cond 
         [(null? vars) '()]
         [(memq (car vars) (cdr vars)) vars]
         [else (duplicates (cdr vars))]))

   (define (checkpat keys pat exp)
      (let ([vars
             (let f ([x pat]
                     [vars '()])
                (cond 
                   [(pair? x)
                    (if (and (pair? (cdr x))
                             (eq? (cadr x) '...)
                             (null? (cddr x)))
                        (f (car x) vars)
                        (f (car x) (f (cdr x) vars)))]
                   [(symbol? x)
                    (cond 
                       [(memq x keys) vars]
                       [(or (eq? x 'with) (eq? x '...))
                        (syntax-error exp "invalid context for" x)]
                       [else (cons x vars)])]
                   [else vars]))])
         (let ([dupls (duplicates vars)])
            (if (pair? dupls)
                (syntax-error exp
                       "duplicate pattern variable name"
                       (car dupls))))))

   (define (parse keys pat acc cntl ids)
      (cond 
         [(symbol? pat)
          (if (memq pat keys) ids (cons (id pat acc cntl) ids))]
         [(pair? pat)
          (cons (id pat acc cntl)
                (if (equal? (cdr pat) '(...))
                    (let ([x (gensym)])
                       (parse keys (car pat) x (id x acc cntl) ids))
                    (parse keys
                           (car pat)
                           (add-car acc)
                           cntl
                           (parse keys (cdr pat) (add-cdr acc) cntl ids))))]
         [else ids]))

   (define (pattern-variable? sym ids)
      (memq sym (map id-name ids)))

   (define (gen keys exp ids loops qqlev)
      (cond 
         [(symbol? exp)
          (let ([id (lookup exp ids)])
             (if (false? id)
                 exp
                 (begin
                    (add-control! (id-control id) loops)
                    (list 'unquote (id-access id)))))]
         [(not (pair? exp)) exp]
         [else
          (cond 
             [(and (syntax-match? '(quasiquote *) exp)
                   (not (pattern-variable? 'quasiquote ids)))
              (list 'unquote
                    (list 'list
                          ''quasiquote
                          (make-quasi (gen keys (cadr exp) ids loops
                                           (if (= qqlev 0)
                                               0
                                               (+ qqlev 1))))))]
             [(and (syntax-match? '(* *) exp)
                   (memq (car exp) '(unquote unquote-splicing))
                   (not (pattern-variable? (car exp) ids)))
              (list 'unquote
                    (if (= qqlev 1)
                        (gen-quotes keys (cadr exp) ids loops)
                        (list 'list
                              (list 'quote (car exp))
                              (make-quasi (gen keys
                                               (cadr exp)
                                               ids
                                               loops
                                               (- qqlev 1))))))]
             [(and (eq? (car exp) 'with)
                   (not (pattern-variable? 'with ids)))
              (if (not (syntax-match? '(with ((* *) ...) *) exp))
                  (syntax-error exp "WITH"))
              (checkpat keys (map car (cadr exp)) exp)
              (list 'unquote
                    (gen-with keys
                              (map car (cadr exp))
                              (map cadr (cadr exp))
                              (caddr exp)
                              ids
                              loops))]
             [(and (pair? (cdr exp)) (eq? (cadr exp) '...))
              (let ([x (loop)])
                 (gen-cons (list 'unquote-splicing
                                 (make-loop x
                                            (gen keys
                                                 (car exp)
                                                 ids
                                                 (cons x loops)
                                                 qqlev)))
                           (gen keys (cddr exp) ids loops qqlev)))]
             [else
              (gen-cons (gen keys (car exp) ids loops qqlev)
                        (gen keys (cdr exp) ids loops qqlev))])]))

   (define (gen-cons head tail)
      (if (null? tail)
          (if (syntax-match? '(unquote-splicing *) head)
              (list 'unquote (cadr head))
              (cons head tail))
          (if (syntax-match? '(unquote *) tail)
              (list head (list 'unquote-splicing (cadr tail)))
              (cons head tail))))

   (define (gen-with keys pats exps body ids loops)
      (let ([temps (map (lambda (x) (gensym)) pats)])
         `(let (,@(map (lambda (t e) `[,t ,(gen-quotes keys e ids loops)])
                       temps
                       exps))
             ,@(let f ([ps pats] [ts temps])
                  (if (null? ps)
                      (let f ([pats pats] [temps temps] [ids ids])
                         (if (null? pats)
                             `(,(make-quasi (gen keys body ids loops 0)))
                             (f (cdr pats)
                                (cdr temps)
                                (parse '() (car pats) (car temps) '() ids))))
                      (let ([m (match-pattern '() (car ps))])
                         (if (eq? m '*)
                             (f (cdr ps) (cdr ts))
                             `((if (not (syntax-match? ',m ,(car ts)))
                                   (syntax-error ,(car ts)
                                          "WITH pattern mismatch"
                                          ',(car ps)))
                               ,@(f (cdr ps) (cdr ts))))))))))

   (define (gen-quotes keys exp ids loops)
      (cond 
         [(syntax-match? '(quote *) exp)
          (make-quasi (gen keys (cadr exp) ids loops 0))]
         [(syntax-match? '(quasiquote *) exp)
          (make-quasi (gen keys (cadr exp) ids loops 1))]
         [(pair? exp)
          (let f ([exp exp])
             (if (pair? exp)
                 (cons (gen-quotes keys (car exp) ids loops) (f (cdr exp)))
                 (gen-quotes keys exp ids loops)))]
         [else exp]))

   (define (lookup exp ids)
      (let loop ([ls ids])
         (cond 
            [(null? ls) #f]
            [(equal? (id-name (car ls)) exp) (car ls)]
            [(subexp? (id-name (car ls)) exp) #f]
            [else (loop (cdr ls))])))

   (define (subexp? exp1 exp2)
      (and (symbol? exp1)
           (let f ([exp2 exp2])
              (or (eq? exp1 exp2)
                  (and (pair? exp2)
                       (or (f (car exp2))
                           (f (cdr exp2))))))))

   (define (add-control! id loops)
      (if (not (null? id))
          (begin
             (if (null? loops)
                 (syntax-error 'rewrite-rules "missing ellipsis in expansion"))
             (let ([x (loop-ids (car loops))])
                (if (not (memq id x))
                    (loop-ids! (car loops) (cons id x))))
             (add-control! (id-control id) (cdr loops)))))

   (define (make-loop loop body)
      (let ([ids (loop-ids loop)])
         (if (null? ids)
             (syntax-error 'rewrite-rules "extra ellipsis in expansion"))
         (cond 
            [(equal? body (list 'unquote (id-name (car ids))))
             (id-access (car ids))]
            [(and (null? (cdr ids))
                  (syntax-match? '(unquote (* *)) body)
                  (eq? (cadadr body) (id-name (car ids))))
             `(map ,(caadr body) ,(id-access (car ids)))]
            [else
             `(map (lambda ,(map id-name ids) ,(make-quasi body))
                   ,@(map id-access ids))])))

   (define (match-pattern keys pat)
      (cond 
         [(symbol? pat)
          (if (memq pat keys) 
              (if (memq pat '(* $ ...)) `($ ,pat) pat) 
              '*)]
         [(pair? pat)
          (if (and (pair? (cdr pat))
                   (eq? (cadr pat) '...)
                   (null? (cddr pat)))
              `(,(match-pattern keys (car pat)) ...)
              (cons (match-pattern keys (car pat))
                    (match-pattern keys (cdr pat))))]
         [else pat]))

   (define (make-quasi exp)
      (if (and (pair? exp) (eq? (car exp) 'unquote))
          (cadr exp)
          (list 'quasiquote exp)))

   (define (make-clause keys cl x)
      (cond 
         [(syntax-match? '(* * *) cl)
          (let ([pat (car cl)]
                [fender (cadr cl)]
                [exp (caddr cl)])
             (checkpat keys pat pat)
             (let ([ids (parse keys pat x '() '())])
                `((and (syntax-match? ',(match-pattern keys pat) ,x)
                       ,(gen-quotes keys fender ids '()))
                  ,(make-quasi (gen keys exp ids '() 0)))))]
         [(syntax-match? '(* *) cl)
          (let ([pat (car cl)]
                [exp (cadr cl)])
             (checkpat keys pat pat)
             (let ([ids (parse keys pat x '() '())])
                `((syntax-match? ',(match-pattern keys pat) ,x)
                  ,(make-quasi (gen keys exp ids '() 0)))))]
         [else (syntax-error 'rewrite-rules "invalid clause" cl)]))

   (install-transformer! 'rewrite-rules
      (named-lambda (rewrite-rules=> x)
         (cond
            [(and (syntax-match? '(rewrite-rules (* * ...) * ...) x)
                  (symbol? (caadr x))
                  (not (memq '... (cadr x))))
             (let ([n (string->symbol 
                         (string-append (symbol->string (caadr x)) "=>"))]
                   [y (gensym)])
                `(named-lambda (,n ,y)
                    (cond
                       ,@(map (lambda (c) (make-clause (cadr x) c y)) (cddr x))
                       [else (syntax-error ,y ',(caadr x))])))]
            [else (syntax-error x "REWRITE-RULES")])))

) ;end of `let' body

(install-transformer! 'with
   (named-lambda (with=> x)
      (syntax-error x "WITH outside REWRITE-RULES")))

;;
;; Ye olde true extend-syntax

(install-transformer! 'extend-syntax
   (rewrite-rules (extend-syntax)
     [(extend-syntax (t k ...) r ...)
      (install-transformer! 't
         (rewrite-rules (t k ...) r ...))]))

;;
;; fake define-syntax and syntax-rules 
;; (useful for simple, portable macros)

(extend-syntax (define-syntax syntax-rules)
   [(define-syntax m (syntax-rules (k ...) r ...))
    (extend-syntax (m k ...) r ...)])

(install-transformer! 'syntax-rules
   (named-lambda (syntax-rules=> x)
      (syntax-error x "SYNTAX-RULES outside DEFINE-SYNTAX")))

;;;
;;; VI. Syntax extensions (Part 5: Extra syntax forms)

;;;
;;; transformers for extra syntax forms defined in various Scheme 
;;; dialects and/or useful for SXM purposes

(extend-syntax (when)
   [(when e1 e2 ...)
    (if e1 (begin e2 ...))])

(extend-syntax (unless)
   [(unless e1 e2 ...)
    (if e1 (void) (begin e2 ...))])

(extend-syntax (while)
   [(while cnd e1 e2 ...)
    (with ([loop (gensym)])
       (let loop () (when cnd e1 e2 ... (loop))))])

(extend-syntax (until)
   [(until cnd e1 e2 ...)
    (with ([loop (gensym)])
       (let loop () (unless cnd e1 e2 ... (loop))))])

(extend-syntax (do-list)
   [(do-list (var lst) . body)
    (do-list (var lst (void)) . body)]
   [(do-list (var lst res) ex1 ex2 ...)
    (symbol? 'var)
    (with ([loop (gensym)] [tail (gensym)])
       (let loop ([tail lst])
          (let ([var (if (pair? tail) (car tail) #f)])
             (if (pair? tail)
                 (begin ex1 ex2 ... (loop (cdr tail)))
                 res))))])

(extend-syntax (do-times)
   [(do-times (var cnt) . body)
    (do-times (var cnt (void)) . body)]
   [(do-times (var cnt res) ex1 ex2 ...)
    (symbol? 'var)
    (with ([loop (gensym)] [top (gensym)])
       (let loop ([var 0] [top cnt])
          (if (< var top)
              (begin ex1 ex2 ... (loop (1+ var) top))
              res)))])

(extend-syntax (repeat)
   [(repeat cnt e1 e2 ...)
    (with ([loop (gensym)] [var (gensym)])
       (let loop ([var cnt])
          (when (> var 0) e1 e2 ... (loop (-1+ var)))))])

(extend-syntax (fluid-let)
  [(fluid-let ([x v] ...) exp1 exp2 ...)
   (with ([tmp (gensym)] [(t ...) (map (lambda (x) (gensym)) '(x ...))])
      (let ([tmp (void)] [t v] ...)
         (dynamic-wind
            (lambda () (begin (set! tmp x) (set! x t) (set! t tmp)) ...)
            (lambda () exp1 exp2 ...)
            (lambda () (begin (set! tmp t) (set! t x) (set! x tmp)) ...))))])

(extend-syntax (let-values)
   [(let-values (ll e) e1 e2 ...)
    (call-with-values (lambda () e) (lambda ll e1 e2 ...))])

(extend-syntax (set-values!)
   [(set-values! (x ...) e)
    (with ([(t ...) (map (lambda (x) (gensym)) '(x ...))])
       (call-with-values
          (lambda () e)
          (lambda (t ...) (set! x t) ...)))])

(extend-syntax (catch)
   [(catch var exp ...)
    (symbol? 'var)
    (call-with-current-continuation
       (lambda (var) exp ...))])

;;;
;;; rewrite rules for `define-structure' macro as defined in "The Scheme 
;;; Programming Language" (1st edition) by R. Kent Dybvig

(extend-syntax (define-structure)
  [(define-structure (name id1 ...))
   (define-structure (name id1 ...) ())]
  [(define-structure (name id1 ...) ([id2 val] ...))
   (with ([constructor
           (string->symbol (string-append "make-" (symbol->string 'name)))]
          [predicate
           (string->symbol (string-append (symbol->string 'name) "?"))]
          [(access ...)
           (map (stack-lambda (#f x)
                   (string->symbol
                      (string-append (symbol->string 'name) "-" 
                                     (symbol->string x))))
                '(id1 ... id2 ...))]
          [(assign ...)
           (map (stack-lambda (#f x)
                   (string->symbol
                      (string-append "set-" (symbol->string 'name) "-"
                                            (symbol->string x) "!")))
                '(id1 ... id2 ...))]
          [count (length '(name id1 ... id2 ...))])
     (with ([(index ...)
             (let f ([i 1])
                (if (= i 'count)
                    '()
                    (cons i (f (+ i 1)))))])
       (begin
          (define constructor
             (named-lambda (constructor id1 ...)
                (let* ([id2 val] ...)
                   (vector 'name id1 ... id2 ...))))
          (define predicate
             (stack-lambda (predicate obj)
                (and (vector? obj)
                     (= (vector-length obj) count)
                     (eq? (vector-ref obj 0) 'name))))
             (define access
                (stack-lambda (access obj)
                  (vector-ref obj index)))
          ...
          (define assign
             (stack-lambda (assign obj newval)
             (vector-set! obj index newval)))
          ...)))])


(extend-syntax (variant-case else)
  [(variant-case var) 
   (signal-condition (vector 'variant-case-mismatch var))]
  [(variant-case var (else exp1 exp2 ...)) 
   (begin exp1 exp2 ...)]
  [(variant-case exp clause ...)
   (not (symbol? 'exp))
   (with ([var (gensym)])
      (let ([var exp]) (variant-case var clause ...)))]
  [(variant-case var (name (field ...) exp1 exp2 ...) clause ...)
   (with ([predicate
           (string->symbol (string-append (symbol->string 'name) "?"))]
          [(access ...)
           (map (stack-lambda (#f x)
                   (string->symbol
                      (string-append (symbol->string 'name) "-" 
                                     (symbol->string x))))
                '(field ...))])
      (if (predicate var)
          (let ([field (access var)] ...) exp1 exp2 ...)
          (variant-case var clause ...)))])


;;;
;;; VII. Functions not implemented in C for various reasons

;; strings

(ac/stack-define (string-upcase str) 
   (string-upcase! (string-copy str)))

(ac/stack-define (string-downcase str) 
   (string-downcase! (string-copy str)))

(ac/stack-define (string-capitalize str) 
   (string-capitalize! (string-copy str)))

;; formatting

(stack-define (sprintf format #!rest objects)
   (stack-let ([port (open-port string-output)])
      (apply fprintf (cons port (cons format objects)))
      (get-output-string port)))

;;;
;;; end of core.cs

