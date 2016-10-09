
;;;
;;; Word Frequency Counting example from "The SCHEME Programming Language"

(define (get-word p)
   (let ([c (read-char p)])
      (if (eq? (char-type c) 'letter)
	  (list->string
	     (let loop ([c c])
		(if (memq (char-type c) '(letter digit))
		    (cons c (loop (read-char p)))
		    (begin
		       (unread-char c p)
		       '()))))
	  c)))

(define (char-type c)
   (cond 
      [(eof-object? c) c]
      [(char-alphabetic? c) 'letter]
      [(char-numeric? c) 'digit]
      [else c]))

(define-structure (tree word) 
   ([left '()] [right '()] [count 1]))

(define (tree node word)
   (cond 
      [(null? node) (make-tree word)]
      [(string=? word (tree-word node))
       (set-tree-count! node (+ (tree-count node) 1))
       node]
      [(string<? word (tree-word node))
       (set-tree-left! node (tree (tree-left node) word))
       node]
      [else (set-tree-right! node (tree (tree-right node) word)) node]))

(define (tree-print node p)
   (unless (null? node)
      (tree-print (tree-left node) p)
      (fprintf p "~a ~a~%" (tree-count node) (tree-word node))
      (tree-print (tree-right node) p)))

(define (wc infn)
   (let ([ip (open-input-file infn)]
	   [op (current-output-port)])
      (let loop ([root '()])
	 (let ([w (get-word ip)])
	    (cond 
	       [(eof-object? w) (tree-print root op)]
	       [(string? w) (loop (tree root w))]
	       [else (loop root)])))
      (close-input-port ip)))

(time (wc "pickle"))
