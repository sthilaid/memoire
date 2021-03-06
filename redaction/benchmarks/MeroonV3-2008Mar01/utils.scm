;;; $Id: utils.scm,v 1.1 2005/02/25 22:19:37 lucier Exp $ 
;;; Copyright (c) 1990-96 by Christian Queinnec. All rights reserved.

;;;                ***********************************************
;;;                    Small, Efficient and Innovative Class System
;;;                                    Meroon 
;;;                              Christian Queinnec  
;;;                    Ecole Polytechnique & INRIA--Rocquencourt
;;;                ************************************************

;;; This file contains some utilities that must be present before
;;; loading other things. It might be the case that some of these functions
;;; already appear in your favorite Scheme implementation, then just
;;; set the appropriate features to eliminate them.

;;; A prologue for OScheme

(if-meroon-feature (or OScheme)
  (begin
    (require "lstutls")
    (set-load-path! (cons "~queinnec/DEA/Meroon-V3" (load-path)))
    ;(define trace-load 
    ;  (let ((orig-load load))
    ;    (lambda (filename)
    ;      (format (current-output-port) "Loading %0\n" filename)
    ;      (orig-load filename))))
    ;(set! load trace-load)   
    (define number->string integer->string)
    (define number? integer?) )
  #t )

;;; Returns true only if the predicate P is true on each term of the
;;; lists in args. Only used with a single list in Meroon

(define every? 
  (let ()
    (meroon-declare-not-inlinable)
    (lambda (p . args)
      (let andmap ((args args) (value #t))
        (if (let any-at-end? ((ls args))
              (and (pair? ls)
                   (or (not (pair? (car ls)))
                       (any-at-end? (cdr ls)))))
            value
            (let ((value (oo-apply p (oo-map car args))))
              (and value (andmap (oo-map cdr args) value)))))) ) )

;;; Returns true only if P is true on at least one term of the lists in args. 
;;; Only used once in the if-meroon-feature macro.

(define (any? p . args)
  (let ormap ((args args) (value #f))
    (if (let any-at-end? ((ls args))
          (and (pair? ls)
               (or (not (pair? (car ls)))
                   (any-at-end? (cdr ls)))))
        value
        (let ((value (apply p (map car args))))
          (or value (ormap (map cdr args) value))))))

;;; NOTE: It is now possible to use and and or specifiers in conditionalized
;;; definitions.

;;; Creates a new symbol. In fact, this is a weak imitation since
;;; these symbols might already exist and are falsifiable as well.
;;; MacScheme 1.5 gensym does not accept an argument.
;;; Test that feature with:
;;;     (gensym)

(if-meroon-feature (or bigloo gambit MacScheme scheme48)
    #t
    (define gensym
      (let ((counter 99))
        (lambda args
          (set! counter (fx+ counter 1))
          (symbol-concatenate (if (pair? args) (car args) 'G)
                              counter ) ) ) ) )

;;; A physical reverse.
;;; Define it here so it can be used by Fools for meroon-integer->string.

(if-meroon-feature (or Scheme->C slib elk gambit MacScheme 
                       scheme48 fools vscm guile )
    (define (reverse! l)
      (define (nreverse l r)
        (if (pair? l)
            (let ((cdrl (cdr l)))
              (set-cdr! l r)
              (nreverse cdrl l) )
            r ) )
      (nreverse l '()) )
    #t )

;;; number->string changed more than once in RnRS.

(if-meroon-feature (and PC-Scheme 3.03)
  ;; case of PC-Scheme 3.03
  (define (meroon-integer->string n)
    (number->string n '(int)) )
  ;; Keep this begin for fools which does not like a macro call to expand
  ;; into a form with the same macro.
  (begin
    ;; Case of Fools
    (if-meroon-feature fools
      (define (meroon-integer->string n)
        (let ((result '()))
          (define *output-base* 10)
          (define *number-images*
            (vector #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 
                    #\A #\B #\C #\D #\E #\F ) )
          (define (prin-integer n)
            (let ((p (fxquotient n *output-base*)))
              (when (fx> p 0) (prin-integer p))
              (set! result (cons (vector-ref *number-images* 
                                             (fxremainder n *output-base*) )
                                 result )) ) )
          (when (fx< n 0) 
            (set! result (cons #\- result))
            (set! n (fx- n)) )
          (prin-integer n)
          (list->string (reverse! result)) ) )
      ;; Normal case:
      (define meroon-integer->string number->string) ) ) )

;;; Forge a new symbol, the name of which is the concatenation of all
;;; these things. Does not seem worth memoizing this function.

(define symbol-concatenate 
  (let ()
    (meroon-declare-not-inlinable)
    (lambda names
      (string->symbol
       (oo-apply string-append
                 (oo-map (lambda (thing)
                           (cond ((symbol? thing) 
                                  (symbol->string thing) )
                                 ((string? thing) thing)
                                 ((number? thing)
                                  (meroon-integer->string thing) )
                                 (else (internal-meroon-error 
                                        "non coercible to String"
                                        thing )) ) )
                         names ) ) ) ) ) )

;;; A well known Common Lisp function

(define (identity x) x)

;;; Returns the list of numbers [start start+1 .... end [

(define (iota start end)
  (if (fx< start end)
      (cons start (iota (fx+ 1 start) end))
      '() ) )

;;; (apply f end '(e11 e12 ... e1n) '(e21 e22 ...) ... '(ep1 ... ep1n)) ->
;;; (f (f ... (f end e1n e2n ... epn) ... ) e11 e21 ... ep1)
;;; Renamed into meroon-reduce since Fools defines it with a binayr form 
;;; with end and list swapped.
;;; reduce already exists in MIT but is different.

(define (meroon-reduce f end list . lists)
  ;; F == (lambda (result item) result)
  (define (reduce1 f end l)
    (if (pair? l)
        (f (reduce1 f end (cdr l)) (car l))
        end ) )
  (define (reduce2 f end l1 l2)
    (if (and (pair? l1) (pair? l2))
        (f (reduce2 f end (cdr l1) (cdr l2)) (car l1) (car l2))
        end ) )
  (define (reduceN f end lists)
    (if (every? pair? lists)
        (oo-apply f (reduceN f end (oo-map cdr lists)) (oo-map car lists))
        end ) )
  (case (length lists)
    ((0) (reduce1 f end list))
    ((1) (reduce2 f end list (car lists)))
    (else (reduceN f end (cons list lists))) ) )

;;; Like append but with surgery.

(if-meroon-feature (or Scheme->C slib elk gambit MacScheme scheme48
                       vscm vanilla )
    (define (append! l1 l2)
      (cond ((pair? l1) 
             (if (pair? (cdr l1))
                 (append! (cdr l1) l2)
                 (set-cdr! l1 l2) )
             l1 )
            (else l2) ) )
    #t )

;;; Flatten a list.

(define (flat l)
  (define (flatten l r)
    (cond ((pair? l)
           (flatten (car l) (flatten (cdr l) r)) )
          ((null? l) r)
          (else (cons l r)) ) )
  (flatten l '()) )

(if-meroon-feature (or elk scheme48 vscm MIT mzscheme vanilla)
    (define (atom? e) 
      (not (pair? e)) )
    #t )

(if-meroon-feature (or elk slib gambit PC-Scheme scheme48 fools vscm
                       MIT vanilla mzscheme guile
                       (and bigloo 2.1) )
    (define (remove item list)
      (if (pair? list)
          (if (equal? item (car list))
              (cdr list)
              (cons (car list) (remove item (cdr list))) )
          list ) )
    #t )

(if-meroon-feature (or slib gambit scheme48 vscm mzscheme vanilla)
    (define (last-pair l)
      (if (pair? l)
          (let scan ((cdrl (cdr l)) (result l))
            (if (pair? cdrl)
                (scan (cdr cdrl) cdrl)
                result ) )
          (oo-error 'last-pair "Not a pair" l) ) )
    #t )

(if-meroon-feature (or Scheme->C slib gambit MacScheme scheme48 
                       MIT vscm vanilla guile )
    (define call/cc call-with-current-continuation)
    #t )

(if-meroon-feature (or (and PC-Scheme 3.03) MacScheme)
    (define (char-numeric? c)
      (and (char>=? c #\0) (char<=? c #\9)) )
    #t )

(if-meroon-feature (and MacScheme 1.5)
  (define modulo remainder)
  #f )

;;; map2 is needed in PC-scheme which only has a unary map.

(if-meroon-feature (and PC-scheme 3.03)
  (define (map2 fn l1 l2)
    (if (and (pair? l1) (pair? l2))
        (cons (fn (car l1) (car l2))
              (map2 fn (cdr l1) (cdr l2)) )
        '() ) )
  #t )

;;; Anomalies are normally caught by Meroon, displayed then given back
;;; to the underlying implementation using the oo-error function.

(if-meroon-feature (or elk bigloo MacScheme scheme48 guile)
    (define (oo-error within msg . culprits)
      (error within msg culprits) )
    #t )
(if-meroon-feature (or Scheme->C slib gambit)
    (define oo-error 
      (let ()
        (meroon-declare-not-inlinable)
        (lambda (within msg . culprits)
          (error 'Meroon msg) ) ) )
    #t )
(if-meroon-feature PC-Scheme
    (define (oo-error within msg . culprits)
      ;; ignore within parameter; error is a macro in PC Scheme
      (error msg culprits) )
    #t )
(if-meroon-feature MIT
    (define (oo-error within msg . culprits)
      (error-procedure within (cons msg culprits) '()) )
    #t )
(if-meroon-feature fools
  (define (oo-error within msg . culprits)
    (error within "~a ~a" msg culprits) )
  #f )
(if-meroon-feature vscm
  (define (oo-error . arguments)
    (error arguments) )
  #f )
(if-meroon-feature (or OScheme)
  (define (oo-error within msg . culprits)
    (error "Meroon error %0 %1 %2" within msg culprits) )
  #t )
(if-meroon-feature (or mzscheme)
  (define (oo-error within msg . culprits)
    (error msg within culprits) )
  #t )
(if-meroon-feature (or vanilla)
  (define (oo-error within msg . culprits)
    (newline)
    (write `(*** an error occurred ***
                 within ,within
                 reason is ,msg
                 culprits are ,@culprits ))
    (newline)
    (car oo-error) )
  #t )

;;; This function should never be called. It is only called in case of
;;; internal errors. It is defined there for the case it is called
;;; before the end of the bootstrap.

(define internal-meroon-error 
  (let ()
    (meroon-declare-not-inlinable)
    (lambda args
      (oo-error 'meroon "Internal error: should never be seen." args) ) ) )

;;; Create a Keyword from a symbol.

(define (make-meroon-keyword symbol)
  (string->meroon-keyword (symbol->string symbol)) )

;;; Convert a string into a keyword:

(if-meroon-feature (or gambit)
  (define string->meroon-keyword string->keyword)
  (if-meroon-feature (or bigloo)
    (define (string->meroon-keyword s)
      (string->keyword (string-append ":" s)) )
    (define (string->meroon-keyword s)
      (string->symbol (string-append ":" s)) ) ) )

;;; Make meroon macros to define macros pervasive themselves.
;;; See justification in the meroon.gsc prologue.

(if-meroon-feature (or pervasive-meroon-macros)
  (make-meroon-macro-pervasive)
  #f )

;;; end of utils.scm
