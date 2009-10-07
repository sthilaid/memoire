;;; $Id: vsg.tst,v 1.4 1995/02/28 14:01:13 queinnec Exp $
;;; Copyright (c) 1990-94 by Christian Queinnec. All rights reserved.
;;; Tests suite for vsg.scm

;;; Compose two costs into a single number. 
(define (regular-cost-function i j)
  (+ i j 1) )
   ---
;;; Remove indeterminacy for testing. This is an internal function
;;; that allows to choose the policy along which terms are selected in
;;; a Joint CStream when they have a similar cost.
(define (default-Joint-CStream-chooser o) #t)
   ---

;;; Test directly the construction of simple CStreams.
(begin
  (define q (make-Enumerated-CStream 1 '(a b c)))
  (CStream->list q 10) )
   (a b c)

(begin 
  (define u (make-joint-Cstream 
             q q default-Joint-CStream-chooser ))
  (CStream->list u 10) )
   (a b c a b c)

(CStream->list (make-joint-Cstream 
                u u default-Joint-CStream-chooser )
               40 )
   (A B C A B C A B C A B C)

(CStream->list (make-Product-CStream 
                q                       ; father
                q                       ; mother
                regular-cost-function
                cons )                  ; comp-fn
               12 )
  ((A . A) (A . B) (A . C) (B . C) (C . C) (B . B) (C . B) (B . A) (C . A))

(CStream->list
 (make-Product-CStream 
  q (make-Product-CStream
     q 
     q
     regular-cost-function
     cons )
  regular-cost-function
  cons )
 30 )
    ((A A . A) (A A . B) (A A . C) (A B . C) (A C . C) (A B . B) 
     (A C . B) (A B . A) (A C . A) (B C . A) (C C . A) (B B . A) 
     (C B . A) (B C . B) (C C . B) (B B . B) (C B . B) (B C . C) (C C . C)
     (B B . C) (C B . C) (B A . C) (C A . C) (B A . B) (C A . B) (B A . A) 
     (C A . A))

(begin (define qq (make-Relay-CStream q))
       (CStream->list qq 10) )
    (a b c)
(let ((qq (make-Relay-CStream
           (make-Relay-CStream
            (make-Relay-CStream
             q ) ) )))
  (CStream->list qq 10) )
    (a b c)

(let ((qq q))
  (CStream->list (make-Delayed-CStream 
                  (lambda (o) qq)
                  "No comment" ) 
                 12 ) )
   (a b c)

;;; The stream of all integers.
(define *integer-stream*
  (let ((i 0))
    (define (inc o)
      (set! i (+ i 1))
      (make-Normal-CStream 1 i o) )
    (make-Relay-CStream
     (make-Delayed-CStream inc "the naturals") ) ) )
   ---
(CStream->list *integer-stream* 5)
   (1 2 3 4)
(CStream->list *integer-stream* 5)
   (1 2 3 4)

(CStream->list (make-Kleene-CStream
                q                       ; source
                (lambda (i) (+ 1 i))    ; cost1-fn
                list                    ; comp1-fn
                regular-cost-function
                cons )                  ; comp2-fn
               12 )
   ((A) (B) (C) (A A) (A B) (A C) (B C) (C C) (B B) (C B) (B A))
(CStream->list (make-Kleene-CStream 
                q                       ; source
                (lambda (i) (+ i 1))
                list                    ; comp1-fn
                regular-cost-function
                cons )                  ; comp2-fn
               100 )
   ((A) (B) (C) (A A) (A B) (A C) (B C) (C C) (B B) (C B) (B A) (C A) 
    (A A A) (A A B) (A A C) (A B C) (A C C) (A B B) (A C B) (A B A) (A C A) 
    (B C A) (C C A) (B B A) (C B A) (B C B) (C C B) (B B B) (C B B) (B C C)
    (C C C) (B B C) (C B C) (B A C) (C A C) (B A B) (C A B) (B A A) (C A A)
    (A A A A) (A A A B) (A A A C) (A A B C) (A A C C) (A A B B) (A A C B)
    (A A B A) (A A C A) (A B C A) (A C C A) (A B B A) (A C B A) (A B C B)
    (A C C B) (A B B B) (A C B B) (A B C C) (A C C C) (A B B C) (A C B C) 
    (A B A C) (A C A C) (A B A B) (A C A B) (A B A A) (A C A A) (B C A A)
    (C C A A) (B B A A) (C B A A) (B C A B) (C C A B) (B B A B) (C B A B)
    (B C A C) (C C A C) (B B A C) (C B A C) (B C B C) (C C B C) (B B B C)
    (C B B C) (B C C C) (C C C C) (B B C C) (C B C C) (B C B B) (C C B B)
    (B B B B) (C B B B) (B C C B) (C C C B) (B B C B) (C B C B) (B C B A)
    (C C B A) (B B B A) (C B B A) (B C C A))

(CStream->list (make-Concatenated-CStream q q) 10)
   (a b c a b c)

;;; Testing the build-Recursive-CStream syntax.
(CStream->list (build-Recursive-CStream self
                 (enum 'a 'b 'c) )
               10 )
   (a b c)

;;; Similar to (Kleene q) ie sequences of A, B and Cs.
(define v
  (build-recursive-CStream self
     (Kleene q) ) )
   ---
(CStream->list v 1)
   ()
(CStream->list v 2)
   ((a))
(CStream->list v 3)
  ((a)(b))
(CStream->list v 4)
  ((a)(b)(c))
(CStream->list v 12)
   ((A) (B) (C) (A A) (A B) (A C) (B C) (C C) (B B) (C B) (B A))

(CStream->list (build-recursive-CStream self
                 (letrec ((v q)) `(plus ,v ,v)) ) 
               10 )
  ((PLUS A A) (PLUS B B) (PLUS C C))

(define uu (build-recursive-CStream self
             (letrec ((w q)
                      (v q) )
               `(plus ,v ,w) ) ))
   ---
(CStream->list uu 10)
   ((PLUS A A) (PLUS B A) (PLUS C A) (PLUS C B) (PLUS C C) (PLUS B B) 
    (PLUS B C) (PLUS A B) (PLUS A C))

(CStream->list (build-recursive-CStream self
                (letrec ((v q)
                         (w q)
                         (u q) )
                  `(plus ,v ,w ,u) ) ) 
               30 )
   ((PLUS A A A) (PLUS A B A) (PLUS A C A) (PLUS B C A) (PLUS C C A)
    (PLUS B B A) (PLUS C B A) (PLUS B A A) (PLUS C A A) (PLUS C A B)
    (PLUS C A C) (PLUS B A B) (PLUS B A C) (PLUS C B B) (PLUS C B C)
    (PLUS B B B) (PLUS B B C) (PLUS C C B) (PLUS C C C) (PLUS B C B)
    (PLUS B C C) (PLUS A C B) (PLUS A C C) (PLUS A B B) (PLUS A B C) 
    (PLUS A A B) (PLUS A A C))

(define v
    (build-recursive-CStream self
      (++ q
          (+ (Kleene self)
             (letrec ((variables (Kleene q))
                      (body self) )
               `(lambda ,variables ,body) ) ) ) ) )
   ---
(CStream->list v 50)
   (A B C (A) (B) (C) ((A)) ((B)) ((C)) (((A))) (((B))) (((C))) (A A)
    (A B) (A C) (B C) (C C) (B B) (C B) (B A) (C A) (LAMBDA (A) A) 
    (LAMBDA (A) B) (LAMBDA (A) C)(LAMBDA (B) C) (LAMBDA (C) C)
    (LAMBDA (B) B) (LAMBDA (C) B) (LAMBDA (B) A) (LAMBDA (C) A) 
    ((((A)))) ((((B)))) ((((C)))) ((A A)) ((A B)) ((A C)) ((B C))
    ((C C))((B B)) ((C B)) ((B A)) ((C A)) ((LAMBDA (A) A)) 
    ((LAMBDA (A) B)) ((LAMBDA (A) C)) ((LAMBDA (B) C)) ((LAMBDA (C) C))
    ((LAMBDA (B) B)) ((LAMBDA (C) B)))

(CStream->list
    (build-recursive-CStream self
      (letrec ((exp1 (++ q (* exp1 exp1) ))) 
        exp1 ) )
    50 )
     (A B C (A . A) (A . B) (A . C) (B . C) (C . C) (B . B) (C . B) 
      (B . A) (C . A)(A A . A) (A A . B) (A A . C) (A B . C) (A C . C)
      (A B . B) (A C . B) (A B . A)(A C . A) (B C . A) (C C . A) (B B . A)
      (C B . A) (B C . B) (C C . B) (B B . B)(C B . B) (B C . C) (C C . C)
      (B B . C) (C B . C) (B A . C) (C A . C) (B A . B)(C A . B) (B A . A)
      (C A . A) ((A . A) . C) ((A . B) . C) ((A . C) . C) ((B . C) . C)
      ((C . C) . C) ((B . B) . C) ((C . B) . C) ((B . A) . C) ((C . A) . C)
      ((A . A) . B))

(CStream->list 
  (build-recursive-CStream self
                 (++ q (* self self)) )
  50 )
   (A B C (A . A) (A . B) (A . C) (B . C) (C . C) (B . B) (C . B) (B . A)
      (C . A) (A A . A) (A A . B) (A A . C) (A B . C) (A C . C) (A B . B)
      (A C . B) (A B . A) (A C . A) (B C . A) (C C . A) (B B . A) (C B . A) 
      (B C . B) (C C . B) (B B . B) (C B . B) (B C . C) (C C . C) (B B . C) 
      (C B . C) (B A . C) (C A . C) (B A . B) (C A . B) (B A . A) (C A . A)
      ((A . A) . C) ((A . B) . C) ((A . C) . C) ((B . C). C) ((C . C) . C)
      ((B . B) . C) ((C . B) . C) ((B . A) . C) ((C . A) . C) ((A . A) . B))

(CStream->list
    (build-recursive-CStream self
      (letrec ((exp1 (++ q (* exp1 exp1) ))
               (exp2 (Kleene exp1)) ) 
        `(,exp1 ,exp2) ) )
    50 )
   ((A (A)) (A (B)) (A (C)) (B (C)) (C (C)) (B (B)) (C (B)) (B (A)) (C (A))
    (A ((A . A))) (A ((A . B))) (A ((A . C))) (A ((B . C)))
    (A ((C . C))) (A ((B . B))) (A((C . B))) (A ((B . A))) (A ((C . A)))
    (A (A A)) (A (A B)) (A (A C)) (A (B C)) (A (C C)) (A (B B)) (A (C B))
    (A (B A)) (A (C A)) (B (C A)) (C (C A)) (B (B A)) (C (B A)) (B (C B))
    (C (C B)) (B (B B)) (C (B B)) (B (C C)) (C (C C)) (B (B C)) (C (B C))
    (B (A C)) (C (A C)) (B (A B)) (C (A B)) (B (A A)) (C (A A))
    (B ((C . A))) (C ((C . A))) (B ((B . A))) (C ((B . A))))

;;;====================================================== Example
;;; A big example: the Scheme grammar.
;;; Put in the VAR stream below the names you want to use.

(define Scheme-CStream
  (build-Recursive-CStream 
   self
   (let ((var (enum 'x 'y 't 'car 'set-car! 'cons)))
     (++ var
         (+ (Kleene self)
            (letrec ((body self))
              `(begin . ,body) )
            (letrec ((form self))
              `(quote ,form) )
            (letrec ((c self)
                     (th self)
                     (el self) )
              `(if ,c ,th ,el) )
            (letrec ((v var)
                     (form self) )
              `(set! ,v ,form) )
            (letrec ((variables (Kleene var))
                     (body self) )
              `(lambda ,variables ,body) ) ) ) ) ) )
   ---
(CStream->list Scheme-CStream 10)
     (X Y T CAR SET-CAR! CONS (X) (Y) (T))
(CStream->list Scheme-CStream 50)
   (X Y T CAR SET-CAR! CONS (X) (Y) (T) (CAR) (SET-CAR!) (CONS)
    (BEGIN . X) (BEGIN . Y) (BEGIN . T) (BEGIN . CAR) (BEGIN . SET-CAR!)
    (BEGIN . CONS) (QUOTE X) (QUOTE Y) (QUOTE T) (QUOTE CAR) (QUOTE SET-CAR!)
    (QUOTE CONS) ((X)) ((Y)) ((T)) ((CAR)) ((SET-CAR!)) ((CONS)) ((BEGIN . X))
    ((BEGIN . Y)) ((BEGIN . T)) ((BEGIN . CAR)) ((BEGIN . SET-CAR!))
    ((BEGIN . CONS)) ((QUOTE X)) ((QUOTE Y)) ((QUOTE T)) ((QUOTE CAR))
    ((QUOTE SET-CAR!)) ((QUOTE CONS)) (BEGIN X) (BEGIN Y) (BEGIN T)
    (BEGIN CAR) (BEGIN SET-CAR!) (BEGIN CONS) (BEGIN BEGIN . X))
(CStream->list Scheme-CStream 100)
   (X Y T CAR SET-CAR! CONS (X) (Y) (T) (CAR) (SET-CAR!) (CONS)
    (BEGIN . X) (BEGIN . Y) (BEGIN . T) (BEGIN . CAR) (BEGIN . SET-CAR!)
    (BEGIN . CONS) (QUOTE X) (QUOTE Y) (QUOTE T) (QUOTE CAR) (QUOTE SET-CAR!)
    (QUOTE CONS) ((X)) ((Y)) ((T)) ((CAR)) ((SET-CAR!)) ((CONS)) ((BEGIN . X))
    ((BEGIN . Y)) ((BEGIN . T)) ((BEGIN . CAR)) ((BEGIN . SET-CAR!))
    ((BEGIN . CONS)) ((QUOTE X)) ((QUOTE Y)) ((QUOTE T)) ((QUOTE CAR))
    ((QUOTE SET-CAR!)) ((QUOTE CONS)) (BEGIN X) (BEGIN Y) (BEGIN T)
    (BEGIN CAR) (BEGIN SET-CAR!) (BEGIN CONS) (BEGIN BEGIN . X) 
    (BEGIN BEGIN . Y) (BEGIN BEGIN . T) (BEGIN BEGIN . CAR)
    (BEGIN BEGIN . SET-CAR!) (BEGIN BEGIN . CONS) (BEGIN QUOTE X)
    (BEGIN QUOTE Y) (BEGIN QUOTE T) (BEGIN QUOTE CAR)
    (BEGIN QUOTE SET-CAR!) (BEGIN QUOTE CONS) (QUOTE (X))
    (QUOTE (Y)) (QUOTE (T)) (QUOTE (CAR)) (QUOTE (SET-CAR!))
    (QUOTE (CONS)) (QUOTE (BEGIN . X)) (QUOTE (BEGIN . Y))
    (QUOTE (BEGIN . T)) (QUOTE (BEGIN . CAR)) (QUOTE (BEGIN . SET-CAR!))
    (QUOTE (BEGIN . CONS)) (QUOTE (QUOTE X)) (QUOTE (QUOTE Y))
    (QUOTE (QUOTE T)) (QUOTE (QUOTE CAR)) (QUOTE (QUOTE SET-CAR!))
    (QUOTE (QUOTE CONS)) (SET! X X) (SET! X Y) (SET! X T) (SET! X CAR)
    (SET! X SET-CAR!) (SET! X CONS) (SET! Y CONS) (SET! T CONS)
    (SET! CAR CONS) (SET! SET-CAR! CONS) (SET! CONS CONS) (SET! Y SET-CAR!)
    (SET! T SET-CAR!) (SET! CAR SET-CAR!) (SET! SET-CAR! SET-CAR!)
    (SET! CONS SET-CAR!) (SET! Y CAR) (SET! T CAR) (SET! CAR CAR)
    (SET! SET-CAR! CAR) (SET! CONS CAR))

(count-terms/cost scheme-cstream 5)
; 0 terms of cost 0.
; 5 terms of cost 1.
; 17 terms of cost 2.
; 89 terms of cost 3.   <- immediate
; 665 terms of cost 4.  <- roughly 30 seconds
; 4805 terms of cost 5. <- roughly 2 minutes if compiled (8 if interpreted).
;;; Now everything is immediate.
(count-terms/cost scheme-cstream 5)
   ---

;;; end of vsg.tst
