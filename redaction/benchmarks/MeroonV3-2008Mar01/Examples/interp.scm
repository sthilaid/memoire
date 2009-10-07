;;; $Id: interp.scm,v 1.2 1994/10/07 08:58:01 queinnec Exp $

;;; This file defines a Scheme interpreter written in Object-Oriented
;;; style. It only serves as a benchmarking program to appreciate the
;;; speed of various variants or ports of Meroon. It may also serve
;;; as an example of a simple program that may be written with Meroon.

;;; This bench defines 16 classes, 4 generic functions and 15 methods
;;; (in addition to the classes and generic functions brought by
;;; Meroon (approx 14 and 20)). When run, it calls ...

;;; NOTE: Don't use the word environment which is used in MIT-Scheme
;;; in make-environment!

;;;ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; The interpreter itself.

;;; The store is implicit. Environment are represented by Alists,
;;; Errors are rudimentary. 

;;; Representations of environment, stack frames and other runtime objects.

(define-class value Object ())
(define-class function value (variables body env))
(define-class primitive value (name address))

(define-class environ Object ())
(define-class null-env environ ())
(define-class full-env environ (others name))
(define-class variable-env full-env (value))

(define-class continuation Object (k))
(define-class if-cont continuation (et ef r))
(define-class set!-cont continuation (n r))
(define-class begin-cont continuation (e* r))
(define-class evfun-cont continuation (e* r))
(define-class apply-cont continuation (f r))
(define-class argument-cont continuation (e* r))
(define-class gather-cont continuation (v))
(define-class bottom-cont continuation (f))

;;; Naming rules for variables.
;;; e, ec, et, ef     Program
;;; e*                sequence of Programs
;;; r                 Env 
;;; k                 Cont
;;; v                 Value
;;; v*                sequence of Values
;;; n                 Id
;;; f                 Fun

(define (evaluate e r k)
  (if (not (pair? e))
    (cond ((symbol? e) (evaluate-variable e r k))
          (else (evaluate-quote e r k)) )
    (case (car e)
     ((quote) (evaluate-quote (cadr e) r k))
     ((if) (evaluate-if (cadr e) (caddr e) (cadddr e) r k))
     ((begin) (evaluate-begin (cdr e) r k))
     ((set!) (evaluate-set! (cadr e) (caddr e) r k))
     ((lambda) (evaluate-lambda (cadr e) (cddr e) r k))
     (else (evaluate-application (car e) (cdr e) r k)) ) ) )

(define (evaluate-variable n r k)
  (lookup r n k) )

(define (evaluate-quote v r k)
  (resume k v) )

(define (evaluate-if ec et ef r k)
  (evaluate ec r (make-if-cont k et ef r)) )

(define (evaluate-set! n e r k)
  (evaluate e r (make-set!-cont k n r)) )

(define (evaluate-lambda n* e* r k)
  (resume k (make-function n* e* r)) )

(define (evaluate-begin e* r k)
  (if (pair? e*)
    (if (pair? (cdr e*))
      (evaluate (car e*) r (make-begin-cont k e* r))
      (evaluate (car e*) r k) )
    (resume k empty-begin-value) ) )

(define (evaluate-application e e* r k)
  (evaluate e r (make-evfun-cont k e* r)) )

(define-generic (invoke (f) v* r k)
  (newline)
  (display "error in invoke : cannot apply : ")
  (show f)
  (wrong "not a function" f r k) )

(define-method (invoke (f primitive) v* r k)
  ((primitive-address f) v* r k) )

(define-method (invoke (f function) v* r k)
  (let ((env (extend-env (function-env f)
                         (function-variables f)
                         v* )))
    (evaluate-begin (function-body f) env k) ) )

(define-method (invoke (f continuation) v* r k)
  (if (= 1 (length v*))
      (resume f (car v*))
      (wrong "Continuations expect one argument" v* r k) ) )

(define (evaluate-arguments e* r k)
  (if (pair? e*)
    (evaluate (car e*) r (make-argument-cont k e* r))
    (resume k no-more-arguments) ) )

(define-generic (resume (k) v)
  (newline)
  (display "error in resume : Not a continuation : ")
  (show k)
  (wrong "Unknown continuation" k) )

(define-method (resume (k if-cont) v)
  (evaluate (if v (if-cont-et k) (if-cont-ef k)) 
            (if-cont-r k)
            (if-cont-k k) ) )

(define-method (resume (k begin-cont) v)
  (evaluate-begin (cdr (begin-cont-e* k)) 
                  (begin-cont-r k) 
                  (begin-cont-k k) ) )

(define-method (resume (k evfun-cont) f)
  (evaluate-arguments (evfun-cont-e* k)
                      (evfun-cont-r k)
                      (make-apply-cont (evfun-cont-k k)
                                       f 
                                       (evfun-cont-r k) ) ) )

(define-method (resume (k apply-cont) v)
  (invoke (apply-cont-f k) 
	  v
	  (apply-cont-r k)
	  (apply-cont-k k) ) )

(define-method (resume (k argument-cont) v)
  (evaluate-arguments (cdr (argument-cont-e* k)) 
                      (argument-cont-r k)
                      (make-gather-cont (argument-cont-k k) v)) )

(define-method (resume (k gather-cont) v*)
  (resume (gather-cont-k k) (cons (gather-cont-v k) v*)) )

(define-method (resume (k bottom-cont) v)
  ((bottom-cont-f k) v) )

(define-generic (lookup (r) n k)
  (newline)
  (display "error in lookup : Not an environment : ")
  (show r)
  (wrong "not an environment" r n k) )

(define-method (lookup (r null-env) n k)
  (wrong "Unknown variable" n r k) )

(define-method (lookup (r variable-env) n k)
  (if (eq? n (variable-env-name r))
    (resume k (variable-env-value r))
    (lookup (variable-env-others r) n k) ) )

(define-method (resume (k set!-cont) v)
  (update! (set!-cont-r k) (set!-cont-n k) (set!-cont-k k) v) )

(define-generic (update! (r) n k v)
  (newline)
  (display "error in lookup : Not an environment : ")
  (show r)
  (wrong "not an environment" r n k) )

(define-method (update! (r null-env) n k v)
  (wrong "Unknown variable" n r k) )

(define-method (update! (r variable-env) n k v)
  (if (equal? n (variable-env-name r))
    (begin (set-variable-env-value! r v)
           (resume k v) )
    (update! (variable-env-others r) n k v) ) )

(define (extend-env env names values)
  (cond ((pair? names)
         (if (pair? values)
             (make-variable-env  
              (extend-env env (cdr names) (cdr values))
              (car names)
              (car values) )
             (wrong "too less values" names) ) )
        ((null? names)
         (if (pair? values)
             (wrong "Too much values" values)
             env ) )
        (else (make-variable-env env names values)) ) )

;;; Some constant settings.

;;; The value of (begin) which is in fact not specified in Scheme
;;; since this is not legal syntax.

(define empty-begin-value 
   '() )

;;; The marker that marks the end of a list of values.

(define no-more-arguments
   '() )

;;; should never appear

(define (internal-error msg . culprits)
  (wrong "Internal error" msg culprits) )

;;; The initial environment.  Two macros allow to enrich the initial
;;; environment.  This file is also prepared as other Meroon source
;;; files to remove these macros.

(define-internal-meroon-macro (definitial name value)
  `(begin (set! r.init (make-variable-env r.init ',name ,value))
          ',name ) )

(define-internal-meroon-macro (defprimitive name value arity)
  `(definitial ,name 
     (make-primitive 
      ',name
      (lambda (v* r k) 
        (if (= ,arity (length v*))
            (resume k (apply ,value v*))
            (wrong "Incorrect arity" ',name v*) ) ) ) ) )

(define r.init (make-null-env))

;;; Define a lot of primitives.

(defprimitive cons cons 2)
(defprimitive car car 1)
(defprimitive cdr cdr 1)
(defprimitive pair? pair? 1)
(defprimitive symbol? symbol? 1)
(defprimitive eq? eq? 2)
(defprimitive null? null? 1)
(defprimitive set-car! set-car! 2)
(defprimitive set-cdr! set-cdr! 2)
(defprimitive + + 2)
(defprimitive - - 2 )
(defprimitive = = 2)
(defprimitive < < 2)
(defprimitive > > 2)
(defprimitive * * 2)
(defprimitive <= <= 2)
(defprimitive >= >= 2)
(defprimitive remainder remainder 2)

(definitial call/cc
    (make-primitive 
     'call/cc
      (lambda (v* r k) 
        (if (= 1 (length v*))
            (invoke (car v*) (list k) r k)
            (wrong "Incorrect arity" 'call/cc v*) ) ) ) )

(definitial apply
  (make-primitive
   'apply
   (lambda (v* r k)
     (if (>= (length v*) 2)
         (if (or (function? (car v*)) 
                 (primitive? (car v*))
                 (continuation? (car v*)) )
             (invoke (car v*)
                     (let conc ((args (cdr v*)))
                       (if (pair? (cdr args))
                           (cons (car args) (conc (cdr args)))
                           (car args) ) )
                     r k )
             (wrong "Not a function" 'apply (car v*)) )
         (wrong "Incorrect arity" 'apply v*) ) ) ) )

(definitial list
  (make-primitive
   'list
   (lambda (v* r k)
     (resume k v*) ) ) )

(definitial t #t)
(definitial f #f)
(definitial nil '())

;;;ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; This interpreter can be test with non regresion tests suite.  The
;;; global environment is not extendible so prepare a few free global
;;; variables. They are not necessary for the bench but since they are
;;; there and slower the lookup for global variables, leave them there
;;; to keep comparable results.

(definitial x 'void)
(definitial y 'void)
(definitial z 'void)
(definitial a 'void)
(definitial b 'void)
(definitial c 'void)
(definitial foo 'void)
(definitial bar 'void)
(definitial hux 'void)
(definitial fib 'void)
(definitial fact 'void)
(definitial visit 'void)
(definitial length 'void)
(definitial primes 'void)
(definitial qsort 'void)
(definitial merge 'void)
(definitial separate 'void)
(definitial kappend 'void)
(definitial wait 'void)
(definitial queens 'void)
(definitial foreach 'void)
(definitial check 'void)
(definitial check-others 'void)
(definitial search-one 'void)
(definitial iota 'void)
(definitial memq 'void)

;;; How to run and test this interpreter.
;;; These functions require the presence of tester.scm

;(define (schfrm)
;  (interpreter 
;   "SchFrm? " 
;   "SchFrm= "
;   #t
;   (lambda (read display error)
;     (set! wrong error)
;     (lambda ()
;       (evaluate (read) 
;                 r.init 
;                 (make-bottom-cont 'void
;                                   (lambda (v) 
;                                     (display v) )) ) ) ) ) )

;;; Functions for non regression tests

;(define (test-schfrm echo?)
;  (display `(start test-schfrm))
;  (suite-test-schfrm "scheme-tests.scm" echo?) )

;(define (suite-test-schfrm file echo?)
;  (suite-test 
;   file "SchFrm? " "SchFrm= " echo?
;   (lambda (read checker error)
;     (set! wrong error)
;     (lambda ()
;       (evaluate (read) 
;                 r.init 
;                 (make-bottom-cont 'void checker) ) ) )
;   equal? ) )

;;; You can run this interpreter with (schfrm)
;;; You can test it on a non regression test suite with (test-schfrm #t)

;;;oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; How to run this bench.

(define (bench-oo times e)
  (do ((i times (- i 1)))
      ((<= i 0) #f)
    (evaluate e
              r.init
              (make-bottom-cont 'void 
                                (lambda (v)
                                  (display v)
                                  (newline) ) ) ) ) )

;;; This is the expression to evaluate *iterations* times. It contains
;;; (fib 10), (fact 20) and Eratosthene's sieve up to 40.

(define *iterations* 10)

(define *bench*
  '((lambda (fib fact primes)
      (set! fib
            (lambda (n)
              (if (<= n 2) 1
                  (+ (fib (- n 1)) (fib (- n 2))) ) ) )
      (set! fact
            (lambda (n)
              ((lambda (factint)
                 (begin (set! factint
                              (lambda (n f)
                                (if (< n 2) 1
                                    (* n (f (- n 1) f)) ) ) )
                        (factint n factint) ) )
               'wait ) ) )
      (set! primes
            (lambda (n f max)
              ((lambda (filter)
                 (begin
                   (set! filter (lambda (p)
                                  (lambda (n) (= 0 (remainder n p))) ))
                   (if (> n max)
                       '()
                       (if (f n)
                           (primes (+ n 1) f max)
                           (cons n
                                 ((lambda (ff)
                                    (primes (+ n 1)
                                            (lambda (p)
                                              (if (f p) t (ff p)) )
                                            max ) )
                                  (filter n) ) ) ) ) ) )
               'wait ) ) )
      ;; The core of the bench
      (fib 10)
      (fact 20)
      (primes 2 (lambda (x) f) 40) )
    ;; bogus initial values of local variables
    'fib 'fact 'primes ) )

(define (start-bench)
  (write `(*** Meroon benchmark *iterations* = ,*iterations*))
  (newline)
  (show-meroon)
  (bench-oo *iterations* *bench*)
  'done )

;;; It is now sufficient to run (start-bench) with a good watch.

;;; end of bench.scm
