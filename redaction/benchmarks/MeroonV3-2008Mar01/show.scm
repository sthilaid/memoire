;;; $Id: show.scm,v 1.1 2005/02/25 22:19:37 lucier Exp $ 
;;; Copyright (c) 1990-96 by Christian Queinnec. All rights reserved.

;;;                ***********************************************
;;;                    Small, Efficient and Innovative Class System
;;;                                    Meroon 
;;;                              Christian Queinnec  
;;;                    Ecole Polytechnique & INRIA--Rocquencourt
;;;                ************************************************

;;; Show printed images of Meroon (or non-Meroon) objects. The generic
;;; function show ignores cycles and might loop, use unveil instead.
;;; If you add your proper methods on show, make sure that show does
;;; not return the instance to be shown to avoid printing circular
;;; entities by the native toplevel printer of Scheme.

(define-generic (show (o) . stream)
  (let ((stream (if (pair? stream) (car stream) (current-output-port))))
    (cond ((boolean? o) (display (if o "#T" "#F") stream))
          ((null? o) (display "()" stream))
          ((pair? o) (show-list o stream))
          ((vector? o) (show-vector o stream))
          ;;  by default, use display
          (else (display o stream)) ) ) )

(define (show-list o . stream)
  (let ((stream (if (pair? stream) (car stream) (current-output-port))))
    (define (show-content o)
      (show (car o) stream)
      (cond ((null? (cdr o)) #t)
            ((pair? (cdr o)) (display " " stream)
                             (show-content (cdr o)) )
            (else (display " . " stream)
                  (show (cdr o) stream) ) ) )
    (display "(" stream)
    (show-content o)
    (display ")" stream) ) )

(define (show-vector o . stream)
  (let ((stream (if (pair? stream) (car stream) (current-output-port)))
        (n (vector-length o)) )
    (define (show-content i)
      (when (fx< i n) 
            (show (vector-ref o i) stream)
            (display " " stream)
            (show-content (fx+ 1 i)) ) )
    (display "#(" stream)
    (show-content 0)
    (display ")" stream) ) )

(define-method (show (o Object) . stream)
  (let ((stream (if (pair? stream) (car stream) (current-output-port)))
        (name (Class-name (object->class o))) )
    (display "#<a" stream)
    ;; a french misinterpretation of english pronunciation
    ;;(case (string-ref (symbol->string name) 0)
    ;;  ((#\a #\e #\i #\o #\u #\y) (write-char #\n stream))
    ;;  (else #f) )
    (write-char #\space stream)
    (display name stream)
    (display ">" stream) ) )

(define-method (show (o Class) . stream)
  (let ((stream (if (pair? stream) (car stream) (current-output-port))))
    (display "#<Class: " stream)
    (display (Class-name o) stream)
    (display ">" stream) ) )

(define-method (show (o Generic) . stream)
  (let ((stream (if (pair? stream) (car stream) (current-output-port))))
    (display "#<Generic: " stream)
    (display (Generic-name o) stream)
    (display ">" stream) ) )

;;; A * or = follows the word Field to indicate if it is a mono or
;;; a Poly-Field. A ! is also inserted if the field is mutable.
(define (show-field status o stream)
  (display "#<" stream)
  (display status stream)
  (if (Field-immutable? o) (display " " stream) (display "! " stream))
  (display (Class-name (number->class (Field-class-number o))) stream)
  (display "." stream)
  (display (Field-name o) stream)
  (display ">" stream) )

(define-method (show (o Mono-Field) . stream)
  (let ((stream (if (pair? stream) (car stream) (current-output-port))))
    (show-field "Mono-Field" o stream) ) )

(define-method (show (o Poly-Field) . stream)
  (let ((stream (if (pair? stream) (car stream) (current-output-port))))
    (show-field "Poly-Field" o stream) ) )

(define-method (show (o Anomaly) . stream)
  (let ((stream (if (pair? stream) (car stream) (current-output-port))))
    (display "#<" stream)
    (display (Anomaly-category o) stream)
    (display "-Anomaly:" stream)
    (display (Anomaly-message o) stream)
    (display ">" stream) ) )

(define-method (show (o Subclass-Dispatcher) . stream)
  (let ((stream (if (pair? stream) (car stream) (current-output-port))))
    (display "#<" stream)
    (display (Class-name (object->class o)) stream)
    (display " " stream)
    (show (number->class (Subclass-Dispatcher-class-number o)) stream)
    (display " No: " stream)
    (show (Subclass-Dispatcher-no o) stream)
    (display " Yes: " stream)
    (show (Subclass-Dispatcher-yes o) stream)
    (display ">" stream) ) )

(define-method (show (o Indexed-Dispatcher) . stream)
  (let ((stream (if (pair? stream) (car stream) (current-output-port))))
    (display "#<" stream)
    (display (Class-name (object->class o)) stream)
    (display "/" stream)
    (show (number->class (Indexed-Dispatcher-class-number o)) stream)
    (display ", no:" stream)
    (show (Indexed-Dispatcher-no o) stream)
    (display ">" stream) ) )

(define-method (show (o Linear-Dispatcher) . stream)
  (let ((stream (if (pair? stream) (car stream) (current-output-port))))
    (display "#<" stream)
    (display (Class-name (object->class o)) stream)
    (show (Linear-Dispatcher-signature o) stream)
    (display ", no:" stream)
    (show (Linear-Dispatcher-no o) stream)
    (display ">" stream) ) )

(define-method (show (o Immediate-Dispatcher) . stream)
  (let ((stream (if (pair? stream) (car stream) (current-output-port))))
    (display "#<" stream)
    (display (Class-name (object->class o)) stream)
    (display ":" stream)
    (show (Immediate-Dispatcher-method o) stream)
    (display ">" stream) ) )

(define-method (show (o Tracing-Dispatcher) . stream)
  (let ((stream (if (pair? stream) (car stream) (current-output-port))))
    (display "#<" stream)
    (display (Class-name (object->class o)) stream)
    (display ":" stream)
    (show (Tracing-Dispatcher-dispatcher o) stream)
    (display ">" stream) ) )

;;;============================================ New library of useful functions
;;; This function displays the inheritance tree of a class.
;;; (show-hierarchy) displays all classes.

(define (show-hierarchy . args)
  (let* ((arg (if (pair? args) (car args) 'Object))
         (class (->Class arg))
         (str (if (pair? args) (cdr args) '()))
         (stream (if (pair? str) (car str) (current-output-port))) )
    (define (show-class c indent)
      (do ((i 0 (fx+ 1 i)))
          ((fx>= i indent))
        (display " " stream) )
      (show c stream)
      (newline stream)
      (for-each (lambda (c) (show-class c (fx+ indent 1)))
                (Class-subclasses c) ) )
    (display "Subclass tree of " stream)
    (display (Class-name class) stream)
    (newline stream)
    (show-class class 0)
    #t ) )

;;; Show all different methods attached to a generic function.
;;; (show-generic) displays all generic functions.

(define (show-generic . args)
  (if (pair? args)
      (oo-apply show-generic-function args)
      (sequence-map (lambda (generic)
                      (when generic (show-generic-function generic)) )
                    *generics* ) ) )

(define (show-generic-function name . stream)
  (let* ((stream (if (pair? stream) (car stream) (current-output-port)))
         (generic (->Generic name))
         (name (Generic-name generic))
         (dispatcher (Generic-dispatcher generic)) )
    (define (show-1-method c super-method indent)
      (let ((current-method (find-method1 dispatcher c)))
        (unless (eq? super-method current-method)
          (do ((i 0 (fx+ 1 i)))
              ((fx>= i indent))
            (display " " stream) )
          (show c stream)
          (newline stream) )
        (for-each (lambda (c) (show-1-method 
                               c current-method (fx+ indent 1) ))
                  (Class-subclasses c) ) ) )
    (define (show-N-method d)
      (cond ((Linear-Dispatcher? d)
             (for-each (lambda (c) 
                         (display " " stream)
                         (show c) )
                       (Linear-Dispatcher-signature d) )
             (newline stream)
             (show-N-method (Linear-Dispatcher-no d)) )
            (else (newline stream)) ) )
    (display "Methods on " stream)
    (display name stream)
    (newline stream)
    (cond ((Generic-1? generic) (show-1-method Object-class #f 0))
          ((Generic-N? generic) (show-N-method (Generic-dispatcher generic))) )
    #t ) )

;;; Show dispatchers of a generic function.
;;; (show-dispatcher) displays all dispatchers.

(define (show-dispatcher . g)
  (if (pair? g)
      (let* ((generic (->Generic (car g))))
        (show (Generic-dispatcher generic)) )
      (sequence-map (lambda (g) (when g 
                                  (display "Dispatcher of ")
                                  (display (Generic-name g))
                                  (newline)
                                  (show-dispatcher g)
                                  (newline) ))
                    *generics* ) ) )

;;; Prints the methods (on mono- or poly- generic) that have class in
;;; their signature. Asked for by Josep Riverola riverola@iese.es

(define (show-methods-for-class class . stream)
  (let* ((stream (if (pair? stream) (car stream) (current-output-port)))
         (class (->Class class)) )
    (sequence-map (lambda (g)
                    (when g (search-methods-for-class g class stream)) )
                  *generics* ) ) )

(define-generic (search-methods-for-class (o) c stream))

(define-method (search-methods-for-class (g Generic-1) c stream)
  (let ((super (Class-super-class c))
        (d (Generic-dispatcher g)) )
    (when super
      (let ((superm (find-method1 d super))
            (m (find-method1 d c)) )
        (unless (eq? m superm)
          (show g stream)
          (display " has a method for " stream)
          (show c stream)
          (newline stream) ) ) ) ) )

(define-method (search-methods-for-class (g Generic-N) c stream)
  (let scan ((d (Generic-dispatcher g)))
    (when (Linear-Dispatcher? d)
      (let ((sig (Linear-Dispatcher-signature d)))
        (when (member c sig)
          (show g stream)
          (display " has a method for " stream)
          (show sig stream)
          (newline stream) ) )
      (scan (Linear-Dispatcher-no d)) ) ) )

;;;=================================================== Debugging
;;; Show a detailed view of the Class hierarchy to check the
;;; renumbering process.

(define (show-detailed-hierarchy . message)
  (let ((class Object-class)
        (stream (current-output-port)) )
    (define (show-class c indent)
      ;;(display c)(newline)                           ; DEBUG
      (do ((i 0 (fx+ 1 i)))
          ((fx>= i indent))
        (display " " stream) )
      (display (careless-Class-name c) stream)
      (display " " stream)
      (display (list (careless-Class-number c)) stream)
      (newline stream)
      (for-each (lambda (cn) (show-class (number->class cn) (fx+ indent 1)))
                (Class-subclass-numbers c) ) )
    (display "Detailed Class tree of " stream)
    (display (careless-Class-name class) stream)
    (when (pair? message) 
      (display " " stream)
      (display (car message) stream) )
    (newline stream)
    (show-class class 0)
    #t ) )

;;; end of show.scm
