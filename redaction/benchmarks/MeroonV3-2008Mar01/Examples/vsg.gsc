;(declare (standard-bindings) (extended-bindings) (block) (fixnum))
;;; $Id: vsg.scm,v 1.6 1996/03/31 12:31:35 queinnec Exp $

;;;======================================================
;;;        VSG: Validation Suite Generation 
;;; Christian Queinnec
;;; Ecole Polytechnique & INRIA-Rocquencourt
;;;======================================================

;;; This file contains the programs associated with the VSG paper:
;;; file://ftp.inria.fr/INRIA/Projects/icsla/Papers/vsg.ps.gz
;;; They are adapted for Scheme and Meroon from the original programs
;;; written in Common Lisp. The easiest way to use this package is to look
;;; at the documentation of the build-Recursive-CStream below. Once a
;;; stream is built, you can consume it with functions such as 
;;; CStream-iterate or CStream->list.

;;; This program use define-internal-meroon-repository-macro as the
;;; macro definer. This is required for programs in the Meroon
;;; repository.

(define-internal-meroon-macro (unless condition . body)
  `(if ,condition #f (begin ,@body)) )
(define-internal-meroon-macro (when condition . body)
  `(if ,condition (begin ,@body)) )

;;;====================================================== All the classes
;;; The instances of these classes tend to form huge cyclic
;;; self-designating values, This might be a good test for GC.

(define-class CStream Object () '`,(make-meroon-keyword virtual))

(define-class Real-CStream CStream ())

(define-class Normal-CStream Real-CStream (cost head tail))

(define-class Mono-CStream CStream (source) '`,(make-meroon-keyword virtual))

(define-class Transducer-CStream Mono-CStream (cost-fn comp-fn))

(define-class Kleene-CStream Mono-CStream
  (cost1-fn comp1-fn cost2-fn comp2-fn) )

(define-class Bi-CStream CStream (father mother) '`,(make-meroon-keyword virtual))

(define-class Joint-CStream Bi-CStream (chooser))

(define-class Concatenated-CStream Bi-CStream ())

(define-class Product-CStream Bi-CStream (cost-fn comp-fn))

(define-class Delayed-CStream CStream (closure comment))

(define-class Relay-CStream CStream (tail))

;;;====================================================== Empty CStream
;;; The Empty-CStream is the only instance of Real-CStream. This avoids
;;; to create an Empty-CStream class with a unique instance while simulating
;;; the needed interface functions.

(define make-empty-CStream 'wait)
(define empty-CStream? 'wait)

(let ((empty-CStream (make-Real-CStream)))
  (set! make-empty-CStream (lambda () empty-CStream))
  (set! empty-CStream? (lambda (s) (eq? s empty-CStream))) )

;;;====================================================== Enumerated CStream
;;; Take a list of discreted values and turn it into a stream that will
;;; simply enumerate them. All these values are considered to be produced
;;; with the same cost.

(define (make-enumerated-CStream cost values)
  (if (pair? values)
      (make-Normal-CStream 
       cost                             ; cost
       (car values)                     ; head
       (make-enumerated-CStream         ; tail
        cost
        (cdr values) ) )
      (make-empty-CStream) ) )

;;;====================================================== Real-CStream

(define *maximum-loops-authorized-for-CStream-tail* 40)

;;; This macro is called as (CStream-force! (stream-field stream)).
;;; It ensures that the content of the field of stream is real ie empty
;;; or a normal stream containing at least one term. For that, it sends
;;; repeatedly the message 'transform' to the content of the field and
;;; update the current object with the result of the transformation.
;;; However, after performing this side-effect it returns the content
;;; of the field which is then ensured to be a Real-CStream.

(define-internal-meroon-macro (CStream-force! form)
  (let* ((reader  (car form))
         (writer  (symbol-concatenate reader '-set!))
         (cstream (cadr form))
         (node    (gensym))
         (g       (gensym))
         (i       (gensym)) )
    `(let ((,node ,cstream))
       (do ((,i 0 (+ 1 ,i))
            (,g (,reader ,node) (,reader ,node)))
           ((Real-CStream? ,g)
            ,g )
         (when (> ,i *maximum-loops-authorized-for-CStream-tail*)
           (vsg-error 'CStream-force! 
                      "Looping (?) CStream" 
                      ,g ) )
         (,writer ,node (CStream-transform ,g)) ) ) ) )

;;; The result of CStream-tail must always be a Real-CStream (ie a
;;; normal or empty stream). It takes a CStream and returns its tail.

(define-generic (CStream-tail (o CStream)))

(define-method (CStream-tail (o Normal-CStream))
  (CStream-force! (Normal-CStream-tail o)) )

(define-method (CStream-tail (o Relay-CStream))
  (CStream-force! (Relay-CStream-tail o)) )

;;;====================================================== transform
;;; The 'transform' message transforms a stream to develop itself a bit.
;;; When a stream is asked to transform itself, it returns another stream
;;; corresponding to a development of the original stream.
;;; CAUTION: CStreams may be quite intricated and may loop. 

;;; This function allows a Joint stream to have a strategy to choose
;;; how to decide to take an element from its left or right source
;;; streams. By default, it toggles a boolean. The test suite forces it
;;; to be deterministic.

(define default-Joint-CStream-chooser
  (let ((VSG-random-state #t))
    (lambda (o)
      (set! VSG-random-state (not VSG-random-state))
      VSG-random-state ) ) )

(define-generic (CStream-transform (o CStream)))

(define-method (CStream-transform (o Joint-CStream))
  (let ((dad (CStream-force! (Joint-CStream-father o)))
        (mom (CStream-force! (Joint-CStream-mother o))) )
    (if (empty-CStream? dad) mom
        (if (empty-CStream? mom) dad
            (let ((c1 (Normal-CStream-cost dad))
                  (c2 (Normal-CStream-cost mom)) )
              (if (if (= c1 c2)
                      ;; This introduces a bit of indeterminacy
                      ((Joint-CStream-chooser o) o)
                      (< c1 c2) )
                  (make-Normal-CStream 
                   c1                   ; cost
                   (Normal-CStream-head dad) ; head
                   (make-Joint-CStream  ; tail
                    (Normal-CStream-tail dad) ; father
                    mom              ; mother
                    default-Joint-CStream-chooser ) )
                  (make-Normal-CStream 
                   c2                   ; cost
                   (Normal-CStream-head mom) ; head
                   (make-Joint-CStream  ; tail
                    dad                 ; father
                    (Normal-CStream-tail mom) ; mother
                    default-Joint-CStream-chooser ) ) ) ) ) ) ) )

(define-method (CStream-transform (o Concatenated-CStream))
  (let ((dad (CStream-force! (Concatenated-CStream-father o))))
    (if (empty-CStream? dad)
        (Concatenated-CStream-mother o)
        (make-Normal-CStream 
         (Normal-CStream-cost dad)      ; cost
         (Normal-CStream-head dad)      ; head
         (make-Concatenated-CStream     ; tail
          (Normal-CStream-tail dad)     ; father
          (Concatenated-CStream-mother o) ) ) ) ) ) ; mother

(define-method (CStream-transform (o Product-CStream))
  (let ((dad (CStream-force! (Product-CStream-father o)))
        (mom (CStream-force! (Product-CStream-mother o))) )
    (if (or (empty-CStream? dad)
            (empty-CStream? mom) )
        (make-empty-CStream)
        (make-Normal-CStream
         ((Product-CStream-cost-fn o)   ; cost
          (Normal-CStream-cost dad)
          (Normal-CStream-cost mom) )
         ((Product-CStream-comp-fn o)   ; head
          (Normal-CStream-head dad)
          (Normal-CStream-head mom) )
         (make-Joint-CStream            ; tail
          (make-Product-CStream         ; father
           dad                          ; father
           (Normal-CStream-tail mom)    ; mother
           (Product-CStream-cost-fn o)  ; cost-fn
           (Product-CStream-comp-fn o) ) ; comp-fn
          (make-Product-CStream         ; mother
           (Normal-CStream-tail dad)    ; father
           (make-enumerated-CStream     ; mother
            (Normal-CStream-cost mom)   ; cost
            (list (Normal-CStream-head mom)) ) ; values
           (Product-CStream-cost-fn o)  ; cost-fn
           (Product-CStream-comp-fn o) ) ; comp-fn
          default-Joint-CStream-chooser ) ) ) ) )

(define-method (CStream-transform (o Kleene-CStream))
  (let ((source (CStream-force! (Kleene-CStream-source o))))
    (if (empty-CStream? source)
        (make-empty-CStream)
        (let* ((first (make-Normal-CStream
                       ((Kleene-CStream-cost1-fn o) ; cost
                        (Normal-CStream-cost source) )
                       ((Kleene-CStream-comp1-fn o) ; head
                        (Normal-CStream-head source) )
                       'ignore ))       ; tail
               (tail (make-Joint-CStream
                      (make-Transducer-CStream ; father
                       (Normal-CStream-tail source) ; source
                       (Kleene-CStream-cost1-fn o) ; cost-fn
                       (Kleene-CStream-comp1-fn o) ) ; comp-fn
                      (make-Product-CStream ; mother
                       source           ; father
                       first            ; mother
                       (Kleene-CStream-cost2-fn o) ; cost-fn
                       (Kleene-CStream-comp2-fn o) ) ; comp-fn
                      default-Joint-CStream-chooser )) )
          (Normal-CStream-tail-set! first tail)
          first ) ) ) )

(define-method (CStream-transform (o Transducer-CStream))
  (let ((dad (CStream-force! (Transducer-CStream-source o))))
    (if (empty-CStream? dad)
        (make-empty-CStream)
        (make-Normal-CStream
         ((Transducer-CStream-cost-fn o) ; cost
          (Normal-CStream-cost dad) )
         ((Transducer-CStream-comp-fn o) ; head
          (Normal-CStream-head dad) )
         (make-Transducer-CStream       ; tail
          (Normal-CStream-tail dad)     ; source
          (Transducer-CStream-cost-fn o) ; cost-fn
          (Transducer-CStream-comp-fn o) ) ) ) ) ) ; comp-fn

(define-method (CStream-transform (o Delayed-CStream))
  ((Delayed-CStream-closure o) o) )

(define-method (CStream-transform (o Relay-CStream))
  (CStream-tail o) )

;;;====================================================== Recursive
;;; CStreams These functions and macros make easier the definition of
;;; CStreams. build-Recursive-CStream is defined as a pervasive macro
;;; so it can be used in other files (such as vsg-bench.scm) [Thanks to
;;; Brad Lucier <lucier@MATH.Purdue.EDU>].

;;;    To build a stream, choose a local name to name it (this
;;; is mandatory if the stream is defined recursively) [I generally
;;; use self]:
;;;	(build-Recursive-CStream self
;;;		... )
;;; Then fill the body of this form with whatever stream you want.
;;; You can use the following forms:
;;;;     <stream-var> a variable whose value is a stream. This variable
;;;                   may be the `self' variable mentioned above.
;;;      (enum <forms>...)
;;;                   returns an enumerated stream whose terms are the values 
;;;                   of <forms>...
;;;      (union <form1> <form2>)
;;;                   returns a stream which is the union of the two streams
;;;                   that are the values of <form1> and <form2>.
;;;      (+ <form1> <form2>)
;;;                   same as union
;;;      (++ <form1> <form2>)
;;;                   returns the concatenation of the stream, value of
;;;                   <form1>, followed by the stream, value of <form2>.
;;;      (* <form1> <form2>) 
;;;                   returns the stream that is the cartesian product of
;;;                   the two streams arguments.
;;;      (Kleene <form>)
;;;                   returns the streams whose items are increasing 
;;;                   sequences of the elements of <form>.
;;;      (let ((<var> <form>) ...) <body>)
;;;                   creates local streams, bind them to the <var> and
;;;                   return the stream described by <body> that can use 
;;;                   for its definition these local variables.
;;;      (letrec ((<var> <form>) ...) <body>)
;;;                   creates local streams, bind them to the <var> and
;;;                   evaluate <body> that can use for its 
;;;                   definition these local variables. NOTE: <body> here
;;;                   is a form that is evaluated to form the terms of the
;;;                   resulting stream (it is not a stream specification).

(define-meroon-macro (build-Recursive-CStream name def)
  `(letrec ((,name (make-Relay-CStream
                    ,(Recursive-CStream-parser ; tail
                      def (cons name '()) ) )))
     ,name ) )

(define (Recursive-CStream-parser exp env)
  (if (pair? exp)
      (case (car exp)
        ((enum)    (Rec-Parse-on-Enum exp env))
        ((+ union) (Rec-Parse-on-Joint exp env))
        ((++)      (Rec-Parse-on-Concatenated exp env))
        ((*)       (Rec-Parse-on-Product exp env))
        ((Kleene)  (Rec-Parse-on-Kleene exp env))
        ((let)     (Rec-Parse-on-let exp env))
        ((letrec)  (Rec-Parse-on-letrec exp env))
        (else      (vsg-error 'Recursive-CStream-parser
                              "unknown stream specification"
                              (car exp) )) )
      (Rec-Parse-on-variable exp env) ) )

(define (Rec-Parse-on-Enum exp env)
  `(make-enumerated-CStream 1 (list . ,(cdr exp))) )

(define (Rec-Parse-on-variable exp env)
  (let ((binding (member exp env))
        (o       (gensym)) )
    (unless (pair? binding)
      (vsg-warning 'Recursive-CStream-parser
                   "external stream name"
                   exp ) )
    `(make-Relay-CStream
      (make-Delayed-CStream             ; tail
       (lambda (,o) ,exp)               ; closure
       ',exp ) ) ) )                    ; comment

(define (Rec-Parse-on-Joint exp env)
  (reduce-from-end
   (lambda (s1 s2)
     `(make-Joint-CStream
       ,s1                              ; father
       ,s2                              ; mother
       default-Joint-CStream-chooser ) )
   (map (lambda (e) (Recursive-CStream-parser e env)) 
        (cdr exp) ) ) )

(define (reduce-from-end fn l)
  (if (pair? l)
      (if (pair? (cdr l))
          (fn (car l) (reduce-from-end fn (cdr l)))
          (car l) )
      (vsg-error 'reduce-from-end
                 "not large enough list"
                 l ) ) )

(define (Rec-Parse-on-Concatenated exp env)
  (reduce-from-end
   (lambda (s1 s2)
     `(make-Concatenated-CStream
       ,s1                              ; father
       ,s2 ) )                          ; mother
   (map (lambda (e) (Recursive-CStream-parser e env))
        (cdr exp) ) ) )

(define (Rec-Parse-on-Product exp env)
  `(make-Product-CStream
    ,(Recursive-CStream-parser (cadr exp) env) ; father
    ,(Recursive-CStream-parser (caddr exp) env) ; mother
    (lambda (i j) (+ i j 1))            ; cost-fn
    cons ) )                            ; comp-fn

(define (Rec-Parse-on-Kleene exp env)
  `(make-Kleene-CStream
    ,(Recursive-CStream-parser (cadr exp) env) ; source
    (lambda (n) (+ n 1)) ; cost1-fn
    list                                ; comp1-fn
    (lambda (i j) (+ i j 1))            ; cost2-fn
    cons ) )                            ; comp2-fn

(define (Rec-Parse-on-let exp env)
  (let* ((bindings (cadr exp))
         (new-env (append (map car bindings)
                          env )) )
    (if (= (length bindings) 0)
        (vsg-error 'Recursive-CStream-parser 
                   "No stream involved in" 
                   exp )
        `(let ,(map (lambda (b)
                      `(,(car b) 
                        ,(Recursive-CStream-parser
                          (cadr b)
                          new-env ) ) )
                    bindings )
           ,(Recursive-CStream-parser (caddr exp) new-env) ) ) ) )

(define (Rec-Parse-on-letrec exp env)
  (let* ((bindings (cadr exp))
         (new-env (append (map car bindings)
                          env )) )
    (if (= (length bindings) 0)
        (vsg-error 'Recursive-CStream-parser 
                   "No stream involved in" 
                   exp )
        `(let ,(map (lambda (b) `(,(car b) 'ignore))
                    bindings )
           ,@(map (lambda (b)
                    `(set! ,(car b) 
                           (make-Relay-CStream
                            ,(Recursive-CStream-parser ; tail
                              (cadr b)
                              new-env ) ) ) )
                  bindings )
           ,(case (length bindings)
              ((1)
               (if (and (symbol? (caddr exp)) (member (caddr exp) new-env))
                   `,(caddr exp)
                   `(make-Transducer-CStream
                     ,(car (car bindings)) ; source
                     (lambda (n) (+ 1 n)) ; cost-fn
                     (lambda (,(car (car bindings))) ; comp-fn
                       ,(caddr exp) ) ) ))
              (else
               (if (and (symbol? (caddr exp)) (member (caddr exp) new-env))
                   `,(caddr exp)
                   (let ((tree (CStream-folder (map car bindings)
                                               cons ))
                         (s1 (gensym))
                         (s2 (gensym)) )
                     `(make-Product-CStream
                       ,(Product-parse (car tree) new-env) ; father
                       ,(Product-parse (cdr tree) new-env) ; mother
                       (lambda (i j) (+ i j 1)) ; cost-fn
                       (lambda (,s1 ,s2) ; comp-fn
                         (let ,(map (lambda (b)
                                      (let ((path (or (CStream-lookup
                                                       (car b) 
                                                       (car tree)
                                                       s1 )
                                                      (CStream-lookup
                                                       (car b) 
                                                       (cdr tree)
                                                       s2 ) )))
                                        `(,(car b) ,path) ) )
                                    bindings )
                           ,(caddr exp) ) ) ) ) ) ) ) ) ) ) )

(define (Product-parse tree env)
  (if (pair? tree)
      `(make-Product-CStream
        ,(Product-parse (car tree) env)  ; father
        ,(Product-parse (cdr tree) env)  ; mother
        +                               ; cost-fn
        cons )                          ; comp-fn
      (Rec-Parse-on-variable tree env) ) )

;;; logarithmically compose by binary FN all terms of L

(define (CStream-folder l fn)
  (if (pair? l)
      (if (pair? (cdr l))
          (CStream-folder `(,@(cddr l) ,(fn (car l) (cadr l)))
                          fn )
          (car l) )
      '() ) )

(define (CStream-lookup var tree path)
  (if (eq? var tree)
      path
      (if (pair? tree)
          (or (CStream-lookup var (car tree) `(car ,path))
              (CStream-lookup var (cdr tree) `(cdr ,path)) )
          #f ) ) )

;;;====================================================== ITERATE on a CStream

;;; Takes a CSTREAM and apply FN on its first N heads.  INDEX numbers
;;; the occurrences. N can be #f in which case do not limit the length
;;; of the produced cstream.

(define (CStream-iterate fn cstream n index)
  (if (if (number? n) (>= n 0) #t)
      (if (empty-CStream? cstream)
          cstream
          (if (Normal-CStream? cstream)
              (begin (fn (Normal-CStream-head cstream)
                         (Normal-CStream-cost cstream) 
                         n 
                         index )
                     (CStream-force! (Normal-CStream-tail cstream))
                     (CStream-iterate 
                      fn 
                      (Normal-CStream-tail cstream)
                      (if (number? n) (- n 1) n)
                      (+ index 1) ) )
              (CStream-iterate fn 
                               (CStream-transform cstream) 
                               (if (number? n) (- n 1) n)
                               index ) ) )
      cstream ) )

;;; Convert the (at most) N first terms of a CStream into a list.

(define-generic (CStream->list (cstream CStream) n)
  (if (> n 0)
      (CStream->list (CStream-transform cstream) (- n 1))
      '() ) )

(define-method (CStream->list (cstream Real-CStream) n)
  ;; Normally, the sole instance is the empty list.
  (if (empty-CStream? cstream) 
      '()
      (vsg-error 'CStream->list 
                 "Not the empty list"
                 cstream ) ) )

(define-method (CStream->list (cstream Normal-CStream cstream) n)
  (if (> n 0)
      (cons (Normal-CStream-head cstream)
            (CStream->list (CStream-force! (Normal-CStream-tail cstream))
                           (- n 1) ) )
      '() ) )

(define-method (CStream->list (cstream Relay-CStream) n)
  (if (> n 0)
      (CStream->list (CStream-tail cstream) (- n 1))
      '() ) )


;;;====================================================== Display
;;; Display the fist N terms of a CStream on a port.

(define (CStream-show cstream n . port)
  (let ((port (if (pair? port) (car port) (current-output-port))))
    (define (show-element head cost n index)
      (show head port)
      (display " ;; " port)
      (display index port)
      (display " (cost: " port)
      (show cost port)
      (display ")" port)
      (newline)  port)
    (CStream-iterate show-element
                     cstream
                     n
                     1 ) ) )

;;; Display the whole structure of a CStream but pay attention to
;;; cycles ie CStreams are walked to find their cycles then they
;;; are printed with cycles elided.

(define *default-displayed-CStream-depth* #f)

(define-method (show (o CStream) . port)
  (let ((port (if (pair? port) (car port) (current-output-port))))
    (display-a-CStream o port *default-displayed-CStream-depth*) ) )

;;; MEM is a Alist counting how many time a CStream appears.

(define (display-a-CStream o port n)
  ;; a hashtable would be better.
  (let ((mem    (list (cons o 0)))
        (newmem '())
        (i      0) )
    ;; build a new alist NEWMEM with the relevant entries of MEM,
    ;; cycles are paired with an index to name it and a boolean saying
    ;; if they are already printed.
    (for-each (lambda (entry)
                (let ((key (car entry))
                      (val (cdr entry)) )
                  (when (> val 1)
                    (set! i (+ i 1))
                    (set! newmem (cons (cons key (cons i #f)) newmem)) ) ) )
              (CStream-walk o mem) )
    (CStream-display o port n newmem) ) )

;;; First, identify cycle in a CStream and enrich the MEM Alist.

(define-generic (CStream-walk (o) mem))

(define-method (CStream-walk (o CStream) mem)
  mem )

(define-method (CStream-walk (o Normal-CStream) mem)
  (let ((c (assq o mem)))
    (if (pair? c)
        (begin
          (set-cdr! c (+ 1 (cdr c)))
          mem )
        (begin
          (set-cdr! mem (cons (cons o 1) (cdr mem)))
          (CStream-walk (Normal-CStream-tail o) mem) ) ) ) )

(define-method (CStream-walk (o Bi-CStream) mem)
  (let ((c (assq o mem)))
    (if (pair? c)
        (begin
          (set-cdr! c (+ 1 (cdr c)))
          mem )
        (begin 
          (set-cdr! mem (cons (cons o 1) (cdr mem)))
          (CStream-walk (Bi-CStream-father o) mem)
          (CStream-walk (Bi-CStream-mother o) mem) ) ) ) )

(define-method (CStream-walk (o Mono-CStream) mem)
  (let ((c (assq o mem)))
    (if (pair? c)
        (begin
          (set-cdr! c (+ 1 (cdr c)))
          mem )
        (begin 
          (set-cdr! mem (cons (cons o 1) (cdr mem)))
          (CStream-walk (Mono-CStream-source o) mem) ) ) ) )

(define-method (CStream-walk (o Relay-CStream) mem)
  (let ((c (assq o mem)))
    (if (pair? c)
        (begin
          (set-cdr! c (+ 1 (cdr c)))
          mem )
        (begin 
          (set-cdr! mem (cons (cons o 1) (cdr mem)))
          (CStream-walk (Relay-CStream-tail o) mem) ) ) ) )
               
;;; Second: display a CStream under control of MEM.

(define-generic (CStream-display (o) s n mem))

;;; Nest the printing forms with all the checks to handle circularity.

(define-internal-meroon-macro (CStream-circle o s n mem . body)
  `(CStream-circle-display ,o ,s ,n ,mem (lambda () . ,body)) )

(define (CStream-circle-display o s n mem body)
  (let ((c (assq o mem)))
    (if (pair? c)
      (if (cdr c)
          ;; (format s "#~D#" (cadr c))
          (begin (display "#" s)
                 (display (cadr c) s)
                 (display "#" s) )
          ;; (format s "#~D=")
          (begin (display "#" s)
                 (display (cadr c) s)
                 (display "=" s)
                 (set-cdr! (cdr c) #t)
                 (body) ) )
      (body) ) ) )

(define-method (CStream-display (o Real-CStream) s n mem)
  (if (empty-CStream? o)
    (display "<>" s)
    (vsg-error 'CStream-display
               "on a non-empty real-CStream"
               o ) ) )

(define-method (CStream-display (o Normal-CStream) s n mem)
  (CStream-circle o s n mem
     (display "{" s)
     (display (Normal-CStream-cost o) s)
     (display "}" s)
     (display (Normal-CStream-head o) s)
     (display " " s)
     (CStream-display (Normal-CStream-tail o) s n mem) ) )

(define-method (CStream-display (o Concatenated-CStream) s n mem)
  (CStream-circle o s n mem
     (display "<CONCAT " s)
     (CStream-display (Concatenated-CStream-father o) s n mem)
     (display " ++ " s)
     (CStream-display (Concatenated-CStream-mother o) s n mem)
     (display ">" s) ) )

(define-method (CStream-display (o Transducer-CStream) s n mem)
  (CStream-circle o s n mem
     (display "<TRANSDUCE " s)
     (CStream-display (Transducer-CStream-source o) s n mem)
     (display ">" s)) )

(define-method (CStream-display (o Joint-CStream) s n mem)
  (CStream-circle o s n mem
     (display "<JOINT " s)
     (CStream-display (Joint-CStream-father o) s n mem)
     (display " + " s)
     (CStream-display (Joint-CStream-mother o) s n mem)
     (display ">" s) ) )

(define-method (CStream-display (o Product-CStream) s n mem)
  (CStream-circle o s n mem
     (display "<PRODUCT " s)
     (CStream-display (Product-CStream-father o) s n mem)
     (display " * " s)
     (CStream-display (Product-CStream-mother o) s n mem)
     (display ">" s) ) )

(define-method (CStream-display (o Kleene-CStream) s n mem)
  (CStream-circle o s n mem
     (display "<KLEENE " s)
     (CStream-display (Kleene-CStream-source o) s n mem)
     (display ">" s) ) )
  
(define-method (CStream-display (o Delayed-CStream) s n mem)
  (CStream-circle o s n mem
     (display "<DELAY " s)
     (display (Delayed-CStream-comment o) s)
     (display ">" s) ) )

(define-method (CStream-display (o Relay-CStream) s n mem)
  (CStream-circle o s n mem
     (display "<RELAY " s)
     (CStream-display (Relay-CStream-tail o) s n mem)
     (display ">" s) ) )

;;;====================================================== Miscellaneous
;;; Error and Warning handling.

(define (vsg-error function message . culprits)
  (display "   ---   ERROR in CStream   ---   ")(newline)
  (display function)
  (display ":")
  (display message)(newline)
  (display culprits)(newline)
  (car message) ) ; force an error in the underlying system.

(define (vsg-warning function message . culprits)
  (display "   ---   WARNING in CStream   ---   ")(newline)
  (display function)
  (display ":")
  (display message)(newline)
  (display culprits)(newline) )

;;; Some statistics on a stream.
;;; Count number of terms with a given cost in a stream ordered by cost.

(define (count-terms/cost cstream maximal-cost)
  (let ((current-cost  0)
        (number-w/cost 0) )
    (call/cc 
     (lambda (exit)
       (CStream-iterate
        (lambda (head cost tail index)
          (cond ((> cost maximal-cost)
                 (display `(,number-w/cost terms of 
                            cost ,current-cost ))
                 (newline)
                 (exit 'done) )
                ((= cost current-cost)
                 (set! number-w/cost (+ number-w/cost 1)) )
                (else (display `(,number-w/cost terms of 
                                                cost ,current-cost ))
                      (newline)
                      (set! number-w/cost 0)
                      (set! current-cost cost) ) ) )
        cstream
        '()
        1 ) ) ) ) )

;;; Tests.
;;; Don't forget to load tester.scm
;;; (test-vsg "Examples/vsg.tst")

(define (test-vsg file)
  (suite-test
   file "?? " "== " #t
   (lambda (read check err)
     (lambda ()
       (check (eval (read))) ) )
   naive-match ) )

;;; end of vsg.scm


