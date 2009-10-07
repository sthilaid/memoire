;; Don't try this at home kids.

(define ##show-all-continuations? #t) ;;@@@ "lib/_repl.scm"

; (##rte-shape $code)
; (##rte-var-ref rte up over)
; (##rte-var-set! rte up over val)

(##gc)

(##closure-length closure)
(##closure-code   closure)
(##closure-ref  closure index)
(##closure-set! closure index val)


> (define-structure test 1st 2nd)
> (define t (make-test 'one "two"))
> (%dump t)
[0]= #<type #11 test>
[1]= one
[2]= "two"
> (define test-type (##vector-ref t 0))
> (%dump test-type)
[0]= #<type #12 type>
[1]= #:##type-2-test
[2]= test
[3]= 8
[4]= #f
[5]= #(1st 0 #f 2nd 0 #f) ;; name options attributes..
[6]= #(1 2)
> (%dump (##vector-ref test-type 1))
[0]= "##type-2-test"
[1]= 159383552
[2]= #f
[3]= 0

;; "_nonstd.scm"
(define-runtime-macro (define-type . args)
  (##define-type-expand 'define-type #f #f args))
;; (##define-type-expand
;;	form-name
;;	super-type-static
;;	super-type-dynamix
;;	args)

(define-runtime-macro (define-structure . args)
  (##define-type-expand 'define-structure #f #f args))

(define-runtime-macro (define-record-type name constructor predicate . fields)
  `(define-type ,name
     constructor: ,constructor
     predicate: ,predicate
     ,@fields))

    (define allowed-field-options
        '((printable:     . (-2 . 0))
          (unprintable:   . (-2 . 1))
          (read-write:    . (-3 . 0))
          (read-only:     . (-3 . 2))
          (equality-test: . (-5 . 0))
          (equality-skip: . (-5 . 4))))

(set! ##define-type-expansion-show? #t)  ;;@@@

> (define-structure test 1st 2nd)
(begin
  (define ##type-2-test
    ((let () (##declare (extended-bindings)) ##structure)
     ##type-type
     ((let () (##declare (extended-bindings)) ##make-uninterned-symbol)
      "##type-2-test")
     'test
     '8
     #f
     '#(1st 0 #f 2nd 0 #f)
     '#(1 2)))
  (define (make-test p1 p2)
    (##declare (extended-bindings))
    (##structure ##type-2-test p1 p2))
  (define (test? obj)
    (##declare (extended-bindings))
    (##structure-direct-instance-of?
     obj
     (let ()
       (##declare (extended-bindings) (not safe))
       (##type-id ##type-2-test))))
  (define (test-1st . #0=(obj))
    (#1=(let () (##declare (extended-bindings)) ##structure-ref)
     obj
     1
     ##type-2-test
     test-1st))
  (define (test-1st-set! . #2=(obj val))
    (#3=(let () (##declare (extended-bindings)) ##structure-set!)
     obj
     val
     1
     ##type-2-test
     test-1st-set!))
  (define (test-2nd . #0#) (#1# obj 2 ##type-2-test test-2nd))
  (define (test-2nd-set! . #2#) 
      (#3# obj val 2 ##type-2-test test-2nd-set!)))


;; header.scm

(##define-macro (macro-slot index struct . val)
  (if (null? val)
    `(##vector-ref  ,struct ,index)
    `(##vector-set! ,struct ,index ,@val)))

; A symbol is represented by an object vector of length 4
; slot 3 = pointer to corresponding global variable (0 if none exists)

; A continuation is represented by an object vector of length 2
; slot 0 = frame object
; slot 1 = dynamic-environment

;; _repl#.scm
(define-type repl-context
  id: cd5f5bad-f96f-438d-8d63-ff887b7b39de
  constructor: macro-make-repl-context
  implementer: implement-type-repl-context
  macros:
  prefix: macro-
  opaque:
  unprintable:

  level
  depth
  channel
  cont
  initial-cont
  prev-level
  prev-depth
)

;; _ecal#.scm

(##define-macro (macro-make-code code-prc cte src stepper subcodes . lst)
  `(let (($code (##vector #f ,code-prc ,cte ,src ,stepper ,@subcodes ,@lst)))
     ,@(let loop ((l subcodes) (i 5) (r '()))
         (if (pair? l)
           (loop (cdr l)
                 (+ i 1)
                 (cons `(##vector-set! (##vector-ref $code ,i) 0 $code) r))
           (reverse r)))
     $code))

(##define-macro (macro-code-link c)
  `(##vector-ref ,c 0))

(##define-macro (macro-code-cprc c)
  `(##vector-ref ,c 1))

(##define-macro (macro-code-cte c)
  `(##vector-ref ,c 2))

(##define-macro (macro-code-locat c)
  `(##vector-ref ,c 3))

(##define-macro (macro-code-locat-set! c l)
  `(##vector-set! ,c 3 ,l))

(##define-macro (macro-code-stepper c)
  `(##vector-ref ,c 4))

(##define-macro (macro-code-length c)
  `(##fixnum.- (##vector-length ,c) 5))

(##define-macro (macro-code-ref c n)
  `(##vector-ref ,c (##fixnum.+ ,n 5)))

(##define-macro (macro-code-set! c n x)
  `(##vector-set! ,c (##fixnum.+ ,n 5) ,x))

(##define-macro (^ n)
  `(##vector-ref $code ,(+ n 5)))

(##define-macro (macro-is-child-code? child parent)
  `(let ((child ,child)
         (parent ,parent))
     (and (##vector? child)
          (##fixnum.< 3 (##vector-length child))
          (##eq? (macro-code-link child) parent))))

(##define-macro (macro-code-run c)
  `(let (($$code ,c))
     ((##vector-ref $$code 1) $$code rte)))

;------------------------------------------------------------------------------

; Macros to create the "code procedure" associated with a code node.

(##define-macro (macro-make-cprc . body)
  `(let ()
     (##declare (not inline) (not interrupts-enabled) (environment-map))
     (lambda ($code rte)
       (let (($$continue
              (lambda ($code rte)
                (##declare (inline))
                ,@body)))
         (##declare (interrupts-enabled))
         ($$continue $code rte)))))

(##define-macro (macro-make-gen params . def)
  `(let ()
     (##declare (not inline))
     (lambda (cte tail? src ,@params) ,@def)))

(##define-macro (macro-gen proc src . args)
  `(,proc cte tail? ,src ,@args))


;; _thread#.scm

; Exception raising.

(##define-macro (macro-raise obj)
  `(let ((obj ,obj))
     (##declare (not safe)) ; avoid procedure check on the call to the handler
     ((macro-current-exception-handler) obj)))


