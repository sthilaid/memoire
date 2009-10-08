
(include "bench_.scm")

(define-type Point x y extender: define-type-of-Point)
(define-type Toto to)
(define-type Tutu tu)
(define-type-of-Point Circle radius)

(define (add p1 p2)
  (cond
   ((and (Tutu? p1)
         (Point? p2))
    'tu)
   ((and (Toto? p1)
         (Point? p2))
    'to)
   ((and (Point? p1)
         (Point? p2))
    'ok)
   (else 'no)))

(define-bench define-type-instance-creation
  ()
  (make-Point 1 2))

(define-bench define-type-access
  ((p (make-Point 1 2)))
  (Point-x p))

(define-bench define-type-modif
  ((p (make-Point 1 2)))
  (Point-x-set! p 21))

(define-bench define-type-dispatch
  ((p1 (make-Point 1 2))
   (p2 (make-Point 3 4)))
  (add p1 p2))

(define-bench define-type-polymorhpic-dispatch
  ((p1 (make-Circle 1 2 12))
   (p2 (make-Circle 3 4 34)))
  (add p1 p2))

(pp (define-type-instance-creation))
(pp (define-type-access))
(pp (define-type-modif))
(pp (define-type-dispatch))
(pp (define-type-polymorhpic-dispatch))
