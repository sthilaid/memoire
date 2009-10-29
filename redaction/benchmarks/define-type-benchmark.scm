
(include "bench_.scm")

(define-type Point x y extender: define-type-of-Point)
(define-type Toto to)
(define-type Tutu tu)
(define-type-of-Point Circle radius)

(define (add2 p1 p2)
  (cond
   ((and (Point? p1)
         (Point? p2))
    'ok)
   (else 'no)))

(define (add5 p1 p2 p3 p4 p5)
  (cond
   ((and (Point? p1)
         (Point? p2)
         (Point? p3)
         (Point? p4)
         (Point? p5))
    'tu)
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

(define-bench define-type-dispatch-2
  ((p1 (make-Point 1 2))
   (p2 (make-Point 3 4)))
  (add2 p1 p2))

(define-bench define-type-dispatch-5
  ((p1 (make-Point 1 2))
   (p2 (make-Point 3 4))
   (p3 (make-Point 1 2))
   (p4 (make-Point 3 4))
   (p5 (make-Point 1 2)))
  (add5 p1 p2 p3 p4 p5))

(define-bench define-type-polymorhpic-dispatch-2
  ((p1 (make-Circle 1 2 12))
   (p2 (make-Circle 3 4 34)))
  (add2 p1 p2))

(define-bench define-type-polymorhpic-dispatch-5
  ((p1 (make-Circle 1 2 12))
   (p2 (make-Circle 3 4 34))
   (p3 (make-Circle 1 2 12))
   (p4 (make-Circle 3 4 34))
   (p5 (make-Circle 1 2 12)))
  (add5 p1 p2 p3 p4 p5))

(pp (define-type-instance-creation))
(pp (define-type-access))
(pp (define-type-modif))
(pp (define-type-dispatch-2))
(pp (define-type-dispatch-5))
(pp (define-type-polymorhpic-dispatch-2))
(pp (define-type-polymorhpic-dispatch-5))
