
(include "bench_.scm")
(include "class/src/class.scm")

(define-class Point () (slot: x) (slot: y))
(define-class Toto () (slot: to))
(define-class Tutu () (slot: tu))
(define-class Circle (Point) (slot: radius))

(define-generic add)
(define-method (add (p1 Point) (p2 Point)) 'ok)
(define-method (add (p1 Tutu) (p2 Point)) 'tu)
(define-method (add (p1 Toto) (p2 Point)) 'to)

(define-method (add (p1 Point) (p2 Point) (p3 Point) (p4 Point) (p5 Point))
  'to)

(define-bench class-direct-instance-creation
  ()
  (make-Point 1 2))

(define-bench class-constructor-instance-creation
  ()
  (new Point 1 2))

(define-bench class-access
  ((p (make-Point 1 2)))
  (Point-x p))

(define-bench class-modif
  ((p (make-Point 1 2)))
  (Point-x-set! p 21))

(define-bench class-dispatch-2
  ((p1 (make-Point 1 2))
   (p2 (make-Point 3 4)))
  (add p1 p2))

(define-bench class-dispatch-5
  ((p1 (make-Point 1 2))
   (p2 (make-Point 3 4))
   (p3 (make-Point 1 2))
   (p4 (make-Point 3 4))
   (p5 (make-Point 1 2)))
  (add p1 p2 p3 p4 p5))

(define-bench class-polymorhpic-dispatch-2
  ((p1 (make-Circle 1 2 12))
   (p2 (make-Circle 3 4 34)))
  (add p1 p2))

(define-bench class-polymorhpic-dispatch-5
  ((p1 (make-Circle 1 2 12))
   (p2 (make-Circle 3 4 34))
   (p3 (make-Circle 1 2 12))
   (p4 (make-Circle 3 4 34))
   (p5 (make-Circle 1 2 12)))
  (add p1 p2 p3 p4 p5))

(pp (class-direct-instance-creation))
(pp (class-constructor-instance-creation))
(pp (class-access))
(pp (class-modif))
(pp (class-dispatch-2))
(pp (class-dispatch-5))
(pp (class-polymorhpic-dispatch-2))
(pp (class-polymorhpic-dispatch-5))
