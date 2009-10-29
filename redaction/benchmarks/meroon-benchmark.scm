
(include "bench_.scm")

(define-class Point Object (x y))
(define-class Toto Object (to))
(define-class Tutu Object (tu))
(define-class Circle Point (radius))

(define-generic (add (p1) (p2)) 'no)
(define-method (add (p1 Point) (p2 Point)) 'ok)
(define-method (add (p1 Tutu) (p2 Point)) 'tu)
(define-method (add (p1 Toto) (p2 Point)) 'to)

(define-generic (add5 (p1) (p2) (p3) (p4) (p5)) 'no)
(define-method (add5 (p1 Point) (p2 Point) (p3 Point) (p4 Point) (p5 Point))
  'to)

(define-bench meroon-instance-creation
  ()
  (make-Point 1 2))

(define-bench meroon-access
  ((p (make-Point 1 2)))
  (Point-x p))

(define-bench meroon-modif
  ((p (make-Point 1 2)))
  (Point-x-set! p 21))

(define-bench meroon-dispatch-2
  ((p1 (make-Point 1 2))
   (p2 (make-Point 3 4)))
  (add p1 p2))

(define-bench meroon-dispatch-5
  ((p1 (make-Point 1 2))
   (p2 (make-Point 3 4))
   (p3 (make-Point 1 2))
   (p4 (make-Point 3 4))
   (p5 (make-Point 1 2)))
  (add5 p1 p2 p3 p4 p5))

(define-bench meroon-polymorhpic-dispatch-2
  ((p1 (make-Circle 1 2 12))
   (p2 (make-Circle 3 4 34)))
  (add p1 p2))

(define-bench meroon-polymorhpic-dispatch-5
  ((p1 (make-Circle 1 2 12))
   (p2 (make-Circle 3 4 34))
   (p3 (make-Circle 1 2 12))
   (p4 (make-Circle 3 4 34))
   (p5 (make-Circle 1 2 12)))
  (add5 p1 p2 p3 p4 p5))

(pp (meroon-instance-creation))
(pp (meroon-access))
(pp (meroon-modif))
(pp (meroon-dispatch-2))
(pp (meroon-dispatch-5))
(pp (meroon-polymorhpic-dispatch-2))
(pp (meroon-polymorhpic-dispatch-5))
