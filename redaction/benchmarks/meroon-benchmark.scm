
(include "bench_.scm")

(define-class Point Object (x y))
(define-class Toto Object (to))
(define-class Tutu Object (tu))
(define-class Circle Point (radius))

(define-generic (add (p1) (p2)) 'no)
(define-method (add (p1 Point) (p2 Point)) 'ok)
(define-method (add (p1 Tutu) (p2 Point)) 'tu)
(define-method (add (p1 Toto) (p2 Point)) 'to)

(define-bench meroon-access
  ((p (make-Point 1 2)))
  (Point-x p))

(define-bench meroon-modif
  ((p (make-Point 1 2)))
  (Point-x-set! p 21))

(define-bench meroon-dispatch
  ((p1 (make-Point 1 2))
   (p2 (make-Point 3 4)))
  (add p1 p2))

(define-bench meroon-polymorhpic-dispatch
  ((p1 (make-Circle 1 2 12))
   (p2 (make-Circle 3 4 34)))
  (add p1 p2))

(pp (meroon-access))
(pp (meroon-modif))
(pp (meroon-dispatch))
(pp (meroon-polymorhpic-dispatch))
