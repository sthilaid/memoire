
(include "bench_.scm")
(include "oops/src/oops-macros.scm")

(define-class Point () (x y))
(define-class Toto () (to))
(define-class Tutu () (tu))
(define-class Circle (Point) (radius))

(define-method (add (p1 Point) (p2 Point)) 'ok)
(define-method (add (p1 Tutu) (p2 Point)) 'tu)
(define-method (add (p1 Toto) (p2 Point)) 'to)

(define-method (add5 (p1 Point) (p2 Point) (p3 Point) (p4 Point) (p5 Point))
  'to)

(define-bench oops-instance-creation
  ()
  (Point x: 1 y: 2))

(define-bench oops-access
  ((p (Point x: 1 y: 2)))
  (x p))

(define-bench oops-modif
  ((p (Point x: 1 y: 2)))
  (x-set! p 21))

(define-bench oops-dispatch-2
  ((p1 (Point x: 1 y: 2))
   (p2 (Point x: 3 y: 4)))
  (add p1 p2))

(define-bench oops-dispatch-5
  ((p1 (Point x: 1 y: 2))
   (p2 (Point x: 3 y: 4))
   (p3 (Point x: 1 y: 2))
   (p4 (Point x: 3 y: 4))
   (p5 (Point x: 1 y: 2)))
  (add5 p1 p2 p3 p4 p5))

(define-bench oops-polymorhpic-dispatch-2
  ((p1 (Circle x: 1 y: 2 radius: 12))
   (p2 (Circle x: 3 y: 4 radius: 34)))
  (add p1 p2))

(define-bench oops-polymorhpic-dispatch-5
  ((p1 (Circle x: 1 y: 2 radius: 12))
   (p2 (Circle x: 3 y: 4 radius: 34))
   (p3 (Circle x: 1 y: 2 radius: 12))
   (p4 (Circle x: 3 y: 4 radius: 34))
   (p5 (Circle x: 1 y: 2 radius: 12)))
  (add5 p1 p2 p3 p4 p5))

(pp (oops-instance-creation))
(pp (oops-access))
(pp (oops-modif))
(pp (oops-dispatch-2))
(pp (oops-dispatch-5))
(pp (oops-polymorhpic-dispatch-2))
(pp (oops-polymorhpic-dispatch-5))
