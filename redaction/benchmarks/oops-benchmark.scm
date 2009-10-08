
(include "bench_.scm")
(include "oops/src/oops-macros.scm")

(define-class Point () (x y))
(define-class Toto () (to))
(define-class Tutu () (tu))
(define-class Circle (Point) (radius))

(define-method (add (p1 Point) (p2 Point)) 'ok)
(define-method (add (p1 Tutu) (p2 Point)) 'tu)
(define-method (add (p1 Toto) (p2 Point)) 'to)

(define-bench oops-instance-creation
  ()
  (Point x: 1 y: 2))

(define-bench oops-access
  ((p (Point x: 1 y: 2)))
  (x p))

(define-bench oops-modif
  ((p (Point x: 1 y: 2)))
  (x-set! p 21))

(define-bench oops-dispatch
  ((p1 (Point x: 1 y: 2))
   (p2 (Point x: 3 y: 4)))
  (add p1 p2))

(define-bench oops-polymorhpic-dispatch
  ((p1 (Circle x: 1 y: 2 radius: 12))
   (p2 (Circle x: 3 y: 4 radius: 34)))
  (add p1 p2))

(pp (oops-instance-creation))
(pp (oops-access))
(pp (oops-modif))
(pp (oops-dispatch))
(pp (oops-polymorhpic-dispatch))
