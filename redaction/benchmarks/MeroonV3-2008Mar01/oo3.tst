;;; $Id: oo3.tst,v 3.28 2001/04/12 19:14:42 queinnec Exp $ 
;;; Copyright (c) 1990-96 by Christian Queinnec. All rights reserved.

;;;                ***********************************************
;;;                    Small, Efficient and Innovative Class System
;;;                                    Meroon 
;;;                              Christian Queinnec  
;;;                    Ecole Polytechnique & INRIA--Rocquencourt
;;;                ************************************************

;;;        Testing Meroon
;;;       Christian Queinnec  
;;;   \'Ecole Polytechnique & INRIA-Rocquencourt
;;;   91128 Palaiseau Cedex --- France

;;; Testing symbol->class
(symbol->class 'Class)
---
(symbol->class 'FoO2)
***
(symbol->class 'FoO2 (lambda (name) 3))
3

;;; testing some initial relationship
(Class-name (symbol->class 'Object))
Object
(Class-name (symbol->class 'Class))
Class
(eq? (Class-super-class (symbol->class 'Pre-Class)) (symbol->class 'Object))
#t

;;; testing object?
(Object? (symbol->class 'Object))
   #t
(Object? (symbol->class 'Class))
   #t
(Object? #t)
   #f
(Object? '())
   #f
(Object? 3)
   #f
(Object? (vector 'a 'b 'c))
   #f
;;; Testing Object? with safer-object
;;; REMOVED these tests that assume starting-offset to be present (this is
;;; only an internal macro).
;(if ;;(memq 'safer-object *meroon-features*)
;    (= 2 (starting-offset))
;    (Object? (vector 0 'a 'b)) 
;    #f )
;   #f
;(if ;;(memq 'safer-object *meroon-features*)
;    (= 2 (starting-offset))
;    (Object? (vector 0)) 
;    #f )
;   #f
   
;;; testing Class?
(Class? (symbol->class 'Class))
   #t
(Class? (symbol->class 'Object))
   #t
(Class? #t)
   #f
(Class? 33)
   #f

;;; testing define-class and generated functions
(define-class Point Object (x y))
   ---
(define the-Point (make-Point 2 44))
   ---
(Point-x the-Point)
   2
(Point-y the-Point)
   44
(Point? the-Point)
   #t
(Object? the-Point)
   #t
(Class? the-Point)
   #f
;this no more exists in Meroon  V3:
;(allocate-Point)
;   ---
((Class-allocator Point-class))
   ---
(begin (set-Point-x! the-Point 3) (Point-x the-Point))
   3
(begin (set-Point-y! the-Point 55) (Point-y the-Point))
   55

;;; testing a subclass of the previous one and related functions
(define-class ColoredPoint Point (color))
   ---
;(Class-fields (symbol->class 'ColoredPoint))
;   (x y color)
(set! the-Point (make-ColoredPoint 2 44 'white))
   ---
(Point-x the-Point)
   2
(Point-y the-Point)
   44
(Point? the-Point)
   #t
(ColoredPoint? the-Point)
   #t
(Object? the-Point)
   #t
(Class? the-Point)
   #f
;this no more exists in Meroon  V3:
;(allocate-ColoredPoint)
;   ---
((Class-allocator ColoredPoint-class))
   ---
(begin (set-Point-x! the-Point 3) (Point-x the-Point))
   3
(begin (set-Point-y! the-Point 55) (Point-y the-Point))
   55
(begin (set-Point-x! the-Point 33) (ColoredPoint-x the-Point))
   33
(begin (set-Point-y! the-Point 5) (ColoredPoint-y the-Point))
   5
(begin (set-ColoredPoint-x! the-Point 3)(ColoredPoint-x the-Point))
   3
(begin (set-ColoredPoint-y! the-Point 55) (ColoredPoint-y the-Point))
   55
(begin (set-ColoredPoint-x! the-Point 33) (Point-x the-Point))
   33
(begin (set-ColoredPoint-y! the-Point 5) (Point-y the-Point))
   5
(ColoredPoint-color the-Point)
   white
(begin (set-ColoredPoint-color! the-Point 'black) 
       (ColoredPoint-color the-Point) )
   black

;;; Testing vectorship 
(define-class NamedColoredPoint ColoredPoint ((* names)))
   ---
(set! the-Point 
      (make-NamedColoredPoint 
       2 44 'pink 
            3 'joe 'jill 'jack ) )
   ---
(NamedColoredPoint-names-length the-Point)
   3
(NamedColoredPoint-names the-Point 1)
   jill
(set-NamedColoredPoint-names! the-Point 0 'jean)
   ---
(NamedColoredPoint-names the-Point 0)
   jean
(NamedColoredPoint-names the-Point 2)
   jack
(Point-x the-Point)
   2
(Point-y the-Point)
   44
(Point? the-Point)
   #t
(ColoredPoint? the-Point)
   #t
(NamedColoredPoint? the-Point)
   #t
(Object? the-Point)
   #t
(Class? the-Point)
   #f
(ColoredPoint-color the-Point)
   pink
;this no more exists in Meroon  V3:
;(allocate-NamedColoredPoint 3)
;   ---
((Class-allocator NamedColoredPoint-class) 3)
   ---
(begin (set-Point-x! the-Point 3) (Point-x the-Point))
   3
(begin (set-Point-y! the-Point 55) (Point-y the-Point))
   55
(begin (set-Point-x! the-Point 33) (ColoredPoint-x the-Point))
   33
(begin (set-Point-y! the-Point 5) (ColoredPoint-y the-Point))
   5
(begin (set-ColoredPoint-x! the-Point 3)(ColoredPoint-x the-Point))
   3
(begin (set-ColoredPoint-y! the-Point 55) (ColoredPoint-y the-Point))
   55
(begin (set-ColoredPoint-x! the-Point 33) (Point-x the-Point))
   33
(begin (set-ColoredPoint-y! the-Point 5) (Point-y the-Point))
   5
(ColoredPoint-color the-Point)
   pink
(begin (set-ColoredPoint-color! the-Point 'black) 
       (ColoredPoint-color the-Point) )
   black
(set! the-Point (make-NamedColoredPoint 1 33 'pink 0))
   ---
(NamedColoredPoint-names-length the-Point)
   0
(NamedColoredPoint-names the-Point 0)
   ***
(NamedColoredPoint-names the-Point 3)
   ***
(set-NamedColoredPoint-names! the-Point 0 'jill)
   ***
(make-NamedColoredPoint 3 2 1 'foo)
   *** ; not a repetition factor
(make-NamedColoredPoint 3 2 1 2 'foo)
   *** ; too less
(make-NamedColoredPoint 3 2 1 2 'foo 'bar 'hux)
   *** ; too much
(set! the-Point (make-NamedColoredPoint 1 33 'pink 1 'joe))
   ---
(NamedColoredPoint-names-length the-Point)
   1
(NamedColoredPoint-names the-Point 0)
   joe
(NamedColoredPoint-names the-Point 1)
   ***
(set-NamedColoredPoint-names! the-Point 0 'jill)
   ---
(NamedColoredPoint-names the-Point 0)
   jill
(set-NamedColoredPoint-names! the-Point 1 'jack)
   ***
(NamedColoredPoint-names the-Point 0)
   jill
(set! the-Point ((Class-allocator NamedColoredPoint-class) 2))
   ---
(set-NamedColoredPoint-names! the-Point 0 'jill)
   ---
(NamedColoredPoint-names the-Point 0)
   jill
(set-NamedColoredPoint-names! the-Point 1 'jack)
   ---
(NamedColoredPoint-names the-Point 1)
   jack
(set-NamedColoredPoint-names! the-Point 2 'joe)
   ***
(NamedColoredPoint-names the-Point 2)
   ***

;;; Adding a mono-field to NamedColoredPoint
(define-class threeDNamedColoredPoint NamedColoredPoint (z))
   ---
(set! the-Point
      (make-threeDNamedColoredPoint
       2 44 'pink
            3 'joe 'jill 'jack
            33 ) )
   ---
(NamedColoredPoint-names-length the-Point)
   3
(threeDNamedColoredPoint-names-length the-Point)
   3
(NamedColoredPoint-names the-Point 1)
   jill
(threeDNamedColoredPoint-names the-Point 1)
   jill
(threeDNamedColoredPoint-names the-Point 2)
   jack
(set-NamedColoredPoint-names! the-Point 0 'jill)
   ---
(NamedColoredPoint-names the-Point 0)
   jill
(set-threeDNamedColoredPoint-names! the-Point 0 'jill)
   ---
(threeDNamedColoredPoint-names the-Point 0)
   jill
(Point-x the-Point)
   2
(Point-y the-Point)
   44
(Point? the-Point)
   #t
(ColoredPoint? the-Point)
   #t
(NamedColoredPoint? the-Point)
   #t
(Object? the-Point)
   #t
(Class? the-Point)
   #f
(ColoredPoint-color the-Point)
   pink
((Class-allocator threeDNamedColoredPoint-class) 3)
   ---
(begin (set-Point-x! the-Point 3) (Point-x the-Point))
   3
(begin (set-Point-y! the-Point 55) (Point-y the-Point))
   55
(begin (set-Point-x! the-Point 33) (ColoredPoint-x the-Point))
   33
(begin (set-Point-y! the-Point 5) (ColoredPoint-y the-Point))
   5
(begin (set-ColoredPoint-x! the-Point 3)(ColoredPoint-x the-Point))
   3
(begin (set-ColoredPoint-y! the-Point 55) (ColoredPoint-y the-Point))
   55
(begin (set-ColoredPoint-x! the-Point 33) (Point-x the-Point))
   33
(begin (set-ColoredPoint-y! the-Point 5) (Point-y the-Point))
   5
(begin (set-ColoredPoint-color! the-Point 'black) 
       (ColoredPoint-color the-Point) )
   black
(set! the-Point (make-threeDNamedColoredPoint 1 33 'pink 0 55))
   ---
(threeDNamedColoredPoint-names-length the-Point)
   0
(threeDNamedColoredPoint-names the-Point 1)
   ***
(threeDNamedColoredPoint-names the-Point 3)
   ***
(set-threeDNamedColoredPoint-names! the-Point 0 'jill)
   ***
(set-threeDNamedColoredPoint-names! the-Point 1 'jill)
   ***
(make-threeDNamedColoredPoint 3 2 1 'foo)
   *** ; not a repetition factor
(make-threeDNamedColoredPoint 3 2 1 2 'foo)
   *** ; too less
(make-threeDNamedColoredPoint 3 2 'foo 'bar 'hux)
   *** ; not a repetition factor
(make-threeDNamedColoredPoint 3 2 1 2 'foo 'bar 0 'hux 'wwrek)
   *** ; too much
(make-threeDNamedColoredPoint 3 2 1 2 'foo 'bar 2 'hux)
   *** ; too less
(make-threeDNamedColoredPoint 3 2 1 2 'foo 'bar 2 'hux 'bar 'wrek 'quux)
   *** ; too much


;;; Testing vectorship inheritance 
(define-class NickNamedColoredPoint NamedColoredPoint ((* nicknames)))
   ---
(set! the-Point 
      (make-NickNamedColoredPoint 
       2 44 'pink 
            3 'joe 'jill 'jack
            2 'adam 'eve ) )
   ---
(NamedColoredPoint-names-length the-Point)
   3
(NickNamedColoredPoint-names-length the-Point)
   3
(NickNamedColoredPoint-nicknames-length the-Point)
   2
(NamedColoredPoint-names the-Point 1)
   jill
(NickNamedColoredPoint-names the-Point 1)
   jill
(NickNamedColoredPoint-names the-Point 2)
   jack
(NickNamedColoredPoint-nicknames the-Point 0)
   adam
(NickNamedColoredPoint-nicknames the-Point 1)
   eve
(NickNamedColoredPoint-nicknames the-Point 2)
   ***
(set-NamedColoredPoint-names! the-Point 0 'jill)
   ---
(NamedColoredPoint-names the-Point 0)
   jill
(set-NickNamedColoredPoint-names! the-Point 0 'jill)
   ---
(NickNamedColoredPoint-names the-Point 0)
   jill
(set-NickNamedColoredPoint-nicknames! the-Point 0 'chaim)
   ---
(NickNamedColoredPoint-nicknames the-Point 0)
   chaim
(Point-x the-Point)
   2
(Point-y the-Point)
   44
(Point? the-Point)
   #t
(ColoredPoint? the-Point)
   #t
(NamedColoredPoint? the-Point)
   #t
(Object? the-Point)
   #t
(Class? the-Point)
   #f
(ColoredPoint-color the-Point)
   pink
((Class-allocator NickNamedColoredPoint-class) 3 2)
   ---
(begin (set-Point-x! the-Point 3) (Point-x the-Point))
   3
(begin (set-Point-y! the-Point 55) (Point-y the-Point))
   55
(begin (set-Point-x! the-Point 33) (ColoredPoint-x the-Point))
   33
(begin (set-Point-y! the-Point 5) (ColoredPoint-y the-Point))
   5
(begin (set-ColoredPoint-x! the-Point 3)(ColoredPoint-x the-Point))
   3
(begin (set-ColoredPoint-y! the-Point 55) (ColoredPoint-y the-Point))
   55
(begin (set-ColoredPoint-x! the-Point 33) (Point-x the-Point))
   33
(begin (set-ColoredPoint-y! the-Point 5) (Point-y the-Point))
   5
(begin (set-ColoredPoint-color! the-Point 'black) 
       (ColoredPoint-color the-Point) )
   black
(set! the-Point (make-NickNamedColoredPoint 1 33 'pink 0 0))
   ---
(NickNamedColoredPoint-names-length the-Point)
   0
(NickNamedColoredPoint-nicknames-length the-Point)
   0
(NickNamedColoredPoint-names the-Point 1)
   ***
(NickNamedColoredPoint-names the-Point 3)
   ***
(set-NickNamedColoredPoint-names! the-Point 0 'jill)
   ***
(set-NickNamedColoredPoint-names! the-Point 1 'jill)
   ***
(NickNamedColoredPoint-nicknames the-Point 3)
   ***
(set-NickNamedColoredPoint-nicknames! the-Point 1 'jill)
   ***
(make-NickNamedColoredPoint 3 2 1 'foo)
   *** ; not a repetition factor
(make-NickNamedColoredPoint 3 2 1 2 'foo)
   *** ; too less
(make-NickNamedColoredPoint 3 2 1 2 'foo 'bar 'hux)
   *** ; not a repetition factor
(make-NickNamedColoredPoint 3 2 1 2 'foo 'bar 0 'hux)
   *** ; too much
(make-NickNamedColoredPoint 3 2 1 2 'foo 'bar 2 'hux)
   *** ; too less
(make-NickNamedColoredPoint 3 2 1 2 'foo 'bar 2 'hux 'bar 'wrek 'quux)
   *** ; too much

;;; test fields after two sequences
(define-class extra-NickNamedColoredPoint NickNamedColoredPoint
  (extra1 extra2) )
   ---
(set! the-Point 
      (make-extra-NickNamedColoredPoint 
       2 44 'pink 
            3 'joe 'jill 'jack
            2 'adam 'eve 
            'apiali 'docious ) )
   ---
(extra-NickNamedColoredPoint-extra2 the-Point)
   docious
(set-extra-NickNamedColoredPoint-extra2! the-Point 'foo)
   ---
(extra-NickNamedColoredPoint-extra2 the-Point)
   foo
(NamedColoredPoint-names-length the-Point)
   3
(extra-NickNamedColoredPoint-names-length the-Point)
   3
(extra-NickNamedColoredPoint-nicknames-length the-Point)
   2
(NamedColoredPoint-names the-Point 1)
   jill
(extra-NickNamedColoredPoint-names the-Point 1)
   jill
(extra-NickNamedColoredPoint-nicknames the-Point 1)
   eve
(set-NamedColoredPoint-names! the-Point 0 'jill)
   ---
(NamedColoredPoint-names the-Point 0)
   jill
(set-extra-NickNamedColoredPoint-names! the-Point 0 'jill)
   ---
(extra-NickNamedColoredPoint-names the-Point 0)
   jill
(set-extra-NickNamedColoredPoint-nicknames! the-Point 0 'chaim)
   ---
(extra-NickNamedColoredPoint-nicknames the-Point 0)
   chaim
(Point-x the-Point)
   2
(Point-y the-Point)
   44
(Point? the-Point)
   #t
(ColoredPoint? the-Point)
   #t
(NamedColoredPoint? the-Point)
   #t
(Object? the-Point)
   #t
(Class? the-Point)
   #f
(ColoredPoint-color the-Point)
   pink
((Class-allocator extra-NickNamedColoredPoint-class) 3 2)
   ---
(begin (set-Point-x! the-Point 3) (Point-x the-Point))
   3
(begin (set-Point-y! the-Point 55) (Point-y the-Point))
   55
(begin (set-Point-x! the-Point 33) (ColoredPoint-x the-Point))
   33
(begin (set-Point-y! the-Point 5) (ColoredPoint-y the-Point))
   5
(begin (set-ColoredPoint-x! the-Point 3)(ColoredPoint-x the-Point))
   3
(begin (set-ColoredPoint-y! the-Point 55) (ColoredPoint-y the-Point))
   55
(begin (set-ColoredPoint-x! the-Point 33) (Point-x the-Point))
   33
(begin (set-ColoredPoint-y! the-Point 5) (Point-y the-Point))
   5
(begin (set-ColoredPoint-color! the-Point 'black) 
       (ColoredPoint-color the-Point) )
   black
; pourquoi pendant un an n'y a-t-il pas eu d'erreurs la ? la precedente
; definition de the-Point omettait 'un 'deux ?
(set! the-Point (make-extra-NickNamedColoredPoint 1 33 'pink 0 0 'un 'deux))
   ---
(extra-NickNamedColoredPoint-names-length the-Point)
   0
(extra-NickNamedColoredPoint-nicknames-length the-Point)
   0
(extra-NickNamedColoredPoint-names the-Point 3)
   ***
(set-extra-NickNamedColoredPoint-names! the-Point 0 'jill)
   ***
(extra-NickNamedColoredPoint-nicknames the-Point 3)
   ***
(set-extra-NickNamedColoredPoint-nicknames! the-Point 0 'jill)
   ***
(make-extra-NickNamedColoredPoint 3 2 1 'foo)
   *** ; not a repetition factor
(make-extra-NickNamedColoredPoint 3 2 1 2 'foo)
   *** ; too less
(make-extra-NickNamedColoredPoint 3 2 1 2 'foo 'bar 'hux)
   *** ; not a repetition factor
(make-extra-NickNamedColoredPoint 3 2 1 2 'foo 'bar 0 'hux)
   *** ; too much
(make-extra-NickNamedColoredPoint 3 2 1 2 'foo 'bar 2 'hux)
   *** ; too less
(make-extra-NickNamedColoredPoint 3 2 1 2 'foo 'bar 2 'hux 'bar 'wrek 'quux)
   ---
(make-extra-NickNamedColoredPoint 3 2 1 2 'foo 'bar 2 'hux 'bar 'wrek 'quux 55)
   *** ; too much
  
;;; Add a third sequence and test.
(define-class hyper-extra-NickNamedColoredPoint extra-NickNamedColoredPoint
  ((* extra)) )
   ---
(hyper-extra-NickNamedColoredPoint-extra-length
 (make-hyper-extra-NickNamedColoredPoint
  3 2 1 2 'foo 'bar 2 'hux 'bar 'wrek 'quux 4 #\A #\B #\C #\D ) )
   4

;;; generic functions
(initialize! 3)
   3
(initialize! #t)
   #t
(initialize! the-Point)
   ---
(show (symbol->class 'Class))
   ---
;;; should print the default: #<a Point>
(show the-Point) 
   ---
(show 34)
   ---
(show 3.14)
   ---
(show #\space)
   ---
(show #\a)
   ---
(show (list #t #f '()))
   ---
;;; remove this test since it uses equal to compare circular structures.
;(equal? (clone (symbol->class 'Class)) (symbol->class 'Class))
;   #t
(let ((pt (make-Point 1 2)))
  (equal? pt (clone pt)) )
   #t
(clone 3) ; 3 is not an instance
   ***
   
"    Now testing generic functions ..."
   ---

;;; generic with default method, discriminating on the second variable
(define-generic (foo x (y) z)
  (list x y z) )
   ---
(foo 3 the-Point 4)
   ---
(length (foo 3 the-Point 4))
   3
(foo 3 4 5) ; 4 is not an instance but there is a default method
   (3 4 5) 
   
;;; Adding methods
(define-method (foo x (o ColoredPoint) z)
  (ColoredPoint-x o) )
   ---
(let ((pt (make-Point 1 2)))
  (length (foo 9 pt 8)) )
   3
(let ((pt (make-ColoredPoint 1 2 'red)))
  (foo 9 pt 8) )
   1
(let ((pt (make-NamedColoredPoint 1 2 'red 1 'jill)))
  (foo 9 pt 8) )
   1

;;; Adding a method above, check that the method on ColoredPoint is not
;;; shadowed.
(define-method (foo x (o Object) z)
  'foo )
   ---
(let ((pt (make-Point 1 2)))
  (foo 9 pt 8) )
   foo
(let ((pt (make-ColoredPoint 1 2 'red)))
  (foo 9 pt 8) )
   1
(let ((pt (make-NamedColoredPoint 1 2 'red 1 'jill)))
  (foo 9 pt 8) )
   1

;;; Adding a method below.
(define-method (foo x (o NamedColoredPoint) z)
  'NamedColoredPoint )
   ---
(let ((pt (make-Point 1 2)))
  (foo 9 pt 8) )
   foo
(let ((pt (make-ColoredPoint 1 2 'red)))
  (foo 9 pt 8) )
   1
(let ((pt (make-NamedColoredPoint 1 2 'red 1 'jill)))
  (foo 9 pt 8) )
   NamedColoredPoint

;;; redefining method, check that it does not redefine that of subclasses.
(define-method (foo x (o ColoredPoint) z)
  (ColoredPoint-y o) )
   ---
(let ((pt (make-Point 1 2)))
  (foo 9 pt 8) )
   foo
(let ((pt (make-ColoredPoint 1 2 'red)))
  (foo 9 pt 8) )
   2
(let ((pt (make-NamedColoredPoint 1 2 'red 1 'jill)))
  (foo 9 pt 8) )
   NamedColoredPoint

;;; Incoherent method
(define-method (foo x) 3)
   ***
(define-method (foo (x) y z) 4)
   ***
(define-method (foo x (y) z . others) 5)
   ***

;;; *classes* overflow (if *classes* is a vector of 10 classes). This highly 
;;; depends on the number of internal classes needed by Meroon itself.
(define-class Point-3d Point (z))
   ---

;;; Adding a subclass and checking that already present methods are inherited
(let ((pt (make-Point-3d 1 2 3)))
  (foo 9 pt 8) )
   foo
;;; adding a method on Point
(define-method (foo x (y Point) z)
  (Point-x y) )
   ---
;;; and recheck that Point-3d see it
(let ((pt (make-Point-3d 11 2 3)))
  (foo 9 pt 8) )
   11

;;; Try to exercize bugs on the elaboration of dispatchers.
(define-class R Object (a))
   ---
(define-class C1 R (b))
   ---
(define-class C2 R (c))
   ---
(define-class C3 R (d))
   ---
(define-generic (gg (o)) 'Object)
    ---
(begin (show-generic 'gg) (show-dispatcher 'gg))
   ---
(define-method (gg (o C1)) 'C1)
   ---
(begin (show-generic 'gg) (show-dispatcher 'gg))
   ---
(list (gg (make-R 1)) (gg (make-C1 2 3)) (gg (make-C2 4 5)) (gg (make-C3 6 7)))
   (Object C1 Object Object)
(define-method (gg (o C2)) 'C2)
   ---
(begin (show-generic 'gg) (show-dispatcher 'gg))
   ---
(list (gg (make-R 1)) (gg (make-C1 2 3)) (gg (make-C2 4 5)) (gg (make-C3 6 7)))
   (Object C1 C2 Object)
(define-method (gg (o C3)) 'C3)
   ---
(begin (show-generic 'gg) (show-dispatcher 'gg))
   ---
(list (gg (make-R 1)) (gg (make-C1 2 3)) (gg (make-C2 4 5)) (gg (make-C3 6 7)))
   (Object C1 C2 C3)
(define-method (gg (o R)) 'R)
   ---
(begin (show-generic 'gg) (show-dispatcher 'gg))
   ---
(list (gg (make-R 1)) (gg (make-C1 2 3)) (gg (make-C2 4 5)) (gg (make-C3 6 7)))
   (R C1 C2 C3)

;;; Redefinition of a class. This test is weird since the semantics of a
;;; class redefinition is unclear. 
(define-class Point-4d Point-3d (h))
   ---
(define-class Point-4d Point-3d (u))
   ---

;;; Testing call-next-method
(begin 
  (define-generic (foo x (y) z)
    'default )
  (define-method (foo x (y Point) z) (list 'Point x))
  (define-method (foo x (y NamedColoredPoint) z) 
    (list 'NamedColoredPoint (call-next-method)) )
  #t )
   ---
(foo 3 (make-Point 1 2) 4)
   (Point 3)
(foo 3 (make-NamedColoredPoint 1 2 'red 1 'Joe) 5)
   (NamedColoredPoint (Point 3))
(foo 3 (make-ColoredPoint 1 2 'red) 5)
   (Point 3)
;; adding a method in between
(define-method (foo x (y ColoredPoint) z)
  (list 'ColoredPoint x) )
   ---
(foo 3 (make-Point 1 2) 4)
   (Point 3)
(foo 3 (make-ColoredPoint 1 2 'red) 5)
   (ColoredPoint 3)
(foo 3 (make-NamedColoredPoint 1 2 'red 1 'Joe) 5)
   (NamedColoredPoint (ColoredPoint 3))
;; Modifying a method in between
(define-method (foo x (y ColoredPoint) z)
  (list 'ColoredPoint (call-next-method)) )
   ---
(foo 3 (make-Point 1 2) 4)
   (Point 3)
(foo 3 (make-NamedColoredPoint 1 2 'red 1 'Joe) 5)
   (NamedColoredPoint (ColoredPoint (Point 3)))
(foo 3 (make-ColoredPoint 1 2 'red) 5)
   (ColoredPoint (Point 3))
;; forcing a call-next-method where there is a default method.
(define-method (foo x (y Point) z)
  (list 'Point (call-next-method)) )
   ---
(foo 3 (make-NamedColoredPoint 1 2 'red 1 'Joe) 5)
   (NamedColoredPoint (ColoredPoint (Point default)))
(foo 3 (make-ColoredPoint 1 2 'red) 5)
   (ColoredPoint (Point default))
(foo 3 (make-Point 1 2) 4)
   (Point default)
;;; Still more complex, add dynamically a method before invoking
;;; call-next-method. It is bad style to put a dynamic define-method.
(define-generic (foo (o)) 'default)
   ---
(define-method (foo (o ColoredPoint)) 
  (define-method (foo (o Point)) 'Point)
  (list (call-next-method)) )
   ---
(foo (make-Point 111 222))
   default
(foo (make-NamedColoredPoint 1 2 'red 1 'Joe))
   (Point)
(foo (make-Point 111 222))
   Point



;;; Show guts
(show *classes*)
   ---
(show (Class-fields (symbol->class 'NamedColoredPoint)))
   ---
(show-hierarchy)
   ---
(show-hierarchy 'Object)   ; same as above
   ---
(show-hierarchy 'Field)
   ---
(show-generic 'show)
   ---
(show-meroon)
   ---
(show-dispatcher 'foo)
   ---


;;; Testing dispatcher compaction. We start by modifying an internal `constant' 
;;; to be sure that dispatchers will be compacted.
(set! *dispatcher-max-depth* 2)
   ---
(define-generic (foo (x Object)) 'default)
   ---
(show-generic 'foo)
   ---
(show-dispatcher 'foo)
   ---
(list (foo (make-Object)))
   (default)
(define-method (foo (x Object)) 'Object)
   ---
; observe that the default should be changed to reflect the new method.
(show-generic 'foo)
   ---
(show-dispatcher 'foo)
   ---
(list (foo (make-Object)))
   (Object)
(define-method (foo (o ColoredPoint)) 'ColoredPoint)
   ---
(show-generic 'foo)
   ---
(show-dispatcher 'foo)
   ---
(list (foo (make-Object)) 
      (foo (make-Point 11 22))
      (foo (make-ColoredPoint 11 22 'red))
      (foo (make-NamedColoredPoint 11 22 'red 1 'Jack)) )
   (Object Object ColoredPoint ColoredPoint)
; observe the two levels of Subclass-Dispatchers on Point then on ColoredPoint
; on the yes subpart.
(define-method (foo (o Point)) 'Point)
   ---
(show-generic 'foo)
   ---
(show-dispatcher 'foo)
   ---
(list (foo (make-Object)) 
      (foo (make-Point 11 22))
      (foo (make-ColoredPoint 11 22 'red))
      (foo (make-NamedColoredPoint 11 22 'red 1 'Jack)) )
   (Object Point ColoredPoint ColoredPoint)
; trigger the compaction since level of Subclass-Dispatcher would exceed 2.
(define-method (foo (o NamedColoredPoint)) 'NamedColoredPoint)
   ---
(show-generic 'foo)
   ---
(show-dispatcher 'foo)
   ---
(list (foo (make-Object)) 
      (foo (make-Point 11 22))
      (foo (make-ColoredPoint 11 22 'red))
      (foo (make-NamedColoredPoint 11 22 'red 1 'Jack)) )
   (Object Point ColoredPoint NamedColoredPoint)
; change the method 
(define-method (foo (o NamedColoredPoint)) 'NamedColoredPoint-33)
   ---
(show-generic 'foo)
   ---
(show-dispatcher 'foo)
   ---
(list (foo (make-Object)) 
      (foo (make-Point 11 22))
      (foo (make-ColoredPoint 11 22 'red))
      (foo (make-NamedColoredPoint 11 22 'red 1 'Jack)) )
   (Object Point ColoredPoint NamedColoredPoint-33)
; change another method
(define-method (foo (o Point)) 'Point-33)
   ---
(show-generic 'foo)
   ---
(show-dispatcher 'foo)
   ---
(list (foo (make-Object)) 
      (foo (make-Point 11 22))
      (foo (make-ColoredPoint 11 22 'red))
      (foo (make-NamedColoredPoint 11 22 'red 1 'Jack)) )
   (Object Point-33 ColoredPoint NamedColoredPoint-33)
; trigger the compaction 
(define-method (foo (o Class)) 'Class)
   ---
(show-generic 'foo)
   ---
(show-dispatcher 'foo)
   ---
(list (foo (make-Object)) 
      (foo (make-Point 11 22))
      (foo (make-ColoredPoint 11 22 'red))
      (foo (make-NamedColoredPoint 11 22 'red 1 'Jack))
      (foo Handy-Class-class) )
   (Object Point-33 ColoredPoint NamedColoredPoint-33 Class)


;;; Tracing generic functions
(define-generic (foo (x) y z) `default)
   ---
(foo 1 2 3)
   default
(define-method (foo (x Point) y z) `point)
   ---
(begin (show-generic 'foo) (show-dispatcher 'foo))
   ---
(foo (make-Point 1 2) 3 4)
   point
(define foo-args #t)
   ---
(generic-trace (symbol->generic 'foo)
               (lambda (x y z) 
                 (display `(before is called))
                 (set! foo-args `call) )
               (lambda (result) 
                 (display `(after is called))
                 `(return ,result) ) )
   ---
;;; All methods appear to be different since they are all traced.
(begin (show-generic 'foo) (show-dispatcher 'foo))
   ---
(set! foo-args 'void)
   ---
(foo 'a 'b 'c)
   (return default)
foo-args
   call
(set! foo-args 'void)
   ---
(foo (make-Point 1 2) 3 4)
   (return point)
foo-args
   call
(generic-untrace (symbol->generic 'foo))
   ---
(set! foo-args 'void)
   ---
(foo 'a 'b 'c)
   default
foo-args
   void
(foo (make-Point 1 2) 3 4)
   point

;;; Tracing generic functions with a dotted variable list
(define-generic (foo (x) y . z) `(default . ,z))
   ---
(foo 1 2)
   (default)
(foo 1 2 3)
   (default 3)
(foo 1 2 3 4 5)
   (default 3 4 5)
(define-method (foo (x Point) y . z) `(point . ,z))
   ---
(foo (make-Point 1 2) 3)
   (point)
(foo (make-Point 1 2) 3 4)
   (point 4)
(foo (make-Point 1 2) 3 4 5 6)
   (point 4 5 6)
(generic-trace (symbol->generic 'foo)
               (lambda (x y . z) (set! foo-args `call))
               (lambda (result) 
                 `(return ,result) ) )
   ---
(set! foo-args 'void)
   ---
(foo 1 2)
   (return (default))
foo-args
   call
(set! foo-args 'void)
   ---
(foo (make-Point 1 2) 3 4 5)
   (return (point 4 5))
foo-args
   call
(generic-untrace (symbol->generic 'foo))
   ---
(set! foo-args 'void)
   ---
(foo 'a 'b 'c)
   (default c)
foo-args
   void
(foo (make-Point 1 2) 3 4)
   (point 4)


;;; testing call-next-method
(define-generic (bar (o)) 'root)
   ---
(define-method (bar (o Object)) 'object)
   ---
(define-method (bar (o ColoredPoint))
  (list 'coloredpoint (call-next-method) (call-next-method)) )
   ---
(bar ((Class-allocator ColoredPoint-class)))
   (coloredpoint object object)
(define-method (bar (o Point)) 'point)
   ---
(bar ((Class-allocator ColoredPoint-class)))
   (coloredpoint point point)
;;; testing the ultimate (call-next-method)
(define-method (bar (o Object))
  (call-next-method) )
   ---
(bar (make-Object))
   root
(bar Point-3d-class)
   root
;;; testing next-method? 
(define-method (bar (o Point-3d)) 
  (next-method?) )
   ---
(not (not (bar ((Class-allocator Point-3d-class)))))
   #t
(bar Point-3d-class)
   root
(define-method (bar (o Object))
  (next-method?) )
   ---
(bar Point-3d-class)
   #f
;;; this does not change by inheritance
(define-method (bar (o Class))
  (call-next-method) )
   ---
(bar Point-3d-class)
   #f


;;; Testing meta object protocol
(is-a? (make-Point 1 2) (symbol->class 'Point))
   #t
(is-a? (make-Point 1 2) Point-class)
   #t
(is-a? (make-Point 1 2) (symbol->class 'ColoredPoint))
   #f
(is-a? (make-Point 1 2) ColoredPoint-class)
   #f
;;; Class-maker no more exists in Meroon V3
;(Point? ((Class-maker (symbol->class 'Point)) 1 2))
;   #t
(Point? ((Class-allocator (symbol->class 'Point))))
   #t

;;; Testing some errors
;(make-Class 'foo) ;; cannot handle this error
;   ***  ; too less arguments
(define-class foo1 Point (x))
   *** ; redefinition of X
(define-class foo2 Point ((x) y))
   *** ; bad field descriptor


;;; Testing mutability of fields
(define-class Immutable-Point Object 
  ((= x :immutable)
   (= y :immutable) ) )
   ---
(set! the-Point (make-Immutable-Point 2 44))
   ---
(Immutable-Point-x the-Point)
   2
(Immutable-Point-y the-Point)
   44
(Immutable-Point? the-Point)
   #t
(Object? the-Point)
   #t
(Class? the-Point)
   #f
(map Field-mutable? (Class-fields (symbol->class 'Immutable-Point)))
   (#f #f)
;;; Testing immutability conservation by inheritance
(define-class Colored-Immutable-Point Immutable-Point
  (color) )
   ---
(map Field-mutable? 
     (Class-fields (symbol->class 'Colored-Immutable-Point)) )
   (#f #f #t)

;;; Testing immutability of a class 
;;; :careless is no more a primitive field option in Meroon V3
(define-class Other-Immutable-Point Object 
  (x y) :immutable)
   ---
(set! the-Point (make-Other-Immutable-Point 2 44))
   ---
(Other-Immutable-Point-x the-Point)
   2
(Other-Immutable-Point-y the-Point)
   44
(Other-Immutable-Point? the-Point)
   #t
(Object? the-Point)
   #t
(Class? the-Point)
   #f
(map Field-mutable? 
     (Class-fields (symbol->class 'Other-Immutable-Point)) )
   (#f #f)
;;; Testing immutability conservation by inheritance
(define-class Colored-Other-Immutable-Point Other-Immutable-Point
  (color) )
   ---
(map Field-mutable? 
     (Class-fields (symbol->class 'Colored-Other-Immutable-Point)) )
   (#f #f #t)
;;; Testing Mutable-Field? generic function
;;; does not exist anymore in V3.
;(map Mutable-Field? 
;     (Class-fields (symbol->class 'Other-Immutable-Point)) )
;   (#f #f)
;;; testing care(ful,less)
;;; Removed these tests since :careless is no more provided. In the 
;;; previous definition of Other-Immutable-Point, fields X and Y were
;;; :careless.
;(set! the-Point (make-Point 2 44))
;   ---
;(Point-x the-Point)
;   2
;(Other-Immutable-Point-x the-Point)
;   2
;(Colored-Other-Immutable-Point-x the-Point)
;   2
;(set! the-Point (make-Colored-Immutable-Point 2 44 'pink))
;   ---
;(Point-x the-Point)
;   ***
;(Other-Immutable-Point-x the-Point)
;   2
;(Colored-Other-Immutable-Point-x the-Point)
;   2
;; test on predefined classes
(Class-name (make-Object))
   ***


;;; testing show-generic-trace

(show-generic-trace 'foo)
   ---
(foo (make-Point 1 2) 3)
   (point)
(foo (make-Point 1 2) 3 4)
   (point 4)
(foo (make-Point 1 2) 3 4 5 6)
   (point 4 5 6)
(show-generic-trace 'foo)
   ---   ; just redefine foo to be traced exactly the same
(foo (make-Point 1 2) 3)
   (point)
(foo (make-Point 1 2) 3 4)
   (point 4)
(foo (make-Point 1 2) 3 4 5 6)
   (point 4 5 6)
(show-generic-untrace 'foo)
   ---  ; no more trace
(foo (make-Point 1 2) 3)
   (point)
(foo (make-Point 1 2) 3 4)
   (point 4)
(foo (make-Point 1 2) 3 4 5 6)
   (point 4 5 6)
(show-generic-untrace 'foo)
   ---
(foo (make-Point 1 2) 3)
   (point)
(foo (make-Point 1 2) 3 4)
   (point 4)
(foo (make-Point 1 2) 3 4 5 6)
   (point 4 5 6)

;;; test a class without slots
(define-class X1 Object ())
   ---
(make-X1)
   ---
(X1? (make-X1))
   #t
;;; subclassing a class without slots, first create some fields
(define-class X2 X1 (x y))
   ---
(define-class X3 X2 ())
   ---
(set! the-Point (make-X2 11 22))
   ---
(X2-x the-Point)
   11
(X3? the-Point)
   #f
(X2? the-Point)
   #t
(X1? the-Point)
   #t


;;; Testing exceptions
(define-class 3 Object ())
   ***
(define-class X5 3 ())
   ***
(define-class X4 Unknown ())
   ***
;;; I do not check whether it is possible to redefine basic classes.
;(define-class Anomaly Object ())
;   ***
;(define-class Anomaly Anomaly ())
;   ***
;;; allocate may take a fix number of arguments and errors are then catched
;;; by the underlying Scheme.
;(allocate-NamedColoredPoint 1 -3 3)
;   ***
((Class-allocator NamedColoredPoint-class) 'foo)
   ***
((Class-allocator NamedColoredPoint-class) -2)
   ***
;;; same reason: allocate-NamedColoredPoint is unary.
;(allocate-NamedColoredPoint)
;   ***
;;; Removed these tests since now they are directly handled by the
;;; underlying Scheme
;(make-Point 11 22 33)
;   ***
;(make-Point)
;   ***
;(make-Point 11)
;   ***
(make-NamedColoredPoint 11 22 'red 0 9)
   ***
(make-NamedColoredPoint 11 22 'red 0 9 8)
   ***
(make-NamedColoredPoint 11 22 'red 1 9 8)
   ***
(make-NamedColoredPoint 11 22 'red 'foo 9)
   ***
(make-NamedColoredPoint 11 22 'red -3 9)
   ***
(define-class Y5 Point (x))
   *** ; redefine the name of the field
(define-class Y4 Point ((* x)))
   *** ; redefine the type of the field
(define-class Y0 Object (3))
   ***
(define-class Y1 Object ((= 3)))
   ***
(define-class Y2 Object ((= foo :bla-bla)))        ; Not an error
   ---
(ColoredPoint-color (make-Point 111 222))
   ***
(set-ColoredPoint-color! (make-Point 111 222) 'red)
   ***
(symbol->class 3)
   ***
(symbol->generic 4)
   ***
(define-generic (zz))
   ***
(define-generic (zz a b . c))
   ***
(define-generic foo (a b))
   ***
(define-method foo)
   ***
(define-method foo a)
   ***
(define-generic (zz a (o) . c))
   ---
(define-method (zz (a)))
   ***
(define-method (zz (a) b))
   ***
(define-method (zz (a) c . b))
   ***
(define-method (zz s (a) b))
   ***
;;; signature incompatibility
(define-method (zz (a Point)))
   ***
(define-method (zz (a Point) b))
   ***
(define-method (zz (a Point) c . b))
   ***
(define-method (zz s (a Point) b))
   ***
(define-method (yy (a Class)) 4)
   ***
(define-method (clone (a ZZZ)) 5)
   ***
(show-hierarchy 3.14)
   ***
(show-generic 3)
   ***
(generic-trace 33 car car)
   ***
(generic-trace zz 2 car)
   ***
(generic-trace zz car cdr)
   ***
(generic-untrace 3)
   ***
(show-generic-trace 44)
   ***
(show-generic-untrace 44)
   ***

;;; Testing that :prototype does no harm
(define-generic (zztop (o)) 'default)
   ---
(define-method (zztop (o ColoredPoint)) 'ColoredPoint)
   ---
(set! the-Point (make-NamedColoredPoint 33 44 'red 2 'joe 'jill))
   ---
(zztop the-Point)
   ColoredPoint
(zztop (make-ColoredPoint 55 66 'blue))
   ColoredPoint
(zztop (make-Point 55 66))
   default
;;; With Meroon V3, this registers a new class in *classes* with the
;;; previous classnumber but leaves the accessors to be the old ones
;;; which close the old class. This interacts with renumbering.
(define-class Point Object (x y) :prototype)
   --- 
(zztop the-Point)
   ColoredPoint
(zztop (make-ColoredPoint 55 66 'blue))
   ColoredPoint
(zztop (make-Point 55 66))
   default
(define-class ColoredPoint Point (color) :prototype)
   --- 
(zztop the-Point)
   ColoredPoint
(zztop (make-ColoredPoint 55 66 'blue))
   ColoredPoint
(zztop (make-Point 55 66))
   default
(define-class NamedColoredPoint ColoredPoint ((* names)) :prototype)
   --- 
(zztop the-Point)
   ColoredPoint
(zztop (make-ColoredPoint 55 66 'blue))
   ColoredPoint
(zztop (make-Point 55 66))
   default
;;; this cannot be done in interpreted mode. There is a redefinition of the 
;;; fields and this has an unclear semantics.
;(define-class Point Object (x y z) :prototype) ;; one more field
;   ***
;(define-class Point Object (x) :prototype) ;; one field less
;   ***
;(define-class Point Object (x yy) :prototype) ;; one field name changed
;   ***


;;; testing :eponymous. This changed in V3, the name of the class now 
;;; is <class-name>-class so the :eponymous keyword is ignored.
(define-class eponym Object (x) :eponymous)
   ---
(eq? eponym-class (symbol->class 'eponym))
   #t

;;; Testing show-unveiled
(unveil (symbol->class 'Poly-Field))
   ---
;;; This was buggy
(unveil (cons 3 (make-Point 33 44)))
   ---
;;; testing unveil with circularity
(define-class A Object (x) :immutable)
   ---
(define-class B Object (y) :mutable)
   ---
(begin (set! the-Point (make-A (make-B 'wait)))
       (set-B-y! (A-x the-Point) the-Point)
       (unveil the-Point) )
   ---
(define-class C A (y (* z)))
   ---
(begin (set! the-Point (make-C (make-B 'wait)
                               'y 
                               3 (make-C 11 'yy 0) 
                                 (make-B 22)
                                 (make-B 33) ))
       (set-B-y! (A-x the-Point) the-Point)
       (set-B-y! (C-z the-Point 1) (C-z the-Point 2))
       (unveil the-Point) )
   ---
;;; test dotted lists and cyclic vectors.
(unveil (let ((o (list (make-A 33))))
          (set-cdr! o o)
          o ))
   ---
(unveil (let ((o (list (make-A 333))))
          (set-cdr! o (list o))
          o ))
   ---
(unveil (let* ((v (vector 'aa 2 3))
               (o (list v)) )
          (vector-set! v 1 o)
          (set-cdr! o (list v)) 
          v ))
   ---
;;; Change the *unveil-maximal-indent* constant
(set! *unveil-maximal-indent* 3)
   ---
(unveil (let* ((v (vector 'aa 2 3))
               (o (list v)) )
          (vector-set! v 1 o)
          (set-cdr! o (list v)) 
          v ))
   ---

;;; testing field-value
;(define-class A Object (x) :immutable) ; already defined above
;(define-class C A (y (* z))) ; already defined above
(define-class D C (t))
   ---
(define-class E D ((* u)))
   ---
(define-class F E (v))
   ---
(set! the-Point (make-F 'xx 'yy 3 'zz0 'zz1 'zz2 'tt 4 'u0 'u1 'u2 'u3 'vv))
   ---
(let ((f (list-ref (Class-fields (symbol->class 'A)) 0)))
  (field-value the-Point f) )
   xx
(let ((f (list-ref (Class-fields (symbol->class 'C)) 0)))
  (field-value the-Point f) )
   xx
(let ((f (list-ref (Class-fields (symbol->class 'C)) 1)))
  (field-value the-Point f) )
   yy
(let ((f (list-ref (Class-fields (symbol->class 'C)) 2)))
  (field-value the-Point f 0) )
   zz0
(let ((f (list-ref (Class-fields (symbol->class 'C)) 2)))
  (field-value the-Point f 2) )
   zz2
(let ((f (list-ref (Class-fields (symbol->class 'D)) 2)))
  (field-value the-Point f 2) )
   zz2
(let ((f (list-ref (Class-fields (symbol->class 'F)) 2)))
  (field-value the-Point f 2) )
   zz2
(let ((f (list-ref (Class-fields (symbol->class 'D)) 3)))
  (field-value the-Point f) )
   tt
(let ((f (list-ref (Class-fields (symbol->class 'F)) 4)))
  (field-value the-Point f 2) )
   u2
(let ((f (list-ref (Class-fields (symbol->class 'F)) 5)))
  (field-value the-Point f) )
   vv
;;;;;;;;;;;;;;;;;;;;;;;;;;                        testing set-field-value
(let ((f (list-ref (Class-fields (symbol->class 'A)) 0)))
  (set-field-value! the-Point 'xxx f)
  (field-value the-Point f) )
   *** ; immutable field
(let ((f (list-ref (Class-fields (symbol->class 'C)) 0)))
  (set-field-value! the-Point 'xxxx f)
  (field-value the-Point f) )
   *** ; immutable field by inheritance
(let ((f (list-ref (Class-fields (symbol->class 'C)) 1)))
  (set-field-value! the-Point 'yyy f)
  (field-value the-Point f) )
   yyy
(let ((f (list-ref (Class-fields (symbol->class 'C)) 2)))
  (set-field-value! the-Point 'zzz0 f 0)
  (field-value the-Point f 0) )
   zzz0
(let ((f (list-ref (Class-fields (symbol->class 'C)) 2)))
  (set-field-value! the-Point 'zzz2 f 2)
  (field-value the-Point f 2) )
   zzz2
(let ((f (list-ref (Class-fields (symbol->class 'D)) 2)))
  (set-field-value! the-Point 'zzz22 f 2)
  (field-value the-Point f 2) )
   zzz22
(let ((f (list-ref (Class-fields (symbol->class 'F)) 2)))
  (set-field-value! the-Point 'zzz222 f 2)
  (field-value the-Point f 2) )
   zzz222
(let ((f (list-ref (Class-fields (symbol->class 'D)) 3)))
  (set-field-value! the-Point 'ttt f)
  (field-value the-Point f) )
   ttt
(let ((f (list-ref (Class-fields (symbol->class 'F)) 4)))
  (set-field-value! the-Point 'uu2 f 2)
  (field-value the-Point f 2) )
   uu2
(let ((f (list-ref (Class-fields (symbol->class 'F)) 5)))
  (set-field-value! the-Point 'vvv f)
  (field-value the-Point f) )
   vvv
;;;;;;;;;;;;;;;;;;;;;;;;;;;  testing initialize-field-value!
(set! the-Point ((Class-allocator F-class) 3 4))
   ---
(let ((f (list-ref (Class-fields (symbol->class 'A)) 0)))
  (initialize-field-value! the-Point 'xxx f)
  (field-value the-Point f) )
   xxx
(let ((f (list-ref (Class-fields (symbol->class 'C)) 0)))
  (initialize-field-value! the-Point 'xxxx f)
  (field-value the-Point f) )
   *** ; Already initialized field
(let ((f (list-ref (Class-fields (symbol->class 'C)) 2)))
  (initialize-field-value! the-Point 'zzz0 f 0)
  (field-value the-Point f 0) )
   zzz0
(let ((f (list-ref (Class-fields (symbol->class 'C)) 2)))
  (initialize-field-value! the-Point 'zzz2 f 2)
  (field-value the-Point f 2) )
   zzz2
(let ((f (list-ref (Class-fields (symbol->class 'D)) 2)))
  (initialize-field-value! the-Point 'zzz22 f 2)
  (field-value the-Point f 2) )
   *** ; already  initialized field
(let ((f (list-ref (Class-fields (symbol->class 'F)) 2)))
  (initialize-field-value! the-Point 'zzz1 f 1)
  (field-value the-Point f 1) )
   zzz1
(let ((f (list-ref (Class-fields (symbol->class 'D)) 3)))
  (initialize-field-value! the-Point 'ttt f)
  (field-value the-Point f) )
   ttt
(let ((f (list-ref (Class-fields (symbol->class 'F)) 4)))
  (initialize-field-value! the-Point 'uu2 f 2)
  (field-value the-Point f 2) )
   uu2
(let ((f (list-ref (Class-fields (symbol->class 'F)) 5)))
  (initialize-field-value! the-Point 'vvv f)
  (field-value the-Point f) )
   vvv
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; testing field-defined?
;;; the-Point = 
;;;   #(35 XXX (???) 3 ZZZ0 ZZZ1 ZZZ2 TTT 4 (???) (???) UU2 (???) VVV)
(let ((f (list-ref (Class-fields (symbol->class 'F)) 5)))
  (field-defined? the-Point f) )
   #t
(let ((f (list-ref (Class-fields (symbol->class 'F)) 4)))
  (field-defined? the-Point f 3) )
   #f
(let ((f (list-ref (Class-fields (symbol->class 'F)) 4)))
  (field-defined? the-Point f 2) )
   #t
(let ((f (list-ref (Class-fields (symbol->class 'E)) 4)))
  (field-defined? the-Point f 3) )
   #f
(let ((f (list-ref (Class-fields (symbol->class 'E)) 4)))
  (field-defined? the-Point f 2) )
   #t
(let ((f (list-ref (Class-fields (symbol->class 'F)) 2)))
  (field-defined? the-Point f 3) )
   *** ; out of bounds
(let ((f (list-ref (Class-fields (symbol->class 'F)) 2)))
  (field-defined? the-Point f 2) )
   #t
(let ((f (list-ref (Class-fields (symbol->class 'C)) 1)))
  (field-defined? the-Point f) )
   #f
;;; Bug found by <lucier@math.purdue.edu>
(field-defined? 33 'z)
   *** ; not an instance
;;;;;;;;;;;;;;;;;;;;;;; testing field-length
(let ((f (list-ref (Class-fields (symbol->class 'F)) 2)))
  (field-length the-Point f) )
   3
(let ((f (list-ref (Class-fields (symbol->class 'F)) 4)))
  (field-length the-Point f) )
   4
(let ((f (list-ref (Class-fields (symbol->class 'F)) 3)))
  (field-length the-Point f) )
   *** ; not a poly-field
(let ((f (retrieve-named-field hyper-extra-NickNamedColoredPoint-class
                               'extra )))
  (field-length (make-hyper-extra-NickNamedColoredPoint
                 3 2 1 2 'foo 'bar 2 'hux 'bar 'wrek 'quux
                 4 #\A #\B #\C #\D )
                f ) )
   4
;;; Bug found by <lucier@math.purdue.edu>
(field-length 33 'z)
   *** ; not an instance
;;;;;;;;;;;;;;;;;;;;;;; retesting field-value in presence of undefined fields.
(let ((f (list-ref (Class-fields (symbol->class 'F)) 5)))
  (field-value the-Point f) )
   vvv
(let ((f (list-ref (Class-fields (symbol->class 'F)) 4)))
  (field-value the-Point f 3) )
   ***
(let ((f (list-ref (Class-fields (symbol->class 'F)) 4)))
  (field-value the-Point f 2) )
   uu2
(let ((f (list-ref (Class-fields (symbol->class 'F)) 1)))
  (field-value the-Point f) )
   *** ; uninitialized
(let ((f (list-ref (Class-fields (symbol->class 'F)) 0)))
  (field-value the-Point f) )
   xxx
;;; Bug found by <lucier@math.purdue.edu>
(field-value 33 'z)
   *** ; not an instance
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; retesting field-value on symbols
(field-value the-Point 'x)
   xxx
(field-value the-Point 'z 1)
   zzz1
(field-defined? the-Point 'z 1)
   #t
(field-defined? the-Point 'u 1)
   #f
(begin (initialize-field-value! the-Point 'uu0 'u 0)
       (field-value the-Point 'u 0) )
   uu0
(begin (initialize-field-value! the-Point 'yy0 'y)
       (field-value the-Point 'y) )
   yy0
(begin (set-field-value! the-Point 'uu1 'u 0)
       (field-value the-Point 'u 0) )
   uu1
(begin (set-field-value! the-Point 'tt1 't)
       (field-value the-Point 't) )
   tt1
(field-value the-Point 'laskd)
   ***
(set-field-value! the-Point 'val 'laskd)
   ***
(initialize-field-value! the-Point 'val 'laskd 3)
   ***
(field-defined? the-Point 'laskd)
   ***
;;; Bug found by <lucier@math.purdue.edu>
(set-field-value! 33 'z 'truc)
   *** ; not an instance
(initialize-field-value! 33 'val 'laskd 3)
   *** ; not an instance

;;; testing substituable?
;(define-class A Object (x) :immutable) ; defined above
;(define-class B Object (y) :mutable)   ; defined above
(let ((o1 (make-A 22))
      (o2 (make-A 22)) )
  (substituable? o1 o2) )
   #t
(let ((o1 (make-A 22))
      (o2 (make-A 33)) )
  (substituable? o1 o2) )
   #f
(let ((o1 (make-B 22))
      (o2 (make-A 33)) )
  (substituable? o1 o2) )
   #f
(let ((o1 (make-B 22))
      (o2 (make-B 33)) )
  (substituable? o1 o2) )
   #f
(let ((o1 (make-B 22))
      (o2 (make-B 22)) )
  (substituable? o1 o2) )
   #f ; since Bs are mutable
(let ((o1 (make-B 22)))
  (substituable? o1 o1) )
   #t
;;; testing recursion
(let ((o1 (make-A (make-B 22)))
      (o2 (make-A (make-B 22))) )
  (substituable? o1 o2) )
   #f
(let* ((ob (make-B 22))
       (o1 (make-A ob))
       (o2 (make-A ob)) )
  (substituable? o1 o2) )
   #t
;;; testing cyclic objects (but substituable? does not need to go down
;;; the cycle.
(let* ((ob (make-B 22))
       (o1 (make-A ob))
       (o2 (make-A ob)) )
  (set-B-y! ob o1)
  (substituable? o1 o2) )
   #t
(let* ((ob (make-B 22))
       (o1 (make-A (make-A ob)))
       (o2 (make-A (make-A ob))) )
  (set-B-y! ob o1)
  (substituable? o1 o2) )
   #t
;;; testing really cyclic objects
(let* ((ob1 ((Class-allocator A-class)))
       (ob2 ((Class-allocator A-class)))
       (o1 (make-A (make-A ob1)))
       (o2 (make-A (make-A ob2))) )
  ;; (initialize-field-value! o value field)
  (initialize-field-value! ob1 o1 (car (Class-fields A-class)))
  (initialize-field-value! ob2 o2 (car (Class-fields A-class)))
  (unveil (list o1 o2))
  (substituable? o1 o2) )
   #t
(let* ((ob1 ((Class-allocator A-class)))
       (ob2 ((Class-allocator A-class)))
       (o1 (make-A (make-A ob1)))
       (o2 (make-A (make-A ob2))) )
  ;; (initialize-field-value! o value field)
  (initialize-field-value! ob1 o2 (car (Class-fields A-class)))
  (initialize-field-value! ob2 o1 (car (Class-fields A-class)))
  (unveil (list o1 o2))
  (substituable? o1 o2) )
   #t
(let* ((ob1 ((Class-allocator A-class)))
       (ob2 ((Class-allocator A-class)))
       (o1 (make-A ob1))
       (o2 (make-A (make-A ob2))) )
  ;; (initialize-field-value! o value field)
  (initialize-field-value! ob1 o2 (car (Class-fields A-class)))
  (initialize-field-value! ob2 o1 (car (Class-fields A-class)))
  (unveil (list o1 o2))
  (substituable? o1 o2) )
   #t

;;; Testing generic calls before and after renumbering
(show-detailed-hierarchy "DEBUG") ; ;DEBUG
   ---
(define-method (zztop (o C)) 'C)
   ---
(define-method (zztop (o A)) 'A)
   ---
(define oo1 (make-C 'x 'y 3 'z0 'z1 'z2))
   ---
(define oo2 (make-D 'xx 'yy 1 'zz0 'tt))
   ---
(display `(oo1 is ,oo1 and oo2 is ,oo2))
  ---
(unveil (->Generic 'zztop)) ; DEBUG
   ---
;;; force C and D classes to be renumbered.
(define-class YY Y2 ())
   ---
(show-detailed-hierarchy "DEBUG") ; ;DEBUG
   ---
(unveil (->Generic 'zztop)) ; DEBUG
   ---
(define-class G D ())
   ---
(unveil (->Generic 'zztop)) ; DEBUG
   ---
;;; Adapted this test to Meroon feature: (not renumber-classes)
;;; so or the two instances were renumbered or none of them is.
(let* ((ocn1 (object->class-number oo1))
       (ocn2 (object->class-number oo2))
       (r1 (zztop oo1))
       (r2 (zztop oo2)) )
  (display `(ocn1 was ,ocn1 now ,(object->class-number oo1)))
  (display `(ocn2 was ,ocn2 now ,(object->class-number oo2)))
  (let* ((rr1 (eq? r1 (zztop oo1)))
         (rr2 (not (= ocn1 (object->class-number oo1))))
         (rr3 (eq? r2 (zztop oo2)))
         (rr4 (not (= ocn2 (object->class-number oo2)))) )
    (display `(results are ,rr1 ,rr2 ,rr3 ,rr4))
    (and rr1 (eq? rr2 rr4) rr3) ) )
   #t


;;; Testing multi-methods
(define-generic (bar (o) (v)))
   ---
(bar 3 4)
   ***
(define-generic (bar (o) (v)) 'root)
   ---
(bar 3 4)
   root
(define-method (bar (o Point) (v Point))
  (- (Point-x v) (Point-x o)) )
   ---
(bar 3 4)
   root
(bar 3 (make-Point 1 2))
   root
(bar (make-Point 1 2) 3)
  root
(bar (make-Point 1 2) (make-Point 4 5))
   3
(define-method (bar (o Point) (v Point))
  (+ (Point-x v) (Point-x o)) )
   ---
(bar (make-Point 1 2) (make-Point 4 5))
   5
(show-generic 'bar)
   ---
(define-method (bar (o ColoredPoint) (v Point))
  (list (ColoredPoint-color o) (Point-x v)) )
   ---
(show-generic 'bar)
   ---
(bar 3 4)
   root
(bar 3 (make-Point 1 2))
   root
(bar (make-Point 1 2) 3)
  root
(bar (make-Point 1 2) (make-Point 4 5))
   5
(bar (make-ColoredPoint 1 22 'pink) (make-Point 4 5))
   (pink 4)
(define-method (bar (o Point-3d) (v Point))
  (list (Point-3d-z o) (Point-y v)) )
   ---
(show-generic 'bar)
   ---
(bar 3 4)
   root
(bar 3 (make-Point 1 2))
   root
(bar (make-Point 1 2) 3)
  root
(bar (make-Point 1 2) (make-Point 4 5))
   5
(bar (make-ColoredPoint 1 22 'pink) (make-Point 4 5))
   (pink 4)
(bar (make-Point-3d 1 2 3) (make-Point 4 5))
   (3 5)
(bar (make-ColoredPoint 1 22 'pink) (make-ColoredPoint 4 5 'red))
   (pink 4)
(bar (make-Point 11 22) (make-ColoredPoint 4 5 'red))
   15
(define-method (bar (o Class) (v Point))
  (list (Class-name o) (Point-x v)) )
   ---
(show-generic 'bar)
   ---
(bar 3 4)
   root
(bar 3 (make-Point 1 2))
   root
(bar (make-Point 1 2) 3)
  root
(bar (make-Point 1 2) (make-Point 4 5))
   5
(bar (make-ColoredPoint 1 22 'pink) (make-Point 4 5))
   (pink 4)
(bar (make-Point-3d 1 2 3) (make-Point 4 5))
   (3 5)
(bar (make-ColoredPoint 1 22 'pink) (make-ColoredPoint 4 5 'red))
   (pink 4)
(bar (make-Point 11 22) (make-ColoredPoint 4 5 'red))
   15
(bar Point-class (make-ColoredPoint 4 5 'red))
   (Point 4)
;;; testing call-next-method
(define-method (bar (o Point-3d) (v Point))
  (list (call-next-method) (Point-3d-z o) (Point-y v)) )
   ---
(show-generic 'bar)
   ---
(bar 3 4)
   root
(bar 3 (make-Point 1 2))
   root
(bar (make-Point 1 2) 3)
  root
(bar (make-Point 1 2) (make-Point 4 5))
   5
(bar (make-ColoredPoint 1 22 'pink) (make-Point 4 5))
   (pink 4)
(bar (make-Point-3d 11 2 3) (make-Point 4 5))
   (15 3 5)
(bar (make-ColoredPoint 1 22 'pink) (make-ColoredPoint 4 5 'red))
   (pink 4)
(bar (make-Point 11 22) (make-ColoredPoint 4 5 'red))
   15
(bar Point-class (make-ColoredPoint 4 5 'red))
   (Point 4)
;;; testing the final default method 
(define-method (bar (o Point) (v Point))
  (list 'Point 'Point (call-next-method)) )
   ---
(bar 3 4)
   root
(bar 3 (make-Point 1 2))
   root
(bar (make-Point 1 2) 3)
  root
(bar (make-Point 1 2) (make-Point 4 5))
   (Point Point root)
(bar (make-ColoredPoint 1 22 'pink) (make-Point 4 5))
   (pink 4)
(bar (make-Point-3d 11 2 3) (make-Point 4 5))
   ((Point Point root) 3 5)
(bar (make-ColoredPoint 1 22 'pink) (make-ColoredPoint 4 5 'red))
   (pink 4)
(bar (make-Point 11 22) (make-ColoredPoint 4 5 'red))
   (Point Point root)
(bar Point-class (make-ColoredPoint 4 5 'red))
   (Point 4)

;;; testing renumbering on multimethods.
(define-class Y3 YY ())
   ---
(bar 3 4)
   root
(bar 3 (make-Point 1 2))
   root
(bar (make-Point 1 2) 3)
  root
(bar (make-Point 1 2) (make-Point 4 5))
   (Point Point root)
(bar (make-ColoredPoint 1 22 'pink) (make-Point 4 5))
   (pink 4)
(bar (make-Point-3d 11 2 3) (make-Point 4 5))
   ((Point Point root) 3 5)
(bar (make-ColoredPoint 1 22 'pink) (make-ColoredPoint 4 5 'red))
   (pink 4)
(bar (make-Point 11 22) (make-ColoredPoint 4 5 'red))
   (Point Point root)
(bar Point-class (make-ColoredPoint 4 5 'red))
   (Point 4)

;;;  testing with more than 2 discriminating variables as well as others
(define-generic (bar a (b) c (d) (e) . rest)
  'default )
   ---
(bar 3 (make-Point 11 22) (make-ColoredPoint 4 5 'red) 'd  'e)
   default
(define-method (bar a (b Point) c (d Point)))
   ***
(define-method (bar a (b Point) c (d Point) e))
   ***
(define-method (bar a (b Point) c (d Point) (e Point))
   (+ (Point-x b) (Point-y e)) )
   ***
(define-method (bar a (b Point) c (d Point) (e Point) . rest)
   (+ (Point-x b) (Point-y e)) )
   ---
(bar 3 (make-Point 11 22) 'c (make-ColoredPoint 4 5 'red) 44)
   default
(bar 3 (make-Point 11 22) 'c (make-ColoredPoint 4 5 'red) (make-Point 6 7))
   18
(define-method (bar a (b Point) c (d ColoredPoint) (e Point) . rest)
   (* (Point-x b) (Point-y d)) )
   ---
(bar 3 (make-Point 11 22) 'c (make-ColoredPoint 4 5 'red) (make-Point 6 7) 't)
   55
(bar 3 (make-Point 11 22) 'c (make-ColoredPoint 4 5 'red) (make-Point 6 7)
     555 666 777 )
   55

;;;  testing clashes between methods
(define-method (bar a (b ColoredPoint) c (d Point) (e Point) . rest)
   (* (Point-x b) (Point-y d)) )
   *** ;  clash on ColoredPoint*ColoredPoint*Point
;;; Manuel detects a bug here with Bigloo, so trace somethings to see.
;;; The bug was a right->left evaluation in the parse-disc function!
;(begin (trace combine)
;       (unveil (->Generic 'bar))
;       (show-generic-trace 'insert-N-method!) )
;   ---
(define-method (bar a (b ColoredPoint) c (d Point) (e Class) . rest)
   (* (Point-x b) (Point-y d)) )
   --- ; do not clash

(bar 3 (make-Point 11 22) 'c (make-ColoredPoint 4 5 'red) Point-class)
   default
(bar 3 (make-ColoredPoint 11 22 'blue) 
     'c (make-ColoredPoint 4 5 'red) Point-class )
   55
;;; testing the ambiguity on a call-next-method
(define-method (bar a (b ColoredPoint) c (d ColoredPoint) (e Class) . rest)
   (* (Point-x b) (Point-y d)) )
   --- ; do not clash
(define-method (bar a (b Point) c (d ColoredPoint) (e Class) . rest)
   (* (Point-x d) (Point-y d)) )
   *** ; clash with ColoredPoint*Point*Class

;;; tracing generic with multi-methods
(show-generic-trace 'bar)
   ---
(bar 3 (make-Point 11 22) 'c (make-ColoredPoint 4 5 'red) Point-class)
   default
(bar 3 (make-ColoredPoint 11 22 'blue) 
     'c (make-ColoredPoint 4 5 'red) Point-class )
   55
(show-generic-untrace)
   ---
(bar 3 (make-Point 11 22) 'c (make-ColoredPoint 4 5 'red) Point-class)
   default
(bar 3 (make-ColoredPoint 11 22 'blue) 
     'c (make-ColoredPoint 4 5 'red) Point-class )
   55


;;; Testing interaction of trace with class or method addition.
;;; Bug raised by "Riverola, Josep" <riverola@iese.es> after the Wenceslas
;;; version (where I introduced Tracing-Dispatcher).
;;; call generic function on a method and on default.
(show-generic-trace 'bar)
   ---
(show-dispatcher 'bar)
   ---
(define-class Y4 Y3 (yy))
   ---
(bar 3 (make-Point 11 22) 'c (make-ColoredPoint 4 5 'red) Point-class)
   default
(bar 3 (make-ColoredPoint 11 22 'blue) 
     'c (make-ColoredPoint 4 5 'red) Point-class )
   55
(define-method (bar  a (b ColoredPoint) c (d ColoredPoint) (e Class) . rest)
   (+ (Point-x b) (Point-y d)) )
   --- 
(bar 3 (make-Point 11 22) 'c (make-ColoredPoint 4 5 'red) Point-class)
   default
(bar 3 (make-ColoredPoint 11 22 'blue) 
     'c (make-ColoredPoint 4 5 'red) Point-class )
   16
(show-generic-untrace)
   ---
(show-dispatcher 'bar)
   ---
(bar 3 (make-Point 11 22) 'c (make-ColoredPoint 4 5 'red) Point-class)
   default
(bar 3 (make-ColoredPoint 11 22 'blue) 
     'c (make-ColoredPoint 4 5 'red) Point-class )
   16
;;; Same tests on a mono-method generic function
;;; call generic function on a method and on default.
(show-dispatcher 'zztop)
   ---
(show-generic-trace 'zztop)
   ---
(define-class Y5 Y3 (zz))
   ---
(show-dispatcher 'zztop)
   ---
(zztop 33)
   default
(zztop (make-NamedColoredPoint 33 44 'red 2 'joe 'jill))
   ColoredPoint
(define-method (zztop (o NamedColoredPoint)) 'NamedColoredPoint)
   ---
(zztop 33)
   default
(zztop (make-NamedColoredPoint 33 44 'red 2 'joe 'jill))
   NamedColoredPoint
(show-generic-untrace)
   ---
(show-dispatcher 'zztop)
   ---
(zztop 33)
   default
(zztop (make-NamedColoredPoint 33 44 'red 2 'joe 'jill))
   NamedColoredPoint




;;; Testing instantiate special form
(define-class Point-wi Object 
  ((= x :initializer (lambda () 11))
   (= y :initializer (lambda () 22)) ) )
   ---
(instantiate Point-wi)
   ---
(Point-wi? (instantiate Point-wi))
   #t
(instantiate Point-wi :y 44)
   ---
(Point-wi-x (instantiate Point-wi :y 44))
   11
(instantiate Point-wi :x 44 :y 33)
   ---
(let ((pt (instantiate Point-wi :x 44 :y 33)))
  (list (Point-wi-x pt) (Point-wi-y pt)) )
   (44 33)
;;; change order of x and y
(instantiate Point-wi :y 44 :x 33)
   ---
(let ((pt (instantiate Point-wi :y 44 :x 33)))
  (list (Point-wi-x pt) (Point-wi-y pt)) )
   (33 44)
;;; Test if instantiate works with pre-classes.  
;;; Bug found by riverola@iese.es (Josep Riverola)
;;; This example also requires macroexpansion to be left-to-right
;;; (fact triggered by meroon.vscm).
(begin
  (define-class ColoredPoint-wi Point-wi
    ((= color :initializer (lambda () 'pink))) )
  (let ((pt (instantiate ColoredPoint-wi)))
    (list (Point-wi-x pt) (Point-wi-y pt) (ColoredPoint-wi-color pt)) ) )
   (11 22 pink)

;;; for future tests on :maybe-uninitialized
(define-class NamedColoredPoint-wi ColoredPoint-wi
  ((* names :maybe-uninitialized)) )
   ---
(instantiate Point-wi :x 44 :x 5)
  *** ; X twice specified
(instantiate NamedColoredPoint-wi)
   *** ; no size
(instantiate NamedColoredPoint-wi :names-length)
   *** ; missing size
(instantiate Point-wi :bla-bla 1 (+ 2 2))
   *** ; unused option
(instantiate Point-wi :x 3 :bla-bla 1 (+ 2 2) :y 4)
   *** ; unused option
(instantiate NamedColoredPoint-wi :y 44 :x 33 :names-length 2)
   ---
;;; allocate an indexed field with a statically known size
(NamedColoredPoint-wi-names-length 
 (instantiate NamedColoredPoint-wi :y 44 :x 33 :names-length 2) )
   2
;;; allocate an indexed field with a statically unknown size
(let ((len 2))
  (instantiate NamedColoredPoint-wi :y 44 :x 33 :names-length len) )
   ---
(let ((len 2))
  (NamedColoredPoint-wi-names-length 
   (instantiate NamedColoredPoint-wi :y 44 :x 33 :names-length len) ) )
   2
(NamedColoredPoint-wi-names-length 
 (instantiate NamedColoredPoint-wi :y 44 :x 33 
              :names 21 22 23 24 25 26 27 28 29 30
                     31 32 33 34 35 36 37 38 39 40
                     41 42 43 44 45 46 47 48 49 50 ) )
   30
(NamedColoredPoint-wi-color 
  (instantiate NamedColoredPoint-wi :names-length 2) )
   pink
(instantiate NamedColoredPoint-wi :names-length 2 3)
   *** ; too much sizes
(instantiate NamedColoredPoint-wi :names 'joe 'jill)
   ---
(instantiate NamedColoredPoint-wi :names 'joe 'jill :y 55)
   ---
(let ((pt (instantiate NamedColoredPoint-wi :names 'joe 'jill :y 55)))
  (list (NamedColoredPoint-wi-names pt 0)
        (NamedColoredPoint-wi-names pt 1) ) )
   (joe jill)
;;; with a non computable at expandtime size for names
(instantiate NamedColoredPoint-wi
  :names-length (+ 1 1) :x 33 :y 44 :color 'red )
   ---
(NamedColoredPoint-wi-color
 (instantiate NamedColoredPoint-wi :names-length (+ 1 1) 
              :x 33 :y 44 :color 'red ) )
   red
(instantiate NamedColoredPoint-wi :names-length (+ 1 1) 
             :names 3 :x 33 :y 44 :color 'red )
   *** ; :names-length and :names must not appear at the same time
(instantiate NamedColoredPoint-wi :names 33
             :names 3 :x 33 :y 44 :color 'red )
   *** ; duplicated :names 
(instantiate NamedColoredPoint-wi :names-length (+ 1 1) 
             :names-length 3 :x 33 :y 44 :color 'red )
   *** ; duplicated :names-length 
;;; uninitialized fields are really unninitialized
(field-defined? (instantiate NamedColoredPoint-wi :names-length (+ 1 1))
                'names 0 )
   #f
;;; initialization on a poly field
(define-class NamedColoredPoint-wi2 ColoredPoint-wi
  ((* names :initializer (lambda (i) (* 3 i)))) )
   ---
;;; NOTE never reuse continuations taken in initializers.
(instantiate NamedColoredPoint-wi2
  :names-length (+ 1 1) :x 33 :y 44 :color 'red )
   ---
(NamedColoredPoint-wi2-names
 (instantiate NamedColoredPoint-wi2 :names-length (+ 1 1) 
              :x 33 :y 44 :color 'red )
 1 )
   3
;;; test that redefinition with initializers work (since Field
;;; instances are shared). Redefinition is not advised.
;(define-class ColoredPoint-wi Point 
;  ((= color :initializer (lambda () 'blue))) )
;   ---
;(instantiate ColoredPoint-wi :x 2)
;   ---
;(ColoredPoint-wi-color (instantiate ColoredPoint-wi :x 2))
;   blue
;;; Riverola, Josep <riverola@iese.es> found the following bug
;;; The indexed field is not initialized if being the first field.
(define-class Deep-Purple Object
  ((* content :initializer (lambda (j) (* 2 j)))) )
   ---
(let ((o (instantiate Deep-Purple
           :content-length 3) ))
  (Deep-Purple-content o 1) )
   2
(define-class Deep-Purple2 Object
  ((= void :initializer (lambda () 'void))
   (* content :initializer (lambda (j) (* 2 j))) ) )
   ---
(let ((o (instantiate Deep-Purple2
           :content-length 3) ))
  (Deep-Purple2-content o 1) )
   2
(let ((o (instantiate Deep-Purple2
           :void 'foo
           :content-length 3) ))
  (Deep-Purple2-content o 1) )
   2
(define-class Deep-Purple3 Object
  ((* content :initializer (lambda (j) (* 2 j)))
   (= void :initializer (lambda () 'void)) ) )
   ---
(let ((o (instantiate Deep-Purple3
           :content-length 3) ))
  (Deep-Purple3-content o 1) )
   2
(let ((o (instantiate Deep-Purple3
           :void 'foo
           :content-length 3) ))
  (Deep-Purple3-content o 1) )
   2


;;; testing the :maybe-uninitialized
(define-class ColoredPoint-wi3 Point-wi
  ((= color)) )
   ---
(define-class TypedColoredPoint-wi ColoredPoint-wi3
  ( (= type :maybe-uninitialized)) )
   ---
(instantiate ColoredPoint-wi3 :y 4 :x 2 :color 'black)
   ---
(ColoredPoint-wi3-color (instantiate ColoredPoint-wi3 :x 2 :y 4 :color 'black))
   black
(instantiate ColoredPoint-wi3)
   *** ; no color
(instantiate TypedColoredPoint-wi :x 2)
   *** ; no color
(instantiate TypedColoredPoint-wi :x 2 :type 'sale)
   *** ; no color
(instantiate TypedColoredPoint-wi :x 2 :color 'blue)
   ---
(instantiate TypedColoredPoint-wi :type 'sale :color 'blue)
   ---
;;; this error is not caught by Meroon since it is an arity error in the
;;; macro expansion.
;(instantiate)
;   ***


;;; Coercers
(Class? (->Class 'Point))
   #t
(Class-name (->Class 'Point))
   Point
(Generic? (->Generic 'bar))
   #t
(define-class Point3 Point ())
   ---
(define-method (->Point3 (o A))
  (make-Point3 (A-x o) (A-x o)) )
   ---
(Point3? (->Point3 (instantiate A :x 3)))
   #t
(let ((pt (->Point3 (instantiate A :x 3))))
  (= (Point3-x pt) (Point3-y pt)) )
   #t
(let ((pt (->Point3 (instantiate C :x 3 :z 0 1 :y 4))))
  (= (Point3-x pt) (Point3-y pt)) )
   #t


;;; testing maximal class in define-generic
(define-generic (foo (a Point)) 'point)
   ---
(define-method (foo (a ColoredPoint)) 'coloredpoint)
   ---
(define-method (foo (a Class)) 'coloredpoint)
   *** ; Class is not a subclass of Point
;;; same tests with multimethod
(define-generic (foo (a Point) x (aa Class)) 'point)
   ---
(define-method (foo (a ColoredPoint) y (z Class)) 'coloredpoint)
   ---
(define-method (foo (a Class) y (Z Class)) 'coloredpoint)
   *** ; Class is not a subclass of Point
(define-method (foo (a Point) y (Z Point)) 'coloredpoint)
   *** ; Point is not a subclass of Class
(define-method (foo (a Point) y z) 'coloredpoint)
   ***
(define-method (foo a y (Z Point)) 'coloredpoint)
   ***


;;; Test metaclass, changed in Meroon V3
(define-class Counting-Class MeroonV2-Class 
  ((= counter :initializer (lambda () 0))) )
   ---
(define-method (generate-maker (o Counting-Class) class-options)
  (let* ((name (Class-name o))
         (maker-name (symbol-concatenate 'make- name)) )
    `(begin ,(call-next-method)
            (set! ,maker-name 
                  (let ((old ,maker-name)
                        (o (->Class ',name)) )
                    (lambda args
                      (set-Counting-Class-counter! 
                       o (+ 1 (Counting-Class-counter o)) )
                      (apply old args) ) ) ) ) ) )
   ---
;;; a new version of generate-maker without set!
(define-method (generate-maker (o Counting-Class) class-options)
  (let* ((name (Class-name o))
         (maker-name (symbol-concatenate 'make- name)) )
    `(meroon-define ,maker-name 
       (let ()
         ,(call-next-method)
         (let ((old ,maker-name)
               (o (->Class ',name)) )
           (lambda args
             (set-Counting-Class-counter! 
              o (+ 1 (Counting-Class-counter o)) )
             (apply old args) ) ) ) ) ) )
   ---
(define-class Counted-Point Point 
  ((= z :immutable))
  :metaclass Counting-Class )
   ---
(->Class 'Counted-Point)
   ---
(Counting-Class-counter (->Class 'Counted-Point))
   0
(define pt (make-Counted-Point 22 33 44))
   ---
(Counted-Point-x pt)
   22
(Point-y pt)
   33
(Counting-Class-counter (->Class 'Counted-Point))
   1
;;; another solution with initialize! (effects are cumulated now)
(define-method (initialize! (o Counted-Point))
  (let ((cl (object->class o)))
    (set-Counting-Class-counter! 
     cl (+ 1 (Counting-Class-counter cl)) )
    o ) )
   ---
(set! pt (make-Counted-Point 222 333 444))
   ---
(Counted-Point-x pt)
   222
(Point-y pt)
   333
(Counting-Class-counter (->Class 'Counted-Point))
   3


;;; Testing non handy metaclass, new in Meroon V3. 
;;; This metaclass records the last N created instances. N is an
;;; allocation parameter of the metaclass to be allocated, it must be
;;; an immediate value since this is allocated at expansion-time
;;; (without eval). Since the metaclass of Memo-Class is Handy-Class,
;;; the accompanying functions are created.
(define-class Memo-Class Handy-Class 
  ((* instance :initializer (lambda (i) #f))) )
   ---
(define-method (generate-maker (o Memo-Class) class-options)
  (let* ((name (Class-name o))
         (variable (symbol-concatenate name '-class)) )
    `(define-method (initialize! (o ,name))
       (let ((number (Memo-Class-instance-length ,variable)))
         (do ((i 1 (+ 1 i)))
             ((= i number))
           (set-Memo-Class-instance!
            ,variable (- i 1) (Memo-Class-instance ,variable i) ) )
         (set-Memo-Class-instance! ,variable (- number 1) o)
         o ) ) ) )
   ---
(define-class Memo-Point Point
  ((= z :immutable))
  :metaclass Memo-Class 3 )
   ---
(->Class 'Memo-Point)
   ---
(list (Memo-Class-instance-length Memo-Point-class)
      (Memo-Class-instance Memo-Point-class 0)
      (Memo-Class-instance Memo-Point-class 1)
      (Memo-Class-instance Memo-Point-class 2) )
   (3 #f #f #f)
(begin (instantiate Memo-Point :x 11 :y 22 :z 33)
       (instantiate Memo-Point :x 44 :y 55 :z 66) )
   ---
(Memo-Class-instance Memo-Point-class 0)
   #f
(instantiate Memo-Point :x 7 :y 8 :z 9)
   ---
(field-value (Memo-Class-instance Memo-Point-class 0) 'x)
   11
(field-value (Memo-Class-instance Memo-Point-class 2) 'z)
   9
(instantiate Memo-Point :x 11 :y 12 :z 13)
   ---
(field-value (Memo-Class-instance Memo-Point-class 2) 'z)
   13



;;; Metaclass for Field
(define-class C1 Object 
  ((color :initializer (lambda (i) 'pink)
          :metaclass Poly-Field )) )
   ---
(Poly-Field? (retrieve-named-field (->Class 'C1) 'color))
   #t
;;; Extending Mono-Field                  Actually bugged in renum.scm
;(define-class Checked-Mono-Field Mono-Field
;  ( predicate ; test field content
;    ) )
;   ---

;;; Thanks to Josep Riverola, when using = or * no :metaclass should
;;; be present. Sep 1st, 1995.
(define-class CC1 Object
  ((= color :metaclass Poly-field)) )
   *** 
(define-class CC2 Object
  ((* color :metaclass Mono-field)) )
   *** 
;;; even if they are the real metaclass to use
(define-class CC1a Object
  ((= color :metaclass Mono-field)) )
   *** 
(define-class CC2a Object
  ((* color :metaclass Poly-field)) )
   *** 



;;; Tests handy method definitions.
(define-generic (foo (a) b c))
   ---
(define-handy-method (foo (a Point) b c)
  (list x y) )
   ---
(foo (make-Point 55 66) 7 8)
   (55 66)
(define-handy-method (foo (a Point) x y)
  (list x y) )
   ---
(foo (make-Point 55 66) 7 8)
   (7 8)
(define-handy-method (foo (a Point) b c)
  (let ((x 1)) (list x y)) )
   ---
(foo (make-Point 55 66) 7 8)
   (1 66)
;;; also test if (let () ...) is allowed.
(define-handy-method (foo (a Point) b c)
  (let () (list y)) )
   ---
(foo (make-Point 55 66) 7 8)
   (66)
(define-handy-method (foo (a Point) b c)
  (letrec ((x 1)) (list x y)) )
   ---
(foo (make-Point 55 66) 7 8)
   (1 66)
(define-handy-method (foo (a Point) b c)
  (let loop ((x 1)) (list x y)) )
   ---
(foo (make-Point 55 66) 7 8)
   (1 66)
(define-handy-method (foo (a Point) b c)
  (let* ((x 1)) (list x y)) )
   ---
(foo (make-Point 55 66) 7 8)
   (1 66)
(define-handy-method (foo (a Point) b c)
  (define (bar y) (+ y y))
  (bar x) )
   ---
(foo (make-Point 55 66) 7 8)
   110
(define-handy-method (foo (a Point) b c)
  (define bar (lambda (y) (+ x y y)))
  (bar x) )
   ---
(foo (make-Point 55 66) 7 8)
   165

;;; testing with-access
(let ((pt (make-Point 55 66)))
  (with-access pt (Point x) 
    3 ) )
   3
(let ((pt (make-Point 55 66)))
  (with-access pt (Point x) 
    3 4 5 ) )
   5
(let ((pt (make-Point 55 66)))
  (with-access pt (Point) 
    3 4 5 ) )
   5
(let ((pt (make-Point 55 66)))
  (with-access pt (Point x) 
    x ) )
   55
(let ((pt (make-Point 55 66)))
  (with-access pt (Point x y) 
    (+ x y) ) )
   121
(let ((pt (make-Point 55 66)))
  (with-access (car (list pt)) (Point x y) 
    (+ x y) ) )
   121
(let ((pt (make-Point 55 66)))
  (with-access pt (Point x y z) 
    (+ x y) ) )
   *** ; no Z field
(let ((pt (make-Point 55 66)))
  (with-access pt (Poilaksdnt x y) 
    (+ x y) ) )
   *** ; no such class
(let ((pt (make-Point 55 66)))
  (with-access pt (Point x y) 
    (set! x (+ x y)) )
  (Point-x pt) )
  121
(let ((pt (make-Counted-Point 222 333 444)))
  (with-access pt (Counted-Point z x)
    (set! z z) ) )
   *** ; Z immutable field
;;; with-access within with-access
(let ((pt (make-Counted-Point 222 333 444)))
  (with-access pt (Counted-Point z x)
    (list z (with-access pt (Counted-Point x)
              x )) ) )
   (444 222)
;;; with-access within with-access and with shadowing
(let ((pt (make-Counted-Point (make-Point 11 22) 333 444)))
  (with-access pt (Counted-Point z x)
    (list z (with-access x (Point y)
              (list y) ) )) )
   (444 (22))
(let ((pt (make-Counted-Point (make-Point 11 22) 333 444)))
  (with-access pt (Counted-Point z x)
    (list z (with-access x (Point x y)
              (list x y) ) )) )
   (444 (11 22))
;;; Add tests to check if substitution goes through quasiquotations.
;;; Bug found by Hubert Canon <canon@polytechnique.fr>.
(let ((pt (make-ColoredPoint 11 22 'pink)))
  (with-access pt (ColoredPoint color x)
    `(,color a ,@(list x 'b) c) ) )
   (pink a 11 b c)
(let ((pt (make-ColoredPoint 11 22 'pink)))
  (with-access pt (ColoredPoint color x)
    `(,color a ,@`(,x b) . c) ) )
   (pink a 11 b . c)
(let ((pt (make-ColoredPoint 11 22 'pink)))
  (with-access pt (ColoredPoint color x)
    `(,color a ,@(with-access pt (Point x y)
                   `(,x b ,color) ) . c )) )
   (pink a 11 b pink . c)
;;; Add test for (let () ...) which was considered as a named let.
;;; Bug found by <lucier@math.purdue.edu>.
(let ((pt (make-ColoredPoint 11 22 'pink)))
  (with-access pt (ColoredPoint color x)
    (let ()
      (define (test) color)
      (test) ) ) )
   pink

;;; testing top-class specification 
(define-generic (foo (o Point)) 'default)
   ---
(define-method (foo (o Class)) 'class)
   *** ; Class is not a subclass of Point.
(define-generic (foo (o Point) (oo Point)) 'default)
   ---
(define-method (foo (o ColoredPoint) (oo Class)) 11)
   ***  ; Class is not a subclass of Point.
(define-method (foo (o Class) (oo ColoredPoint)) 11)
   ***  ; Class is not a subclass of Point.

;;; Redefining inherited field initializers
;;; Recall that
;(define-class Point-wi Object 
;  ((= x :initializer (lambda () 11))
;   (= y :initializer (lambda () 22)) ) )
;(define-class ColoredPoint-wi Point-wi
;  ((= color :initializer (lambda () 'pink))) )
(let ((pt (instantiate Point-wi)))
  (list (Point-wi-x pt) (Point-wi-y pt)) )
   (11 22)
(let ((pt (instantiate ColoredPoint-wi)))
  (list (Point-wi-x pt) (Point-wi-y pt) (ColoredPoint-wi-color pt)) )
   (11 22 pink)
;; OK, now redefine the initializer of ColoredPoint-wi
(set-Field-initializer! (retrieve-named-field ColoredPoint-wi-class 'color)
                        (lambda () 'red) )
   ---
;;; test it on a fresh instance
(let ((pt (instantiate ColoredPoint-wi)))
  (list (Point-wi-x pt) (Point-wi-y pt) (ColoredPoint-wi-color pt)) )
   (11 22 red)
;;; Redefine the initializer of a field of the superclass
(set-Field-initializer! (retrieve-named-field Point-wi-class 'y)
                        (lambda () 33) )
   ---
;;; test it on a fresh instance
(let ((pt (instantiate Point-wi)))
  (list (Point-wi-x pt) (Point-wi-y pt)) )
   (11 33)
;;; The problem now is that this redefinition is not inherited since
;;; we get 22 instead of 33 in this example. If we want 33, initializers
;;; cannot be stored inside Field but where then ? in classes ?
(let ((pt (instantiate ColoredPoint-wi)))
  (list (Point-wi-x pt) (Point-wi-y pt) (ColoredPoint-wi-color pt)) )
   (11 22 red)

;;; Testing  show-methods-for-class
(show-methods-for-class Class-class)
   ---

;;; Testing the duplicate form.
(let* ((o (make-Point 11 22))
       (no (duplicate (o Point))) )
  (list (Point-x no) (Point-y no)) )
   (11 22)
(let* ((o (make-Point 11 22))
       (no (duplicate (o Point)
                :y 33 )) )
  (list (Point-x no) (Point-y no)) )
   (11 33)
(let* ((o (make-Point 11 22))
       (no (duplicate (o Point)
                :x 22 :y 33 )) )
  (list (Point-x no) (Point-y no)) )
   (22 33)
(let* ((o (make-Point 11 22))
       (no (duplicate (o Point)
                :y 33 :x 22 )) )
  (list (Point-x no) (Point-y no)) )
   (22 33)
;;; It is possible to restrict the class of the duplicated object.
(let* ((o (make-ColoredPoint 11 22 'red))
       (no (duplicate (o Point)
                :x 22 :y 33 )) )
  (list (Point? no) (ColoredPoint? no)) )
   (#t #f)
(let* ((o (make-ColoredPoint 11 22 'red))
       (no (duplicate (o Point)
                :x 22 :y 33 )) )
  (list (Point-x no) (Point-y no)) )
   (22 33)
(let* ((o (make-NamedColoredPoint 11 22 'red 2 'joe 'jill))
       (no (duplicate (o Point)
                :x 22 :y 33 )) )
  (list (Point-x no) (Point-y no)) )
   (22 33)
;;; Uninitialized values are propagated
(let* ((o (instantiate ColoredPoint-wi3 :x 11 :color 'red))
       (no (duplicate (o TypedColoredPoint-wi)
                :x 22 )) )
  (list (Point-wi-x no) (field-defined? no 'type)) )
   (22 #f)
;;; Indexed field are handled
(let* ((o (make-NamedColoredPoint 11 22 'red 2 'joe 'jill))
       (no (duplicate (o NamedColoredPoint)
                :y 33 )) )
   (list (NamedColoredPoint-y no)
         (NamedColoredPoint-names-length no)
         (NamedColoredPoint-names no 0) ) )
   (33 2 joe)
;;; The length of an indexed field can be changed to be shorter
(let* ((o (make-NamedColoredPoint 11 22 'red 2 'joe 'jill))
       (no (duplicate (o NamedColoredPoint)
                :names 'jack )) )
   (list (NamedColoredPoint-x no)
         (NamedColoredPoint-names-length no)
         (NamedColoredPoint-names no 0) ) )
   (11 1 jack)
;;; The length of an indexed field can be changed to be longer
(let* ((o (make-NamedColoredPoint 11 22 'red 1 'joe))
       (no (duplicate (o NamedColoredPoint)
                :names 'jack 'jill)) )
   (list (NamedColoredPoint-x no)
         (NamedColoredPoint-names-length no)
         (NamedColoredPoint-names no 0) ) )
   (11 2 jack)
;;; Exercise some errors
;; This is no longer an error withh the new semantics of duplicate.
;(let* ((o (make-Point 11 22))
;       (no (duplicate (o ColoredPoint)
;                :color 'blue )) )
;  no )
;   *** ; o is not a ColoredPoint.
(let* ((o (make-NamedColoredPoint 11 22 'red 1 'joe))
       (no (duplicate (o (->Class 'NamedColoredPoint))
                :names 'jack 'jill)) )
   (list (NamedColoredPoint-x no)
         (NamedColoredPoint-names-length no)
         (NamedColoredPoint-names no 0) ) )
   ***  ; not a class name
(let* ((o (make-ColoredPoint 11 22 'red))
       (no (duplicate (o ColoredPoint)
                :painting 'blue )) )
  no )
   *** ; no painting field in ColoredPoint.
(let* ((o (make-Point-wi 11 22))
       (no (duplicate (o NamedColoredPoint-wi)
                :y 33 :names-length 2 )) )
  (list (Point-wi-x no) (Point-wi-y no) (ColoredPoint-wi-color no)) )
   ***  ; no kw-length in duplicate forms.

;;; More sophisticated uses of duplicate. We saw above that the class
;;; may be restricted, it may also be extended.
(let* ((o (make-Point 11 22))
       (no (duplicate (o ColoredPoint)
                :color 'red)) )
  (ColoredPoint? no) )
   #t
(let* ((o (make-Point 11 22))
       (no (duplicate (o ColoredPoint)
                :color 'blue)) )
  (list (Point-x no) (Point-y no) (ColoredPoint-color no)) )
   (11 22 blue)
(let* ((o (make-Point 11 22))
       (no (duplicate (o ColoredPoint)
                :color 'red :y 33)) )
  (list (Point-x no) (Point-y no) (ColoredPoint-color no)) )
   (11 33 red)
;;; but mandatory fields should not be missing.
(let* ((o (make-Point 11 22))
       (no (duplicate (o ColoredPoint)
                :y 33)) )
  (list (Point-x no) (Point-y no) (ColoredPoint-color no)) )
   *** ; missing color ie o should be a ColoredPoint
;;; unless they have an initializer
(let* ((o (make-Point-wi 11 22))
       (no (duplicate (o ColoredPoint-wi)
                :y 33 )) )
  (list (Point-wi-x no) (Point-wi-y no) (ColoredPoint-wi-color no)) )
   (11 33 red)
;;; or if they are allowed to be left uninitialized
(let* ((o (make-Point-wi 11 22))
       (no (duplicate (o TypedColoredPoint-wi)
                :y 33 :color 'blue )) )
  (list (Point-wi-x no) (Point-wi-y no) 
        (ColoredPoint-wi3-color no)
        (field-defined? no 'type) ) )
   (11 33 blue #f)
;;; mandatory poly-fields should not be missing.
(let* ((o (make-Point 11 22))
       (no (duplicate (o NamedColoredPoint)
                :color 'red )) )
  (list (Point-x no) (Point-y no) (ColoredPoint-color no)) )
   *** ; names are missing
(let* ((o (make-Point 11 22))
       (no (duplicate (o NamedColoredPoint)
                :names 'joe 'jill :color 'red )) )
  (list (Point-x no) (Point-y no) (ColoredPoint-color no)) )
   (11 22 red)

;;; It is also possible to go from one class to a sibling if necessary 
;;; fields are specified.
(let* ((o (instantiate threeDNamedColoredPoint 
           :x 11 :y 22 :color 'blue :names 'joe 'jill 'jack :z 333  ))
       (no (duplicate (o NickNamedColoredPoint)
              :nicknames "AA" "BB" )) )
  (NickNamedColoredPoint? no) )
   #t
(let* ((o (instantiate threeDNamedColoredPoint 
           :x 11 :y 22 :color 'blue :names 'joe 'jill 'jack :z 333  ))
       (no (duplicate (o NickNamedColoredPoint)
              :nicknames "AA" "BB" )) )
  (NickNamedColoredPoint-nicknames no 1) )
   "BB"
(let* ((o (instantiate threeDNamedColoredPoint 
           :x 11 :y 22 :color 'blue :names 'joe 'jill 'jack :z 333  ))
       (no (duplicate (o NickNamedColoredPoint)
              :names 'a 'b )) )
  'noothing )
    *** ; missing nicknames

;;; Fields are compared by eq? not by names.
(let* ((o (make-Point-wi 11 22))
       (no (duplicate (o Point))) )
  'nothing )
   *** ; missing fields      
;;; It is possible to compute the first argument of duplicate
(let ((no (duplicate ((instantiate threeDNamedColoredPoint 
                        :x 11 :y 22 :color 'blue :names 'joe 'jill 'jack 
                        :z 333 )
                      NickNamedColoredPoint)
                     :nicknames "AA" "BB" )) )
  (NickNamedColoredPoint-nicknames no 1) )
   "BB"



;;; A new instantiation form that allows to co-instantiate mutually
;;; referent objects. The problem was discussed in oopil.ps but 
;;; really asked for by Riverola.

(co-instantiate) 
   ---
(define-class Couple Object
  (left right) )
   ---
;; Recall that co-instantiate expands into a begin form starting with
;; define forms so wrap them 
(let ((x 0))
  (co-instantiate
   (x Couple :left 2 :right 3) )
  (Couple-left x) )
   2
(let ((x 0))
  (co-instantiate
   (x Couple :left 2 :right 3) )
  (Couple-right x) )
   3
(let ((c (let ((x 0))
           (co-instantiate
            (x Couple :left x :right x) ))))
  (and (eq? (Couple-left c) c)
       (eq? (Couple-right c) c) ) )
   #t
(let ((x (let ((x 0)(y 1))
           (co-instantiate
            (x Couple :left x :right y)
            (y Couple :right x :left y) ))))
  (and (eq? (Couple-left x) x)
       (let ((y (Couple-right x)))
         (and (eq? x (Couple-right y))
              (eq? y (Couple-left y)) ) ) ) )
   #t
(let ((x (let ((x 0)(y 0))
           (co-instantiate
            (x Couple :left x :right y)
            (y Couple :right (Couple-left x) :left y) ))))
  (and (eq? (Couple-left x) x)
       (let ((y (Couple-right x)))
         (and (eq? x (Couple-right y))
              (eq? y (Couple-left y)) ) ) ) )
   #t
;;; Still thanks to Riverola, check that no parameter is also possible.
(let ((x 0))
  (co-instantiate
   (x Point-wi) ) )
   ---
;;; some errors
(co-instantiate
 (x Couple :left x)
 (y Couple :right (Couple-left x) :left y) )
   *** ; missing :right field
(co-instantiate
 (x) )
   *** ; missing classname
(co-instantiate
 () )
   *** ; missing name
;;; with default initializers
(define-class Couple2 Object
  ((= left :initializer (lambda () 'foo))
   (= right :initializer (lambda () 'bar)) ) )
   ---
(let ((x (let ((x 0)(y 1))
           (co-instantiate
            (x Couple2 :left y)
            (y Couple2 :right (Couple2-left x)) ))))
  (eq? 'bar (Couple2-right x)) )
   #t
(let ((x (let ((x 0)(y 1))
           (co-instantiate
            (x Couple2 :left y)
            (y Couple2 :right (Couple2-left x)) ))))
  (let ((y (Couple2-left x)))
    (eq? y (Couple2-right y)) ) )
   #t

;;; Local tests.
(let ((x+y (let ((x 0)(y 1))
             (with-co-instantiation
              ((x Couple2 :left y)
               (y Couple2 :right (Couple2-left x)) )
              (cons x y) ))))
  (let ((x (car x+y))
        (y (cdr x+y)) )
    (eq? y (Couple2-right y)) ) )
   #t
;;; with polyfields
(define-class Couple3 Object
  ((= left :initializer (lambda () 'foo))
   (* right :initializer (lambda (i) i)) ) )
   ---
(let ((x (let ((x 0)(y 1))
           (co-instantiate
            (x Couple3 :left y :right-length 2)
            (y Couple3 :right x (Couple3-left x) y) ))))
  (let ((y (Couple3-left x)))
    (eq? y (Couple3-right y 1)) ) )
   #t

;;; Local tests.
(let ((x+y (with-co-instantiation
            ((x Couple3 :left y :right-length 2)
             (y Couple3 :right x (Couple3-left x) y) )
            (cons x y) )))
  (let ((x (car x+y))
        (y (cdr x+y)) )
    (eq? y (Couple3-right y 1)) ) )
   #t
;;; Examples of the documentation
(define-class TrucMuche Object
   ((= left :initializer (lambda () 33))
    (* right) ) )
   ---
(TrucMuche-right-length
 (let ((p 0)(x 1))
   (co-instantiate
    (p TrucMuche :right x x x)
    (x Point :x 22 :y (TrucMuche-left p)) ) ) )
   3
(let ((tm (let ((p 0)(x 1))
            (co-instantiate
             (p TrucMuche :right x x x)
             (x Point :x 22 :y (TrucMuche-left p)) ))))
  (eq? (TrucMuche-right tm 1)
       (TrucMuche-right tm 2) ) )
   #t
(with-co-instantiation
     ((p TrucMuche :right x x x)
      (x Point :x 22 :y (TrucMuche-left p)) )
  (TrucMuche-right-length p) )
   3

;;; Testing the modify form
(let ((o (make-Point 11 22)))
  (Point? (modify (o Point))) )
   #t
;;; the object to modify should be of the right class.
(let ((o (make-ColoredPoint 11 22 'pink)))
  (Point? (modify (o Point))) )
   #t
(let ((o (make-ColoredPoint 11 22 'pink)))
  (Point? (modify (o NamedColoredPoint))) )
   *** ;  wrong class
;;; but one may give a superclass
(let ((o (make-ColoredPoint 11 22 'pink)))
  (ColoredPoint? (modify (o Point))) )
   #t
;;; the first argument may be computed.
(ColoredPoint? (modify ((make-ColoredPoint 11 22 'pink) Point)))
   #t
;;; But not the class
(modify ((make-ColoredPoint 11 22 'pink) (->Class 'Point)))
   *** ; not a class name
;;; Fields may be specified
(let ((o (make-ColoredPoint 11 22 'pink)))
  (Point-x (modify (o Point)
              :x 33 )) )
   33
(let ((o (make-ColoredPoint 11 22 'pink)))
  (Point-x (modify (o Point)
              :y 44 :x 33 )) )
   33
;;; but fields should exist
(let ((o (make-ColoredPoint 11 22 'pink)))
  (Point-x (modify (o Point)
              :y 44 :z 33 )) )
   *** ; no z field
;;; but  fields should exist in the mentioned class
(let ((o (make-ColoredPoint 11 22 'pink)))
  (Point-x (modify (o Point)
              :y 44 :color 33 )) )
   *** ; no color field
;;; Fields must be mutable
(let ((o (make-A 11)))
  (modify (o A)
     :x 33 ) )
   *** ; x is immutable
;;; A poly-field may be modified
(let ((o (make-NamedColoredPoint 
          2 44 'pink
          3 'joe 'jill 'jack )))
  (NamedColoredPoint-names
   (modify (o NamedColoredPoint)
     :names 'a 'b 'c )
   0 ) )
   a
(let ((o (make-NamedColoredPoint 
          2 44 'pink
          3 'joe 'jill 'jack )))
  (NamedColoredPoint-names
   (modify (o NamedColoredPoint)
     :y 88 )
   0 ) )
   joe
;;; but not its length
(let ((o (make-NamedColoredPoint 
          2 44 'pink
          3 'joe 'jill 'jack )))
  (NamedColoredPoint-names
   (modify (o NamedColoredPoint)
     :names 'a 'b 'c 'd )
   0 ) )
   ***
(let ((o (make-NamedColoredPoint 
          2 44 'pink
          3 'joe 'jill 'jack )))
  (NamedColoredPoint-names
   (modify (o NamedColoredPoint)
     :names 'a 'b )
   0 ) )
   ***
;;; :kw-length cannot be used.
(let ((o (make-NamedColoredPoint 
          2 44 'pink
          3 'joe 'jill 'jack )))
  (NamedColoredPoint-names
   (modify (o NamedColoredPoint)
     :names-length 2 )
   0 ) )
   ***
;;; even with the right size
(let ((o (make-NamedColoredPoint 
          2 44 'pink
          3 'joe 'jill 'jack )))
  (NamedColoredPoint-names
   (modify (o NamedColoredPoint)
     :names-length 3 )
   0 ) )
   ***

;;; 27 VIII 96: Paul Wilson suggests to unify Object Systems in Scheme.
;;; First suggestion is to allow for that alternate syntax.
(define-class CCa (Object) 
  (x y) )
   ---
(eq? Object-class (Class-super-class CCa-class))
   #t

;;; Test instantiate-from 
(let* ((o (make-Point 11 22))
       (no (instantiate-from (o Point))) )
  (list (Point-x no) (Point-y no)) )
   (11 22)
(let* ((o (make-Point 11 22))
       (no (instantiate-from (o Point)
                :y 33 )) )
  (list (Point-x no) (Point-y no)) )
   (11 33)
(let* ((o (make-Point 11 22))
       (no (instantiate-from (o Point)
                :x 22 :y 33 )) )
  (list (Point-x no) (Point-y no)) )
   (22 33)
(let* ((o (make-Point 11 22))
       (no (instantiate-from (o Point)
                :y 33 :x 22 )) )
  (list (Point-x no) (Point-y no)) )
   (22 33)
;;; The class of the instantiated-from object respects the class of
;;; the cloned object (contrarily to duplicate):
(let* ((o (make-ColoredPoint 11 22 'red))
       (no (instantiate-from (o Point)
                :x 22 :y 33 )) )
  (ColoredPoint? no) )
   #t
(let* ((o (make-ColoredPoint 11 22 'red))
       (no (instantiate-from (o Point)
                :x 22 :y 33 )) )
  (list (Point? no) (ColoredPoint? no)) )
   (#t #t)
(let* ((o (make-ColoredPoint 11 22 'red))
       (no (instantiate-from (o Point)
                :x 22 :y 33 )) )
  (list (Point-x no) (Point-y no)) )
   (22 33)
(let* ((o (make-ColoredPoint 11 22 'red))
       (no (instantiate-from (o Point)
                :x 22 :y 33 )) )
  (list (ColoredPoint-x no) (ColoredPoint-y no)) )
   (22 33)
(let* ((o (make-NamedColoredPoint 11 22 'red 2 'joe 'jill))
       (no (instantiate-from (o Point)
                :x 22 :y 33 )) )
  (NamedColoredPoint? no) )
   #t
(let* ((o (make-NamedColoredPoint 11 22 'red 2 'joe 'jill))
       (no (instantiate-from (o Point)
                :x 22 :y 33 )) )
  (list (ColoredPoint-x no)
        (ColoredPoint-y no)
        (NamedColoredPoint-names-length no)
        (NamedColoredPoint-names no 0) ) )
   (22 33 2 joe)
;;; Indexed field are handled
(let* ((o (make-NamedColoredPoint 11 22 'red 2 'joe 'jill))
       (no (instantiate-from (o NamedColoredPoint)
                :y 33 )) )
   (list (NamedColoredPoint-y no)
         (NamedColoredPoint-names-length no)
         (NamedColoredPoint-names no 0) ) )
   (33 2 joe)
;;; The length of an indexed field can be changed to be shorter
(let* ((o (make-NamedColoredPoint 11 22 'red 2 'joe 'jill))
       (no (instantiate-from (o NamedColoredPoint)
                :names 'jack )) )
   (list (NamedColoredPoint-x no)
         (NamedColoredPoint-names-length no)
         (NamedColoredPoint-names no 0) ) )
   (11 1 jack)
;;; The length of an indexed field can be changed to be longer
(let* ((o (make-NamedColoredPoint 11 22 'red 1 'joe))
       (no (instantiate-from (o NamedColoredPoint)
                :names 'jack 'jill)) )
   (list (NamedColoredPoint-x no)
         (NamedColoredPoint-names-length no)
         (NamedColoredPoint-names no 0) ) )
   (11 2 jack)
;;; Exercise some errors
(let* ((o (make-NamedColoredPoint 11 22 'red 1 'joe))
       (no (instantiate-from (o (->Class 'NamedColoredPoint))
                :names 'jack 'jill)) )
   (list (NamedColoredPoint-x no)
         (NamedColoredPoint-names-length no)
         (NamedColoredPoint-names no 0) ) )
   ***  ; not a class name
(let* ((o (make-ColoredPoint 11 22 'red))
       (no (instantiate-from (o ColoredPoint)
                :painting 'blue )) )
  no )
   *** ; no painting field in ColoredPoint.
(let* ((o (make-Point-wi 11 22))
       (no (instantiate-from (o NamedColoredPoint-wi)
                :y 33 :names-length 2 )) )
  (list (Point-wi-x no) (Point-wi-y no) (ColoredPoint-wi-color no)) )
   ***  ; no kw-length in instantiate-from forms.
;;; should be an instance of the mentioned class
(let* ((o (make-Point 11 22))
       (no (instantiate-from (o NamedColoredPoint))) )
  'none )
   *** ; not a NamedColoredPoint

;;; Examples from documentation (slightly adapted):
(let ((pt (make-NamedColoredPoint
           11 22 'blue 3 'Joe 'Jack 'Jill )))
  (ColoredPoint-x 
   (instantiate-from (pt Point)
     :x 33 ) ) )
   33
(let ((pt (make-NamedColoredPoint
           11 22 'blue 3 'Joe 'Jack 'Jill )))
  (ColoredPoint-y
   (instantiate-from (pt Point)
     :x 33 ) ) )
   22
(let ((pt (make-NamedColoredPoint
           11 22 'blue 3 'Joe 'Jack 'Jill )))
  (NamedColoredPoint-names-length
   (instantiate-from (pt Point)
     :x 33 ) ) )
   3
(let ((pt (make-NamedColoredPoint
           11 22 'blue 3 'Joe 'Jack 'Jill )))
  (NamedColoredPoint-names-length
   (instantiate-from (pt NamedColoredPoint)
     :names 'Tina 'Tim 'Tom 
     :x 33 ) ) )
    3
(let ((pt (make-NamedColoredPoint
           11 22 'blue 3 'Joe 'Jack 'Jill )))
  (NamedColoredPoint-names
   (instantiate-from (pt NamedColoredPoint)
     :names 'Tina 'Tim 'Tom 
     :x 33 )
   0 ) )
    Tina
(let ((pt (make-NamedColoredPoint
           11 22 'blue 3 'Joe 'Jack 'Jill )))
  (NamedColoredPoint-names-length
   (instantiate-from (pt NamedColoredPoint)
     :names 'Tina 'Tim 
     :x 33 ) ) )
    2

"
Hmm, this sounds all good... We can stop now and announce it to the world!



**************************************************************************
                     All tests successfully run!
***************************************************************************
"
   ---

;;; end of oo3.tst
