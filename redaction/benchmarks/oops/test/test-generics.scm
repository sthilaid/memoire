;;; FILE: "test-generics.scm"
;;; IMPLEMENTS: Generic function tests for Oops
;;; LANGUAGE: Gambit Scheme (v4 beta12)
;;; AUTHOR: Ken Dickey
;;;
;;; COPYRIGHT (c) 2005 by Kenneth Alan Dickey
;;;
;;;   Licensed under the Apache License, Version 2.0 (the "License");
;;;   you may not use this file except in compliance with the License.
;;;   You may obtain a copy of the License at
;;;
;;;       http://www.apache.org/licenses/LICENSE-2.0
;;;
;;;   Unless required by applicable law or agreed to in writing, software
;;;   distributed under the License is distributed on an "AS IS" BASIS,
;;;   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;;   See the License for the specific language governing permissions and
;;;   limitations under the License.

;==============================================================;
(include "testing-macros.scm")
;==============================================================;
(include "../src/oops-macros.scm")

(add-test-suite 'generics)  ;; @@ FIXME: stateful one-shot tests (can't retest)

;; Setup for test-generics tests
;; is defined in "test-class.scm"
#|
(define-class <point> ()  ( (x init-value: 0) (y init-value: 0) ) )

(define-class <depth> ()  ( (z init-value: 0) ) )
  
(define-class <3d-point> ( <point> <depth> ) ())

(define-class <ship> ( <point> ) ( (direction init-value: 40) ) )

(define-class <submarine> ( <ship> <depth> )
                          ()
                          (lambda (sub) (y-set! sub 33)))

(define-class <constant> () ((value init-value: 33 allocation: constant:)))

(define p3    (<3d-point>))
(define ship1 (<ship> x: 23 y: 14))
(define sub1  (<submarine> x: 124 z: -23))
(define c1 (<constant>))
(define c2 (<constant> value: "a string"))
|#

(define-method (->string (p <point>))     (format #f "#<point x:~a y:~a>" (x p) (y p)))
(define-method (->string (p <3d-point>))  (format #f "#<3d-point x:~a y:~a z:~a>" (x p) (y p) (z p)))
(define-method (->string (s <ship>))      (format #f "#<ship ~a>" (next-method)))
(define-method (->string (s <submarine>)) (format #f "#<submarine ~a depth:~a>" (next-method) (z s)))

(add-equal-test 'generics
                "#<point x:3 y:4>"
                 (->string (<point> x: 3 y: 4))
                "(->string (<point> x: 3 y: 4))")

(add-equal-test 'generics
                "#<3d-point x:0 y:0 z:0>"
                (->string p3)
                "(->string p3) -- inherited slots")

(add-equal-test 'generics
                "#<ship #<point x:23 y:14>>"
                 (->string ship1)
                " (->string ship1) -- next-method")

(add-equal-test 'generics
                "#<submarine #<ship #<point x:124 y:33>> depth:-23>"
                (->string sub1)
                "(->string sub1) -- next next-method")

;; class-unions

(define-class int-or-string (<string> <integer>) () abstract-class-error)

(define-generic (m a b) (list 'default a b))

(define-method (m (i <integer>) (s <string>))
  (list 'int i (next-method (number->string i) s)))

(define-method (m (c <character>) s) (list 'char c (next-method)))

(define-method (m (i1 <integer>) (i2 <integer>))
  (list 'int-int i1 i2 (next-method)))



(add-equal-test 'generics
                '(int-int 3 5 (default 3 5))
                (m 3 5)
                "(m 3 5)")


(add-equal-test 'generics
                '(default "three" "five")
                (m "three" "five")
                "(m \"three\" \"five\")")


(add-equal-test 'generics
                '(int 3 (default "3" "five"))
                (m 3 "five")
                "(m 3 \"five\")")


(add-equal-test 'generics
                '(char #\c (default #\c "five"))
                (m #\c "five")
                "(m #\\c \"five\")")

(define baz-enum (<one-of> 'foo 'bar 'baz 'quux))
(define-method (baz (f baz-enum)) (list 'baz     f))
(define-method (baz  f)           (list 'NON-baz f))

(add-equal-test 'generics
                '(baz quux)
                (baz 'quux)
                "(baz 'quux)")


(add-equal-test 'generics
                '(NON-baz barf)
                (baz 'barf)
                "(baz 'barf)")


(define-method m1 (method ((a <integer>) b) 'integer))
(define-method m1 (method ((a <real>)    b) 'real))
(define-method (m1 a b) 'top)

(add-eq-test 'generics 'integer  (m1 2    3))
(add-eq-test 'generics 'integer  (m1 2   #t))
(add-eq-test 'generics 'real     (m1 1.2 'a))
(add-eq-test 'generics 'real     (m1 3.# 'a))
(add-eq-test 'generics 'integer  (m1 2    3))
(add-eq-test 'generics 'top      (m1 #t  #f))

(ensure-exception-raised 'generics
    wrong-number-of-arguments-exception?
    (m1 1 2 3)
    "(m1 1 2 3) too many args")


(define-method m2 (method ((a <integer>) (b <integer>)) 'int-int))
(define-method m2 (method ((a <integer>) (b <real>   )) 'int-real))
(define-method m2 (method ( a            (b <number> )) 'top-number))

;; (M 1 2 3) --> error
(add-eq-test 'generics 'int-int    (m2  1   2))
(add-eq-test 'generics 'int-real   (m2  1 2.1))
(add-eq-test 'generics 'top-number (m2 'a   1))

;; types

(define-structure quux glerph blox fargle)
(define q1 (make-quux 1 2 3))
(define q1-type (type q1))
(define-structure quux2 glerph blox fargle)
(define q2 (make-quux2 1 2 3))
(define q2-type (type q2))
(define-structure quux3 glerph blox fargle)
(define q3 (make-quux3 1 2 3))

(define-generic (m3 a b c)
  (list "default" 'm3 a b c))
(define-method (m3 (a (<singleton> 3)) (b (<one-of> 1 2 3 4 5 6 7 8 9)) c)
  (list 'm3 'singleton 'one-of a b c))
(define-method (m3 (a (<subclass-type> subclass-of: <string>)) (b <number>) c)
  (list 'm3 'subclass-of-string a b c))
(define-method (m3 a (b <string>) (c (<predicate-type> superclass: <integer> test-for: even?)))
  (list 'm3 'even? a b c))
(define-method (m3 a (b <character>) c)
  (list 'm3 'char a b c))
(define-method (m3 a (b q1-type) c)
  (list 'm3 a 'quux1 c))
(define-method (m3 a (b q2-type) c)
  (list 'm3 a 'quux2 c))

(add-equal-test 'generics
                '(m3 even? 0 "one" 2)
                (m3 0 "one" 2))

(add-equal-test 'generics
                '("default" m3 0 "one" 3)
                (m3 0 "one" 3))

(add-equal-test 'generics
                (list 'm3 'subclass-of-string <string> 3 4)
                (m3 <string> 3 4))

(add-equal-test 'generics
                '("default" m3 2 3 4)
                (m3 2 3 4))

(add-equal-test 'generics
                '(m3 singleton one-of 3 4 5)
                (m3 3 4 5))

(add-equal-test 'generics
                '(m3 char 1 #\2 3)
                (m3 1 #\2 3))

(add-equal-test 'generics
                '(m3 1 quux1 3)
                (m3 1 q1 3))

(add-equal-test 'generics
                '(m3 1 quux2 3)
                (m3 1 q2 3))

(add-equal-test 'generics
                (list "default" 'm3 1 q3 3)
                (m3 1 q3 3))

#|
(add-equal-test 'generics
                expected
                actual
                "msg")


(add-equal-test 'generics
                expected
                actual
                "msg")

|#

;===========================E=O=F==============================;
