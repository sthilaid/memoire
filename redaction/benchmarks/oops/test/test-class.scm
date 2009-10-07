;;; FILE: "test-class.scm"
;;; IMPLEMENTS: Unit tests for class code [for Oops]
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

(add-test-suite 'classes)  ;; @@ FIXME: stateful one-shot tests (can't retest)

;; Setup for test-class tests

;;(require 'oops)
(include "../src/oops-macros.scm")

(define-class <point> ()  ( (x init-value: 0) (y init-value: 0) ) )

(define-class <depth> ()  ( (z init-value: 0) ) )
  
(define-class <3d-point> ( <point> <depth> ) ())

(define-class <ship> ( <point> ) ( (direction init-value: 40) ) )

(define-class <submarine> ( <ship> <depth> )
                          ()
                          (lambda (sub) (y-set! sub 33)))

(define-class <constant> () ((value init-value: 33 allocation: constant:)))

(define p3   (<3d-point>))
(define ship1 (<ship> x: 23 y: 14))
(define sub1 (<submarine> x: 124 z: -23))
(define c1 (<constant>))
(define c2 (<constant> value: "a string"))


;; Tests Proper

(add-equal-test 'classes
                '(<class> <function> <type> <value>)
                (map class-name (class-precedence-list <class>))
                "<class> class-precedence-list")

(add-equal-test 'classes
                '(<ship> <point> <value>)
                (map class-name (class-precedence-list <ship>))
                "<ship> class-precedence-list")

(add-eq-test 'classes
             <integer>
             (class-of 34)
             "(class-of 34)")

(add-eq-test 'classes
             <real>
             (class-of 3.14)
             "(class-of 3.14)")

(add-eq-test 'classes
             <rational>
             (class-of 3/4)
             "(class-of 3/4)")

(add-eq-test 'classes
             <complex>
             (class-of 3+4i)
             "(class-of 3+4i)")

(add-eq-test 'classes
          '<3d-point>
          (class-name (class-of p3))
          "3d-point class")

(add-equal-test 'classes
          0
          (y p3)
          "3d-point inherited slot y")

(add-equal-test 'classes
          40
          (let ( (old-y (y p3)) )
            (y-set! p3 40)
            (let ( (result (y p3)) )
              (y-set! p3 old-y)
              result)) ;; makes test repeatable
          "3d-point y-set!")

(add-equal-test 'classes
          0
          (z p3)
          "3d-point inherited slot z")

(add-equal-test 'classes
          124
          (x sub1)
          "submarine x")

(add-equal-test 'classes
          33
          (y sub1)
          "submarine y (set via init-function)")

(add-equal-test 'classes
          -23
          (z sub1)
          "submarine z")

(add-equal-test 'classes
          40
          (direction sub1)
          "submarine direction")

(ensure-exception-raised
    'classes
    error-exception?
    (%moby-define-class "<submarine>"
                   '( <ship> <depth> )
                   '()
                   #f
                   '((lambda (sub) (y-set! sub 33))))
    "name not a symbol")

(ensure-exception-raised
    'classes
    error-exception?
    (%moby-define-class '<submarine>
                   '( <ship> <barf> )
                   '()
                   #f
                   '((lambda (sub) (y-set! sub 33))))
    "super-class not found")

(ensure-exception-raised
    'classes
    error-exception?
    (%moby-define-class '<submarine>
                   '( <ship> #t )
                   '()
                   #f
                   '((lambda (sub) (y-set! sub 33))))
    "super-class not a class")

(ensure-exception-raised
    'classes
    error-exception?
    (%moby-define-class '<submarine>
                   '( <ship> <depth> )
                   '()
                   #f
                   '((lambda () (y-set! sub 33))))
    "bad init-function")

(ensure-exception-raised
    'classes
    error-exception?
    (%moby-define-class '<submarine>
                   '( <ship> <depth> )
                   '()
                   #f
                   '((lambdaX (sub) (y-set! sub 33))))
    "bad init-function")

(ensure-exception-raised
    'classes
    error-exception?
    (%moby-define-class '<submarine>
                   '( <ship> <depth> )
                   '()
                   #f
                   '((lambda (sub))))
    "bad init-function")

(ensure-exception-raised
    'classes
    error-exception?
    (%moby-define-class '<submarine>
                   '( <ship> <depth> )
                   '(y z y)
                   #f
                   '((lambda (sub) (y-set! sub 33))))
    "duplicate slot names")

(add-equal-test 'classes
          33
          (value c1)
          "constant defaulted")

(add-equal-test 'classes
          "a string"
          (value c2)
          "constant initialized")

(ensure-exception-raised
    'classes
    unbound-global-exception?
    (value-set! c1 'foo)
    "value-set! unbound setter")

(ensure-exception-raised
    'classes
    unbound-global-exception?
    (value-set! c2 'barf)
    "change immutable value for c2")



;; Slot Inheritance

(define-class <A> () (a))
(define-class <B> () (b))
(define-class <C> () (c))
(define-class <D> (<A> <B>) (d)) ;; '(a b d)
(define-class <E> (<A> <C>) (e)) ;; '(a c e)
(define-class <F> (<D> <E>) (f)) ;; '(a b d c e f)

(define (class-all-slots c) (oops#map-append class-direct-slots (reverse (class-precedence-list c))))

(add-equal-test 'classes
          '(a)
          (map slot-info-name (class-all-slots <A>))
          "Slots of <A>")

(add-equal-test 'classes
	  '(a b d)
          (map slot-info-name (class-all-slots <D>))
          "Slots of <D>")

(add-equal-test 'classes
          '(a c e)
          (map slot-info-name (class-all-slots <E>))
          "Slots of <E>")

(add-equal-test 'classes
                '(a b d c e f)
                (map slot-info-name (class-all-slots <F>))
                "Slots of <F>")

;; class precedence

(add-equal-test 'classes
                '(<F> <E> <C> <D> <B> <A> <value>)
                (map class-name (class-precedence-list <F>))
                "class-precedence-list <F>")

(define-class <cpx1> (<number> <value>) (r i))
(define c7 (<cpx1> r: 10))
(i-set! c7 3)

(add-equal-test 'classes
                10
                (r c7))

(add-equal-test 'classes
                3
                (i c7))

(add-eq-test 'classes
             '<cpx1>
             (class-name (class-of c7)))


;; virtual slots
(define-class <cpx2> (<number> <value>)
  (;; Actual slots use rectangular coordinates
   (real-slot init-value: 0)
   (imag-slot init-value: 0)
   ;; Virtual slots do conversion
   (cpx-magnitude
      allocation: virtual: 
      slot-ref: 
        (lambda (o)
	  (let ((r (real-slot o)) (i (imag-slot o)))
	    (sqrt (+ (* r r) (* i i)))))
      slot-set!:
        (lambda (o m)
	  (let ((a (cpx-angle o)))
	    (real-slot-set! o (* m (cos a)))
	    (imag-slot-set! o (* m (sin a)))))
   )
   (cpx-angle
      allocation: virtual: 
      slot-ref: 
        (lambda (o)
	  (atan (real-slot o) (imag-slot o)))
      slot-set!: 
	(lambda (o a)
	  (let ((m (cpx-magnitude o)))
	    (real-slot-set! o (* m (cos a)))
	    (imag-slot-set! o (* m (sin a)))))
  ) )
)

(define c3 (<cpx2> real-slot: 12 imag-slot: 20))

(add-equal-test 'classes
                12
                (real-slot c3))

; IEEE floats give the wrong answer fast.
; Be sure it is the same fast wrong answer...
(add-equal-test 'classes
                (atan (real-slot c3) (imag-slot c3))
                (cpx-angle c3))

(define c4 (<cpx2> real-slot: 12 imag-slot: 20))
(imag-slot-set! c4 10)
(real-slot-set! c4 1)

(add-equal-test 'classes
                (atan (real-slot c4) (imag-slot c4))
                (cpx-angle c4))

(add-equal-test 'classes
                (let ((r (real-slot c4)) (i (imag-slot c4)))
                  (sqrt (+ (* r r) (* i i))))
                (cpx-magnitude c4))

;;@@FIXME set angle, magnitude & check real-slot and imag-slot

;; virtual slot with generic setter & getter
(define-class <foo> () 
    ((x init-value: 3)
     (y init-value: 6)
     (zed allocation: virtual: 
	slot-ref: (lambda (i) (+ (x i) (y i)))
	slot-set!: (lambda (i v) (x-set! i (- v (y i))))))
)

(define foo1 (<foo>))

(add-equal-test 'classes
                9
                (zed foo1))

(define foo2 (<foo>))

;;(ensure-setters 'zed)
(zed-set! foo2 8) 

(add-equal-test 'classes
                2
                (x foo2))

(add-equal-test 'classes
                8
                (zed foo2))


;; Constant slot
(define-class <c3> () ((a init-value: 515 allocation: constant: )))

(define c515 (<c3>))

(add-equal-test 'classes
                515
                (a c515))

(ensure-exception-raised
    'classes
    error-exception? ;; unbound
    (a-set! c2 'barf)
    "Change immutable value for c2")


;; Class slot
(define-class <cs> () ((a init-value: 3 allocation: class: )))
(define cs1 (<cs>))
(define cs2 (<cs>))
(a-set! cs2 315)

(add-equal-test 'classes
                315
                (a cs1))


(define-class <cs-child> (<cs>) ())
(define cs-child1 (<cs-child>))

(add-equal-test 'classes
                315
                (a cs1))

;; each-subclass gets its own shared slot
(define-class <es> () ((a init-value: 32 allocation: each-subclass: )))
(define-class <a1> (<es>) ())
(define-class <b1> (<es>) ())

(define es1 (<es>))
(define a1 (<a1>))
(define a2 (<a1>))
(define b1 (<b1>))
(a-set! a2 "X")

(add-equal-test 'classes
                "X"
                (a a1))
(add-equal-test 'classes
                32
                (a es1))
(add-equal-test 'classes
                32
                (a b1))



;;(run-tests-for 'classes)

'test-class

;===========================E=O=F==============================;
