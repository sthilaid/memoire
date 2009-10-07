;;; FILE: "test-types.scm"
;;; IMPLEMENTS: Unit tests for <type> code [for Oops]
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
;; (require 'oops)

(add-test-suite 'types)


;; trivial

(add-equal-test 'types
                #t
                (subtype? <number> <number>))

(add-equal-test 'types
                #t
                (is-a? 3 <number>))


;; subclass

(add-equal-test 'types
                #t
                (subtype? <integer> <number>))

(add-equal-test 'types
                #t
                (is-a? (<singleton> 3) <singleton>))

(add-equal-test 'types
                #t
                (is-a? (<singleton> 3) <type>))

;; <singleton>

(add-equal-test 'types
                #t
                (is-a? 3 (<singleton> 3)))

(add-equal-test 'types
                #f
                (is-a? 4 (<singleton> 3)))

(add-equal-test 'types
                #t
                (is-a? #f <boolean>))

(add-equal-test 'types
                #t
                (is-a? #t <boolean>))


;; <one-of>

(add-equal-test 'types
                #t
                (is-a? 3 (<one-of> 2 3 4 5)))

(add-equal-test 'types
                #t
                (is-a? (symbol->string 'str)
                       (apply <one-of>
                              (map symbol->string '(a str test)))))

(add-equal-test 'types
                #t
                (is-a? 'bar (apply <one-of>
                                   '(foo bar baz quuz)))
                "enum")


;; <limited-range>

(add-equal-test 'types
                #t
                (subtype? <u8int> <integer>))

(add-equal-test 'types
                #f
                (subtype? <integer> <u8int>))

(add-equal-test 'types
                #t
                (is-a? 3 <u8int>))

(add-equal-test 'types
                #f
                (is-a? 789 <u8int>))

(add-equal-test 'types
                #t
                (subtype? <u8int> <s8int>))

(add-equal-test 'types
                #f
                (subtype? <s8int> <u8int>))

;; <subclass-type>
(add-equal-test 'types
                #t
                (subtype? <integer> (<subclass-type> subclass-of: <integer>)))

(add-equal-test 'types
                #t
                (subtype? <integer> (<subclass-type> subclass-of: <real>)))

(add-equal-test 'types
                #t
                (subtype? <u8int> (<subclass-type> subclass-of: <integer>)))

(add-equal-test 'types
                #t
                (subtype? (<one-of> 'foo 'bar) (<one-of> 'foo 'bar 'baz)))

(add-equal-test 'types
                #f
                (subtype? (<one-of> 'foo 'bar 'quux) (<one-of> 'foo 'bar)))

(add-equal-test 'types
                #f
                (subtype? (<one-of> 'foo 'bar 'quux) (<one-of> 'foo 'bar 'baz)))

(define <even?> (<predicate-type> superclass: <integer> test-for: even?))

(add-equal-test 'types
                #f
                (is-a? 3 <even?>))

(add-equal-test 'types
                #f
                (is-a? 'four <even?>))

(add-equal-test 'types
                #t
                (is-a? 4 <even?>))

(add-equal-test 'types
                #t
                (subtype? <even?> <integer>))

(add-equal-test 'types
                #t
                (subtype? <even?> (<subclass-type> subclass-of: <integer>)))

(add-equal-test 'types
                #f
                (subtype? <even?> <u8int>))

(add-equal-test 'types
                #f
                (subtype? <u8int> <even?>))

;; <record>s and <record-type>s

(define-structure giant fee fiw fo fum)

(define g1 (make-giant 1 2 3 4))

(add-eq-test 'types
             'giant
             (name (type g1)))


(add-equal-test 'types
                #f
                (is-a? 3 (type g1)))

(add-equal-test 'types
                #t
                (is-a? g1 (type g1)))

(add-equal-test 'types
                #t
                (subtype? (type g1) <record>))

(add-equal-test 'types
                #f
                (subtype? (type g1) <record-type>))

(add-equal-test 'types
                #t
                (subtype? (type (type g1)) <record-type>))


;;(run-tests-for 'types)


;===========================E=O=F==============================;
