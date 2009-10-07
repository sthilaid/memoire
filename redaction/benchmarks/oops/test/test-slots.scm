;;; FILE: "test-slots.scm"
;;; IMPLEMENTS: Unit tests for slots [for Oops]
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

(add-test-suite 'slots)

;; for virtual setter/getter tests
(define val #!void)
(define val-set!
  '(lambda (ignore v) (set! val v)))
(define val-ref
  '(lambda (ignore) v))

;; for init-fun
(define val-init
  (lambda () (set! val 'initialized)))


;; Tests Proper

(add-equal-test 'slots
          (oops#really-make-slot-info 'foo instance: #f #f not-set-by-user: 0 #f #f)
          (oops#make-slot-info 'foo init-value: 0)
          "slot-info creation")

(add-equal-test 'slots
          (oops#make-slot-info
           'foo
           allocation: instance:
           slot-ref: #f
           slot-set!: #f
           use-init-keyword: not-set-by-user:
           init-value: #!unbound
           init-fun: #f
           type: #f)
          (oops#process-slot 'foo)
          "(process-slot 'foo)")

(add-equal-test 'slots
          (oops#make-slot-info
           'foo
           allocation: instance:
           slot-ref: #f
           slot-set!: #f
           use-init-keyword: not-set-by-user:
           init-value: #!unbound
           init-fun: #f
           type:     #f)
          (oops#process-slot '(foo))
          "(process-slot '(foo))")

(add-equal-test 'slots
          (oops#really-make-slot-info
           'foo	     ; name
           instance: ; allocation
           #f        ; slot-ref
           #f        ; slot-set!
           not-set-by-user:  ; use-init-keyword
           '(quote 0) ; init-value: 
           #f        ; init-fun:
           #f        ; type:
           )
          (oops#process-slot '(foo init-value: 0))
          "(process-slot '(foo init-value: 0))")

(add-equal-test 'slots
          (oops#make-slot-info
           'foo
           allocation: class:
           slot-ref: #f
           slot-set!: #f
           use-init-keyword: not-set-by-user:
           init-value: '(quote #f)
           init-fun: #f
           type: #f)
          (oops#process-slot '(foo allocation: class: init-value: #f))
          "(process-slot '(foo allocation: class:  init-value: #f))")

(ensure-exception-raised 'slots
                         error-exception?
                         (oops#process-slot `(foo allocation: class:))
                         "class allocation w/o init")

(add-equal-test 'slots
          (oops#make-slot-info
           'foo
           allocation: virtual:
           slot-ref:   (list 'quote val-ref)
           slot-set!:  (list 'quote val-set!)
           use-init-keyword: not-set-by-user:
           init-value: #!unbound
           init-fun: #f
           type: #f)
          (oops#process-slot (list 'foo allocation: virtual:
                              slot-ref:  val-ref
                              slot-set!: val-set!))
          "(process-slot 'foo: virt-allocation +ref,set)")

(ensure-exception-raised 'slots
                         error-exception?
                         (oops#process-slot `(foo allocation: virtual:
                                             slot-set!: ,val-set!))
                         "virtual allocation w/o getter")

(ensure-exception-raised 'slots
                         error-exception?
                         (oops#process-slot `(foo allocation: virtual:
                                             slot-ref:  ,val-ref))
                         "virtual allocation w/o setter")

(ensure-exception-raised 'slots
                         error-exception?
                         (oops#process-slot '(foo allocation: virtual:))
                         "virtual allocation w/o getter or setter")

(add-equal-test 'slots
          (oops#make-slot-info
           'foo
           allocation: instance:
           slot-ref:   #f
           slot-set!:  #f
           use-init-keyword: not-set-by-user:
           init-value: #!unbound
           init-fun:   (list 'quote val-init)
           type: #f)
          (oops#process-slot `(foo init-function: ,val-init))
          "(process-slot '(foo init-function: val-init))")

(add-equal-test 'slots
          (oops#make-slot-info
           'foo
           allocation: virtual:
           slot-ref:   (list 'quote val-ref)
           slot-set!:  (list 'quote val-set!)
           use-init-keyword: #f
           init-value: '(quote 37)
           init-fun: #f
           type: <integer>)
          (oops#process-slot (list 'foo
                              allocation: virtual:
                              slot-ref:  val-ref
                              slot-set!: val-set!
                              use-init-keyword: #f
                              init-value: 37
                              type: <integer>
                              ))
          "(process-slot 'foo: + many options)")

(add-equal-test 'slots
          (oops#make-slot-info
           'foo
           allocation: each-subclass:
           slot-ref:   #f
           slot-set!:  #f
           use-init-keyword: #t
           init-value: '(quote 37)
           init-fun: #f
           type: <integer>)
          (oops#process-slot (list 'foo
                              allocation: each-subclass:
                              use-init-keyword: #t
                              init-value: 37
                              type: <integer>
                              ))
          "(process-slot 'foo: + other options)")

(ensure-exception-raised 'slots
                         error-exception?
                         (oops#process-slot (list 'foo allocation: virtual:
                                             slot-ref:  val-ref
                                             slot-set!: val-set!
                                             use-init-keyword: #f
                                             init-value: 
                                             type: <integer>))
                         "uneven number of key, value pairs")

(ensure-exception-raised 'slots
                         error-exception?
                         (oops#process-slot (list 'foo allocation: virtual:
                                             slot-ref:  val-ref
                                             slot-set!: val-set!
                                             use-init-keyword: #f
                                             init-value: 23
                                             init-function:  val-init
                                             type: <integer>))
                         "both int-value: and init-function:")

;;(run-tests-for 'slots)

'test-slots

;===========================E=O=F==============================;
