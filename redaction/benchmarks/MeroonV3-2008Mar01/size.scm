;;; $Id: size.scm,v 1.1 2005/02/25 22:19:37 lucier Exp $
;;; Copyright (c) 1990-96 by Christian Queinnec. All rights reserved.

;;;                ***********************************************
;;;                    Small, Efficient and Innovative Class System
;;;                                    Meroon 
;;;                              Christian Queinnec  
;;;                    Ecole Polytechnique & INRIA--Rocquencourt
;;;                ************************************************

;;; This file defines a set of functions that compute the size of Meroon.
;;; It walks any internal objects defined to make Meroon work and
;;; cumulates their size. The total size is obtained with: (show-meroon)

(define (show-meroon . stream)
  (let ((stream (if (pair? stream) (car stream) (current-output-port))))
    (newline stream)
    (display meroon-version stream)
    (newline stream)
    (display "Total number of classes: " stream)
    (display *class-number* stream)
    (newline stream)
    (display "Total number of generic functions: " stream)
    (display *generic-number* stream)
    (newline stream)
    (let ((sum (show-meroon-size Object-class)))
      (set! sum (fx+ sum (vector-length *classes*)))
      (sequence-map (lambda (c) (set! sum (fx+ sum (show-meroon-size c))))
                    *classes*  )
      (set! sum (fx+ sum (vector-length *generics*)))
      (sequence-map (lambda (g) (set! sum (fx+ sum (show-meroon-size g))))
                    *generics*  )
      (display "(estimated) internal size of Meroon: " stream)
      (display sum stream)
      (display " pointers" stream)
      (newline stream) )
    #t ) )

(define-generic (show-meroon-size (o))
  (cond ((vector? o) (vector-length o))
        ((pair? o) (fx+ (show-meroon-size (car o))
                      (show-meroon-size (cdr o)) ))
        (else 0) ) )

;;; Return the overall size of the instance

(define-method (show-meroon-size (o Object))
  (fx+ (starting-offset) (instance-length o)) )

(define-method (show-meroon-size (o Class))
  (let ((sum (call-next-method)))
    (for-each (lambda (field) 
                (set! sum (fx+ sum (show-meroon-size field))) )
              (Class-fields o) )
    sum ) )

(define-method (show-meroon-size (o Generic))
  (fx+ (call-next-method)
     (show-meroon-size (Generic-dispatcher o)) ) )

(define-method (show-meroon-size (o Subclass-Dispatcher))
  (fx+ (call-next-method)
       (fx+ (show-meroon-size (Subclass-Dispatcher-yes o))
	    (show-meroon-size (Subclass-Dispatcher-no o)) ) ) )

(define-method (show-meroon-size (o Indexed-Dispatcher))
  (fx+ (call-next-method)
     (show-meroon-size (Indexed-Dispatcher-no o)) ) )

(define-method (show-meroon-size (o Linear-Dispatcher))
  (fx+ (call-next-method)
      (fx+  (length (Linear-Dispatcher-signature o))
	    (show-meroon-size (Linear-Dispatcher-no o)) ) ) )

(define-method (show-meroon-size (o Global-Dispatcher))
  (fx+ (call-next-method)
       (vector-length *classes*) ) )

;;; end of size.scm
