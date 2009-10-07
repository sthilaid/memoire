;;; $Id: instance.gsc,v 1.2 2005/05/04 02:44:43 lucier Exp lucier $
;;; Copyright (c) 1990-96 by Christian Queinnec. All rights reserved.

;;;                ***********************************************
;;;                    Small, Efficient and Innovative Class System
;;;                                    Meroon V3
;;;                              Christian Queinnec  
;;;                   \'Ecole Polytechnique & INRIA--Rocquencourt
;;;                ************************************************

;;;                 Gambit-C adaptation of instance.scm

;;; Use a special subtype for Meroon objects. This allows the Object?
;;; predicate to be safe (so the safer-object feature is just a waste
;;; of room that should be avoided). Thanks to Brad Lucier
;;; <lucier@MATH.Purdue.EDU> for these files.

;;; This file only contains functions. Associated macros are in the 
;;; macinst.gsc file.

;;; Test if a Scheme object is a Meroon object. 

(define (Object? o)
  (declare (extended-bindings)(not safe))
  (##meroon? o))

(define-meroon-macro (meroon-ref o i)
  `(let ((o ,o)
	 (i ,i))
     (declare (not safe)(extended-bindings))
     (##vector-ref o i)))

(define-meroon-macro (meroon-set! o i val)
  `(let ((o ,o)
	 (i ,i)
	 (val ,val))
     (declare (not safe)(extended-bindings))
     (##vector-set! o i val)))

(define-internal-meroon-macro (meroon-length o)
  `(let ((o ,o))
     (declare (extended-bindings)(not safe))
     (##vector-length ,o)))

(define-internal-meroon-macro (make-meroon n val)
  `(let ((n ,n)
	 (val ,val))
     (declare (extended-bindings)(not safe))
     (let ((result (##make-vector n val)))
       (##subtype-set! result (subtype-meroon))
       result)))

;;; Extracts the internal number of a class from within an instance.
;;; now in macinst.gsc

;;(define (object->class-number o)
;;  (declare (not safe))
;;  (if (or (not (meroon-safer-object))
;;	  (Object? o))
;;      (meroon-ref o 0)
;;      (report-meroon-error 'Access object->class-number "Not an object---Shouldn't happen---Please investigate and report this error" o)))

;;; Shallow copy of an instance.

(define (instance-clone o)

  ;; Copy instance old[start..end[ into instance new[start..end[
  
  (define (copy-instance-slice! old new start end)
    (let copy ((i start))
      (when (fx< i end)
	    (meroon-set! new i (meroon-ref old i))
	    (copy (fx+ i 1)) ) ) )
  
  (if (or (not (meroon-safer-object))
	  (Object? o))
      (let* ((n (meroon-length o))
	     (r (make-meroon n meroon-uninitialized)) )
	(copy-instance-slice! o r 0 n)
	r )
      (report-meroon-error 'Access instance-clone "Not an object---Shouldn't happen---Please investigate and report this error" o)))
  
;;; Local Variables:
;;; mode: scheme
;;; End:

;;; end of instance.gsc
