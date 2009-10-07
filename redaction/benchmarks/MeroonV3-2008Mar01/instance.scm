;;; $Id: instance.scm,v 1.1 2005/02/25 22:19:37 lucier Exp lucier $
;;; Copyright (c) 1990-96 by Christian Queinnec. All rights reserved.

;;;                ***********************************************
;;;                    Small, Efficient and Innovative Class System
;;;                                    Meroon 
;;;                              Christian Queinnec  
;;;                    Ecole Polytechnique & INRIA--Rocquencourt
;;;                ************************************************

;;; This file defines the representation of instances. They are
;;; implemented as vectors with a first slot holding the instantiation
;;; link. The words offset and index are distinguished; the former
;;; denotes offsets within the instance representation while the
;;; latter denotes natural numbers used within indexed fields.

;;; This file only contains functions. The associated macros are in the
;;; macinst.scm file.

;;; Test if a Scheme object is a Meroon object. 
;;; Rather weak ! Could be improved when Scheme can create new types.
;;; Note that native Scheme objects are not Meroon objects and no attempt 
;;; is done to handle that since this would pose many problems if the user
;;; wants, for example, to subclass vectors or functions.

(if-meroon-feature safer-object
  (define (Object? o)
    (and (vector? o)
         (fx>= (vector-length o) (starting-offset))
         (eq? meroon-unique-tag (vector-ref o 1)) ) )
  (define (Object? o)
    (and (vector? o)
         (let ((cn (object->class-number o)))
           (and (number? cn)
                (exact? cn)
                (fx<= 0 cn)
                (fx< cn *class-number*)
                (fx= cn (careless-Class-number (number->class cn))) ) ) ) ) )

;;; Of course, no one should use this hidden tag.
;;; Don't put that definition in macinst.scm since it would be compiled
;;; in every file and will get non-eq but equal values.

(if-meroon-feature safer-object
  (define meroon-unique-tag
    (cons 'meroon 'tag) )
  #t )

;;; Extracts the internal number of a class from within an instance.

(define (object->class-number o)
  (vector-ref o 0) )

;;; Shallow copy of an instance.

(define (instance-clone o)
  (let* ((n (vector-length o))
         (r (make-vector n)) )
    (copy-vector-slice! o r 0 n)
    r ) )

;;; end of instance.scm
