;;; $Id: macinst.scm,v 1.1 2005/02/25 22:19:37 lucier Exp $
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

;;; This file *SHOULD CONTAIN MACROS ONLY*. The associated functions
;;; are in the instance.scm file.

;;; Most of these functions are implemented as macros for speed.

;;; This variable designates the first offset in a vector to hold a slot.
;;; It usually is 1 (since offset 0 is reserved to the instantiation link).
;;; REMARK: if you specify the 'safer-object' feature then all Meroon 
;;; objects will contain a unique tag to make Object? safer.

(if-meroon-feature safer-object
    (define-meroon-macro (starting-offset)
      2 )
    (define-meroon-macro (starting-offset)
      1 ) )

;;; Access to instances. Since all accesses are checked, these
;;; functions would better avoid to check that o is a vector, that the
;;; offset is a natural and that the offset lies within the vector.

(define-meroon-macro (instance-ref o offset)
  `(vector-ref ,o (fx+ ,(starting-offset) ,offset)) )

(define-meroon-macro (instance-set! o offset value)
  `(vector-set! ,o (fx+ ,(starting-offset) ,offset) ,value) )

;;; Builds an instance with CN as instantiation link and ARGS as content.
;;; ARGS are values for mono- or poly- fields as well as their size.
;;; Since vectors are initialized with the undefined value, it is
;;; needless to reinitialize them with this undefined value.

(if-meroon-feature safer-object
  (define-meroon-macro (instance cn . args)
    `(vector ,cn meroon-unique-tag . ,args) )
  (define-meroon-macro (instance cn . args)
    (cond
     ((fx= 1 (starting-offset))
      `(vector ,cn . ,args))
     (else 
      (let ((g (gensym)))
        `(let ((,g (make-vector (fx+ ,(starting-offset) ,(length args))
                                meroon-uninitialized )))
                (vector-set! ,g 0 ,cn)
                ,@(meroon-reduce (lambda (code i arg)
                                   (if (eq? arg 'meroon-uninitialized)
                                       code
                                       (cons `(instance-set! ,g ,i ,arg)
                                             code ) ) )
                                 '()
                                 (iota 0 (length args))
                                 args )
                ,g ) ) ) ) ) )

;;; Allocate an empty instance with CN as instantation link. Caution,
;;; the result is not necessarily a well formed instance. It might
;;; need to be skeletized if containing poly-fields.

(if-meroon-feature safer-object
  (define-internal-meroon-macro (allocate-empty-instance cn size)
    (let ((g (gensym)))
      `(let ((,g (make-vector (fx+ ,(starting-offset) ,size) 
                              meroon-uninitialized )))
         (vector-set! ,g 0 ,cn)
         (vector-set! ,g 1 meroon-unique-tag)
         ,g ) ) )
  (define-internal-meroon-macro (allocate-empty-instance cn size)
    (let ((g (gensym)))
      `(let ((,g (make-vector (fx+ ,(starting-offset) ,size) 
                              meroon-uninitialized )))
         (vector-set! ,g 0 ,cn)
         ,g ) ) ) )

;;; Allocate an instance with a given CONTENT. Similar to instance,
;;; except that CONTENT is given as a list. 

(if-meroon-feature safer-object
  (define-internal-meroon-macro (allocate-full-instance cn content)
    `(oo-apply vector ,cn meroon-unique-tag ,content) )
  (define-internal-meroon-macro (allocate-full-instance cn content)
    (let ((g (gensym))
          (c (gensym)) )
      (if (fx= 1 (starting-offset))
          `(letrec ((,g (oo-apply vector ,cn ,content)))
             ,g )
          `(let* ((,c ,content)
                  (,g (allocate-empty-instance ,cn (length ,c))) )
             (fill-instance! ,g 0 ,c) ) ) ) ) )

;;; This one is actually needed but curious. It is actually needed by
;;; maker.scm to check a posteriori the allocation of an instance. It
;;; is also used by size.scm to know the size of Meroon. It returns
;;; the number of values contained in an instance (instantiation link
;;; not comprised).

(define-internal-meroon-macro (instance-length o)
  `(fx- (vector-length ,o) ,(starting-offset)) )

;;; Emit a declaration that inlining here is worthless.

(define-internal-meroon-macro (meroon-declare-not-inlinable)
  '(quote *nothing-at-all*) )

;;; end of macinst.scm
