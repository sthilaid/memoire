;;; $Id: alloc.scm,v 1.1 2005/02/25 22:19:37 lucier Exp lucier $
;;; Copyright (c) 1990-97 by Christian Queinnec. All rights reserved.

;;;                ***********************************************
;;;                    Small, Efficient and Innovative Class System
;;;                                    Meroon 
;;;                              Christian Queinnec  
;;;                    Ecole Polytechnique & INRIA--Rocquencourt
;;;                ************************************************

;;; This file defines the basic machinery to allocate instances. It is
;;; not general since it is not yet possible to define generic
;;; functions. This will come later with maker.scm to implement
;;; general-make.  A bunch of makers are precomputed, it is possible
;;; to augment the size of the bunch if you care about it.

;;; Try to create appropriate makers with a correct arity to avoid
;;; using general-make and consing a lot.

(define (make-maker class)
  (let ((poly-fields-number (count-Poly-Fields (careless-Class-fields class))))
    (case poly-fields-number
      ((0) (create-zero-poly-maker class))
      ((1) (if (Poly-Field? (car (last-pair (careless-Class-fields class))))
               (create-one-final-poly-maker class)
               (lambda args (general-make class args)) ))
      (else (lambda args (general-make class args))) ) ) )

;;;oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; Precompute a bunch of makers so they can be shared.

;;; Makers without Poly-Field.

(define (create-zero-poly-maker class)
  (generate-bunch-of-zero-poly-makers 10 class) )

;;; Same trick for makers with only one final Poly-Field.

(define (create-one-final-poly-maker class)
  (generate-bunch-of-one-final-poly-makers 10 class) )

;;; end of alloc.scm
