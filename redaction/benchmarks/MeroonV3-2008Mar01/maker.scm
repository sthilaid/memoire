;;; $Id: maker.scm,v 1.1 2005/02/25 22:19:37 lucier Exp $ 
;;; Copyright (c) 1990-96 by Christian Queinnec. All rights reserved.

;;;                ***********************************************
;;;                    Small, Efficient and Innovative Class System
;;;                                    Meroon 
;;;                              Christian Queinnec  
;;;                    Ecole Polytechnique & INRIA--Rocquencourt
;;;                ************************************************

;;; This file defines the family of make-<class> facilities. The
;;; instantiate (see fill.scm file) special form is theoretically
;;; sufficient to specify any kind of allocation but it is often the
;;; case that for classes with only a few fields (for instance, a
;;; Point with x and y fields), a make-Point function is preferable.
;;; Keywords as offered by instantiate are better to specify a large
;;; number of fields and to specify which one must be initialized by
;;; default. 

;;; Most of the makers are already defined in alloc.sc where it was
;;; not possible to use generic functions. It is now possible so we
;;; can define general-make.

;;; The problem with make-<class> functions is to avoid a huge
;;; consumption of conses due to the dotted variables in make-general.
;;; Since the user can only gives regular values to make-<class>, it
;;; is useless to call fill-other-fields! on this fresh instance.

(define (general-make class args)
  (let ((ins-tance (allocate-full-instance (Class-number class) args)))
    (initialize! (check-conformity ins-tance)) ) )

;;;oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; Here, we allocate en masse the instance then check a posteriori
;;; that it is a regular instance ie sizes are correct and in 
;;; appropriate locations.  If correct, the instance is returned.

(define (check-conformity ins-tance)
  (let ((class (object->class ins-tance))
        (len (instance-length ins-tance)) )
    (let check ((fields (Class-fields class))
                (index 0) )
      (if (pair? fields)
          (if (fx< index len)
              (let ((new-index (Field-check-conformity 
                                (car fields) ins-tance index ) ))
                (if new-index 
                    (check (cdr fields) new-index)
                    (report-meroon-error
                     'Syntax (symbol-concatenate 'make- (Class-name class))
                     "Incorrect allocation for" (car fields) ) ) )
              (report-superfluous-allocation ins-tance) )
          (cond ((fx= index len) ins-tance)
                ((fx< index len) (report-superfluous-allocation ins-tance))
                (else 
                 (report-meroon-error
                  'Syntax (symbol-concatenate 'make- (Class-name class))
                  "Missing allocation arguments" ) ) ) ) ) ) )

;;; This generic function checks, for each type of Field, if the
;;; allocation is correct. It returns the position of the next field
;;; or #f if ill-formed. CAUTION, do not trace this generic function
;;; since INSTANCE is not guaranteed to be a correctly formed
;;; instance.

(define-generic (Field-check-conformity (field Field) ins-tance index))

(define-method (Field-check-conformity (field Mono-Field) ins-tance index)
  (fx+ 1 index) )

(define-method (Field-check-conformity (field Poly-Field) ins-tance index)
  (let ((content (instance-ref ins-tance index)))
    (check-size content field)
    (fx+ index 1 content) ) )

(define (report-superfluous-allocation ins-tance)
  (report-meroon-error
   'Syntax (symbol-concatenate 'make- (Class-name (object->class ins-tance)))
   "Superfluous allocation arguments" ) )

;;; end of maker.scm
