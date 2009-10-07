;;; $Id: postset.scm,v 1.1 2005/02/25 22:19:37 lucier Exp $
;;; Copyright (c) 1990-2000 Christian Queinnec. All rights reserved.

;;;                ***********************************************
;;;                    Small, Efficient and Innovative Class System
;;;                                    Meroon 
;;;                              Christian Queinnec  
;;;                    Ecole Polytechnique & INRIA--Rocquencourt
;;;                ************************************************

;;; This file extends the MeroonV3 system with new meta-classes that
;;; introduce a new naming scheme that is, if a field F is mutable,
;;; its writer is named F-set! (rather than set-F! as before). PAY
;;; ATTENTION that when loaded this file modifies the predefined
;;; behavior of define-class. New classes will have this type of names
;;; but old classes still keep their old names. This is another
;;; example of the use of the Meroon MOP. Another word of caution is
;;; required: accessors/predicate/maker are now explicit lambda-forms
;;; that may be inlined by an aggressive compiler. However, this may
;;; take a lot of space compared to the predefined strategy where
;;; these functions were implemented as shared closures.

;;; Refine basic classes

(define-class Inlinable-Class Handy-Class ())
(define-class Inlinable-Mono-Field Mono-Field ())
(define-class Inlinable-Poly-Field Poly-Field ())

;;; Install them as default metaclasses.

(set! *standard-class-metaclass-name* 'Inlinable-Class)
(set! *standard-mono-field-metaclass-name* 'Inlinable-Mono-Field)
(set! *standard-poly-field-metaclass-name* 'Inlinable-Poly-Field)

;;; The new naming scheme.

(define (generate-reader-name field class)
  (let ((classname (careless-Class-name class))
        (fieldname (careless-Field-name field)) )
    (symbol-concatenate classname '- fieldname) ) )
(define (generate-writer-name field class)
  (let ((classname (careless-Class-name class))
        (fieldname (careless-Field-name field)) )
    (symbol-concatenate classname '- fieldname '- 'set!) ) )
(define (generate-lengther-name field class)
  (let ((classname (careless-Class-name class))
        (fieldname (careless-Field-name field)) )
    (symbol-concatenate classname '- fieldname '- 'length) ) )

(define (generate-class-name class)
  (let ((classname (careless-Class-name class)))
    (symbol-concatenate classname '- 'class) ) )
(define (generate-predicate-name class)
  (let ((classname (careless-Class-name class)))
    (symbol-concatenate classname "?") ) )
(define (generate-allocator-name class)
  (let ((classname (careless-Class-name class)))
    (symbol-concatenate 'allocate- classname) ) )
(define (generate-maker-name class)
  (let ((classname (careless-Class-name class)))
    (symbol-concatenate 'make- classname) ) )

;;; Define an accessor for all fields (and not only for proper fields
;;; as for Handy-Classes).

(define-method (generate-accessors (class Inlinable-Class) class-options)
  `(begin 
     #t                                 ; so the body is never empty
     ,@(oo-map (lambda (field)
                 (Field-generate-Handy-accessors field class) )
               (Class-fields class) ) ) )

;;; {{{ Generate inlinable accessors:

;;; Modify the implementation to use these names. In the same time,
;;; modify the generation so accessors are defined by explicit lambda
;;; forms (potentially inlinable) rather than shared closures. This
;;; was required by Brad Lucier <lucier@math.purdue.edu>.

(define-method (Field-generate-Handy-accessors (field Inlinable-Mono-Field) 
                                               class )
  (let ((class-variable (generate-class-name class)) )
    `(begin
       (meroon-define ,(generate-reader-name field class)
         ,(generate-fast-careful-reader field class-variable) )
       ,@(if (Field-immutable? field)
             `()
             `((meroon-define ,(generate-writer-name field class)
                 ,(generate-fast-careful-writer 
                   field class-variable ) )) ) ) ) )

(define-method (Field-generate-Handy-accessors (field Inlinable-Poly-Field)
                                               class )
  (let ((class-variable (generate-class-name class)) )
    `(begin 
       (meroon-define ,(generate-lengther-name field class)
         ,(generate-fast-careful-lengther field class-variable) )
       (meroon-define ,(generate-reader-name field class)
         ,(generate-fast-careful-reader field class-variable) )
       ,@(if (Field-immutable? field)
             `()
             `((meroon-define ,(generate-writer-name field class)
                 ,(generate-fast-careful-writer
                   field class-variable ) )) ) ) ) )

;;; }}}
;;; {{{ Generate an inlinable predicate:

(define-method (generate-predicate (class Inlinable-Class) class-options)
  (let ((o (gensym)))
    `(meroon-define ,(generate-predicate-name class)
       (lambda (,o)
         (and (Object? ,o)
              (careless-subclass? (object->class ,o) 
                                  ,(generate-class-name class) ) ) ) ) ) )

;;; }}}
;;; {{{ Generate an inlinable maker:
;;; Only when there are no poly fields.

(define-method (generate-maker (class Inlinable-Class) class-options)
  (if (find-option-present? 'virtual class-options)
      `'**no-maker**
      (let ((poly-fields-number (count-Poly-Fields 
                                 (careless-Class-fields class) )))
        `(meroon-define ,(generate-maker-name class)
           ,(if (fx= 0 poly-fields-number)
                (let ((field-names (oo-map Field-name 
                                           (careless-Class-fields class) )))
                  `(lambda ,field-names
                     (initialize! (instance (careless-Class-number
                                             ,(generate-class-name class) )
                                            ,@field-names )) ) )
                `(make-maker ,(generate-class-name class)) ) ) ) ) )

;;; }}}
;;; end of postset.scm
