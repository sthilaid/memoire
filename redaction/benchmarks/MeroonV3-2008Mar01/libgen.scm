;;; $Id: libgen.scm,v 1.1 2005/02/25 22:19:37 lucier Exp lucier $ 
;;; Copyright (c) 1990-96 by Christian Queinnec. All rights reserved.

;;;                ***********************************************
;;;                    Small, Efficient and Innovative Class System
;;;                                    Meroon 
;;;                              Christian Queinnec  
;;;                    Ecole Polytechnique & INRIA--Rocquencourt
;;;                ************************************************

;;; This file defines some generic functions. Everything is in place
;;; now so they can be defined as normal generic functions. Nevertheless
;;; not to add useless eta-redexes, we directly add methods to generic
;;; functions using an internal entry point: add-1-method!.

;;;==================================================================== Coercer
;;; Coercers are named with an arrow and a class-name. These are generic 
;;; functions created at class-load time and can be customized. Since
;;; symbol is not a Meroon class, it is not possible to specialize on
;;; them so we redefine these generic functions instead.

(define-generic (->Object (o)) o)

(define-generic (->Class (o))
  (cond ((symbol? o) (symbol->class o))
        ((and (fixnum? o) (fx>= o 0) (fx< o *class-number*))
         (number->class o) )
        (else (report-meroon-error 
               'Domain '->Class "Cannot coerce to Class" o )) ) )

(define-method (->Class (o Class)) o)

(define-generic (->Generic (o))
  (cond ((symbol? o) (symbol->generic o))
        (else (report-meroon-error 
               'Domain '->Generic "Cannot coerce to Generic" o )) ) )

(define-method (->Generic (o Generic)) o)

;;;====================================================== Generic retrofit
;;; Retrofit all functions that should be offered as generic functions.

(define-retrofitted-generic (is-a? o (class Pre-Class))
  ((Class-class Class-is-a?)
   (View-class  View-is-a?) ) )

(define-retrofitted-generic (compute-value-offset o (field Pre-Field) . index)
  ((Mono-Field-class Mono-Field-compute-value-offset)
   (Poly-Field-class Poly-Field-compute-value-offset)
   (Virtual-Field-class Virtual-Field-compute-value-offset) )
  (unless (Object? o)
    (report-meroon-error 'Access 'compute-value-offset
                         "Not an Object" o ) )
  (let ((field (retrieve-named-field (object->class o) 
                                     field 
                                     report-bad-coercion-to-field )))
    (if (pair? index)
        (compute-value-offset o field (car index))
        (compute-value-offset o field) ) ) )

(define-retrofitted-generic (field-value o (field Pre-Field) . index)
  ((Mono-Field-class Mono-Field-field-value)
   (Poly-Field-class Poly-Field-field-value)
   (Virtual-Field-class Virtual-Field-field-value) )
  (unless (Object? o)
    (report-meroon-error 'Access 'field-value
                         "Not an Object" o ) )
  (let ((field (retrieve-named-field (object->class o) 
                                     field 
                                     report-bad-coercion-to-field )))
    (if (pair? index)
        (field-value o field (car index))
        (field-value o field) ) ) )

(define-retrofitted-generic (set-field-value! o value (field Pre-Field) . index)
  ((Mono-Field-class Mono-Field-set-field-value!)
   (Poly-Field-class Poly-Field-set-field-value!)
   (Virtual-Field-class Virtual-Field-set-field-value!) )
  (unless (Object? o)
    (report-meroon-error 'Access 'set-field-value!
                         "Not an Object" o ) )
  (let ((field (retrieve-named-field (object->class o) 
                                     field 
                                     report-bad-coercion-to-field )))
    (if (pair? index)
        (set-field-value! o value field (car index))
        (set-field-value! o value field) ) ) )

(define-retrofitted-generic (initialize-field-value! 
                             o value (field Pre-Field) . index )
  ((Mono-Field-class Mono-Field-initialize-field-value!)
   (Poly-Field-class Poly-Field-initialize-field-value!)
   (Virtual-Field-class Virtual-Field-initialize-field-value!) )
  (unless (Object? o)
    (report-meroon-error 'Access 'initialize-field-value!
                         "Not an Object" o ) )
  (let ((field (retrieve-named-field (object->class o) 
                                     field 
                                     report-bad-coercion-to-field )))
    (if (pair? index)
        (initialize-field-value! o value field (car index))
        (initialize-field-value! o value field) ) ) )

(define-retrofitted-generic (field-defined? o (field Pre-Field) . index)
  ((Mono-Field-class Mono-Field-field-defined?)
   (Poly-Field-class Poly-Field-field-defined?)
   (Virtual-Field-class Virtual-Field-field-defined?) )
  (unless (Object? o)
    (report-meroon-error 'Access 'field-defined?
                         "Not an Object" o ) )
  (let ((field (retrieve-named-field (object->class o) 
                                     field 
                                     report-bad-coercion-to-field )))
    (if (pair? index)
        (field-defined? o field (car index))
        (field-defined? o field) ) ) )

(define-retrofitted-generic (generate-offset ovar (field Pre-Field) 
                                          fieldvar . indexvar )
  ((Mono-Field-class Mono-Field-generate-offset)
   (Poly-Field-class Poly-Field-generate-offset) ) )

(define-retrofitted-generic (add-subclass name (super-class Class) 
                              own-field-descs class-options )
  ((Class-class Class-add-subclass)) )

;;; This might loop or be incredibly slow, so avoid it. To be tested????

(if-meroon-feature fully-reflective-dispatching
    (define-retrofitted-generic (find-method (d Dispatcher) cn)
      ()
      (default-find-method d cn)  )
    #f )

;;; The tracing-dispatcher method will be added after (in trace.scm)

(define-retrofitted-generic (enlarge-dispatcher! (d Dispatcher))
  ((Global-Dispatcher-class Global-Dispatcher-enlarge-dispatcher!))
  d )

;;; The two following generic functions are difficult since they use
;;; themselves to be defined. The hack is that define-retrofitted-generic
;;; uses an alternate name for the definition then patches things
;;; appropriately.

(define-retrofitted-generic (augment-dispatcher! (d Dispatcher) class method)
  ((Immediate-Dispatcher-class Immediate-Dispatcher-augment-dispatcher!)
   (Subclass-Dispatcher-class Subclass-Dispatcher-augment-dispatcher!)
   (Indexed-Dispatcher-class Indexed-Dispatcher-augment-dispatcher!)
   (Global-Dispatcher-class Global-Dispatcher-augment-dispatcher!)
   (Tracing-Dispatcher-class Tracing-Dispatcher-augment-dispatcher!) ) )

(define-retrofitted-generic (compress-dispatcher! 
                             (d Dispatcher) level top-class)
  ((Immediate-Dispatcher-class Immediate-Dispatcher-compress-dispatcher!)
   (Subclass-Dispatcher-class Subclass-Dispatcher-compress-dispatcher!)
   (Indexed-Dispatcher-class Indexed-Dispatcher-compress-dispatcher!)
   (Global-Dispatcher-class Global-Dispatcher-compress-dispatcher!)
   (Tracing-Dispatcher-class Tracing-Dispatcher-compress-dispatcher!) ) )

(define-retrofitted-generic (rebalance-dispatcher! d (dno Dispatcher))
  ((Immediate-Dispatcher-class Immediate-Dispatcher-rebalance-dispatcher!)
   (Subclass-Dispatcher-class Subclass-Dispatcher-rebalance-dispatcher!)
   (Indexed-Dispatcher-class Indexed-Dispatcher-rebalance-dispatcher!)
   (Global-Dispatcher-class Global-Dispatcher-rebalance-dispatcher!)
   (Tracing-Dispatcher-class Tracing-Dispatcher-rebalance-dispatcher!) ) )

(define-retrofitted-generic (update-dispatcher! (d Dispatcher) class)
  ((Immediate-Dispatcher-class Immediate-Dispatcher-update-dispatcher!)
   (Subclass-Dispatcher-class Subclass-Dispatcher-update-dispatcher!)
   (Indexed-Dispatcher-class Indexed-Dispatcher-update-dispatcher!)
   (Global-Dispatcher-class Global-Dispatcher-update-dispatcher!)
   (Tracing-Dispatcher-class Tracing-Dispatcher-update-dispatcher!) ) )

(define-retrofitted-generic (Generic-update! (generic Generic) class)
  ((Generic-1-class Generic-1-update!)
   (Generic-N-class Generic-N-update!) ) )

(define-retrofitted-generic (generate-accessors (o Pre-Class) class-options)
  ((Class-class Class-generate-accessors)
   (Handy-Class-class Handy-Class-generate-accessors)
   (MeroonV2-Class-class MeroonV2-Class-generate-accessors)
   (View-class View-generate-accessors) ) )

(define-retrofitted-generic (generate-fast-careful-reader 
                             (field Pre-Field) class-variable )
  ((Mono-Field-class Mono-Field-generate-fast-careful-reader)
   (Poly-Field-class Poly-Field-generate-fast-careful-reader) ) )   

(define-retrofitted-generic (generate-fast-careful-lengther
                             (field Pre-Field) class-variable )
  ((Poly-Field-class Poly-Field-generate-fast-careful-lengther) ) )

(define-retrofitted-generic (generate-fast-careful-writer 
                             (field Pre-Field) class-variable )
  ((Mono-Field-class Mono-Field-generate-fast-careful-writer)
   (Poly-Field-class Poly-Field-generate-fast-careful-writer) ) )

(define-retrofitted-generic (Field-generate-MeroonV2-accessors 
                             (field Pre-Field) class )
  ((Mono-Field-class Mono-Field-generate-MeroonV2-accessors)
   (Poly-Field-class Poly-Field-generate-MeroonV2-accessors)
   (Virtual-Field-class Virtual-Field-generate-MeroonV2-accessors) ) )

(define-retrofitted-generic (Field-generate-Handy-accessors 
                             (field Pre-Field) class )
  ((Mono-Field-class Mono-Field-generate-Handy-accessors)
   (Poly-Field-class Poly-Field-generate-Handy-accessors)
   (Virtual-Field-class Virtual-Field-generate-Handy-accessors) ) )

(define-retrofitted-generic (generate-predicate (o Pre-Class) class-options)
  ((Class-class (lambda (o class-options) `'()))
   (Handy-Class-class Handy-Class-generate-predicate)
   (View-class View-generate-predicate) ) )

;;; No maker for a View.
(define-retrofitted-generic (generate-maker (o Class) class-options)
  ((Class-class (lambda (o class-options) `'()))
   (Handy-Class-class Handy-Class-generate-maker) ) )

;;; No coercer as well
(define-retrofitted-generic (generate-coercer (o Class) class-options)
  ((Class-class (lambda (o class-options) `'()))
   (Handy-Class-class Handy-Class-generate-coercer) ) )

(define-retrofitted-generic (generate-accompanying-functions 
                             (o Pre-Class) class-options )
  ((Class-class Class-generate-accompanying-functions)
   (Handy-Class-class Handy-Class-generate-accompanying-functions)
   (View-class View-generate-accompanying-functions) ) )

;;;=================================================================== Library

;;; Automatically called on new instances and predefined to do nothing.
;;; It is there to be customized on your classes. 

(define-retrofitted-generic (initialize! (o))
  () 
  o )

;;;======================================================================= copy
;;; This generic function returns a copy of any Meroon object. 
;;; This is only a shallow (not recursive) copy.

(define-generic (clone (o)))

(define-method (clone (o Object))
  (initialize! (instance-clone o)) )

;;;========================================================= Personal notes
;;; On pourrait ecrire (define-method (->Class (o Symbol))..) et enrichir
;;; la methode par defaut.

;;; end of libgen.scm 
