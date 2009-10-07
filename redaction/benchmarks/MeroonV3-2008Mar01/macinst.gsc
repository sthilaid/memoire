;;; $Id: macinst.gsc,v 1.3 2005/05/04 02:44:43 lucier Exp lucier $
;;; Copyright (c) 1990-96 by Christian Queinnec. All rights reserved.

;;;                ***********************************************
;;;                    Small, Efficient and Innovative Class System
;;;                                    Meroon V3
;;;                              Christian Queinnec  
;;;                   \'Ecole Polytechnique & INRIA--Rocquencourt
;;;                ************************************************

;;;                 Gambit-C adaptation of macinst.scm

;;; Use a special subtype for Meroon objects. This allows the Object?
;;; predicate to be safe (so the safer-object feature is just a waste
;;; of room that should be avoided). Thanks to Brad Lucier
;;; <lucier@MATH.Purdue.EDU> for these files.

(define-meroon-macro (subtype-meroon) 6)
(define-meroon-macro (starting-offset)  1 )

(define-meroon-macro (meroon-safer-object) #f)

(define-internal-meroon-macro (setup-accessors)
  (if (meroon-safer-object)
      '(begin
	 
	 (define (instance-ref o offset)
	   (declare (inlining-limit 10000) (standard-bindings) (extended-bindings))
	   (let ((real-offset (fx+ (starting-offset) offset)))
	     (if (and (Object? o)
		      (fixnum? real-offset)
		      (fx< 0 real-offset (meroon-length o)))
		 (meroon-ref o real-offset)
		 (report-meroon-error 'Access instance-ref "Either not an object or invalid offset---Shouldn't happen---Please investigate and report this error" o offset))))
	 
	 (define (instance-set! o offset value)
	   (declare (inlining-limit 10000) (standard-bindings) (extended-bindings))
	   (let ((real-offset (fx+ (starting-offset) offset)))
	     (if (and (Object? o)
		      (fixnum? real-offset)
		      (fx< 0 real-offset (meroon-length o)))
		 (meroon-set! o real-offset value)
		 (report-meroon-error 'Access instance-set! "Either not an object or invalid offset---Shouldn't happen---Please investigate and report this error" o offset))))
	 
	 (define (instance-length o)
	   (declare (inlining-limit 10000) (standard-bindings) (extended-bindings))
	   (if (Object? o)
	       (fx- (meroon-length o) (starting-offset))
	       (report-meroon-error 'Access instance-length "Not an object---Shouldn't happen---Please investigate and report this error" o)))

	 (define (object->class-number o)
	   (declare (inlining-limit 10000) (standard-bindings)(extended-bindings))
	   (if (Object? o)
	       (meroon-ref o 0)
	       (report-meroon-error 'Access object->class-number "Not an object---Shouldn't happen---Please investigate and report this error" o)))

	 )
      '(begin

	 (define-meroon-macro (instance-ref o offset)
	   `(let ((o ,o) (offset ,offset))
              (declare (standard-bindings)(not safe))
              (meroon-ref o (fx+ (starting-offset) offset))))

	 (define-meroon-macro (instance-set! o offset value)
	   `(let ((o ,o) (offset ,offset) (value ,value))
              (declare (standard-bindings)(not safe))
	      (meroon-set! o (fx+ (starting-offset) offset) value)))
	 
	 (define-internal-meroon-macro (instance-length o)
	   `(let ((o ,o))
              (declare (standard-bindings)(not safe))
	      (fx- (meroon-length o) (starting-offset))))

	 (define-meroon-macro (object->class-number o)
           `(let ((o ,o))
	      (meroon-ref o 0)))

	 )))

(setup-accessors)


;;; Builds an instance with CN as instantiation link and ARGS as content.
;;; ARGS are values for mono- or poly- fields as well as their size.
;;; Since vectors are initialized with the undefined value, it is
;;; needless to reinitialize them with this undefined value.


(define-meroon-macro (instance cn . args)
  `(let ()
     (declare (extended-bindings) (not safe))
     (##subtype-set! (##vector ,cn ,@args) (subtype-meroon))))

;;; Allocate an empty instance with CN as instantation link. Caution,
;;; the result is not necessarily a well formed instance. It might
;;; need to be skeletized if containing poly-fields.


(define-internal-meroon-macro (allocate-empty-instance cn size)
`(let ((cn ,cn)
       (size ,size))
   (declare  (standard-bindings) (extended-bindings))
   (let ((result (make-meroon (fx+ (starting-offset) size) 
				meroon-uninitialized )))
     (meroon-set! result 0 cn)
     result)))

;;; Allocate an instance with a given CONTENT. Similar to instance,
;;; except that CONTENT is given as a list. 

(define-internal-meroon-macro (allocate-full-instance cn content)
  `(let ((cn ,cn)
	 (content ,content))
     (declare  (standard-bindings) (extended-bindings)(not safe))
     (##subtype-set! (apply ##vector cn content) (subtype-meroon))))


;;; Emit a declaration that inlining here is worthless.

(define-internal-meroon-macro (meroon-declare-not-inlinable)
  '(declare (not inline) (inlining-limit 0)) )

;;; Local Variables:
;;; mode: scheme
;;; End:

;;; end of macinst.gsc
