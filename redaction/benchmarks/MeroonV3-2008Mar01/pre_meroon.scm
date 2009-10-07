;;; $Id: meroon.gsc,v 1.1 2005/02/25 22:20:34 lucier Exp lucier $
;;; Copyright (c) 1990-97 by Christian Queinnec. All rights reserved.

;;;                            *******************************
;;;                            Gambit-C Adaptation for Meroon
;;;                            *******************************

;;; This file contains the necessary adaptation to be interpreted by Gambit-C.
;;; This prologue now works with Gambit-C 2.7.
;;;  (load "meroon.gsc")
;;;  (load "meroon.scm")

;;; This program is distributed in the hope that it will be useful.
;;; Use and copying of this software and preparation of derivative works
;;; based upon this software are permitted, so long as the following
;;; conditions are met:
;;;      o credit to the authors is acknowledged following current
;;;        academic behaviour
;;;      o no fees or compensation are charged for use, copies, or
;;;        access to this software
;;;      o this copyright notice is included intact.
;;; This software is made available AS IS, and no warranty is made about 
;;; the software or its performance. 

;;; Bug descriptions, use reports, comments or suggestions are welcome.
;;; Send them to    
;;;   <queinnec@polytechnique.fr>   or to:   <Christian.Queinnec@inria.fr>
;;;                                 or to:
;;;   Christian Queinnec                     Christian Queinnec
;;;   Laboratoire d'Informatique de l'X      INRIA -- Rocquencourt
;;;   Ecole Polytechnique                    Domaine de Voluceau, BP 105
;;;   91128 Palaiseau                        78153 Le Chesnay Cedex
;;;   France                                 France

;;;************************************************
;;;    Small, Efficient and Innovative Class System
;;;       Christian Queinnec  
;;;   \'Ecole Polytechnique & INRIA-Rocquencourt
;;;************************************************

;;; The first macro is a macro definer that allows these macros to
;;; remain in the compiled image. The second just defines a macro
;;; local to the sources but normally not visible from the user.

(define-macro (define-meroon-macro call . body)
  `(begin (eval '(define-macro ,call . ,body))
          (define-macro ,call . ,body) ) )

(define-macro (define-internal-meroon-macro call . body)
  `(define-macro ,call . ,body) )

;;; Programs of the Meroon repository use define*-meroon-macro and
;;; users' programs may also want to use portable macros.  Make these
;;; macros themselves pervasive. [Thanks to Brad Lucier
;;; <lucier@MATH.Purdue.EDU> for that nice suggestion].

;;; The problem are (1) the current file is not compiled so there is
;;; no associated .o file (2) how to make define-meroon-macro
;;; pervasive without using define-meroon-macro and thus stumbling
;;; into regression problems (3) how to make define-meroon-macro
;;; portably pervasive with a unique code (that must not depend then
;;; on the particular implementation of define-meroon-macro). 
;;; The simplest thing seems to require that each prologue defines
;;; a (make-meroon-macro-pervasive) macro that will be called in a file
;;; to be compiled (ie utils.scm).

(define-meroon-macro (make-meroon-macro-pervasive)
  (list 
   'eval 
   ''(begin 
       (define-macro (define-meroon-macro call . body)
          `(begin (eval '(define-macro ,call . ,body))
                  (define-macro ,call . ,body) ) )
       (define-macro (define-internal-meroon-macro call . body)
          `(define-macro ,call . ,body) )
       #t ) ) )

;;; Conditional compilation: The variable *meroon-features* defines
;;; the features of Meroon to be taken into account when loading (or
;;; compiling Meroon). 

(define *meroon-features* 
  '( gambit C 2.7 DSSSL
     pervasive-meroon-macros
     ;; Used by Brad Lucier to speed up Meroon:
     inline-predicate
     ) )

;;; Local Variables:
;;; mode: scheme
;;; End:

;;; End of adaptation of Meroon for Gambit-C.
;;; $Id: macros.scm,v 1.1 2005/02/25 22:19:37 lucier Exp $
;;; Copyright (c) 1990-97 by Christian Queinnec. All rights reserved.

;;;                ***********************************************
;;;                    Small, Efficient and Innovative Class System
;;;                                    Meroon 
;;;                              Christian Queinnec  
;;;                    Ecole Polytechnique & INRIA--Rocquencourt
;;;                ************************************************

;;; This file contains macros (and only macros) used in the sources of
;;; Meroon. Since prologues define how macros can be defined, new
;;; macros can be shared here. However they are conditionalized since
;;; some of them do already appear in some implementations. The first
;;; macro is therefore the if-meroon-feature conditionalizer.

;;; Conditional compilation: The variable *meroon-features* defines
;;; the features of Meroon to be taken into account when loading (or
;;; compiling Meroon). Possible features are:
;;;    bootstrap:                 when bootstrapping
;;;    scheme version:            to identify the Scheme system
;;; With that macro, we can conditionalize definitions with boolean expressions
;;; like (or elk (and (not bigloo) Scheme->C)).

(define-internal-meroon-macro (if-meroon-feature condition th-en . el-se)
  ((lambda ()
     (define (feature-present? condition)
       (if (pair? condition) 
           (case (car condition)
             ((and) 
              (if (pair? (cdr condition))
                  (if (pair? (cddr condition))
                      (and (feature-present? (cadr condition))
                           (feature-present? (cons 'and (cddr condition))) )
                      (feature-present? (cadr condition)) )
                  #f ) )
             ((or) 
              (if (pair? (cdr condition))
                  (if (pair? (cddr condition))
                      (or (feature-present? (cadr condition))
                          (feature-present? (cons 'or (cddr condition))) )
                      (feature-present? (cadr condition)) )
                  #t ) )
             ((not) (not (feature-present? (cadr condition))))
             (else (member condition *meroon-features*)) )
           (member condition *meroon-features*) ) )
     (if (feature-present? condition) 
         th-en 
         `(begin #t . ,el-se) ) ) ) )
;;; Test: (if-meroon-feature (or) 1 2)

;;; MacScheme 1.5 does not like define forms inside toplevel begin forms
;;; but supports set! to undefined global variables.

(if-meroon-feature (and MacScheme 1.5)
  (define-meroon-macro (meroon-define call . body)
    (if (pair? call) 
        `(meroon-define ,(car call) 
            (lambda ,(cdr call) . ,body) )
        `(set! ,call . ,body) ) )
  (define-meroon-macro (meroon-define call . body)
    `(define ,call . ,body) ) )

;;; This macro defines a function that will be redefined as a generic
;;; function later in the sources so users can customize it.  As
;;; confirmed by vscmV0r4 and M. Blume, (begin (define a 1) a) is not
;;; regular Scheme since this begin form mixes a definition and an
;;; expression.  So changed the expansion of define-temporary to avoid
;;; that.

(if-meroon-feature (or vscm)
  (define-internal-meroon-macro (define-temporary call . body)
    `(meroon-define ,call ,@body) )
  (define-internal-meroon-macro (define-temporary call . body)
    (let ((name (if (symbol? call) call (car call))))
      `(begin (meroon-define ,call ,@body)
              (set! ,name ,name)
              ',name ) ) ) )

;;; Inline accessors for better performance. 

(define-internal-meroon-macro (define-inline call . body)
  (if (pair? call)
      (let ((name (car call))
            (variables (cdr call)) )
        `(define-internal-meroon-macro (,name . arguments)
           (cons (cons 'lambda (cons ',variables ',body))
                 arguments ) ) )
      (let* ((name call)
             (other-name (if (and (pair? body)(null? (cdr body)))
                             (car body)
                             (error 'define-inline
                                    "Incorrect syntax: ~A"
                                    `(define-inline ,call ,@body) ) )) )
        `(define-internal-meroon-macro (,name . arguments)
           (cons ',other-name arguments) ) ) ))

;;; PC-Scheme 3.03 only accepts binary apply and so is MacScheme 1.5 and Fools.
;;; Test that feature with:
;;;    (apply list 1 2 '(3))

(if-meroon-feature (or (and PC-scheme 3.03) (and MacScheme 1.5) fools)
    (define-internal-meroon-macro (oo-apply fn . args)
      (define (consify args)
        (if (pair? (cdr args))
            `(cons ,(car args) ,(consify (cdr args)))
            (car args) ) )
      `(apply ,fn ,(if (pair? args) (consify args) '())) )
    (define-internal-meroon-macro (oo-apply fn . args) 
      `(apply ,fn ,@args) ) )

;;; as well as only binary map.
;;; Test that feature with:
;;;     (map list '(1 2) '(3 4))

(if-meroon-feature (and PC-scheme 3.03)
  ;; map2 is defined in utils.scm
  (define-internal-meroon-macro (oo-map fn . args)
    (case (length args)
      ((1) `(map ,fn ,(car args)))
      ((2) `(map2 ,fn ,(car args) ,(cadr args)))
      (else (oo-error 'oo-map "with more than 2 lists" fn args)) ) )
  (define-internal-meroon-macro (oo-map fn . args)
    `(map ,fn ,@args) ) )

;;; The traditional when and unless. 
;;; Test that feature with:
;;;     (when 1 2 3)

(if-meroon-feature (or bigloo slib gambit (and PC-Scheme 3.03) 
                       MacScheme vscm MIT vanilla guile )
    (define-internal-meroon-macro (when condition . body)
      `(if ,condition (begin ,@body)) )
    #t )

;;; PC-Scheme/Geneva has when but not unless.
;;; Test that feature with:
;;;     (unless 1 2 3)

(if-meroon-feature (or bigloo slib gambit PC-Scheme MacScheme
                       MIT vscm vanilla guile )
    (define-internal-meroon-macro (unless condition . body)
      `(if ,condition #f (begin ,@body)) )
    #t )

;;;ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; For weird compilation reasons all pervasive macros appear here,
;;; they are just mapped over a library function that will do the real
;;; work.

;;; appeared in definers.scm

;;; The macro to define a class. The definition is first syntactically
;;; checked then a class object is created and is asked to parse its
;;; definition in order to generate the appropriate code to load.

(define-meroon-macro (define-class . definition)
  (check-and-process-class-definition definition) )

(define-meroon-macro (define-generic call . body)
  (process-define-generic-form call body) )

(define-meroon-macro (define-method call . body)
  (process-define-method-form call body) )

(define-meroon-macro (define-handy-method call . body)
  (process-define-handy-method call body) )

;;; appeared in clone.scm

(define-meroon-macro (duplicate desc . parms)
  (process-duplicate-form desc parms)  )

(define-meroon-macro (instantiate-from desc . parms)
  (process-instantiation-from-form desc parms)  )

;;; appeared in modify.scm

(define-meroon-macro (modify desc . parms)
  (process-modify-form desc parms)  )

;;; appeared in alloc.scm

(define-internal-meroon-macro (generate-bunch-of-zero-poly-makers max class)
  `(case (length (careless-Class-fields ,class))
     ,@(oo-map (lambda (i) 
                 (define (generate-zero-poly-maker class-variable field-names)
                   `(lambda ,field-names
                      (initialize! (instance (careless-Class-number
                                              ,class-variable)
                                             ,@field-names )) ) )
                 `((,i) ,(generate-zero-poly-maker 
                          class
                          (oo-map (lambda (i) (symbol-concatenate 'a i))
                                  (iota 0 i) ) )) )
               (iota 0 max) )
     (else (lambda args (general-make ,class args))) ) )

(define-internal-meroon-macro (generate-bunch-of-one-final-poly-makers 
                               max class )
  `(case (length (careless-Class-fields ,class))
     ,@(oo-map (lambda (i)
                 (define (generate-one-final-poly-maker 
                          class-variable field-names )
                   (let* ((temp (reverse field-names))
                          (poly (car temp))
                          (regular (reverse! (cdr temp)))
                          (regular-length (length regular))
                          (g (gensym))
                          (l (gensym)) )
                     `(lambda ,(let hook ((items regular))
                                 (if (pair? items)
                                     (cons (car items) (hook (cdr items)))
                                     (cons l poly) ) )
                        (initialize!
                         (let* ((,g (length ,poly))
                                (v (allocate-empty-instance 
                                    (careless-Class-number ,class-variable)
                                    (fx+ ,regular-length 1 ,g) )) )
                           (check-appropriate-size ,g ,l v)
                           ,@(oo-map (lambda (fn i) `(instance-set! v ,i ,fn))
                                     regular
                                     (iota 0 regular-length) )
                           (instance-set! v ,regular-length ,l)
                           (fill-instance! 
                            v ,(fx+ regular-length 1) ,poly ) ) ) ) ) )
                 `((,i) ,(generate-one-final-poly-maker 
                          class
                          (oo-map (lambda (i) (symbol-concatenate 'a i))
                                  (iota 0 i) ) )) )
               (iota 1 max) )
     (else (lambda args (general-make ,class args))) ) )

;;; appeared in fill.scm

(define-meroon-macro (instantiate class-name . parms)
  (process-instantiate-form class-name parms) )

(define-meroon-macro (co-instantiate . definitions)
  (process-co-instantiate-form definitions 'set!) )

(define-meroon-macro (with-co-instantiation definitions . body)
  (process-with-co-instantiation-form definitions body) )

;;; appeared in libgen.scm

;;; Make generic some of the previous functions so they can be
;;; customized on new types of Class or Field.

(define-internal-meroon-macro (define-retrofitted-generic call clauses . body)
  (let* ((name (car call))
         ;; UMB Scheme does not accept symbols starting with a minus sign.
         (other-name (symbol-concatenate 'meroon- name '- 'internal-generic)) )
    `(begin
       (define-generic ,(cons other-name (cdr call)) ,@body)
       (let ((g (symbol->generic ',other-name)))
         (careless-initialize-Generic-name g ',name)
         ,@(oo-map (lambda (class+method)
                     `(add-1-method! g ,@class+method) )
                   clauses ) )
       (set! ,name ,other-name)
       ',name ) ) )

;;; appeared in walker.scm
;;; MacScheme and PC-Scheme are touchy wrt to local variables that
;;; cannot have the name of a macro. So rename instance into
;;; ins-tance.

(define-meroon-macro (with-access ins-tance specs . body)
  (process-with-access-form ins-tance specs body) )

;;; These macros can appear in the expansion of the previous macros so
;;; they must be pervasive (rather than being define-inline as in the
;;; careless.scm file).

(define-meroon-macro (careless-Generic-dispatcher g)
  `(instance-ref ,g 3) )

(define-meroon-macro (careless-Class-number class)
  `(instance-ref ,class 1) )
(define-meroon-macro (careless-Class-allocator class)
  `(instance-ref ,class 7) )

(define-meroon-macro (careless-Dispatcher-method-finder d)
  `(instance-ref ,d 0) )

;;; Define a View.

(define-meroon-macro (define-view . definition)
  (check-and-process-view-definition definition) )

;;; end of macros.scm
;;; $Id: dsssl.scm,v 1.1 2005/02/25 22:19:37 lucier Exp lucier $
;;; Copyright (c) 1990-97 by Christian Queinnec. All rights reserved.

;;;                ***********************************************
;;;                    Small, Efficient and Innovative Class System
;;;                                    Meroon 
;;;                              Christian Queinnec  
;;;                    Ecole Polytechnique & INRIA--Rocquencourt
;;;                ************************************************

;;; These are the functions to recognize DSSSL keywords. This file is
;;; not loaded by Scheme implementations that do not know how to read
;;; these keywords.

(define (dsssl-optional? thing)
  (eq? thing '#!optional) )

(define (dsssl-rest? thing)
  (eq? thing '#!rest) )

(define (dsssl-key? thing)
  (eq? thing '#!key) )

;;; end of dsssl.scm
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
;;; $Id: careless.scm,v 1.1 2005/02/25 22:19:37 lucier Exp $ 
;;; Copyright (c) 1990-97 by Christian Queinnec. All rights reserved.

;;;                ***********************************************
;;;                    Small, Efficient and Innovative Class System
;;;                                    Meroon 
;;;                              Christian Queinnec  
;;;                    Ecole Polytechnique & INRIA--Rocquencourt
;;;                ************************************************

;;; These functions are needed for the bootstrap. CAUTION: this file
;;; must be regenerated if the structure of the primitive classes
;;; change since offsets are hardwired here. These accessors are used
;;; in safe contexts and are necessary for speed (in absence of a
;;; type-recovery analysis in define-method forms).

(define-inline (careless-Class-name class) 
  (instance-ref class 0) )
(define-inline (careless-initialize-Class-name class name) 
  (instance-set! class 0 name) )
(define-inline (careless-initialize-Class-number class cn)
  (instance-set! class 1 cn) )
(define-inline (careless-Class-fields class)
  (instance-ref class 2) )
(define-inline (careless-initialize-Class-fields class fields)
  (instance-set! class 2 fields) )
(define-inline (careless-Class-depth class) 
  (instance-ref class 3) )
(define-inline (careless-initialize-Class-depth class cn)
  (instance-set! class 3 cn) )
(define-inline (careless-Class-super-number class)
  (instance-ref class 4) )
(define-inline (careless-initialize-Class-super-number class scn)
  (instance-set! class 4 scn) )
(define-inline (careless-Class-subclass-numbers class)
  (instance-ref class 5) )
(define-inline (careless-set-Class-subclass-numbers! class lcn)
  (instance-set! class 5 lcn) )
(define-inline (careless-Class-next class)
  (instance-ref class 6) )
(define-inline (careless-set-Class-next! class cn)
  (instance-set! class 6 cn) )
(define-inline (careless-initialize-Class-allocator class a)
  (instance-set! class 7 a) )
(define-inline (careless-initialize-Class-immutable? class bool)
  (instance-set! class 8 bool) )
;;; Object has depth 0 (see genes1.scm)
(define-inline (careless-Class-super-length class)
  (fx+ 1 (careless-Class-depth class)) )
(define-inline (careless-Class-super class index)
  (if (and (fx>= index 0) (fx<= index (careless-Class-depth class)))
      (instance-ref class (fx+ 10 (fx+ 1 index)))
      (report-bad-index 'super class index) ) )
(define-inline (careless-initialize-Class-super class index cn)
  (if (and (fx>= index 0) (fx<= index (careless-Class-depth class)))
      (instance-set! class (fx+ 10
				(fx+ 1 index))
		     cn)
      (report-bad-index 'super class index) ) )
(define-inline (careless-Class-relnum class index)
  (if (and (fx>= index 0) (fx<= index (careless-Class-depth class)))
      (instance-ref class (fx+ 10
			       (fx+ 1
				    (fx+ (careless-Class-depth class)
					 index))))
      (report-bad-index 'super class index) ) )
(define-inline (careless-initialize-Class-relnum class index cn)
  (if (and (fx>= index 0) (fx<= index (careless-Class-depth class)))
      (instance-set! class (fx+ 10
				(fx+ 1
				     (fx+ (careless-Class-depth class) index)))
		     cn)
      (report-bad-index 'super class index) ) )

(define-inline (careless-Generic-name g)
  (instance-ref g 0) )
(define-inline (careless-initialize-Generic-name g name)
  (instance-set! g 0 name) )
(define-inline (careless-Generic-default g)
  (instance-ref g 1) )
(define-inline (careless-initialize-Generic-default g d)
  (instance-set! g 1 d) )
(define-inline (careless-initialize-Generic-dispatcher g d)
  (instance-set! g 3 d) )
(define-inline (careless-set-Generic-dispatcher! g d)
  (instance-set! g 3 d) )
(define-inline (careless-Generic-top-classes g)
  (instance-ref g 4) )
(define-inline (careless-initialize-Generic-top-classes g cs)
  (instance-set! g 4 cs) )

(define-inline (careless-Field-name field)
  (instance-ref field 0) )
(define-inline (careless-initialize-Field-name field name)
  (instance-set! field 0 name) )
(define-inline (careless-Field-immutable? field)
  (instance-ref field 1) )
(define-inline (careless-initialize-Field-immutable? field bool)
  (instance-set! field 1 bool) )
(define-inline (careless-Field-class-number field)
  (instance-ref field 2) )
(define-inline (careless-initialize-Field-class-number field cn)
  (instance-set! field 2 cn) )
(define-inline (careless-Field-initialized? field)
  (instance-ref field 3) )
(define-inline (careless-initialize-Field-initialized? field bool)
  (instance-set! field 3 bool) )
(define-inline (careless-Field-initializer field)
  (instance-ref field 4) )
(define-inline (careless-initialize-Field-initializer field fun)
  (instance-set! field 4 fun) )
(define-inline (careless-Field-path-length field)
  (instance-ref field 5) )
(define-inline (careless-Field-path field index)
  (instance-ref field (fx+ 5 (fx+ 1 index))) )

;(define-inline (careless-Dispatcher-method-finder d)
;  (instance-ref d 0) )
(define-inline (careless-initialize-Dispatcher-method-finder d v)
  (instance-set! d 0 v) )

(define-inline (careless-Immediate-Dispatcher-method d)
  (instance-ref d 1) )

(define-inline (careless-Global-Dispatcher-method-length d)
  (instance-ref d 1) )
(define-inline (careless-Global-Dispatcher-method d i)
  (instance-ref d (fx+ 2 i)) )
(define-inline (careless-set-Global-Dispatcher-method! d i m)
  (instance-set! d (fx+ 2 i) m) )

(define-inline (careless-Indexed-Dispatcher-class-number d)
  (instance-ref d 1) )
(define-inline (careless-set-Indexed-Dispatcher-class-number! d cn)
  (instance-set! d 1 cn) )
(define-inline (careless-Indexed-Dispatcher-class-depth d)
  (instance-ref d 2) )
(define-inline (careless-set-Indexed-Dispatcher-class-depth! d cn)
  (instance-set! d 2 cn) )
(define-inline (careless-Indexed-Dispatcher-no d)
  (instance-ref d 3) )
(define-inline (careless-set-Indexed-Dispatcher-no! d v)
  (instance-set! d 3 v) )
(define-inline (careless-Indexed-Dispatcher-method-length d)
  (instance-ref d 4) )
(define-inline (careless-Indexed-Dispatcher-method d i)
  (instance-ref d (fx+ 5 i)) )
(define-inline (careless-set-Indexed-Dispatcher-method! d i v)
  (instance-set! d (fx+ 5 i) v) )

(define-inline (careless-Subclass-Dispatcher-class-number d)
  (instance-ref d 1) )
(define-inline (careless-set-Subclass-Dispatcher-class-number! d cn)
  (instance-set! d 1 cn) )
(define-inline (careless-Subclass-Dispatcher-class-depth d)
  (instance-ref d 2) )
(define-inline (careless-set-Subclass-Dispatcher-class-depth! d cn)
  (instance-set! d 2 cn) )
(define-inline (careless-Subclass-Dispatcher-no d)
  (instance-ref d 3) )
(define-inline (careless-set-Subclass-Dispatcher-no! d v)
  (instance-set! d 3 v) )
(define-inline (careless-Subclass-Dispatcher-yes d)
  (instance-ref d 4) )
(define-inline (careless-set-Subclass-Dispatcher-yes! d v)
  (instance-set! d 4 v) )

(define-inline (careless-initialize-Linear-Dispatcher-method d m)
  (instance-set! d 2 m) )

;;; end of careless.scm
;;; $Id: utils.scm,v 1.1 2005/02/25 22:19:37 lucier Exp $ 
;;; Copyright (c) 1990-96 by Christian Queinnec. All rights reserved.

;;;                ***********************************************
;;;                    Small, Efficient and Innovative Class System
;;;                                    Meroon 
;;;                              Christian Queinnec  
;;;                    Ecole Polytechnique & INRIA--Rocquencourt
;;;                ************************************************

;;; This file contains some utilities that must be present before
;;; loading other things. It might be the case that some of these functions
;;; already appear in your favorite Scheme implementation, then just
;;; set the appropriate features to eliminate them.

;;; A prologue for OScheme

(if-meroon-feature (or OScheme)
  (begin
    (require "lstutls")
    (set-load-path! (cons "~queinnec/DEA/Meroon-V3" (load-path)))
    ;(define trace-load 
    ;  (let ((orig-load load))
    ;    (lambda (filename)
    ;      (format (current-output-port) "Loading %0\n" filename)
    ;      (orig-load filename))))
    ;(set! load trace-load)   
    (define number->string integer->string)
    (define number? integer?) )
  #t )

;;; Returns true only if the predicate P is true on each term of the
;;; lists in args. Only used with a single list in Meroon

(define every? 
  (let ()
    (meroon-declare-not-inlinable)
    (lambda (p . args)
      (let andmap ((args args) (value #t))
        (if (let any-at-end? ((ls args))
              (and (pair? ls)
                   (or (not (pair? (car ls)))
                       (any-at-end? (cdr ls)))))
            value
            (let ((value (oo-apply p (oo-map car args))))
              (and value (andmap (oo-map cdr args) value)))))) ) )

;;; Returns true only if P is true on at least one term of the lists in args. 
;;; Only used once in the if-meroon-feature macro.

(define (any? p . args)
  (let ormap ((args args) (value #f))
    (if (let any-at-end? ((ls args))
          (and (pair? ls)
               (or (not (pair? (car ls)))
                   (any-at-end? (cdr ls)))))
        value
        (let ((value (apply p (map car args))))
          (or value (ormap (map cdr args) value))))))

;;; NOTE: It is now possible to use and and or specifiers in conditionalized
;;; definitions.

;;; Creates a new symbol. In fact, this is a weak imitation since
;;; these symbols might already exist and are falsifiable as well.
;;; MacScheme 1.5 gensym does not accept an argument.
;;; Test that feature with:
;;;     (gensym)

(if-meroon-feature (or bigloo gambit MacScheme scheme48)
    #t
    (define gensym
      (let ((counter 99))
        (lambda args
          (set! counter (fx+ counter 1))
          (symbol-concatenate (if (pair? args) (car args) 'G)
                              counter ) ) ) ) )

;;; A physical reverse.
;;; Define it here so it can be used by Fools for meroon-integer->string.

(if-meroon-feature (or Scheme->C slib elk gambit MacScheme 
                       scheme48 fools vscm guile )
    (define (reverse! l)
      (define (nreverse l r)
        (if (pair? l)
            (let ((cdrl (cdr l)))
              (set-cdr! l r)
              (nreverse cdrl l) )
            r ) )
      (nreverse l '()) )
    #t )

;;; number->string changed more than once in RnRS.

(if-meroon-feature (and PC-Scheme 3.03)
  ;; case of PC-Scheme 3.03
  (define (meroon-integer->string n)
    (number->string n '(int)) )
  ;; Keep this begin for fools which does not like a macro call to expand
  ;; into a form with the same macro.
  (begin
    ;; Case of Fools
    (if-meroon-feature fools
      (define (meroon-integer->string n)
        (let ((result '()))
          (define *output-base* 10)
          (define *number-images*
            (vector #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 
                    #\A #\B #\C #\D #\E #\F ) )
          (define (prin-integer n)
            (let ((p (fxquotient n *output-base*)))
              (when (fx> p 0) (prin-integer p))
              (set! result (cons (vector-ref *number-images* 
                                             (fxremainder n *output-base*) )
                                 result )) ) )
          (when (fx< n 0) 
            (set! result (cons #\- result))
            (set! n (fx- n)) )
          (prin-integer n)
          (list->string (reverse! result)) ) )
      ;; Normal case:
      (define meroon-integer->string number->string) ) ) )

;;; Forge a new symbol, the name of which is the concatenation of all
;;; these things. Does not seem worth memoizing this function.

(define symbol-concatenate 
  (let ()
    (meroon-declare-not-inlinable)
    (lambda names
      (string->symbol
       (oo-apply string-append
                 (oo-map (lambda (thing)
                           (cond ((symbol? thing) 
                                  (symbol->string thing) )
                                 ((string? thing) thing)
                                 ((number? thing)
                                  (meroon-integer->string thing) )
                                 (else (internal-meroon-error 
                                        "non coercible to String"
                                        thing )) ) )
                         names ) ) ) ) ) )

;;; A well known Common Lisp function

(define (identity x) x)

;;; Returns the list of numbers [start start+1 .... end [

(define (iota start end)
  (if (fx< start end)
      (cons start (iota (fx+ 1 start) end))
      '() ) )

;;; (apply f end '(e11 e12 ... e1n) '(e21 e22 ...) ... '(ep1 ... ep1n)) ->
;;; (f (f ... (f end e1n e2n ... epn) ... ) e11 e21 ... ep1)
;;; Renamed into meroon-reduce since Fools defines it with a binayr form 
;;; with end and list swapped.
;;; reduce already exists in MIT but is different.

(define (meroon-reduce f end list . lists)
  ;; F == (lambda (result item) result)
  (define (reduce1 f end l)
    (if (pair? l)
        (f (reduce1 f end (cdr l)) (car l))
        end ) )
  (define (reduce2 f end l1 l2)
    (if (and (pair? l1) (pair? l2))
        (f (reduce2 f end (cdr l1) (cdr l2)) (car l1) (car l2))
        end ) )
  (define (reduceN f end lists)
    (if (every? pair? lists)
        (oo-apply f (reduceN f end (oo-map cdr lists)) (oo-map car lists))
        end ) )
  (case (length lists)
    ((0) (reduce1 f end list))
    ((1) (reduce2 f end list (car lists)))
    (else (reduceN f end (cons list lists))) ) )

;;; Like append but with surgery.

(if-meroon-feature (or Scheme->C slib elk gambit MacScheme scheme48
                       vscm vanilla )
    (define (append! l1 l2)
      (cond ((pair? l1) 
             (if (pair? (cdr l1))
                 (append! (cdr l1) l2)
                 (set-cdr! l1 l2) )
             l1 )
            (else l2) ) )
    #t )

;;; Flatten a list.

(define (flat l)
  (define (flatten l r)
    (cond ((pair? l)
           (flatten (car l) (flatten (cdr l) r)) )
          ((null? l) r)
          (else (cons l r)) ) )
  (flatten l '()) )

(if-meroon-feature (or elk scheme48 vscm MIT mzscheme vanilla)
    (define (atom? e) 
      (not (pair? e)) )
    #t )

(if-meroon-feature (or elk slib gambit PC-Scheme scheme48 fools vscm
                       MIT vanilla mzscheme guile
                       (and bigloo 2.1) )
    (define (remove item list)
      (if (pair? list)
          (if (equal? item (car list))
              (cdr list)
              (cons (car list) (remove item (cdr list))) )
          list ) )
    #t )

(if-meroon-feature (or slib gambit scheme48 vscm mzscheme vanilla)
    (define (last-pair l)
      (if (pair? l)
          (let scan ((cdrl (cdr l)) (result l))
            (if (pair? cdrl)
                (scan (cdr cdrl) cdrl)
                result ) )
          (oo-error 'last-pair "Not a pair" l) ) )
    #t )

(if-meroon-feature (or Scheme->C slib gambit MacScheme scheme48 
                       MIT vscm vanilla guile )
    (define call/cc call-with-current-continuation)
    #t )

(if-meroon-feature (or (and PC-Scheme 3.03) MacScheme)
    (define (char-numeric? c)
      (and (char>=? c #\0) (char<=? c #\9)) )
    #t )

(if-meroon-feature (and MacScheme 1.5)
  (define modulo remainder)
  #f )

;;; map2 is needed in PC-scheme which only has a unary map.

(if-meroon-feature (and PC-scheme 3.03)
  (define (map2 fn l1 l2)
    (if (and (pair? l1) (pair? l2))
        (cons (fn (car l1) (car l2))
              (map2 fn (cdr l1) (cdr l2)) )
        '() ) )
  #t )

;;; Anomalies are normally caught by Meroon, displayed then given back
;;; to the underlying implementation using the oo-error function.

(if-meroon-feature (or elk bigloo MacScheme scheme48 guile)
    (define (oo-error within msg . culprits)
      (error within msg culprits) )
    #t )
(if-meroon-feature (or Scheme->C slib gambit)
    (define oo-error 
      (let ()
        (meroon-declare-not-inlinable)
        (lambda (within msg . culprits)
          (error 'Meroon msg) ) ) )
    #t )
(if-meroon-feature PC-Scheme
    (define (oo-error within msg . culprits)
      ;; ignore within parameter; error is a macro in PC Scheme
      (error msg culprits) )
    #t )
(if-meroon-feature MIT
    (define (oo-error within msg . culprits)
      (error-procedure within (cons msg culprits) '()) )
    #t )
(if-meroon-feature fools
  (define (oo-error within msg . culprits)
    (error within "~a ~a" msg culprits) )
  #f )
(if-meroon-feature vscm
  (define (oo-error . arguments)
    (error arguments) )
  #f )
(if-meroon-feature (or OScheme)
  (define (oo-error within msg . culprits)
    (error "Meroon error %0 %1 %2" within msg culprits) )
  #t )
(if-meroon-feature (or mzscheme)
  (define (oo-error within msg . culprits)
    (error msg within culprits) )
  #t )
(if-meroon-feature (or vanilla)
  (define (oo-error within msg . culprits)
    (newline)
    (write `(*** an error occurred ***
                 within ,within
                 reason is ,msg
                 culprits are ,@culprits ))
    (newline)
    (car oo-error) )
  #t )

;;; This function should never be called. It is only called in case of
;;; internal errors. It is defined there for the case it is called
;;; before the end of the bootstrap.

(define internal-meroon-error 
  (let ()
    (meroon-declare-not-inlinable)
    (lambda args
      (oo-error 'meroon "Internal error: should never be seen." args) ) ) )

;;; Create a Keyword from a symbol.

(define (make-meroon-keyword symbol)
  (string->meroon-keyword (symbol->string symbol)) )

;;; Convert a string into a keyword:

(if-meroon-feature (or gambit)
  (define string->meroon-keyword string->keyword)
  (if-meroon-feature (or bigloo)
    (define (string->meroon-keyword s)
      (string->keyword (string-append ":" s)) )
    (define (string->meroon-keyword s)
      (string->symbol (string-append ":" s)) ) ) )

;;; Make meroon macros to define macros pervasive themselves.
;;; See justification in the meroon.gsc prologue.

(if-meroon-feature (or pervasive-meroon-macros)
  (make-meroon-macro-pervasive)
  #f )

;;; end of utils.scm
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
;;; $Id: revision.scm,v 1.1 2005/02/25 22:19:37 lucier Exp lucier $ 
;;; Copyright (c) 1990-2000 Christian Queinnec. All rights reserved.

;;;                ***********************************************
;;;                    Small, Efficient and Innovative Class System
;;;                                    Meroon 
;;;                              Christian Queinnec  
;;;                    Ecole Polytechnique & INRIA--Rocquencourt
;;;                ************************************************

;;; This variable nicknames the current distribution. It is set up by 
;;; Imakefile when producing a new release.

(define meroon-distribution-name "Paques2001+1")

;;; A set of strings to identify the current release. Also shows the
;;; interesting features.

(define meroon-version
  (list "Meroon V3"
        meroon-distribution-name
        "$Revision: 1.1 $" ) )

;;; This couple of numbers is used to check compatibility between various
;;; versions of Meroon. Whenever a class, a generic function or a
;;; method previously compiled with some Meroon version is loaded into a
;;; different one, an anomaly will be signalled.
;;; This huge code only extracts the revision number from the string.

(define meroon-revision
  (let* ((rev "$Revision: 1.1 $")
         (length (string-length rev)) )
    (let scan ((i 0)
               (r 0) )
      (if (fx>= i length) r
          (let ((ch (string-ref rev i)))
            (case ch
              ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
               (scan (fx+ 1 i) (fx+ (fx* 10 r)
                                (fx- (char->integer ch)
                                   (char->integer #\0) ) )) )
              ((#\.) (cons r (scan (fx+ i 1) 0)))
              (else (scan (fx+ 1 i) r)) ) ) ) ) ) )

;;; This function is used to check a number against the current
;;; revision number. It appears in the expansion of define-class,
;;; define-generic and define-method macros.

(define (check-revision rev)
  (unless (equal? rev meroon-revision)
    (report-meroon-error
     'Syntax 'define-generic
     "Compiled with incompatible versions of Meroon" 
     rev meroon-revision ) ) )

;;; Display a banner

(define (display-meroon-banner out)
  (display "[ " out)
  (for-each (lambda (x) 
              (display x out)
              (display " " out) )
            meroon-version )
  (display "]" out)
  (newline out) )

(if-meroon-feature (or gambit)
  (display-meroon-banner (current-output-port))
  #f )

;;; end of revision.scm
(declare (safe))
;;; $Id: walker.scm,v 1.1 2005/02/25 22:19:37 lucier Exp lucier $
;;; Copyright (c) 1990-96 by Christian Queinnec. All rights reserved.

;;;                ***********************************************
;;;                    Small, Efficient and Innovative Class System
;;;                                    Meroon 
;;;                              Christian Queinnec  
;;;                    Ecole Polytechnique & INRIA--Rocquencourt
;;;                ************************************************

;;; This file contains a code walker that is used to expand method
;;; definition. This allows for some access optimizations.

;;; The with-access macro allows to access (read or write) fields as
;;; if they were variables. For instance, if Point is a class with
;;; fields named x and y, say (define-class Point Object (x y)) then
;;; inside the body of a method, x will be equivalent to (Point-x
;;; instance) and (set! x e) to (set-Point-x! instance e).  The
;;; with-access macro is inspired from the CLOS with-slots macro.  A
;;; second macro define-handy-method wraps its body into an
;;; appropriate with-access form to ease the access to fields.

;;; The problem is that it is not possible to write a portable
;;; code-walker in Scheme. If the body of a with-access form contains
;;; user-macro calls then this body must be expanded before
;;; with-access code-walks the body but, at that time, this body can
;;; contain implementation-defined special forms that are unknown from
;;; the code-walker and there is no way to know if something is a
;;; special form or not. So we restrict ourselves to the following
;;; policy which is approximate but portable: the body is walked and
;;; the most usual macros/special forms are recognized and
;;; appropriately handled.  If this fails then you will have to add
;;; new methods to the code-walker to cope with the extra macros you
;;; use. 

;;; Syntax: (with-access instance (class-name field-names) . body)
;;; For example: (with-access o (Point y) (set! y (+ y x)))
;;; is expanded into (set-Point-y! o (+ (Point-y o) x)) disregarding
;;; that x is also a field name. In fact, the two accessors
;;; set-Point-y! and Point-y are also converted into direct field
;;; access.  Finer points: the instance is used for its value (the
;;; binding `o' of the example is not used, in particular, if o is
;;; modified this does not affect the body of the with-access form).
;;; The specifications are just names specifying a class and the
;;; fields that are to be used as locations for fields.  The value of
;;; the form is the value of the last form of its body.  The value of
;;; the instance is checked to belong to the right class at the
;;; beginning of the body.

;;; Note that with-access cannot ease the access to an indexed field,
;;; since a reference to such a location needs an index to be precise.

(define (process-with-access-form ins-tance specs body)
  (check-with-access-specifications
   specs (lambda (class fields)
           (generate-with-access #t ins-tance class fields body '()) ) ) )

;;; Check a little the syntax of a specification. The first term must
;;; name a class while the others must name fields in this class.

(define (check-with-access-specifications specs q)
  (if (not (pair? specs))
    (report-meroon-error 'Syntax 'with-access
                         "Incorrect specification" specs ) )
  (let* ((class-name (car specs))
         (field-names (cdr specs))
         (no-such-class (lambda (name)
                          (report-meroon-error
                           'Syntax 'with-access
                           "No such class" name ) ))
         (class (symbol->class class-name no-such-class))
         (no-such-field (lambda (class name)
                          (report-meroon-error
                           'Syntax 'with-access
                           "No such field" name class ) ))
         (fields (oo-map (lambda (name) 
                           (retrieve-named-field class name no-such-field) )
                         field-names )) )
    (q class fields) ) )

;;; This function expands the body of a with-access fields.
;;; The CARE boolean variable tells if a check must be made or not.
;;; The reason is that in a define-method, we already know for sure
;;; that this test will succeed since the method was selected on that
;;; precise criterium so we do not want to duplicate this test.

(define (generate-with-access care ins-tance class fields body env)
  (let ((o (gensym))
        ;; UMB Scheme does not accept symbols starting with a minus sign.
        (class-var (symbol-concatenate (Class-name class) '- 'class)) )
    (define walk* (make-expander* o class fields))
    `(let ((,o ,ins-tance))
       ,@(if care `((check-class ,o ,class-var 'with-access)) `())
       ((lambda () 
          ,@(walk* body env) )) ) ) )

;;; We assume the syntax to be correct and usual (don't use something
;;; like (my-odd-macro . 33) for instance). This function returns an
;;; expander that will take a form and translates references to fields
;;; into direct accesses. The expander does not expand let, letrec and
;;; other things but only seek references to translate. Whenever a
;;; local variable shadows a field with a similar name then a
;;; reference to that name is no more converted. The expander
;;; is simple-minded and does not handle well an internal definition
;;; with a name to convert.

(define (make-expander* ins-tance class fields)
  ;; take a name N and return #f or the eponymous field
  (define (field-name? n)
    (let search ((fields fields))
      (and (pair? fields)
           (if (eq? n (Field-name (car fields)))
               (car fields)
               (search (cdr fields)) ) ) ) )
  (define (generate-read-access n field)
    (let ((path-length (Field-path-length field)))
      (case path-length
        ((1 2) `(instance-ref ,ins-tance 
                              ,(generate-offset ins-tance field 0 0) ))
        (else `(,(symbol-concatenate (Class-name (Field-defining-class field))
                                '- (Field-name field) )
                ,n )) ) ) )
  (define (generate-write-access n ee field)
    (if (Field-mutable? field)
        (let ((path-length (Field-path-length field)))
          (case path-length
            ((1 2) `(instance-set! ,ins-tance 
                                   ,(generate-offset ins-tance field 0 0)
                                   ,ee ))
            (else `(,(symbol-concatenate 
                      'set- (Class-name (Field-defining-class field))
                      '- (Field-name field) '! )
                    ,n ,ee )) ) )
        (report-meroon-error
         'with-access "Immutable field" field ) ) )
  (define (extend r variables) (append (extract-formals variables) r))
  (define (expand-read-symbol e r)
    (if (memq e r)
        e
        (let ((field (field-name? e)))
          (if field (generate-read-access e field) e) ) ) )
  (define (expand* e* r)
    (oo-map (lambda (e) (expand e r))
            e* ) )
  (define (expand e r)
    ;;(display `(expand ,e ,r))(newline) ; DEBUG
    (if (not (pair? e))
        (if (symbol? e) (expand-read-symbol e r) e)
        (case (car e)
          ((quote) e)
          ((set!) (let ((n (cadr e))
                        (ee (expand (caddr e) r)) )
                    (if (memq n r)
                        `(set! ,n ,ee)
                        (let ((field (field-name? n)))
                          (if field
                              (generate-write-access n ee field)
                              `(set! ,n ,ee) ) ) ) ))
          ;; recognize these binding forms so that local variables can
          ;; shadow field-names.
          ((lambda) `(lambda ,(cadr e)
                       ,@(expand* (cddr e) (extend r (cadr e))) ))
          ((let)
           (if (list? (cadr e)) ; should work with () too!
               `(let ,(oo-map (lambda (binding)
                                `(,(car binding) ,(expand (cadr binding) r)) )
                              (cadr e)  )
                  ,@(expand* (cddr e) (extend r (oo-map car (cadr e)))) )
               `(let ,(cadr e)
                  ,(oo-map (lambda (binding)
                             `(,(car binding) ,(expand (cadr binding) r)) )
                           (caddr e)  )
                  ,@(expand* (cdddr e) 
                              (extend r (cons (cadr e) 
                                              (oo-map car (caddr e)) )) ) ) ) )
          ((let*)
           (if (pair? (cadr e))
               (let ((binding (car (cadr e))))
                 `(let ((,(car binding) ,(expand (cadr binding) r)))
                    ,(expand `(let* ,(cdr (cadr e)) ,@(cddr e))
                             (extend r (list (car binding))) ) ) )
               `(begin ,@(expand* (cddr e) r)) ) )
          ((letrec)
           (let* ((names (oo-map car (cadr e)))
                  (r (extend r names)) )
             `(letrec ,(oo-map (lambda (binding)
                                 `(,(car binding) ,(expand (cadr binding) r)) )
                               (cadr e) )
                ,@(expand* (cddr e) r) ) ) )
          ;; This is erroneous since it does not take into account the
          ;; letrec effect of multiple internal define forms.
          ((define)
           (let ((names (if (pair? (cadr e)) (cadr e) (list (cadr e)))))
             `(define ,(cadr e) ,@(expand* (cddr e) (extend r names))) ) )
          ;; Dont forget with-access itself.
          ((with-access)
           `(with-access ,(expand (cadr e) r)
                ,(caddr e)
              ,@(expand* (cdddr e) (extend r (cdr (caddr e)))) ) )
          ;; And don't forget *quote special forms. Bug found by
          ;; Hubert Canon <canon@polytechnique.fr>.
          ((quasiquote) 
           (let ((e (cadr e)))
             (define (quasi-expand e)
               (if (pair? e)
                   (case (car e)
                     ;; OScheme uses unquote-splice instead.
                     ((unquote unquote-splicing)
                      (list (car e) (expand (cadr e) r)) )
                     (else (cons (quasi-expand (car e))
                                 (quasi-expand (cdr e)) )) )
                   e ) )
             (list 'quasiquote (quasi-expand e)) ) )
          ;; begin, when, unless and others are considered to fall there.
          (else (oo-map (lambda (e) (expand e r)) e)) ) ) )
  expand* )

;;; end of walker.scm
(declare (not safe))
;;; $Id: runtime-gsc.scm,v 1.1 2008/03/02 03:12:45 lucier Exp lucier $ 
;;; Copyright (c) 1990-96 by Christian Queinnec. All rights reserved.

;;;                ***********************************************
;;;                    Small, Efficient and Innovative Class System
;;;                                    Meroon 
;;;                              Christian Queinnec  
;;;                    Ecole Polytechnique & INRIA--Rocquencourt
;;;                ************************************************

;;; This file contains most of the runtime functions of Meroon. It
;;; tries not to contain any thing related to syntax or code
;;; generation (see definers.scm) nor code for the construction of the
;;; initial net of classes and generic functions (see genesis.scm).

;;;========================================================== Representation
;;; Instances are represented by regular vectors. The first slot of
;;; the vector contains the number of the class the instance is direct
;;; member of. Indexed slots are represented by a succession of values
;;; prefixed by the number of such values. For example:
;;;     (define-class Point Object (x y))
;;;     (define-class Polygon Point ((* point)(= z)))
;;; Polygon has instances represented by:
;;;     +--------------+
;;;     + class-number +    representing the Polygon class
;;;     +--------------+
;;;     | value of X   | 
;;;     | value of Y   |
;;;     +  # of points + 
;;;     | point[0]     |
;;;     | point[1]     |
;;;                ... 
;;;     | point[n]     |
;;;     | value of Z   |
;;;     +--------------+

(define (Class-super-length c)
  (fx+ 1 (careless-Class-depth c)) )

;;; Tests if the instance O belongs to CLASS. O is assumed to be an
;;; object.  It is important to use direct (careless) accessors to
;;; avoid endless loops.  This predicate must be very efficient since
;;; it is called before any use of any careful accessor. 

;;; A class is a Meroon object with following structure (we no more show
;;; the internal class number of the instance and only show fields). Here
;;; follows the representation of the Polygon class:
;;;   +----------------------+
;;;   | name                 |  (the symbol Polygon)
;;;   | internal number      |
;;;   | depth                |
;;;   | fields               |
;;;   | super-number         |  (the class number of Point)
;;;   | subclass-numbers     |
;;;   | next                 | 
;;;   | allocator            |
;;;   | immutable?           |
;;;   +     5                +
;;;   | Object-class-number  |  (the class number of Object)
;;;   | Point-class-number   |  (the class number of Point)
;;;   | Polygon-class-number |  (the same as the internal number)
;;;   | relnum/Point         |
;;;   | relnum/Polygon       |
;;;   +----------------------+ 

(define-temporary (is-a? o class)
  (cond ((Class? class) (Class-is-a? o class))
        (else           (default-is-a? o class)) ) )

;;; should never be called.
(define default-is-a? internal-meroon-error)

;;; skip all these variants.
(if-meroon-feature FUTURE
    (begin 

;;; The test is now performed in constant time thanks to Luis Mateu
;;; <mateu@margaux.inria.fr>.  For example, an instance is a Polygon
;;; if the Polygon class number appears as third value in the list of
;;; the supers of its proper class. 

(define (Mateu-Class-is-a? o class)
  (and (Object? o)
       (let ((o-class (object->class o)))
         ;; inline this: (careless-subclass? o-class class)
         (let ((cn (careless-Class-number class))
               (depth (careless-Class-super-length class)) )
           (and (fx<= depth (careless-Class-super-length o-class))
                (fx= cn (careless-Class-super o-class (fx- depth 1))) ) ) ) ) )

;;; A variant that uses the fact that half of the time, o has class
;;; for direct class. 

(define (new-Mateu-Class-is-a? o class)
  (and (Object? o)
       (let* ((ocn (object->class-number o))
              (o-class (number->class ocn)) )
         ;; first check direct instance-ship
         (or (eq? class o-class)
             (let ((cn (careless-Class-number class))
                   (depth (careless-Class-super-length class)) )
               (and (fx<= depth (careless-Class-super-length o-class))
                    (fx= cn (careless-Class-super o-class (fx- depth 1)))
                    ) ) ) ) ) )

;;; Another method where one try to minimize cache default. One scan
;;; the supers of the class of the instance in order to find a given
;;; class-number. This is linear but may be fast if classes hierachy
;;; is shallow.

(define (linear-scan-Class-is-a? o class)
  (and (Object? o)
       (let* ((class-cn (careless-Class-number class))
              (ocn (object->class-number o)) )
         (or (fx= class-cn ocn)
             (let* ((o-class (number->class ocn))
                    (i Class-super-offset)
                    (depth (instance-ref o-class i))
                    (limit (fx+ i 1)) )
               (let scan ((index (fx+ i depth)))
                 (or (fx= class-cn (instance-ref o-class index))
                     (if (fx> index limit)
                         (scan (fx- index 1)) ) ) ) ) ) ) ) )

;;; Still another method (that of Meroon V2). It is linear but less
;;; efficient than in Meroon V2.2 since a class only holds now the
;;; super-number instead of the real super-class directly. It is very
;;; good in practice since vector-ref is an expensive primitive that
;;; has to check that its first argument is a vector, the second is an
;;; integer, positive and less than the vector size. All these tests
;;; make a short linear search with eq? a valuable alternative.

(define (v2-Class-is-a? o class)
  (and (Object? o)
       (let up ((c (object->class o)))
         (or (eq? class c)
             (let ((sc (careless-Class-super-number c)))
               (and sc (up (number->class sc))) ) ) ) ) )

;;; To know the weight of the previous is-a?, double its work.
(define (double-v2-Class-is-a? o class)
  (and (Object? o)
       (let up ((c (object->class o)))
         (or (eq? class c)
             (let ((sc (careless-Class-super-number c)))
               (and sc (up (number->class sc))) ) ) )
       (let up ((c (object->class o)))
         (or (eq? class c)
             (let ((sc (careless-Class-super-number c)))
               (and sc (up (number->class sc))) ) ) ) ) )

) #f ) ; end of if-meroon-feature FUTURE

;;; Choose one of the previous. It would be an error to write
;;; something like (define Class-is-a? <some>-is-a?) since then
;;; Class-is-a? would be a variable and each call to it should check
;;; that it is a function of the appropriate arity. This would yield a
;;; tremendous overhead. Fortunately, the real hardwork for generic
;;; functions is done by subclass?.

(define (Class-is-a? o class)
  (and (Object? o)
       (let* ((ocn     (object->class-number o))
              (o-class (number->class ocn)) )
         ;; first check direct instance-ship
         (or (eq? class o-class)
             (let ((cn    (careless-Class-number class))
                   (depth (careless-Class-super-length class)) )
               (and (fx<= depth (careless-Class-super-length o-class))
                    (fx= cn (careless-Class-super o-class (fx- depth 1)))
                    ) ) ) ) ) )

;;; Generic lookup use this variant of is-a?: subclass?. For the same
;;; performance reasons, it has to be very efficient. 
;;; Try other definitions                                       FUTURE

(define (subclass? class1 class2)
  (unless (and (Class? class1) (Class? class2))
    (report-meroon-error 
     'Domain 'subclass? "Must be classes" class1 class2 ) )
  (careless-subclass? class1 class2) )

;;; Luis Mateu's version:

(define (careless-subclass? class1 class2)
  (let ((cn2    (careless-Class-number class2))
        (depth2 (careless-Class-depth class2)) )
    (and (fx>= (careless-Class-depth class1) depth2)
         (fx= cn2 (careless-Class-super class1 depth2)) ) ) )

;;; Test whether a class implements a view.
;;; This is a safe predicate.

(define (implements? class view)
  (unless (Class? class)
    (report-wrong-class class Class-class 'implements) )
  (unless (View? view)
    (report-wrong-class class View-class 'implements) )
  (assq view (Class-views class)) )

;;; Test whether a value is an instance of a view.
;;; This predicate is unsafe since it is only called from is-a?

(define (View-is-a? o view)
  (and (Object? o)
       (implements? (object->class o) view) ) )

;;; Convert a virtual-field into a field. This is an unsafe accessor
;;; that returns #f if the conversion is not possible.

(define (resolve-virtual-field field class)
  (let* ((view (Virtual-Field-view field))
         (map (assq view (Class-views class))) )
    (if map
        (list-ref map (Virtual-Field-index field))
        #f ) ) )

;;;===========================================================================
;;; This vector records all classes as well as views. Any class can be
;;; retrieved by its number (which acts as an index in this
;;; vector). The vector is automatically extended whenever useful.

(define *classes* (make-vector 35 #f))

;;; The number of classes in *classes* (and the next number to be used
;;; as a class number). In fact this number is also equal to
;;; (Class-next Object-class).

(define *class-number* 0)

;;; Return the class of an object (class-of in other OOL). 

(define (object->class o)
  (vector-ref *classes* (object->class-number o)) )

;;; Convert a number into a class object. It is an error if the
;;; number is not a number associated to any class.  This function is
;;; for internal use and might disappear in the future.

(define (number->class i)
  (vector-ref *classes* i) )

;;; Return the current number associated to a former class-number.

(define (current-class-number cn)
  (careless-Class-number (number->class cn)) )

;;; Return a new number to be used as a class number, extends resources
;;; that depends on the number of classes if necessary.

(define (get-new-class-number)
  (let ((i *class-number*))
    (set! *class-number* (fx+ 1 *class-number*))
    ;; and extend global resources if necessary
    (when (fx>= *class-number* (vector-length *classes*))
      (extend-classes-number!) )
    i ) )

;;; Return the class named NAME or apply DEFAULT on this name.  This
;;; is an expensive function since it scans the vector of classes.
;;; Should be turned into a hash-table with class-names as keys.

(define (symbol->class name . default)
  (let scan ((i (fx- *class-number* 1)))
    (let ((class (vector-ref *classes* i)))
      (if (eq? (careless-Class-name class) name)
          class
          (if (fx> i 0)
              (scan (fx- i 1))
              (if (pair? default)
                  ((car default) name)
                  (report-meroon-error 
                   'Anomaly 'symbol->class 
                   "No such class" name ) ) ) ) ) ) )

;;; This vector records all existing generic functions.  This is not
;;; good for the GC, but is portable since there is no need to open
;;; the guts of the representation of a generic function to find the
;;; dispatch table. A generic function is represented by a regular
;;; Scheme function AND an instance of class Generic kept within this
;;; vector. This generic object is accessed by its name which must
;;; also be the name of the generic function (for macroexpansion reason).
;;; This vector should be a weak vector in a native Meroon.

(define *generics* (make-vector 20 #f))

;;; The number of generic functions in *generics* and the next number to
;;; be used (see next function instead).

(define *generic-number* 0)

;;; Return a new number to be used as a generic number, extends
;;; resource that depends on the number of generic functions if
;;; necessary.

(define (get-new-generic-number)
  (let ((i *generic-number*))
    (when (fx>= *generic-number* (vector-length *generics*))
      (set! *generics* (vector-extend *generics*)) )
    (set! *generic-number* (fx+ 1 *generic-number*)) 
    i ) )

;;; Return the generic function named NAME or apply DEFAULT on this name.
;;; This is an expensive function since it uses sequence-find.

(define (symbol->generic name . default)
  (sequence-find 
   name *generics* 
   Generic-name
   identity
   (if (pair? default)
       (car default)
       (lambda (name) 
         (report-meroon-error 
          'Anomaly 'symbol->generic "No such generic" name ) ) ) ) )

;;; All errors seen by meroon lead to an anomaly object stored in this
;;; variable. This allows to inspect it later.

(define *last-meroon-anomaly* #f)

;;;========================================================== General utilities
;;; Other utilities appear in appropriate prologues, they contain
;;; alternative definitions for implemention-dependent features
;;; and mainly how to define macros.

;;; Return a new bigger vector. If the second argument is present then
;;; the returned vector is at least that long. The initial content of
;;; the vector is copied into the new one.

(define (vector-extend s . at-least)
  (let* ((n (vector-length s))
         (r (make-vector (if (pair? at-least)
                             (fxmax (car at-least) n)
                             (fx+ n (fx+ 1 (fxquotient n 2))) )
                         #f )) )
    (copy-vector-slice! s r 0 n)
    r ) )

;;; Copy vector old[start..end[ into vector new[start..end[

(define (copy-vector-slice! old new start end)
  (let copy ((i start))
    (when (fx< i end)
      (vector-set! new i (vector-ref old i))
      (copy (fx+ i 1)) ) ) )

;;; Map a function FN on a sequence V (a vector or a list).
;;; (sequence-map f #(v1 v2 .. vN)) -> (begin (f v1) (f v2) .. (f vN))

(define (sequence-map fn v)
  (define (vector-map fn v)
    (let ((n (vector-length v)))
      (define (mapvector i)
        (when (fx< i n)
          (fn (vector-ref v i))
          (mapvector (fx+ 1 i)) ) )
      (mapvector 0) ) )
  (cond ((null? v) #f)
        ((vector? v) (vector-map fn v))
        ((pair? v) (for-each fn v))
        (else (oo-error 'sequence-map "Not a sequence" v)) ) )

;;; Look for an object located in a sequence which, when applied key on it,
;;; returns name. If such object does not exist apply default on name.
;;; This function mimics Common Lisp find function on sequence which 
;;; can be lists or vectors.

(define (sequence-find name s key success fail)
  (cond ((null? s) (fail name))
        ((vector? s)
         (let look ((i 0))
           (if (and (fx< i (vector-length s))
                    (vector-ref s i) )
               (if (eq? name (key (vector-ref s i)))
                   (success (vector-ref s i))
                   (look (fx+ i 1)) )
               (fail name) ) ) )
        ((pair? s)
         (let look ((s s))
           (if (pair? s)
               (if (eq? name (key (car s)))
                   (success (car s))
                   (look (cdr s)) )
               (fail name) ) ) )
        (else (oo-error 'sequence-find "Not a sequence" s)) ) )

;;;=============================================================== 
;;; Spread the content of the list CONTENT inside the instance
;;; starting at position INDEX.

(define (fill-instance! ins-tance index content)
  (if (pair? content)
      (begin (instance-set! ins-tance index (car content))
             (fill-instance! ins-tance (fx+ 1 index) (cdr content)) )
      ins-tance ) )

;;;=============================================================== Instance
;;; Instances are represented by vectors. Slots are initially filled
;;; with the `meroon-uninitialized' value. It is therefore possible to test
;;; if a slot has been initialized or not. Meroon objects are all
;;; allocated with the low-level allocate-instance. The initial filling
;;; of all slots can be specified to be different from uninitialized.

(define meroon-uninitialized (list '???))

(define (uninitialized? value)
  (eq? meroon-uninitialized value) )

;;;=============================================================== Define-class
;;; These functions are used when creating classes.

;;; Check at load-time classes compatibility. Check that they have the
;;; same number of fields with the same name and the same class.
;;; DESCRIPTION is a list of couples (field-name . field-class-name)
;;; which is synthetized at macro-expansion time and checked against
;;; at load-time. 

(define (check-fields class super-name description)
  (unless (eq? super-name (Class-name (Class-super-class class)))
    (report-meroon-error
     'Syntax 'define-class "Incompatible super-class" super-name ) )
  (unless (fx= (length description) (length (Class-fields class)))
    (report-meroon-error
     'Syntax 'define-class 
     "Incompatible number of fields" (Class-fields class) ) )
  (unless (every?
           (lambda (f desc)
             (and (eq? (Field-name f) (car desc))
                  (eq? (Class-name (object->class f)) (cdr desc)) ) )
           (Class-fields class) description )
    (report-meroon-error
     'Syntax 'define-class
     "Incompatible :prototype !?" (Class-name class) ) )
  #t )

;;; Return the list of the numbers associated to a class and its
;;; super-classes from the class itself to Object.

(define (collect-super-class-numbers class)
  (let collect ((cn (careless-Class-number class)))
    (cons cn (let ((super (careless-Class-super-number (number->class cn))))
               (if super (collect super) '()) )) ) )

;;; Find the first common superclass of two classes. Since any class
;;; inherits from Object, then the answer is always different from #f
;;; and at least Object.

(define (common-super-class-number c1 c2)
  (let deeper ((i (fxmin (careless-Class-super-length c1) 
                         (careless-Class-super-length c2) )))
    (if (fx= (careless-Class-super c1 i) (careless-Class-super c2 i))
        (careless-Class-super c1 i)
        (and (fx> i 0) (deeper (fx- i 1))) ) ) )
  
;;; To ease loading definitions of class, this variable contains the
;;; last class defined at expansion-time. At load-time, we check this
;;; variable to see if the class is already defined. This allows to
;;; speedup class creation under the interpreter.

(define *last-defined-class* #f)

;;; This function is used by the expansion of define-class to create a
;;; class. It also checks the current revision. The only classes that
;;; are defined while botstrapping are the predefined classes which
;;; are hand-built so do not register them twice. 
;;; CAUTION: The use of if-meroon-feature makes the bootstrap version
;;; unable to do anything other than bootstrapping. 

(define (register-class rev class name superclass own-fields)
  (if-meroon-feature bootstrap
      (symbol->class (careless-Class-name class)) 
      (begin
        (check-revision rev)
        ;; Fill immediately name and super-number since some
        ;; parameters are already at hand.
        (unless (field-defined? class 'name)
          (careless-initialize-Class-name class name) )
        (unless (field-defined? class 'super-number)
          (careless-initialize-Class-super-number
           class (Class-number superclass) ) )
        (Class-add-subclass class superclass own-fields) ) ) )

;;; The run-time function that defines a new class. CLASS is the
;;; new class object that will be inserted in the tree of
;;; classes, it might reuse some fields of an older homonymic
;;; class if that exists but do not reuse the old class object since
;;; it might be of a wrong metaclass. Note that accessors close
;;; over class objects not over the <class>-class global variable.

(define (Class-add-subclass class super-class own-fields)
  (let* ((old-class (symbol->class (careless-Class-name class) 
                                   (lambda (name) #f) ))
         (already-there? old-class) )
    (if already-there? 
        (let ((old-super-class (Class-super-class old-class)))
          ;; unlink the class from its old super-class
          (set-Class-subclass-numbers! 
           old-super-class
           (remove (careless-Class-number old-class)
                   (careless-Class-subclass-numbers old-super-class) ) ) ) )
    ;; Plain initialization. Use initialize-*! to fill immutable fields.
    (let ((strict-supers (collect-super-class-numbers 
                          super-class )))
      (careless-initialize-Class-super-number
       class (careless-Class-number super-class))
      (careless-initialize-Class-depth 
       class (fx+ 1 (careless-Class-depth super-class)) )
      ;; keep the old subclasses even if meaningless. Generally the
      ;; class will regain the same subclasses. This is highly questionable.
      (careless-set-Class-subclass-numbers!
       class (if already-there?
                 (careless-Class-subclass-numbers old-class) ; HACK !
                 (list) ) )
      ;; set the full list of field descriptors, field descriptors of
      ;; the superclass are cloned (so they can be updated (on the
      ;; initializer field for instance) without problem)
      (unless (field-defined? class 'fields)
        (careless-initialize-Class-fields
         class (append (map instance-clone (careless-Class-fields super-class))
                       own-fields ) ) )
      ;; set up the immutability of the class
      (unless (field-defined? class 'immutable?)
        (careless-initialize-Class-immutable?
         class (every? Field-immutable? (careless-Class-fields class)) ) )
      ;; set various numbers. Reuse the old class-number.
      (let* ((cn (if already-there? (careless-Class-number old-class) 
                     (let ((cn (get-new-class-number)))
                       (careless-set-Class-next! Object-class cn)
                       cn ) ))
             ;; collect all the super-numbers (in correct order)
             (supers (reverse! (cons cn strict-supers))) )
        (careless-initialize-Class-number class cn)
        ;; install the class in the vector of all classes
        (vector-set! *classes* cn class)
        ;; Record all its supers.
        (let fill ((i 0)(supers supers))
          (when (pair? supers)
            (careless-initialize-Class-super class i (car supers))
            (fill (fx+ 1 i) (cdr supers)) ) )
        ;; set relative numbers wrt to oneself.
        (careless-set-Class-next!
         class (if already-there? (careless-Class-next old-class) 0) )
        ;; relnum wrt Object is already set up.  Applied on itself,
        ;; will increment the previous 0 just above in the 'next' field.
        (let fill ((i 1)(supers (cdr supers)))
          (when (pair? supers)
            (careless-initialize-Class-relnum
             class i 
             (let ((cls (number->class (car supers))))
               (if already-there? 
                   (careless-Class-relnum old-class i)
                   (let ((nxt (careless-Class-next cls)))
                     (careless-set-Class-next! cls (fx+ nxt 1))
                     nxt ) ) ) )
            (fill (fx+ 1 i) (cdr supers)) ) ) )
      ;; set up fields to refer to the class that introduced them
      (for-each (lambda (field)
                  (unless (field-defined? field 'class-number)
                    (careless-initialize-Field-class-number
                     field (careless-Class-number class)) ) )
                (careless-Class-fields class) )
      ;; Since fields and class number are there, then compute the allocator
      (unless (field-defined? class 'allocator)
        (careless-initialize-Class-allocator
         class (create-allocator class) ) )
      ;; link the class within the subclasses of its super
      (careless-set-Class-subclass-numbers!
       super-class
       (cons (careless-Class-number class) 
             (careless-Class-subclass-numbers super-class) ) )
      ;; Keep the current methods if the class was already existing.
      ;; THIS IS VERY SLOW since all generic functions are inspected.
      (unless already-there? 
        (propagate-super-methods! class) )
      ;; as all created objects, call initialize! on it
      (initialize! class) ) ) )

(define-temporary add-subclass Class-add-subclass)

;;; This will be redefined in fill.scm
(define-temporary (fill-other-fields! o) o)

;;; For all generic functions, propagate the method of the super of
;;; CLASS to CLASS. This is a very slow process.

(define (propagate-super-methods! class)
  (sequence-map (lambda (g) 
                  (when g (Generic-update! g class)) )
                *generics* ) )

;;;================================================================== Allocator
;;; Create the allocator of a class. Only two cases are recognized, if
;;; all fields are Mono-Fields then all instances will have a similar
;;; statically known size. Otherwise the instance to be allocated has
;;; a size which depends on runtime values.

(define (create-allocator class)
  (let* ((fields (careless-Class-fields class))
         (last-field (if (pair? fields) (car (last-pair fields)) #f))
         (n (count-Poly-Fields fields)) )
    (case n
      ((0) 
       (let ((size (if last-field (fx+ 1 (careless-Field-path last-field 0)) 0)))
         (lambda ()
           (allocate-empty-instance (careless-Class-number class) size) ) ) )
      ((1) 
       (let ((size1 (Field-path last-field 0))
             (size2 (if (fx= 1 (Field-path-length last-field))
                        0 (fx+ 1 (Field-path last-field 1)) )) )
         (lambda (size)
           (check-size size class)
           (let ((v (allocate-empty-instance (careless-Class-number class)
                                             (fx+ size1 (fx+ 1 (fx+ size size2) )))))
             (instance-set! v size1 size)
             v ) ) ) )
      (else 
       (lambda sizes
         (let ((v
                (allocate-empty-instance
                 (careless-Class-number class)
                 ;; determine the total size of the instance to allocate
                 (let count ((i 0)(sizes sizes))
                   (if (fx< i n)
                       (if (pair? sizes)
                           (begin
                             (check-size (car sizes) class)
                             (fx+ (Field-path last-field i) (fx+ 1 (fx+ (car sizes)
                                (count (fx+ i 1) (cdr sizes))))) )
                           (report-meroon-error 
                            'Allocation 'Meroon-allocator 
                            "Missing sizes" class '() ) )
                       (if (null? sizes) 
                           (if (fx= n (Field-path-length last-field))
                               0 (fx+ 1 (Field-path last-field i)) )
                           (report-meroon-error 
                            'Allocation 'Meroon-allocator
                            "Too much sizes" sizes ) ) ) ) ) ))
           ;; Structure the bare instance with the correct sizes
           (let skeletize ((i 0)(sizes sizes)(offset 0))
             (when (fx< i n)
               (let ((offset (fx+ offset (Field-path last-field i))))
                 (instance-set! v offset (car sizes))
                 (skeletize (fx+ i 1) (cdr sizes) (fx+ offset (fx+ 1 (car sizes)))) ) ) )
           v ) ) ) ) ) )

;;;========================================================= predicate
;;; This code is similar to check-class.

(if-meroon-feature inline-predicate
  (define-internal-meroon-macro (create-predicate class)
    `(lambda (o)
       (declare (standard-bindings)(extended-bindings)(not safe))
       (and (##meroon? o)
            (careless-subclass? (vector-ref *classes* (object->class-number o)) ,class) ) ) )
  (define (create-predicate class)
    (lambda (o)
      (and (Object? o)
           (careless-subclass? (object->class o) class) ) ) ) 
)
                    
;;;========================================================= initialize!
;;; The following function is temporarily defined and 
;;; is used only during the bootstrap. It will be turned 
;;; into a generic function that the user can customize.

(define-temporary (initialize! o) o)

;;;========================================================= Checking
;;; These functions check some aspect of data. They mainly appear in
;;; generated code to reduce (by factorization) their volume. 

;;; Commplain if a given value SZ is not an acceptable size for an
;;; object to be allocated.

(define (check-size sz hint)
  (unless (and (fixnum? sz) (fx>= sz 0))
    (report-meroon-error 
     'Domain 'allocate "Incorrect size" sz hint ) ) )

;;; Complain if the numbers of given values is equal to the number
;;; that prefix them in an explicit make-<class> form.

(define (check-appropriate-size size1 size2 hint)
  (check-size size2 hint)
  (unless (fx= size1 size2)
    (report-meroon-error
     'Syntax 'make "Wrong number of values in Poly-Field" hint )) )

;;; Complain if a value is not a member of a given class when trying
;;; to access one of its field. The HINT argument only serves when
;;; there is an error to precise a little the nature of the error.
;;; CAUTION: this predicate is difficult: the instance can have an old
;;; instantiation link and the class may be superseded. 

(define (check-class o class hint)
  (if (Object? o)
      (let ((oc (object->class o)))
        (unless (careless-subclass? oc class)
          (report-wrong-class o class hint) ) )
      (report-wrong-class o class hint)) )

(define (report-wrong-class o class hint)
  (report-meroon-error 
   'Domain hint "Inappropriate class" class o ) )

;;; Complain if a value I is a legal index for the Poly-Field at OFFSET in O.

(define (check-index i o offset hint)
  (if (fixnum? i)
      (if (and (fx>= i 0) (fx< i (instance-ref o offset)))
          #t
          (report-bad-index hint o i) )
      (report-meroon-error 
       'Domain hint "Not a legal index" i o offset ) ) )

;;;=================================================== Compare signatures

;;; Two signatures (class11 class12 ...) and (class21 class22 ...)
;;; clash if one can find i and j such that class1i > class2i and
;;; class1j < class2j provided that for all other k, class1k and
;;; class2k are related ie are =, < or >. For a generic function that
;;; discriminates on three variables, these signatures are not
;;; clashing (provided A and B have Object as super):
;;;    Point*ColoredPoint*A  and    ColoredPoint*Point*B.
;;; The function compare-signatures returns "unrelated" for them.

(define (combine s1 s2)
  ;; s2 can only be =, <, > or unrelated
  ;; but s1 (the previous status) can be  =, <, >, unrelated or clash.
  (case  s2 
    ((=)                    s1)
    ((<) (case s1 
           ((= <)           '<)
           ((> clash)       'clash)
           ((unrelated)     'unrelated)
           (else            unknown-combination) ))
    ((>) (case s1
           ((= >)           '>)
           ((< clash)       'clash)
           ((unrelated)     'unrelated)
           (else            unknown-combination) ))
    ((unrelated)            'unrelated)
    (else                   unknown-combination) ) )

(define unknown-combination (list "?<!>?"))
                   
;;; This function compares two signatures ie two lists of class-numbers.
;;; Due to the integration with Scheme there is a problem since Object
;;; is not the class of any Scheme values. The type of a Scheme value
;;; which is not a Meroon object is coded as #f. 

;;; This function returns 
;;;    =          if the two signatures are the same
;;;    <          if all classes of the first are subclasses of the second
;;;    >          if all classes of the first are superclasses of the second
;;;    unrelated  if they are not comparable ie there exists indexes i and j
;;;               that are different and sig1[i] > sig2[i] and 
;;;               sig1[j] < sig2[j]. 

(define (compare-signatures sig1 sig2)
  (define (comp sig1 sig2 status)
    ;; (assume (= (length sig1) (length sig2)))
    (if (pair? sig1)
        (comp (cdr sig1) 
              (cdr sig2) 
              (combine status 
                       (cond 
                        ((eq? (car sig1) (car sig2))       '=)
                        ;; (car sig1)=#f means any Scheme value
                        ((not (car sig1))                  '>)
                        ((not (car sig2))                  '<)
                        ((careless-subclass? (car sig1) (car sig2)) '<)
                        ((careless-subclass? (car sig2) (car sig1)) '>)
                        (else                              'unrelated) ) ) )
        status ) )
  ;(when (symbol->generic 'show (lambda (name) #f))
  ;  (show `(compare-signatures sig1= ,sig1 sig2= ,sig2))(newline) ) ; DEBUG
  (comp sig1 sig2 '=) )

;;; end of runtime.scm
;;; $Id: option.scm,v 1.1 2005/02/25 22:19:37 lucier Exp $
;;; Copyright (c) 1990-97 by Christian Queinnec. All rights reserved.

;;;                ***********************************************
;;;                    Small, Efficient and Innovative Class System
;;;                                    Meroon 
;;;                              Christian Queinnec  
;;;                    Ecole Polytechnique & INRIA--Rocquencourt
;;;                ************************************************

;;; This file contains functions that deal with options for class or
;;; field definitions and for instantiations. Options are prefixed by
;;; a keyword (a symbol with a starting colon) and followed by a
;;; number of expressions (zero, one or many).

;;; Keywords (ie symbols beginning with a colon) can appear in Meroon
;;; special forms. Unlike Common Lisp there might be zero or more than
;;; one value associated to a keyword. So the following is a legal list
;;; of options:               :opt1 123 :opt2 :opt3 a b c
;;; Brad Lucier wanted Meroon to use Gambit keywords (ie foo: instead
;;; of :foo). The solution is to support the two kinds since some 
;;; pseudo-keywords appear here and there within Meroon source files.

;;; The function string->meroon-keyword is needed to expand previous files
;;; so it appears in utils.scm.

(if-meroon-feature (and guile (or ii iii))
  ;; Guile has a special <keyword> type 
  ;; Noticed by boyd@crl.com (Bonnie L. Boyd)
  ;; This has changed with Guile 1.2.
  (define meroon-keyword? keyword?)
  (if-meroon-feature (or DSSSL)
    ;; DSSSL also defines keywords.
    (define (meroon-keyword? e)
      (or (keyword? e)
          ;; Keep these keywords since some of them appear in exodus.scm,
          ;; fill.scm and other files.
          (and (symbol? e)
               (char=? (string-ref (symbol->string e) 0) #\:) ) ) )
    (define (meroon-keyword? e)
      (and (symbol? e)
           (char=? (string-ref (symbol->string e) 0) #\:) ) ) ) )

;;; Check if a keyword has a given name. The name can be a symbol or
;;; list of symbols and does not contain the initial colon character.
;;; Pay attention not to create temporary strings or symbols.

(define (meroon-keyword-eq? thing symbols)
  ;; (assume (meroon-keyword? thing))
  (define starting-index 1)
  (define limit #f)
  (if-meroon-feature (or DSSSL)
    (if (keyword? thing)
        (begin
          (set! thing (keyword->string thing))
          (if-meroon-feature (and bigloo 2.1)
            ;; Bigloo 2.1a converts :foo into ":foo"
            (set! starting-index 1)
            (set! starting-index 0) ) ) )
    ;; This is not a DSSSL keyword.
    #f )
  ;; This may be an old Guile keyword
  (if-meroon-feature (and guile (or ii iii))
    (begin
      (set! thing (keyword->symbol thing))
      (if-meroon-feature (and guile ii)
        ;; Guile-ii converts :foo into -foo
        (set! starting-index 1)
        ;; while guile-iii converts it into foo.
        (set! starting-index 0) ) )
    #f )
  ;; Now the real comparison:
  (let* ((thing (if (string? thing) thing (symbol->string thing)))
         (limit (or limit (string-length thing)))
         (symbols (cond ((pair? symbols) symbols)
                        ((symbol? symbols) (list symbols))
                        (else (report-meroon-error 
                               'Domain 'meroon-keyword-eq?
                               "Not a keyword specification" symbols )) )) )
    (let compare ((i starting-index)(symbols symbols))
      (if (fx< i limit)
          (and (pair? symbols)
               (let* ((sym (symbol->string (car symbols)))
                      (len (string-length sym)) )
                 (let comp ((i i)(j 0))
                   (if (fx>= j len)
                       (compare i (cdr symbols))
                       (and (fx< i limit)
                            (char=? (string-ref thing i) 
                                    (string-ref sym j) )
                            (comp (fx+ 1 i) (fx+ 1 j)) ) ) ) ) )
          (null? symbols) ) ) ) )
;;; Tests: 
;;; #t = (meroon-keyword-eq? ':foo 'foo)
;;; #f = (meroon-keyword-eq? ':foo 'ffoo)
;;; #t = (meroon-keyword-eq? ':foo '(f oo))
;;; #f = (meroon-keyword-eq? ':foo '(fo oo))
;;; #t = (meroon-keyword-eq? ':foo '(f o o))
;;; #f = (meroon-keyword-eq? ':foo '(ff o o))
;;; #f = (meroon-keyword-eq? ':foo 'fooo)
;;; #f = (meroon-keyword-eq? ':foo '(fooo ooo))
;;; #f = (meroon-keyword-eq? ':foo 'f)
;;; #f = (meroon-keyword-eq? ':foo '(f o))

;;; This is the internal function used to scan list of options.  It
;;; returns the list starting with the searched keyword followed by
;;; the associated values. This is so since there are still some
;;; Scheme implementations that have problems with #f and ().

(define (find-option-plus-values keyword options)
  (define (get-values options)
    (if (or (null? options)
            (and (pair? options)(meroon-keyword? (car options))) )
        '()
        (cons (car options) (get-values (cdr options))) ) )
  (let ((options (look4keyword keyword (skip2next-keyword options))))
    (if (pair? options)
        (cons (car options) (get-values (cdr options)))
        '() ) ) )

;;; Look for the values that are prefixed by a KEYWORD in a list of
;;; OPTIONS, if no such options can be found invoke DEFAULT on the
;;; keyword.

(define (find-option-values keyword options default)
  (let ((kw+values (find-option-plus-values keyword options)))
    (if (pair? kw+values)
        (cdr kw+values)
        (default keyword) ) ) )

;;; Look for exacty one value associated with a KEYWORD in a list of
;;; OPTIONS otherwise invoke default on the keyword. It is syntactical
;;; error to not associate the keyword to a single value.

(define (find-option-single-value keyword options default)
  (let ((kw+values (find-option-plus-values keyword options)))
    (if (pair? kw+values)
        (let ((values (cdr kw+values)))
          (if (and (pair? values) (null? (cdr values)))
              (car values)
              (report-meroon-error 
               'Syntax 'find-option
               "This option expects a single value"
               keyword values ) ) )
        (default keyword) ) ) )

;;; Looks for the presence of a keyword in a list of options. The keyword
;;; should not be associated to any values.

(define (find-option-present? keyword options)
  (let ((kw+values (find-option-plus-values keyword options)))
    (if (pair? kw+values)
        (if (null? (cdr kw+values))
            #t
            (report-meroon-error
             'Syntax 'find-option
             "this option should not have associated values" 
             keyword (cdr kw+values) ) )
        #f ) ) )
;;; Tests:
;;; #f = (find-option-present? 'a '(2 3 :c :d 4 :e))
;;; #t = (find-option-present? 'a '(2 3 :c :d 4 :a))
;;; #t = (find-option-present? 'a '(2 3 :c :a :e))

;;; Check that an option occurs at most once.

(define (check-option-present-once? keyword options)
  (let ((options (look4keyword keyword (skip2next-keyword options))))
    (if (pair? options)
        (null? (look4keyword keyword (skip2next-keyword (cdr options))))
        #t ) ) )

;;; remove an option and its associated values from a list of options.
;;; Only remove the first occurrence.

(define (remove-option keyword options)
  (let skip ((options options))
    (if (pair? options)
        (if (and (meroon-keyword? (car options))
                 (meroon-keyword-eq? (car options) keyword) )
            (skip2next-keyword (cdr options))
            (cons (car options) (skip (cdr options))) )
        '() ) ) )

;;; chop the list of options until empty or beginning with a keyword.

(define (skip2next-keyword options)
  (if (or (null? options)
          (and (pair? options)
               (meroon-keyword? (car options)) ) )
      options
      (skip2next-keyword (cdr options)) ) )

;;; chop the list of options until empty or beginning y keyword.

(define (look4keyword keyword options)
  ;; (assume (meroon-keyword? (car options)))
  (if (pair? options)
      (if (meroon-keyword-eq? (car options) keyword)
          options
          (look4keyword keyword (skip2next-keyword (cdr options))) )
      '() ) )

;;; This function may be used as a default when an option is not found.
;;; For example to distinguish a Field initialized with an explicit #f.

(define meroon-option-not-there
  (list 'meroon-option-not-there) )

(define (option-not-there kw) 
  meroon-option-not-there )

(define (absent-option? e)
  (eq? e meroon-option-not-there) )
  
;;; end of option.scm
;;; $Id: access.scm,v 1.1 2005/02/25 22:19:37 lucier Exp $
;;; Copyright (c) 1990-97 by Christian Queinnec. All rights reserved.

;;;                ***********************************************
;;;                    Small, Efficient and Innovative Class System
;;;                                    Meroon 
;;;                              Christian Queinnec  
;;;                    Ecole Polytechnique & INRIA--Rocquencourt
;;;                ************************************************

;;; This file defines the general generic functions to access fields
;;; of instances. These are: field-value, field-defined?,
;;; field-length, set-field-value! and initialize-field-value!. They
;;; all take an instance and a field-descriptor, additionally a value
;;; and an offset.

;;;=============================================================== Field access
;;; An instance of Field contains a path that designates, inside the
;;; instance, the offsets that are important in order to compute the total
;;; offset towards a specific field. Consider for instance
;;;     (define-class Point Object (x y))
;;;     (define-class Polygon Point ((* pt)(= z)))
;;; The field descriptor associated to the Mono-Field Z is:
;;;     +--------------+
;;;     | immutable?   | a boolean 
;;;     | name         | the symbol Z
;;;     | ...          | ...
;;;     +     2        + The path length (Two offsets are important here)
;;;     | 2            | Offset for the PT field
;;;     | 0            | Offset for Z starting after PT
;;;     +--------------+

;;; The total offset for a field in an instance is therefore computed by:

(define (compute-offset o field)
  (let* ((path-length (careless-Field-path-length field))
         (limit (fx- path-length 1)) )
    (let compute ((offset 0)
                  (index 0) )
      (let ((n (fx+ offset (careless-Field-path field index))))
        (if (fx= index limit)
            n
            (compute (fx+ n (fx+ 1 (instance-ref o n))) (fx+ index 1)) ) ) ) ) )

;;;====================================================
;;; compute-value-offset The basic function for a structured access in
;;; an object. It returns an offset. It checks for the class but it
;;; does not look at the initializedness of fields. Fields can then be
;;; read of written with the low-level instance-ref and instance-set!.

(define-temporary (compute-value-offset o field . index)
  (cond ((Mono-Field? field) (Mono-Field-compute-value-offset o field index))
        ((Poly-Field? field) (Poly-Field-compute-value-offset o field index))
        ((Virtual-Field? field) 
         (Virtual-Field-compute-value-offset o field index) )
        ((symbol? field) 
         (unless (Object? o)
           (report-meroon-error 'Access 'compute-value-offset
                                "Not an Object" o ) )
         (let ((field (retrieve-named-field (object->class o) field)))
           (if (pair? index)
               (compute-value-offset o field (car index))
               (compute-value-offset o field) ) ) )
        (else                (internal-meroon-error o field index)) ) )

(define (Mono-Field-compute-value-offset o field index)
  (check-class o (number->class (careless-Field-class-number field)) field)
  (let ((offset (compute-offset o field)))
    offset ) )

(define (Poly-Field-compute-value-offset o field index)
  (check-class o (number->class (careless-Field-class-number field)) field)
  (let ((index (if (pair? index) (car index) 
                   (report-missing-index field o) )))
    (let ((offset (compute-offset o field)))
      (check-index index o offset field)
      (fx+ offset (fx+ 1 index) ) ) ) )

;;; I am under the impression that this function is completely useless
;;; since compute-value-offset is only called on instances of classes
;;; not on instance of views. However this is for sake of completeness.

(define (Virtual-Field-compute-value-offset o field index)
  (if (Object? o)
      (let* ((class (object->class o))
             (new-field (resolve-virtual-field field class)) )
        (if new-field
            (if index
                (apply compute-value-offset o new-field index)
                (compute-value-offset o new-field) )
            (report-meroon-error 'Access "Inappropriate view" o) ) )
      (report-wrong-class o Object-class 'compute-value-offset) ) )

;;;==================================================== field-value
;;; Get the value of a field from an instance. Check if the field is
;;; initialized before returning its content.

;;; NOTE: Suppose we have classes Point and ColoredPoint defined as:
;;;   (define-class Point Object (x y))
;;;   (define-class ColoredPoint Point (color))
;;; then (Point-x o) is similar to 
;;; (field-value o (retrieve-named-field (object->class o) 'x)) but there is
;;; a slight distortion. Consider (ColoredPoint-x (make-Point 11 222)), this
;;; is an error since a Point is not a ColoredPoint. On the contrary
;;; (field-value (make-Point 1 2) (retrieve-named-field ColoredPoint-class 'x))
;;; is not an error since it is possible to access the X field of any Point
;;; since it is Point that introduced the X field.

(define-temporary (field-value o field . index)
  (cond ((Mono-Field? field) (Mono-Field-field-value o field index))
        ((Poly-Field? field) (Poly-Field-field-value o field index))
        ((Virtual-Field? field) (Virtual-Field-field-value o field index))
        ((symbol? field) 
         (unless (Object? o)
           (report-meroon-error 'Access 'field-value "Not an Object" o) )
         (let ((field (retrieve-named-field (object->class o) field)))
           (if (pair? index)
               (field-value o field (car index))
               (field-value o field) ) ) )
        (else                (default-field-value o field index)) ) )

;;; should never be called
(define default-field-value internal-meroon-error)

;;; For uniformity (and since it will become a method and methods do
;;; not have dotted variables), this takes an useless index variable.
(define (Mono-Field-field-value o field index)
  (check-class o (number->class (careless-Field-class-number field)) field)
  (let* ((offset (compute-offset o field))
         (content (instance-ref o offset)))
    (if (uninitialized? content)
        (report-uninitialized-field field o)
        content ) ) )

(define (Poly-Field-field-value o field index)
  (check-class o (number->class (careless-Field-class-number field)) field)
  (let ((index (if (pair? index) (car index) 
                   (report-missing-index field o) )))
    (let ((offset (compute-offset o field)))
      (check-index index o offset field)
      (let ((content (instance-ref o (fx+ offset (fx+ 1 index)))))
        (if (uninitialized? content)
            (report-uninitialized-field field o index)
            content ) ) ) ) )

(define (Virtual-Field-field-value o field index)
  (if (Object? o)
      (let* ((class (object->class o))
             (new-field (resolve-virtual-field field class)) )
        (if new-field
            (if index
                (apply field-value o new-field index)
                (field-value o new-field) )
            (report-meroon-error 'Access "Inappropriate view" o) ) )
      (report-wrong-class o Object-class 'field-value) ) )  

;;;==================================================== set-field-value!
;;; Change the content of a (possibly indexed) field. The field must
;;; of course be mutable otherwise an anomaly is signalled.

;;; Set the value of a field in an instance.  Pay attention to the
;;; order of the arguments. Here I wanted to keep index as an optional
;;; argument so it must be the last one therefore value had to be
;;; before.

(define-temporary (set-field-value! o value field . index)
  (cond 
   ((Mono-Field? field) (Mono-Field-set-field-value! o value field index))
   ((Poly-Field? field) (Poly-Field-set-field-value! o value field index))
   ((Virtual-Field? field) (Virtual-Field-set-field-value! o value field index))
   ((symbol? field) 
    (unless (Object? o)
      (report-meroon-error 'Access 'set-field-value! "Not an Object" o) )
    (let ((field (retrieve-named-field (object->class o) field)))
      (if (pair? index)
          (set-field-value! o value field (car index))
          (set-field-value! o value field) ) ) )
   (else                (default-set-field-value! o value field index)) ) )

;;; should never be called
(define default-set-field-value! internal-meroon-error)

(define (Mono-Field-set-field-value! o value field index)
  (check-class o (number->class (careless-Field-class-number field)) field)
  (let ((offset (compute-offset o field)))
    (if (careless-Field-immutable? field)
        (report-immutable-field field o)
        (instance-set! o offset value) ) ) )

(define (Poly-Field-set-field-value! o value field index)
  (check-class o (number->class (careless-Field-class-number field)) field)
  (let ((index (if (pair? index) (car index) 
                   (report-missing-index field o) )))
    (if (careless-Field-immutable? field)
        (report-immutable-field field o index)
        (let ((offset (compute-offset o field)))
          (check-index index o offset field)
          (instance-set! o (fx+ offset (fx+ 1 index)) value) ) ) ) )

(define (Virtual-Field-set-field-value! o value field index)
  (if (Object? o)
      (let* ((class (object->class o))
             (new-field (resolve-virtual-field field class)) )
        (if new-field
            (if index
                (apply set-field-value! o value new-field index)
                (set-field-value! o value new-field) )
            (report-meroon-error 'Access "Inappropriate view" o) ) )
      (report-wrong-class o Object-class 'set-field-value!) ) )

;;;==================================================== initialize-field-value!
;;; Initialize an uninitialized field in an instance. Note that this
;;; is not really a side-effect (an object is more like an
;;; I-structure) and the mutability of the field is not checked. A
;;; predicate also exists to check whether a field is defined or not:
;;; field-defined?

;;; Initialize the value of a field in an instance.
(define-temporary (initialize-field-value! o value field . index)
  (cond ((Mono-Field? field) 
         (Mono-Field-initialize-field-value! o value field index) )
         ((Poly-Field? field) 
          (Poly-Field-initialize-field-value! o value field index) )
         ((Virtual-Field? field) 
          (Virtual-Field-initialize-field-value! o value field index) )
         ((symbol? field)
          (unless (Object? o)
            (report-meroon-error 'Access 'initialize-field-value! 
                                 "Not an Object" o) )
          (let ((field (retrieve-named-field (object->class o) field)))
            (if (pair? index)
                (initialize-field-value! o value field (car index))
                (initialize-field-value! o value field) ) ) )
         (else                
          (default-initialize-field-value! o value field index) ) ) )

;;; should never be called
(define default-initialize-field-value! internal-meroon-error)

(define (Mono-Field-initialize-field-value! o value field index)
  (check-class o (number->class (careless-Field-class-number field)) field)
  (let* ((offset (compute-offset o field))
         (content (instance-ref o offset)) )
    (if (uninitialized? content)
        (instance-set! o offset value)
        (report-already-initialized field o value) ) ) )

(define (Poly-Field-initialize-field-value! o value field index)
  (check-class o (number->class (careless-Field-class-number field)) field)
  (let ((index (if (pair? index) (car index) 
                   (report-missing-index field o) )))
    (let ((offset (compute-offset o field)))
      (check-index index o offset field)
      (let ((content (instance-ref o (fx+ offset (fx+ 1 index)))))
        (if (uninitialized? content)
            (instance-set! o (fx+ offset (fx+ 1 index)) value)
            (report-already-initialized field o value index) ) ) ) ) )

(define (Virtual-Field-initialize-field-value! o value field index)
  (if (Object? o)
      (let* ((class (object->class o))
             (new-field (resolve-virtual-field field class)) )
        (if new-field
            (if index
                (apply initialize-field-value! o value new-field index)
                (initialize-field-value! o value new-field) )
            (report-meroon-error 'Access "Inappropriate view" o) ) )
      (report-wrong-class o Object-class 'initialize-field-value!) ) )

;;;==================================================== field-defined?
;;; Tests if the content of a (possibly indexed) field is initialized.
;;; This predicate is intended to be used by reflective
;;; meta-programmers only.

(define-temporary (field-defined? o field . index)
  (cond ((Mono-Field? field) (Mono-Field-field-defined? o field index))
        ((Poly-Field? field) (Poly-Field-field-defined? o field index))
        ((Virtual-Field? field) (Virtual-Field-field-defined? o field index))
        ((symbol? field)
         (unless (Object? o)
           (report-meroon-error 'Access 'field-defined? "Not an Object" o) )
         (let ((field (retrieve-named-field (object->class o) field)))
           (if (pair? index)
               (field-defined? o field (car index))
               (field-defined? o field) ) ) )
         (else                (default-field-defined? o field index)) ) )

;;; should never be called
(define default-field-defined? internal-meroon-error)

(define (Mono-Field-field-defined? o field index)
  (check-class o (number->class (careless-Field-class-number field)) field)
  (let ((offset (compute-offset o field)))
    (not (uninitialized? (instance-ref o offset))) ) )

(define (Poly-Field-field-defined? o field index)
  (check-class o (number->class (careless-Field-class-number field)) field)
  (let ((index (if (pair? index) (car index) 
                   (report-missing-index field o) )))
    (let ((offset (compute-offset o field)))
      (check-index index o offset field)
      (not (uninitialized? (instance-ref o (fx+ offset (fx+ 1 index))))) ) ) )

(define (Virtual-Field-field-defined? o field index)
  (if (Object? o)
      (let* ((class (object->class o))
             (new-field (resolve-virtual-field field class)) )
        (if new-field
            (if index
                (apply field-defined? o new-field index)
                (field-defined? o new-field) )
            (report-meroon-error 'Access "Inappropriate view" o) ) )
      (report-wrong-class o Object-class 'field-defined?) ) )

;;;==================================================== field-length
;;; Returns the length of an indexed field.

(define-temporary (field-length o field)
  (cond ((Poly-Field? field) (Poly-Field-field-length o field))
        ((Mono-Field? field) (Mono-Field-field-length o field))
        ((Virtual-Field? field) (Virtual-Field-field-length o field))
        ((symbol? field) 
         (unless (Object? o)
           (report-meroon-error 'Access 'field-length "Not an Object" o) )
         (field-length o (retrieve-named-field (object->class o) field)) )
        (else                (default-field-length o field)) ) )

;;; should never be called
(define default-field-length internal-meroon-error)

(define (Poly-Field-field-length o field)
  (check-class o (number->class (careless-Field-class-number field)) field)
  (let ((offset (compute-offset o field)))
    (instance-ref o offset) ) )

(define (Mono-Field-field-length o field)
  (report-meroon-error 'Access field "Non indexed field" o field) )

(define (Virtual-Field-field-length o field)
  (if (Object? o)
      (let* ((class (object->class o))
             (new-field (resolve-virtual-field field class)) )
        (if new-field
            (field-length o new-field)
            (report-meroon-error 'Access "Inappropriate view" o) ) )
      (report-wrong-class o Object-class 'field-length) ) )

;;;====================================================== Pseudo accessors
;;; These functions hide class-numbers in favor of classes. They
;;; correspond to usual combinations of functions.

;;; return the superclass of a class.

(define (Class-super-class class)
  (if (Class? class)
      (number->class (Class-super-number class))
      (report-meroon-error
       'Domain 'Class-super-class "Not a class" class ) ) )

;;; Return the list of the subclasses of a class. This list does not
;;; share anything with the list of subclass-numbers.

(define (Class-subclasses class)
  (if (Class? class)
      (oo-map number->class (Class-subclass-numbers class))
      (report-meroon-error
       'Domain 'Class-subclasses "Not a class" class ) ) )

;;; return the class which defines this field.

(define (Field-defining-class field)
  (if (Field? field)
      (number->class (Field-class-number field))
      (report-meroon-error
       'Domain 'Field-defining-class "Not a field" field ) ) )

;;; Retrieve a field with a name. It is possible to specify the
;;; default if no such field is found.

(define (retrieve-named-field class name . default)
  (let look ((fields (careless-Class-fields class)))
    (if (pair? fields)
        (if (eq? name (careless-Field-name (car fields)))
            (car fields)
            (look (cdr fields)) )
        (if (pair? default)
            ((car default) class name)
            (report-bad-coercion-to-field class name) ) ) ) )

;;; This error reporter is often used with the previous function.

(define (report-bad-coercion-to-field class o)
  (report-meroon-error
   'Domain 'field-value "Not coercible to a Field" o class ) )

;;; Tells if a field is mutable.

(define (Field-mutable? field)
  (not (Field-immutable? field)) )

;;;=========================================================================
;;; These functions are used in definers.scm.

;;; To ease the access through field-value and similar generic
;;; accessors, important offsets are pre-computed and recorded in the
;;; field itself. The Field instance is already allocated, this function
;;; just fills its path indexed field.

(define (set-important-offsets! field preceding-fields)
  (if (pair? preceding-fields)
      (let ((last-field (car (last-pair preceding-fields))))
        (Field-generate-next-offset! field last-field) )
      (initialize-field-value! field 0 'path 0) )
  field )

(define-temporary (Field-generate-next-offset! field last-field)
  (cond ((Mono-Field? last-field)
         (Mono-Field-generate-next-offset! field last-field) )
        ((Poly-Field? last-field)
         (Poly-Field-generate-next-offset! field last-field) )
        (else (report-meroon-error 'internal 'Field-generate-next-offset!
                 "Unknown type of field" last-field )) ) )

;;; If last-field is a Mono-Field then the new field has the same path
;;; except for the last offset which is one more.

(define (Mono-Field-generate-next-offset! field last-field)
  (let ((path-length (Field-path-length last-field)))
    (let fill ((i 0))
      (if (fx< i (fx- path-length 1))
          (begin
            (initialize-field-value! 
             field (Field-path last-field i) 'path i )
            (fill (fx+ i 1)) )
          (initialize-field-value! 
           field (fx+ (Field-path last-field i) 1) 'path i ) ) ) ) )

;;; If last-field is a Poly-Field, then the new field has a path with
;;; one more level. It extends the path of the last-field with a final zero.

(define (Poly-Field-generate-next-offset! field last-field)
  (let ((path-length (Field-path-length last-field)))
    (let fill ((i 0))
      (if (fx< i path-length)
          (begin
            (initialize-field-value! 
             field (Field-path last-field i) 'path i )
            (fill (fx+ i 1)) )
          (initialize-field-value! field 0 'path i) ) ) ) )

;;;oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; Count the number of Poly-Fields in a list of fields. This is used
;;; to compute how to generate accessors to fields.

(define (count-Poly-Fields fields)
  (let count ((fields fields))
    (if (pair? fields)
        (if (Poly-Field? (car fields))
            (fx+ 1 (count (cdr fields)))
            (count (cdr fields)) )
        0 ) ) )

;;;ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; compute accessors (by closure rather to improve code sharing).

(define (Mono-Field-create-careful-reader class fieldname)
  (let* ((field (retrieve-named-field class fieldname))
         (path-length (careless-Field-path-length field)) )
    (case path-length
      ((1) (let ((offset (careless-Field-path field 0)))
             (lambda (o)
               (check-class o class fieldname)
               (instance-ref o offset) ) ))
      ((2) (let* ((offset1 (careless-Field-path field 0))
                  (offset2 (careless-Field-path field 1))
                  (offset (fx+ offset1 (fx+ 1 offset2))) )
             (lambda (o)
               (check-class o class fieldname)
               (instance-ref o (fx+ offset (instance-ref o offset1))) ) ))
      (else (lambda (o) (field-value o field))) ) ) )

(define (Mono-Field-create-careful-writer class fieldname)
  (let* ((field (retrieve-named-field class fieldname))
         (path-length (careless-Field-path-length field)) )
    (case path-length
      ((1) (let ((offset (careless-Field-path field 0)))
             (lambda (o v)
               (check-class o class fieldname)
               (instance-set! o offset v) ) ))
      ((2) (let* ((offset1 (careless-Field-path field 0))
                  (offset2 (careless-Field-path field 1))
                  (offset (fx+ offset1 (fx+ 1 offset2))) )
             (lambda (o v)
               (check-class o class fieldname)
               (instance-set! o (fx+ offset (instance-ref o offset1)) v) ) ))
      (else (lambda (o v) (set-field-value! o v field))) ) ) )

(define (Poly-Field-create-careful-lengther class fieldname)
  (let* ((field (retrieve-named-field class fieldname))
         (path-length (careless-Field-path-length field)) )
    (case path-length
      ((1) (let ((offset (careless-Field-path field 0)))
             (lambda (o)
               (check-class o class fieldname)
               (instance-ref o offset) ) ))
      ((2) (let* ((offset1 (careless-Field-path field 0))
                  (offset2 (careless-Field-path field 1))
                  (offset (fx+ offset1 (fx+ 1 offset2))) )
             (lambda (o)
               (check-class o class fieldname)
               (instance-ref o (fx+ offset (instance-ref o offset1))) ) ))
      (else (lambda (o) (field-length o field))) ) ) )

(define (Poly-Field-create-careful-reader class fieldname)
  (let* ((field (retrieve-named-field class fieldname))
         (path-length (careless-Field-path-length field)) )
    (case path-length
      ((1) (let* ((offset (careless-Field-path field 0))
                  (offset+1 (fx+ offset 1)) )
             (lambda (o i)
               (check-class o class fieldname)
               (check-index i o offset fieldname)
               (instance-ref o (fx+ offset+1 i)) ) ))
      (else (lambda (o i) (field-value o field i))) ) ) )

(define (Poly-Field-create-careful-writer class fieldname)
  (let* ((field (retrieve-named-field class fieldname))
         (path-length (careless-Field-path-length field)) )
    (case path-length
      ((1) (let* ((offset (careless-Field-path field 0))
                  (offset+1 (fx+ offset 1)) )
             (lambda (o i v)
               (check-class o class fieldname)
               (check-index i o offset fieldname)
               (instance-set! o (fx+ offset+1 i) v) ) ))
      (else (lambda (o i v) (set-field-value! o v field i))) ) ) )

;;;ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; Generate an inlined reader if simple enough.
;;; CLASS-VARIABLE is the name of the variable that will hold the class.

(define-temporary (generate-fast-careful-reader field class-variable)
  ((cond ((Mono-Field? field) Mono-Field-generate-fast-careful-reader)
         ((Poly-Field? field) Poly-Field-generate-fast-careful-reader)
         (else                default-generate-fast-careful-reader) )
   field class-variable ) )

;;; should never be called
(define default-generate-fast-careful-reader internal-meroon-error)

(define (Mono-Field-generate-fast-careful-reader field class-variable)
  (let ((path-length (careless-Field-path-length field))
        (fieldname (careless-Field-name field)) )
    (case path-length
      ((1) 
       (let ((o (gensym))
             (offset (careless-Field-path field 0)) )
         ;; This lambda may be inlined if the compiler is aggressive enough:
         `(lambda (,o) 
            (check-class ,o ,class-variable ',fieldname)
            (instance-ref ,o ,offset) ) ) )
      ((2)
       (let* ((o (gensym))
              (offset1 (careless-Field-path field 0))
              (offset2 (careless-Field-path field 1))
              (offset (fx+ offset1 (fx+ 1 offset2))) )
         ;; This lambda may be inlined if the compiler is aggressive enough:
         `(lambda (,o)
            (check-class ,o ,class-variable ',fieldname)
            (instance-ref ,o (fx+ ,offset (instance-ref ,o ,offset1))) ) ))
      (else ;; No chance to have a fast accessor:
       `(Mono-Field-create-careful-reader ,class-variable ',fieldname) ) ) ) )

(define (Poly-Field-generate-fast-careful-reader field class-variable)
  (let ((path-length (careless-Field-path-length field))
        (fieldname (careless-Field-name field)) )
    (case path-length
      ((1) (let* ((o (gensym))
                  (i (gensym))
                  (offset (careless-Field-path field 0))
                  (offset+1 (fx+ offset 1)) )
             ;; This lambda may be inlined if the compiler is aggressive enough:
             `(lambda (,o ,i)
                (check-class ,o ,class-variable ',fieldname)
                (check-index ,i ,o ,offset ',fieldname)
                (instance-ref ,o (fx+ ,offset+1 ,i)) ) ))
      (else  ;; No chance to have a fast accessor:
       `(Poly-Field-create-careful-reader ,class-variable ',fieldname) ) ) ) )

;;;ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; Generate as well a fast and careful lengther:
;;; CLASS-VARIABLE is the name of the variable that will hold the class.

(define-temporary (generate-fast-careful-lengther field class-variable)
  ((cond ((Poly-Field? field) Poly-Field-generate-fast-careful-lengther)
         (else                default-generate-fast-careful-lengther) )
   field class-variable ) )

;;; should never be called
(define default-generate-fast-careful-lengther internal-meroon-error)

(define (Poly-Field-generate-fast-careful-lengther field class-variable)
  (let ((path-length (careless-Field-path-length field))
        (fieldname (careless-Field-name field)) )
    (case path-length
      ((1) (let* ((o (gensym))
                  (offset (careless-Field-path field 0)) )
             ;; This lambda may be inlined if the compiler is aggressive enough:
             `(lambda (,o)
                (check-class ,o ,class-variable ',fieldname)
                (instance-ref ,o ,offset) ) ))
      (else  ;; No chance to have a fast accessor:
       `(Poly-Field-create-careful-lengther ,class-variable ',fieldname) ) ) ) )
  

;;;ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; Generate an inlined writer if simple enough.
;;; CLASS-VARIABLE is the name of the variable that will hold the class.

(define-temporary (generate-fast-careful-writer field class-variable)
  ((cond ((Mono-Field? field) Mono-Field-generate-fast-careful-writer)
         ((Poly-Field? field) Poly-Field-generate-fast-careful-writer)
         (else                default-generate-fast-careful-writer) )
   field class-variable ) )

;;; should never be called
(define default-generate-fast-careful-writer internal-meroon-error)

(define (Mono-Field-generate-fast-careful-writer field class-variable)
  (let ((path-length (careless-Field-path-length field))
        (fieldname (careless-Field-name field)) )
    (case path-length
      ((1) 
       (let ((o (gensym))
             (v (gensym))
             (offset (careless-Field-path field 0)) )
         ;; This lambda may be inlined if the compiler is aggressive enough:
         `(lambda (,o ,v) 
            (check-class ,o ,class-variable ',fieldname)
            (instance-set! ,o ,offset ,v) ) ) )
      ((2)
       (let* ((o (gensym))
              (v (gensym))
              (offset1 (careless-Field-path field 0))
              (offset2 (careless-Field-path field 1))
              (offset (fx+ offset1 (fx+ 1 offset2))) )
         ;; This lambda may be inlined if the compiler is aggressive enough:
         `(lambda (,o ,v)
            (check-class ,o ,class-variable ',fieldname)
            (instance-set! ,o (fx+ ,offset (instance-ref ,o ,offset1)) ,v) ) ))
      (else ;; No chance to have a fast accessor:
       `(Mono-Field-create-careful-writer ,class-variable ',fieldname) ) ) ) )

(define (Poly-Field-generate-fast-careful-writer field class-variable)
  (let ((path-length (careless-Field-path-length field))
        (fieldname (careless-Field-name field)) )
    (case path-length
      ((1) (let* ((o (gensym))
                  (i (gensym))
                  (v (gensym))
                  (offset (careless-Field-path field 0))
                  (offset+1 (fx+ offset 1)) )
             ;; This lambda may be inlined if the compiler is aggressive enough:
             `(lambda (,o ,i ,v)
                (check-class ,o ,class-variable ',fieldname)
                (check-index ,i ,o ,offset ',fieldname)
                (instance-set! ,o (fx+ ,offset+1 ,i) ,v) ) ))
      (else  ;; No chance to have a fast accessor:
       `(Poly-Field-create-careful-writer ,class-variable ',fieldname) ) ) ) )

;;;ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; Generate an expression that will compute an offset in an instance.
;;; OVAR is the name of the variable that will hold the instance,
;;; INDEXVAR is the name of the index if any.

(define-temporary (generate-offset ovar field fieldvar . indexvar)
  ((cond ((Mono-Field? field) Mono-Field-generate-offset)
         ((Poly-Field? field) Poly-Field-generate-offset)
         (else                default-generate-offset) )
   ovar field fieldvar 
   (if (pair? indexvar) (car indexvar) 'void) ) )

;;; should never be called
(define default-generate-offset internal-meroon-error)

(define (Mono-Field-generate-offset ovar field fieldvar indexvar)
  (let ((path-length (careless-Field-path-length field)))
    (case path-length
      ((1) (let ((offset (careless-Field-path field 0)))
             offset ))
      ((2) (let ((indexed-offset (careless-Field-path field 0))
                 (offset (careless-Field-path field 1)) )
             `(fx+ ,(fx+ indexed-offset (fx+ 1 offset))
                 (instance-ref ,ovar ,indexed-offset) ) ))
      (else `(compute-offset ,ovar ,fieldvar)) ) ) )

(define (Poly-Field-generate-offset ovar field fieldvar indexvar)
  (let ((path-length (careless-Field-path-length field)))
    (case path-length
      ((1) (let ((offset (careless-Field-path field 0)))
             `(fx+ ,(fx+ offset 1) ,indexvar) ))
      (else `(compute-offset ,ovar ,fieldvar)) ) ) )

;;; end of access.scm
;;; $Id: dispatch.scm,v 1.1 2005/02/25 22:19:37 lucier Exp $ 
;;; Copyright (c) 1990-97 by Christian Queinnec. All rights reserved.

;;;                ***********************************************
;;;                    Small, Efficient and Innovative Class System
;;;                                    Meroon 
;;;                              Christian Queinnec  
;;;                    Ecole Polytechnique & INRIA--Rocquencourt
;;;                ************************************************

;;; This file contains the run-time functions that allows generic
;;; dispatching. Many functions are generic so one can add new kind of
;;; dispatchers. In that case, one has to extend the find-method
;;; function which is not generic since it must runs fast. 

;;; The predefined dispatchers classes appear in genesis.scm. Some
;;; types exist:
;;;   Immediate-Dispatcher does not check the class of the
;;;     discriminating variable and imposes the method to be found.
;;;   Subclass-Dispatcher tests the class of the discriminating variable 
;;;     and proposes two dispatchers whether the discriminating variable is
;;;     an instance or not of this class.
;;;   Indexed-Dispatcher finds a method in a vector indexed by the class-number
;;;     of the discriminating instance.
;;;   Global-Dispatcher uses a classes-wide Indexed-Dispatcher.
;;;   Linear-Dispatcher uses an Alist-like structure for multi-methods.

;;; Dispatchers in generic functions look like a decision tree that cannot
;;; be deeper than the following depth. When a method is added, the
;;; whole dispatcher is compressed to respect this depth. A zero depth
;;; means that all generic functions dispatch automatically with an
;;; Indexed-Dispatcher, a high number favours the elaboration of a
;;; decision tree which a high number of run-time tests. 
;;; A negative value is a special case meaning "dont try to compress
;;; dispatchers". It is used for debug purposes.

(define *dispatcher-max-depth* 
  (if-meroon-feature use-global-dispatchers
      -1    ; Use Global-Dispatchers
      3     ; compress dispatchers into less than 3 levels
      ) )

;;; extend the vector of classes as well as the dispatch tables of all
;;; generic functions (whether traced or not) that need it. This is
;;; necessary for indexed dispatchers that might need to be extended
;;; to take the new classes into account. Global-Dispatchers always
;;; have the size of vector *classes*. Whenever this latter grows, the
;;; formers grow similarly.

(define (extend-classes-number!)
  (set! *classes* (vector-extend *classes*))
  (sequence-map 
   (lambda (g)
     (when g (careless-set-Generic-dispatcher! 
              g (enlarge-dispatcher! 
                 (careless-Generic-dispatcher g) ) )) )
   *generics* ) )

;;; Global-Dispatchers have to be enlarged and only them. They
;;; generally appear at the top position of dispatchers except if the
;;; generic function is traced (see trace.scm). As other generic
;;; functions of that file, it will be redefined in the libgen.scm
;;; file.

(define-temporary (enlarge-dispatcher! d)
  (cond ((Global-Dispatcher? d) (Global-Dispatcher-enlarge-dispatcher! d))
        (else                   d) ) )

(define (Global-Dispatcher-enlarge-dispatcher! d)
  (let* ((size (vector-length *classes*))
         (oldn (careless-Global-Dispatcher-method-length d))
         (newd ((Class-allocator Global-Dispatcher-class) size)) )
    (careless-initialize-Dispatcher-method-finder
     newd Global-Dispatcher-find-method )
    (do ((i 0 (fx+ i 1)))
        ((fx>= i oldn))
      (careless-set-Global-Dispatcher-method!
       newd i (careless-Global-Dispatcher-method d i) ) )
    newd ) )

;; A method to fill holes in Indexed-Dispatcher. It is a sure sign of
;;; some internal error if it is ever invoked.

(define (barfing-method . arguments)
  (oo-apply report-meroon-error 'Domain 'barfing-method 
            "Absent method" arguments ) )

;;; Report that no method has been found for this GENERIC function and
;;; this set of ARGUMENTS. To increase code sharing, this is default
;;; method generator: see the expansion of define-generic.

(define (create-default-generic-method generic)
  (lambda arguments
    (oo-apply report-meroon-error 'Domain generic
              "No method for" arguments ) ) )

;;; Create an indexed dispatcher. NO is the default dispatcher and CN
;;; is the number of the class on which to index the methods.

(define (create-Indexed-Dispatcher cn . no)
  (let ((cl (number->class cn)))
    (let* ((no (if (pair? no) 
                   (car no) 
                   (make-Immediate-Dispatcher 
                    Immediate-Dispatcher-find-method
                    #f ) ))
           (sz (careless-Class-next cl))
           (size (fx+ sz (fx+ 1 (fxquotient sz 2)))) ; extra room
           (d ((Class-allocator Indexed-Dispatcher-class) size)) )
      (careless-initialize-Dispatcher-method-finder
       d Indexed-Dispatcher-find-method )
      (careless-set-Indexed-Dispatcher-class-number! d cn)
      (careless-set-Indexed-Dispatcher-class-depth! d (careless-Class-depth cl))
      (set-Indexed-Dispatcher-no! d no)
      (do ((i 0 (fx+ 1 i)))
          ((fx>= i size))
        (set-Indexed-Dispatcher-method! d i barfing-method) )
      (initialize! d) ) ) )

;;; Takes an Indexed-Dispatcher and extends it.

(define (extend-Indexed-Dispatcher d)
  (let ((old-size (Indexed-Dispatcher-method-length d))
        (newd (create-Indexed-Dispatcher 
               (Indexed-Dispatcher-class-number d)
               (Indexed-Dispatcher-no d) )))
    (do ((i 0 (fx+ 1 i)))
        ((fx= i old-size) newd)
      (set-Indexed-Dispatcher-method! 
       newd i (Indexed-Dispatcher-method d i) ) ) ) )

;;; Create an alternative dispatcher annd fill dependent slots.

(define (create-Subclass-Dispatcher cn no yes)
  (let ((cl (number->class cn)))
    (make-Subclass-Dispatcher 
     Subclass-Dispatcher-find-method
     cn (careless-Class-depth cl) no yes ) ) )

;;;============================================================= create generic
;;; Create a new Generic instance, also check version compatibility.
;;; Try to reuse the number associated to a generic having the same
;;; name if any.

(define register-Generic-1 
  (let ()
    (meroon-declare-not-inlinable)
    (lambda (rev name default-method signature top-class-name)
      (check-revision rev)
      (let ((g (make-Generic-1
                name                    ; name
                default-method          ; default
                signature               ; variables
                meroon-uninitialized    ; dispatcher
                meroon-uninitialized ) )) ; top-classes
        ;; set up a default method if there is none
        (unless (field-defined? g 'default)
          (careless-initialize-Generic-default
           g (create-default-generic-method g) ) )
        ;; set up the top-classes
        (careless-initialize-Generic-top-classes
         g (list (if top-class-name 
                     (symbol->class top-class-name 
                                    (lambda (name)
                                      (report-meroon-error
                                       'Syntax 'define-generic
                                       "No such class" name ) ) )
                     #f )) )
        ;; create the dispatcher
        (careless-set-Generic-dispatcher! 
         g (if-meroon-feature use-global-dispatchers
             (let* ((size (vector-length *classes*))
                    (d ((Class-allocator Global-Dispatcher-class) size)) )
               (careless-initialize-Dispatcher-method-finder
                d Global-Dispatcher-find-method ) 
               d )
             (let ((defdispatcher (make-Immediate-Dispatcher 
                                   Immediate-Dispatcher-find-method
                                   #f ))
                   (top-class (car (careless-Generic-top-classes g))) )
               (if (and (Class? top-class)
                        (not (eq? top-class Object-class)) )
                   (create-Indexed-Dispatcher 
                    (careless-Class-number top-class)
                    defdispatcher )
                   defdispatcher ) ) ) )
        ;; Propagate default-method to Global-Dispatcher or Indexed-Dispatcher
        (let ((d (careless-Generic-dispatcher g)))
          (when (Global-Dispatcher? d)
            (let ((size (Global-Dispatcher-method-length d))
                  (default (careless-Generic-default g)) )
              (do ((i 0 (fx+ 1 i)))
                  ((fx>= i size))
                (careless-set-Global-Dispatcher-method! d i default) ) ) )
          (when (Indexed-Dispatcher? d)
            (let ((size (Indexed-Dispatcher-method-length d))
                  (default (careless-Generic-default g)) )
              (do ((i 0 (fx+ 1 i)))
                  ((fx>= i size))
                (careless-set-Indexed-Dispatcher-method! d i default) ) ) ) )
        ;; register in the sequence of all generic functions
        (let* ((i (or (look-generic (Generic-name g))
                      (get-new-generic-number) )))
          (vector-set! *generics* i g) )
        ;; generic functions are regular instances so submit them to initialize!
        (initialize! (fill-other-fields! g)) ) ) ) )

(define register-Generic-N 
  (let ()
     (meroon-declare-not-inlinable)
     (lambda (rev name default-method signature . top-class-names)
       (check-revision rev)
       (let ((g (make-Generic-N
                 name                   ; name
                 default-method         ; default
                 signature              ; variables
                 meroon-uninitialized   ; dispatcher
                 meroon-uninitialized ) )) ; top-classes
         ;; set up a default method if there is none
         (unless (field-defined? g 'default)
           (careless-initialize-Generic-default
            g (create-default-generic-method g) ) )
         ;; set up the top-classes
         (careless-initialize-Generic-top-classes
          g (oo-map (lambda (name)
                      (if name (symbol->class name 
                                              (lambda (name)
                                                (report-meroon-error
                                                 'Syntax 'define-generic
                                                 "No such class" name ) ) )
                          #f ) )
                    top-class-names ) )
         ;; create the dispatcher
         (careless-set-Generic-dispatcher! 
          g (make-Immediate-Dispatcher 
             Immediate-Dispatcher-find-multi-method ; <- multi-method
             #f ) )
         ;; register in the sequence of all generic functions
         (let* ((i (or (look-generic (Generic-name g))
                       (get-new-generic-number) )))
           (vector-set! *generics* i g) )
         ;; generic functions are regular instances so submit them to initialize!
         (initialize! (fill-other-fields! g)) ) ) ) )

;;; Find if a previous generic function with the same name is
;;; registered, return its number if it exists.

(define (look-generic name)
  (define (look i)
    (if (fx< i *generic-number*)
        (if (eq? name (careless-Generic-name (vector-ref *generics* i)))
            i (look (fx+ 1 i)) )
        #f ) )
  (look 0) )

;;; The behavior of a generic function with only one discriminating
;;; variable. If you cannot find a method, then try to see if the
;;; object has an obsolete class-number, update it and retry to find a
;;; method.  This time if such a method cannot be found then default
;;; to the Generic default.

(define (careless-determine-method1 g o1)
  (if (Object? o1)
      (let ((d (careless-Generic-dispatcher g)))
        (let ((method (find-method1 d (object->class o1))))
          (or method
              (careless-Generic-default g) ) ) )
      (careless-Generic-default g) ) )

;;; The general function to determine a method. O* are the discriminating
;;; instances, since to discriminate on more than one argument is
;;; rare, the search through linear dispatchers expensive, update
;;; class-numbers immediately. Due to the integration with Scheme,
;;; there is a problem since Object is not the top-class ie there are
;;; Scheme values that are not objects. This might be the case of:
;;;         (define-generic (foo (o) (v)))
;;;         (define-method  (foo (o <some-class>>) (v)) ...)
;;; The actual value for v does not need to be an object at all.

(define (determine-method g . o*)
  ;; there is always at least one discriminating variable
  (if (null? (cdr o*))
      (careless-determine-method1 g (car o*))
      (let ((method (find-multi-method 
                     (careless-Generic-dispatcher g)
                     (oo-map (lambda (o) 
                               (if (Object? o) (object->class o) #f) )
                             o* ) )))
        (or method
            (careless-Generic-default g) ) ) ) )

;;;========================================================== find-method
;;; find-method works for generic functions with a single discriminating
;;; variable. It takes a dispatcher and a class then extracts from 
;;; the dispatcher, the appropriate method. Note that no method is
;;; coded as #f so you might have to fetch the default method in the
;;; generic instance if you get such a result.
;;; find-method is not generic since that would be unbearably slow.

(define (Immediate-Dispatcher-find-method d cl)
  (careless-Immediate-Dispatcher-method d) )

(define (Subclass-Dispatcher-find-method d cl)
  (if (let ((depth (careless-Subclass-Dispatcher-class-depth d))
            (number (careless-Subclass-Dispatcher-class-number d)) )
        (and (fx>= (careless-Class-depth cl) depth)
             (fx= (careless-Class-super cl depth) number) ) )
      (find-method1 (careless-Subclass-Dispatcher-yes d) cl)
      (find-method1 (careless-Subclass-Dispatcher-no d) cl) ) )

(define (Indexed-Dispatcher-find-method d cl)
  (let ((depth  (careless-Indexed-Dispatcher-class-depth d))
        (number (careless-Indexed-Dispatcher-class-number d)) )
    (if (and (fx>= (careless-Class-depth cl) depth)
             (fx= (careless-Class-super cl depth) number) )
        (careless-Indexed-Dispatcher-method
         d (careless-Class-relnum cl depth) )
        (find-method1 (careless-Indexed-Dispatcher-no d) cl) ) ) )

(define (Global-Dispatcher-find-method d cl)
  (careless-Global-Dispatcher-method d (careless-Class-number cl)) )

;;; (define (Tracing-Dispatcher-find-method d cl)...) see trace.scm

;;; These counters are used when profiling. With the first benchmark
;;; (Examples/interp.scm) results are (with Ida+4):
;;; ((dispatch index= 77758 subclass= 815590 immediate= 491882 
;;;     global= 0 else= 0) (generic-calls= 569565) )

(define *indexed-dispatch* 	0)
(define *subclass-dispatch* 	0)
(define *immediate-dispatch* 	0)
(define *global-dispatch* 	0)
(define *else-dispatch* 	0)

;;; General method for finding a method on Generic-1.
;;; The original definition was:
;;;    (define (find-method1 d cl)
;;;      ((careless-Dispatcher-method-finder d) d cl) )
;;; This was involving too much invokations. So I open-code the usual
;;; dispatcher cases. This allows to have a recursion which is better
;;; compiled. Why I did not think to it before ?! The reason is that
;;; it is no longer possible to change the find method of these usual
;;; dispatchers since they now are inlined.

(define (find-method1 d cl)
  (let ((cn (object->class-number d)))
    (cond 
     ((fx= cn original-Subclass-Dispatcher-class-number)
      (if-meroon-feature profile
        (set! *subclass-dispatch* (fx+ *subclass-dispatch* 1))
        #t )
      (if (let ((depth  (careless-Subclass-Dispatcher-class-depth d))
                (number (careless-Subclass-Dispatcher-class-number d)) )
            (and (fx>= (careless-Class-depth cl) depth)
                 (fx= (careless-Class-super cl depth) number) ) )
          (find-method1 (careless-Subclass-Dispatcher-yes d) cl)
          (find-method1 (careless-Subclass-Dispatcher-no d) cl) ) )
     ((fx= cn original-Immediate-Dispatcher-class-number)
      (if-meroon-feature profile
        (set! *immediate-dispatch* (fx+ *immediate-dispatch* 1))
        #t )
      (careless-Immediate-Dispatcher-method d) )
     ((fx= cn original-Indexed-Dispatcher-class-number)
      (if-meroon-feature profile
        (set! *indexed-dispatch* (fx+ *indexed-dispatch* 1))
        #t )
      (let ((depth  (careless-Indexed-Dispatcher-class-depth d))
            (number (careless-Indexed-Dispatcher-class-number d)) )
        (if (and (fx>= (careless-Class-depth cl) depth)
                 (fx= (careless-Class-super cl depth) number) )
            (careless-Indexed-Dispatcher-method
             d (careless-Class-relnum cl depth) )
            (find-method1 (careless-Indexed-Dispatcher-no d) cl) ) ) )
     ((fx= cn original-Global-Dispatcher-class-number)
      (if-meroon-feature profile
        (set! *global-dispatch* (fx+ *global-dispatch* 1))
        #t )
      (careless-Global-Dispatcher-method d (careless-Class-number cl)) )
     ;; Generic method for all other cases.
     (else 
      (if-meroon-feature profile
        (set! *else-dispatch* (fx+ *else-dispatch* 1))
        #t )
      ((careless-Dispatcher-method-finder d) d cl) ) ) ) )

;;;========================================================= create method
;;; This function is called at run-time to officially register the method.
;;; It also check the revision version of Meroon. This is just an
;;; entry point that is specialized on the number of discriminating
;;; variables.

(define register-method 
  (let ()
    (meroon-declare-not-inlinable)
    (lambda (generic-name
             variable-list
             rev
             method-maker
             . discriminating-class-names )
      (check-revision rev)
      (let ((g (symbol->generic generic-name
                                (lambda (name)
                                  (report-meroon-error 
                                   'Domain 'define-method
                                   "No such generic function"
                                   generic-name discriminating-class-names
                                   variable-list method-maker ) ) ))
            (classes (oo-map (lambda (name)
                               (symbol->class name
                                              (lambda (name)
                                                (report-meroon-error 
                                                 'Domain 'define-method
                                                 "No such class" 
                                                 generic-name name ) ) ) )
                             discriminating-class-names )) )
        (unless (coherent-variables? (Generic-variables g) variable-list)
          (report-meroon-error
           'Syntax 'define-method "Non congruent lambda-lists" 
           (Generic-variables g) variable-list ) )
        ;; only accepts methods that are compatible with the definition of
        ;; the generic function.
        (unless (memq (compare-signatures classes (Generic-top-classes g))
                      '(< =) )
          (report-meroon-error
           'Syntax 'define-method "method prohibited by the generic declaration"
           (Generic-top-classes g) classes ) )
        (case (length discriminating-class-names)
          ((1) (let ((c (car classes)))
                 (add-1-method! g c (method-maker g c 'void)) ))
          (else (add-N-method! g classes method-maker)) )
        generic-name ) ) ) )

;;; Test the congruence of two variable lists.  They may both contain
;;; a final n-ary (dotted) variable (which cannot be a discriminating
;;; variable (of course)). This predicate is used to test if a
;;; method is compatible with a generic function.

(if-meroon-feature (or DSSSL)
  (define (coherent-variables? generic-vars method-vars)
    (define dsssl? #f)
    (define rest? #f)
    (define (extract-keywords formals)
      ;; #!key appear at the end of the formals in DSSSL
      (if (pair? formals)
          (if (pair? (car formals))
              (cons (caar formals) (extract-keywords (cdr formals)))
              (cons (car formals) (extract-keywords (cdr formals))) )
          '() ) )
    (define (set-included? set1 set2)
      (if (pair? set1)
          (and (memq (car set1) set2)
               (set-included? (cdr set1) set2) )
          #t ) )
    (define (set-equal? set1 set2)
      (if (pair? set1)
          (and (memq (car set1) set2)
               (set-equal? (cdr set1) (remove (car set1) set2)) )
          (null? set2) ) )
    (define (cv generic-vars method-vars)
      (if (pair? generic-vars)
          (if (pair? method-vars)
              (if (and (not dsssl?)
                       (pair? (car generic-vars))
                       (pair? (car method-vars)) )
                  ;; similar discriminating variable
                  (cv (cdr generic-vars) (cdr method-vars))
                  ;; not a discriminating variable
                  (cond 
                   ((dsssl-optional? (car generic-vars))
                    (set! dsssl? #t)
                    (and (equal? (car generic-vars) (car method-vars))
                         (cv (cdr generic-vars) (cdr method-vars)) ) )
                   ((dsssl-rest? (car generic-vars))
                    (set! dsssl? #t)
                    (set! rest? #t)
                    (and (equal? (car generic-vars) (car method-vars))
                         (cv (cdr generic-vars) (cdr method-vars)) ) )
                   ((dsssl-key? (car generic-vars))
                    ;; keywords may be unordered
                    (and (equal? (car generic-vars) (car method-vars))
                         ((if rest? set-included? set-equal?)
                          (extract-keywords (cdr generic-vars))
                          (extract-keywords (cdr method-vars)) ) ) )
                   (else
                    (if (not dsssl?)
                        ;; similar non discriminating variables
                        (and (symbol? (car generic-vars))
                             (symbol? (car method-vars))
                             (cv (cdr generic-vars) (cdr method-vars)) )
                        ;; similar optional or rest variables
                        (cv (cdr generic-vars) (cdr method-vars)) ) ) ) )
              #f )
          (or (and (null? generic-vars) (null? method-vars))
              ;; similar dotted variable (meaningless in DSSSL)
              (and (not dsssl?)
                   (symbol? generic-vars)
                   (symbol? method-vars) ) ) ) )
    (cv generic-vars method-vars) )
  ;; R4RS normal comparison
  (define (coherent-variables? la lb)
    (if (pair? la)
        (if (pair? lb)
            (and (or;; similar discriminating variable
                  (and (pair? (car la))
                       (pair? (car lb)) )
                  ;; similar regular variable
                  (and (symbol? (car la))
                       (symbol? (car lb)) ) )
                 (coherent-variables? (cdr la) (cdr lb)) )
            #f )
        (or (and (null? la) (null? lb))
            ;; similar dotted variable
            (and (symbol? la) (symbol? lb)) ) ) )
)

;;; For Brad Lucier, so he may redefine it to support part of
;;; DSSSL-style arguments list. I now think that he does not need it
;;; any longer. This assignment hurts the exportation of this function
;;; for Bigloo so I comment it.
;;;(set! coherent-variables? coherent-variables?)

;;;======================================================== update dispatcher
;;; When a class is created then, for each generic function, propagate
;;; the methods of the superclass towards this class. While scanning
;;; dispatchers, update also the copy of the max fields of
;;; Indexed-Dispatchers of Subclass-Dispatchers [Recall that a fresh
;;; class is created with the highest class-number possible (by
;;; renumbering) so the min field is already correct.]  Another case
;;; is possible when class already existed. In that case, just reset
;;; the methods. The dispatchers are supposed to verify that if C1 is
;;; a super of C2 then C1 cannot occur in the no part of a
;;; Subclass-Dispatcher on C2.

;;; This function confers to GENERIC the method of the super of CLASS.

(define-temporary (Generic-update! generic class)
  (cond ((Generic-1? generic) (Generic-1-update! generic class))
        ((Generic-N? generic) (Generic-N-update! generic class))
        (else (report-meroon-error 'internal 'Generic-update!
                                   "No such method" generic class)) ) )

;;; It is not necessary to update generic-N functions since
;;; multimethods use Linear-Dispatcher that do not require to be
;;; updated.

(define (Generic-N-update! generic class)
  generic )

;;; Walk a Generic-1 dispatcher to update Subclass-Dispatcher and
;;; Indexed-Dispatcher ones. update-dispatcher! is a generic function.

(define (Generic-1-update! generic class)
  (set-Generic-dispatcher! 
   generic (update-dispatcher! (Generic-dispatcher generic) class) ) )

(define-temporary (update-dispatcher! d class)
  ((cond ((Immediate-Dispatcher? d) Immediate-Dispatcher-update-dispatcher!)
         ((Subclass-Dispatcher? d)  Subclass-Dispatcher-update-dispatcher!)
         ((Indexed-Dispatcher? d)   Indexed-Dispatcher-update-dispatcher!)
         ((Global-Dispatcher? d)    Global-Dispatcher-update-dispatcher!)
         ((Tracing-Dispatcher? d)   Tracing-Dispatcher-update-dispatcher!)
         (else                      default-update-dispatcher!) )
   d class ) )

;;; should never be called
(define default-update-dispatcher! internal-meroon-error)

(define (Immediate-Dispatcher-update-dispatcher! d class)
  d )

(define (Subclass-Dispatcher-update-dispatcher! d class)
  (let ((depth (careless-Subclass-Dispatcher-class-depth d))
        (number (careless-Subclass-Dispatcher-class-number d)) )
    (if (and (fx>= (careless-Class-depth class) depth)
             (fx= (careless-Class-super class depth) number) )
        (careless-set-Subclass-Dispatcher-yes! 
         d (update-dispatcher! (careless-Subclass-Dispatcher-yes d) class) )
        (careless-set-Subclass-Dispatcher-no! 
         d (update-dispatcher! (careless-Subclass-Dispatcher-no d) class) ) ) 
    d ) )

(define (Indexed-Dispatcher-update-dispatcher! d class)
  (let* ((depth  (careless-Subclass-Dispatcher-class-depth d))
         (number (careless-Subclass-Dispatcher-class-number d)) )
    (let ((d (if (fx> (careless-Indexed-Dispatcher-method-length d) 
                    (careless-Class-next (number->class number)) )
                 d 
                 (extend-Indexed-Dispatcher d) )))
      (if (and (fx>= (careless-Class-depth class) depth)
               (fx= (careless-Class-super class depth) number) )
          (let* ((supercl (number->class (careless-Class-super-number class)))
                 (method  (find-method1 d supercl))
                 (old-method (find-method1 d class)) )
            (define (fill! cn)
              (let* ((cl (number->class cn))
                     (content (find-method1 d cl)) )
                (when (eq? content old-method)
                  (careless-set-Indexed-Dispatcher-method! 
                   d (careless-Class-relnum cl depth) method )
                  (for-each fill! (careless-Class-subclass-numbers 
                                   (number->class cn) )) ) ) )
            (fill! (careless-Class-number class)) )
          ;; walk the `no' part
          (careless-set-Indexed-Dispatcher-no!
           d (update-dispatcher! (careless-Indexed-Dispatcher-no d) class) ) )
      d ) ) )

(define (Global-Dispatcher-update-dispatcher! d class)
  (let* ((supercl    (number->class (careless-Class-super-number class)))
         (method     (find-method1 d supercl))
         (old-method (find-method1 d class)) )
    (define (fill! cn)
      (let* ((cl      (number->class cn))
             (content (find-method1 d cl)) )
        (when (eq? content old-method)
          (careless-set-Global-Dispatcher-method! d cn method)
          (for-each fill! (careless-Class-subclass-numbers 
                           (number->class cn) )) ) ) )
    (fill! (careless-Class-number class))
    d ) )

;;; Traced generic are transparent for addition of classes.

(define (Tracing-Dispatcher-update-dispatcher! d class)
  (set-Tracing-Dispatcher-dispatcher!
   d (update-dispatcher! (Tracing-Dispatcher-dispatcher d) class) )
  d )

;;;======================================================== augment dispatcher
;;; Add a method to a generic function on a particular class. This is
;;; actually done by assigning the generic instance an augmented
;;; dispatcher. It can also be done with a functional style, in this way
;;; a new generic instance would be synthetized rather than modifying
;;; the old one but this consumes a stupendous amount of dispatchers
;;; when a new class is created.

(define (add-1-method! generic class method)
  (let* ((dd (Generic-dispatcher generic))
         (d (augment-dispatcher! dd class method)) )
    (set! d (compress-dispatcher! d *dispatcher-max-depth* Object-class))
    (set-Generic-dispatcher! generic d)
    (Generic-name generic) ) )

;;; Compute a new dispatcher that provides method on class without
;;; side-effect. Try to compress dispatchers if few methods otherwise
;;; use indexed dispatch tables.

(define-temporary (augment-dispatcher! d class method)
  ((cond ((Immediate-Dispatcher? d) Immediate-Dispatcher-augment-dispatcher!)
         ((Subclass-Dispatcher? d)  Subclass-Dispatcher-augment-dispatcher!)
         ((Indexed-Dispatcher? d)   Indexed-Dispatcher-augment-dispatcher!)
         ((Global-Dispatcher? d)    Global-Dispatcher-augment-dispatcher!)
         ((Tracing-Dispatcher? d)   Tracing-Dispatcher-augment-dispatcher!)
         (else                      default-augment-dispatcher!) )
   d class method ) )

;;; should never be called
(define default-augment-dispatcher! internal-meroon-error)

(define (Immediate-Dispatcher-augment-dispatcher! d class method)
  (cond ((eq? method (Immediate-Dispatcher-method d)) d)
        ;; Often the case:
        ((eq? class Object-class)
         (make-Immediate-Dispatcher 
          Immediate-Dispatcher-find-method
          method ) )
        (else (let ((yes (make-Immediate-Dispatcher 
                          Immediate-Dispatcher-find-method
                          method ))
                    (no d) )
                (create-Subclass-Dispatcher
                 (Class-number class) no yes ) )) ) )

;;; Add a method on a class in an Indexed-Dispatcher. There are two
;;; cases whether the class already exists (then patch the vector
;;; of methods (to avoid copying the Indexed-Dispatcher)) or is
;;; freshly created. In this latter case, a new Indexed-Dispatcher
;;; must be allocated to hold this new method as well as all the old
;;; ones.

(define (Indexed-Dispatcher-augment-dispatcher! d class method)
  (let* ((cn    (Indexed-Dispatcher-class-number d))
         (depth (Indexed-Dispatcher-class-depth d))
         (cl    (number->class cn)) )
    ;; extend the indexed-dispatcher if needed
    (let ((d (if (fx> (Indexed-Dispatcher-method-length d)
                    (careless-Class-next cl) )
                 d 
                 (extend-Indexed-Dispatcher d) )))
      (cond 
       ;; also take care of class = cl:
       ((careless-subclass? class cl)
        (let ((old-method (find-method1 d class)))
          (define (fill! cn)
            (let* ((cl      (number->class cn))
                   (content (find-method1 d cl)) )
              (when (eq? content old-method)
                (set-Indexed-Dispatcher-method! 
                 d (careless-Class-relnum cl depth) method )
                (for-each fill! (Class-subclass-numbers 
                                 (number->class cn) )) ) ) )
          (fill! (Class-number class))
          d ) )
       ;; Add a Subclass-Dispatcher before theIndexed-Dispatcher
       ((careless-subclass? cl class)
        (let ((no-d (Indexed-Dispatcher-no d)))
          (set-Indexed-Dispatcher-no! 
           d (make-Immediate-Dispatcher
              Immediate-Dispatcher-find-method
              method ) )
          (make-Subclass-Dispatcher
           Subclass-Dispatcher-find-method
           (careless-Class-number class)
           (careless-Class-depth class)
           no-d
           d ) ) )
       ;; Augment (physically) the no part of this Indexed-Dispatcher
       (else
        (let ((no-d (augment-dispatcher! (Indexed-Dispatcher-no d) 
                                         class method )))
          (set-Indexed-Dispatcher-no! d no-d)
          d ) ) ) ) ) )

(define (Global-Dispatcher-augment-dispatcher! d class method)
  (let ((old-method (find-method1 d class)))
    (define (fill! cn)
      (let* ((cl      (number->class cn))
             (content (find-method1 d cl)) )
        (when (eq? content old-method)
          (set-Global-Dispatcher-method! d cn method)
          (for-each fill! (Class-subclass-numbers cl)) ) ) )
    (fill! (careless-Class-number class)) )
  d )

;;; Traced dispatchers are transparent for method addition.

(define (Tracing-Dispatcher-augment-dispatcher! d class method)
  (set-Tracing-Dispatcher-dispatcher!
   d (augment-dispatcher! (Tracing-Dispatcher-dispatcher d) class method) )
  d )

;;; Three cases depending on the relation between CLASS and the class
;;; on which the dispatcher dispatches: CL. One invariant that must be
;;; respected is: if C1 is a superclass of C2 then a
;;; Subclass-Dispatcher on C2 cannot have a dispatcher for C1 in its
;;; no part.

(define (Subclass-Dispatcher-augment-dispatcher! d class method)
  (let ((cl (number->class (Subclass-Dispatcher-class-number d))))
    (cond 
     ;; augment the `yes' part
     ((eq? cl class)
      (let ((yes-d (augment-dispatcher! (Subclass-Dispatcher-yes d) 
                                        class method )))
        (if (and (Subclass-Dispatcher? yes-d)
                 (fx= (Subclass-Dispatcher-class-number d)
                    (Subclass-Dispatcher-class-number yes-d) ) )
            ;; remove one level
            (set-Subclass-Dispatcher-yes! d (Subclass-Dispatcher-yes yes-d))
            ;; keep it
            (set-Subclass-Dispatcher-yes! d yes-d) )
        d ) )            
     ;; also augment the `yes' part
     ((careless-subclass? class cl)
      (set-Subclass-Dispatcher-yes!
       d (augment-dispatcher! (Subclass-Dispatcher-yes d) class method) )
      d )
     ;; insert a new level of Subclass-Dispatcher. Extract from the no part
     ;; the dispatchers inferior to class.
     ((careless-subclass? cl class)
      (let ((newd (create-Subclass-Dispatcher
                   (Class-number class)
                   (Subclass-Dispatcher-no d)
                   (create-Subclass-Dispatcher
                    (Subclass-Dispatcher-class-number d)
                    (make-Immediate-Dispatcher
                     Immediate-Dispatcher-find-method
                     method )
                    (Subclass-Dispatcher-yes d) ) )))
        (set-Subclass-Dispatcher-no!
         newd (rebalance-dispatcher! newd (Subclass-Dispatcher-no newd)) )
        newd ) )
     ;; augment the `no' part
     (else (set-Subclass-Dispatcher-no! 
            d (augment-dispatcher! (Subclass-Dispatcher-no d) class method) )
           d )) ) )

;;; Note (*): This must avoid the following bug: suppose you have
;;; classes A with subclass B and C.  You define mb as a method for B,
;;; then mc for C, you thus have a Subclass-Dispatcher on B with mb as
;;; yes and in the no part a Subclass-Dispatcher with mc as yes and #f
;;; as no.  Suppose now that you define ma for A then you must not put
;;; mc in the no part of a Subclass-Dispatcher for A nor you can put
;;; ma in the no part of the Subclass-Dispatcher for B. You have to
;;; rebalance the tree of dispatchers.

(define-temporary (rebalance-dispatcher! d dno)
  ((cond 
    ((Subclass-Dispatcher? dno)  Subclass-Dispatcher-rebalance-dispatcher!)
    ((Immediate-Dispatcher? dno) Immediate-Dispatcher-rebalance-dispatcher!)
    ((Indexed-Dispatcher? dno)   Indexed-Dispatcher-rebalance-dispatcher!)
    ((Global-Dispatcher? dno)    Global-Dispatcher-rebalance-dispatcher!)
    ((Tracing-Dispatcher? dno)   Tracing-Dispatcher-rebalance-dispatcher!)
    (else                        internal-meroon-error) )
   d dno ) )

(define (Subclass-Dispatcher-rebalance-dispatcher! d dno)          
  (let ((cl (number->class (Subclass-Dispatcher-class-number d)))
        (clno (number->class (Subclass-Dispatcher-class-number dno))) )
    (if (careless-subclass? clno cl)
        (let ((no (Subclass-Dispatcher-no dno)))
          (set-Subclass-Dispatcher-no!
           dno (Subclass-Dispatcher-yes d) )
          (set-Subclass-Dispatcher-yes! d dno)
          (rebalance-dispatcher! d no) )
        ;; Due to the invariant, clno and cl are unrelated
        (set-Subclass-Dispatcher-no!
         dno (rebalance-dispatcher! 
              d (Subclass-Dispatcher-no dno) ) ) ) ) )

(define (Immediate-Dispatcher-rebalance-dispatcher! d dno)
  dno )

(define (Indexed-Dispatcher-rebalance-dispatcher! d dno)
  (let ((cl (number->class (Subclass-Dispatcher-class-number d)))
        (clno (number->class (Indexed-Dispatcher-class-number dno))) )
    (if (careless-subclass? clno cl)
        (let ((no (Indexed-Dispatcher-no dno)))
          (set-Indexed-Dispatcher-no!
           dno (Subclass-Dispatcher-yes d) )
          (set-Subclass-Dispatcher-yes! d dno)
          (rebalance-dispatcher! d no) )
        ;; Due to the invariant, clno and cl are unrelated
        (set-Indexed-Dispatcher-no!
         dno (rebalance-dispatcher! 
              d (Indexed-Dispatcher-no dno) ) ) ) ) )

(define (Global-Dispatcher-rebalance-dispatcher! d dno)
  dno )

(define (Tracing-Dispatcher-rebalance-dispatcher! d dno)
  dno )

;;;======================================================== compress dispatcher
;;; Convert a dispatcher into an Indexed-Dispatcher if too bushy. This
;;; will speed-up method looking. Compressing dispatchers use LEVEL
;;; to record the depth of the dispatching tree. Whenever too deep,
;;; access is done through an indexing scheme. TOP-CLASS is the
;;; greatest possible class for an instance reaching the current D.

(define-temporary (compress-dispatcher! d level top-class)
  ((cond ((Immediate-Dispatcher? d) Immediate-Dispatcher-compress-dispatcher!)
         ((Indexed-Dispatcher? d)   Indexed-Dispatcher-compress-dispatcher!)
         ((Subclass-Dispatcher? d)  Subclass-Dispatcher-compress-dispatcher!)
         ((Global-Dispatcher? d)    Global-Dispatcher-compress-dispatcher!)
         ((Tracing-Dispatcher? d)   Tracing-Dispatcher-compress-dispatcher!)
         (else                      internal-meroon-error) )
   d level top-class ) )

;;; Converts a dispatcher into an indexed dispatcher. It will be
;;; accessed by all instances of CLASS. If the class is the Object class
;;; then use a Global-Dispatcher instead. This is slightly faster for the
;;; default method and also when walking dispatchers.

(define (indexize-dispatcher dispatcher class)
  (if (eq? class Object-class)
      (globalize-dispatcher dispatcher)
      (really-indexize-dispatcher dispatcher class) ) )

(define (really-indexize-dispatcher dispatcher class)
  ;;(display `(indexize-dispatcher ,dispatcher ,class))(newline) ; DEBUG
  (let* ((depth  (careless-Class-depth class))
         (number (careless-Class-number class))
         (newd   (create-Indexed-Dispatcher 
                  number 
                  (make-Immediate-Dispatcher 
                   Immediate-Dispatcher-find-method 
                   #f ) )) )
    (let fill! ((cn number))
      (let ((cl (number->class cn)))
        (set-Indexed-Dispatcher-method! 
         newd (careless-Class-relnum cl depth) (find-method1 dispatcher cl) )
        (for-each fill! (Class-subclass-numbers cl)) ) )
    ;;(display `(result is ,newd))(newline) ; DEBUG
    newd ) )

;;; Should be faster to insert the default method instead of #f but
;;; this will prevent changing the default method of generic
;;; functions.

(define (globalize-dispatcher dispatcher)
  ;;(display `(globalize-dispatcher ,dispatcher))(newline) ; DEBUG
  (let* ((size (vector-length *classes*))
         (newd ((Class-allocator Global-Dispatcher-class) size)) )
    (careless-initialize-Dispatcher-method-finder
     newd Global-Dispatcher-find-method )
    (do ((i 0 (fx+ i 1)))
        ((fx>= i size))
      (set-Global-Dispatcher-method! newd i #f) )
    (let fill! ((cn (careless-Class-number Object-class)))
      (let ((cl (number->class cn)))
        (set-Global-Dispatcher-method! 
         newd cn (find-method1 dispatcher cl) )
        (for-each fill! (Class-subclass-numbers cl)) ) )
    ;;(display `(result is ,newd))(newline) ; DEBUG
    newd ) )

(define (Global-Dispatcher-compress-dispatcher! d level top-class)
  d )

(define (Immediate-Dispatcher-compress-dispatcher! d level top-class)
  d )

(define (Indexed-Dispatcher-compress-dispatcher! d level top-class)
  d )

(define (Tracing-Dispatcher-compress-dispatcher! d level top-class)
  d )

;;; When compressing a Subclass-Dispatcher and if the dispatcher
;;; corresponding to the NO part is indexed then there is no point to
;;; not indexize also the YES part since they are accessed by the same
;;; TOP-CLASS. This version of compress-dispatcher! is no longer
;;; purely functional to lessen data consumption.

(define (Subclass-Dispatcher-compress-dispatcher! d level top-class)
  (if (fx<= level 0) 
      (indexize-dispatcher d top-class)
      (let ((no-d (compress-dispatcher! 
                   (Subclass-Dispatcher-no d) (fx- level 1) top-class )))
        (if (or (Indexed-Dispatcher? no-d)
                (Global-Dispatcher? no-d) )
            (indexize-dispatcher d top-class)
            (let ((yes-d (compress-dispatcher! 
                          (Subclass-Dispatcher-yes d)
                          (fx- level 1)
                          (number->class 
                           (Subclass-Dispatcher-class-number d) ) )))
              (if (and (Indexed-Dispatcher? yes-d)
                       (fx= (Subclass-Dispatcher-class-number d)
                          (Indexed-Dispatcher-class-number yes-d) ) )
                  ;; remove the Subclass-Dispatcher level
                  (begin 
                    (set-Indexed-Dispatcher-no! yes-d no-d)
                    yes-d )
                  ;; keep the Subclass-Dispatcher level
                  (begin
                    (set-Subclass-Dispatcher-yes! d yes-d)
                    (set-Subclass-Dispatcher-no! d no-d)
                    d ) ) ) ) ) ) )

;;; NOTE: This compaction is too simple-minded, should be improved!!!

;;; end of dispatch.scm
;;; $Id: handy.scm,v 1.1 2005/02/25 22:19:37 lucier Exp $
;;; Copyright (c) 1990-2000 Christian Queinnec. All rights reserved.

;;;                ***********************************************
;;;                    Small, Efficient and Innovative Class System
;;;                                    Meroon 
;;;                              Christian Queinnec  
;;;                    Ecole Polytechnique & INRIA--Rocquencourt
;;;                ************************************************

;;; This file defines how the accompanying functions of a handy class are
;;; generated. Handy-Class is a subclass of Class, the difference is
;;; that when a Handy-Class is created a lot of accompanying functions
;;; are also created. All the generating functions are generic, are
;;; specialized on Handy-Class and can be further specialized as shown
;;; in the MeroonV2 compatibility metaclass.  The
;;; generate-accompanying-functions is itself generic.

(define (Handy-Class-generate-accompanying-functions class class-options)
  `(begin 
     ,(generate-accessors class class-options)
     ,(generate-predicate class class-options)
     ,(generate-maker class class-options)
     ,(generate-allocator class class-options)
     ,(generate-coercer class class-options) ) )

;;;oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; Handy classes methods

;;; Generates the standard predicate.

(define (Handy-Class-generate-predicate class class-options)
  (let* ((classname (careless-Class-name class))
         ;; UMB Scheme does not accept symbols starting with a minus sign.
         (class-variable (symbol-concatenate classname '- 'class)) )
    `(meroon-define ,(symbol-concatenate classname "?") 
       (create-predicate ,class-variable) ) ) )

(define-temporary generate-predicate Handy-Class-generate-predicate)

;;; Generate coercer except if the class is virtual in which case it
;;; is meaningless.

(define (Handy-Class-generate-coercer class class-options)
  (let* ((name (careless-Class-name class))
         ;; UMB Scheme does not accept symbols starting with a minus sign.
         (coercer-name (symbol-concatenate '- '> name)) )
    (if (find-option-present? 'virtual class-options)
        `'**no-coercer**
        `(begin (define-generic (,coercer-name (o)))
                (define-method (,coercer-name (o ,name)) o) ) ) ) )

(define-temporary generate-coercer Handy-Class-generate-coercer)

;;; Generate the allocator unless the class is virtual.

(define (Handy-Class-generate-allocator class class-options)
  (let* ((name (careless-Class-name class))
         ;; UMB Scheme does not accept symbols starting with a minus sign.
         (class-variable (symbol-concatenate name '- 'class)) )
    (if (find-option-present? 'virtual class-options)
        `'**no-allocator**
        `(meroon-define ,(symbol-concatenate 'allocate- name)
           (Class-allocator ,class-variable) ) ) ) )

(define-temporary (generate-allocator class class-options)
  (cond ((Handy-Class? class) 
         (Handy-Class-generate-allocator class class-options) )
        ((Class? class) `#t) ) )

;;; Generate the maker unless the class is virtual.

(define (Handy-Class-generate-maker class class-options)
  (let* ((classname (Class-name class))
         ;; UMB Scheme does not accept symbols starting with a minus sign.
         (class-variable (symbol-concatenate classname '- 'class)) )
    (if (find-option-present? 'virtual class-options)
        `'**no-maker**
        `(meroon-define ,(symbol-concatenate 'make- classname)
           (make-maker ,class-variable) ) ) ) )

(define-temporary generate-maker Handy-Class-generate-maker)

;;; Generate the accessors. With the new scheme to name accessors,
;;; they only need to be introduced by defining classes. But care is
;;; required for homonymic fieldnames.

(define (Handy-Class-generate-accessors class class-options)
  `(begin 
     #t                                 ; so the body is never empty
     ,@(meroon-reduce
        (lambda (code field)
          (if (fx= (Field-class-number field) (Class-number class))
              (cons (Field-generate-Handy-accessors field class) code)
              code ) )
        `()
        (Class-fields class) ) ) )

;;;oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; Generates the names for the accessors to a field. They are formed
;;; from the name of the field only. get-<field> and set-<field>!
;;; functions are special generic accessing functions that are hidden.

(define-temporary (Field-generate-Handy-accessors field class)
  ((cond ((Mono-Field? field) Mono-Field-generate-Handy-accessors)
         ((Poly-Field? field) Poly-Field-generate-Handy-accessors)
         (else default-Field-generate-Handy-accessors) )
   field class ) )

;;; should never be called.
(define default-Field-generate-Handy-accessors internal-meroon-error)

(define (Mono-Field-generate-Handy-accessors field class)
  (let* ((classname (careless-Class-name class))
         ;; UMB Scheme does not accept symbols starting with a minus sign.
         (class-variable (symbol-concatenate classname '- 'class))
         (fieldname (careless-Field-name field)) )
    `(begin
       (meroon-define ,(symbol-concatenate 'get- fieldname)
         (Mono-Field-create-careful-reader ,class-variable ',fieldname) )
       ,@(if (Field-immutable? field)
             `()
             `((meroon-define ,(symbol-concatenate 'set- fieldname "!")
                 (Mono-Field-create-careful-writer 
                  ,class-variable ',fieldname ) )) ) ) ) )

(define (Poly-Field-generate-Handy-accessors field class)
  (let* ((classname (careless-Class-name class))
         ;; UMB Scheme does not accept symbols starting with a minus sign.
         (class-variable (symbol-concatenate classname '- 'class))
         (fieldname (careless-Field-name field)) )
    `(begin 
       ;; UMB Scheme does not accept symbols starting with a minus sign.
       (meroon-define ,(symbol-concatenate 'get- fieldname '- 'length)
         (Poly-Field-create-careful-lengther ,class-variable ',fieldname) )
       (meroon-define ,(symbol-concatenate 'get- fieldname)
         (Poly-Field-create-careful-reader ,class-variable ',fieldname) )
       ,@(if (Field-immutable? field)
             `()
             `((meroon-define ,(symbol-concatenate 'set- fieldname "!")
                 (Poly-Field-create-careful-writer 
                  ,class-variable ',fieldname ) )) ) ) ) )

;;;ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; Meroon V2 classes
;;; Accessors in Meroon are named as <class>-<field> whereas they are
;;; named get-<field> in Handy classes.

(define (MeroonV2-Class-generate-accessors class class-options)
  `(begin 
     #t                                 ; so the body is never empty
     ,@(oo-map (lambda (field)
                 (Field-generate-MeroonV2-accessors field class) )
               (Class-fields class) ) ) )

(define-temporary (generate-accessors class class-options)
  (cond ((MeroonV2-Class? class) 
         (MeroonV2-Class-generate-accessors class class-options) )
        ((Handy-Class? class) 
         (Handy-Class-generate-accessors class class-options) )
        ((Class? class)
         (Class-generate-accessors class class-options) )
        (else (report-meroon-error 'internal 'generate-accessors
               "Unknown metaclass" class class-options )) ) )

(define (Class-generate-accessors class class-options)
  `'**no-accessors** )

;;;oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; Generates the names for the accessors to a field. They are all formed
;;; from the name of the field and the class. They supersede the
;;; corresponding methods on Handy-Class but they only differ in the
;;; way accessors are named.

(define-temporary (Field-generate-MeroonV2-accessors field class)
  ((cond ((Mono-Field? field) Mono-Field-generate-MeroonV2-accessors)
         ((Poly-Field? field) Poly-Field-generate-MeroonV2-accessors)
         (else default-Field-generate-MeroonV2-accessors) )
   field class ) )

;;; should never be called.
(define default-Field-generate-MeroonV2-accessors internal-meroon-error)

(define (Mono-Field-generate-MeroonV2-accessors field class)
  (let* ((classname (careless-Class-name class))
         ;; UMB Scheme does not accept symbols starting with a minus sign.
         (class-variable (symbol-concatenate classname '- 'class))
         (fieldname (careless-Field-name field)) )
    `(begin
       (meroon-define ,(symbol-concatenate classname '- fieldname)
         (Mono-Field-create-careful-reader ,class-variable ',fieldname) )
       ,@(if (Field-immutable? field)
             `()
             `((meroon-define
                ,(symbol-concatenate 'set- classname '- fieldname "!")
                (Mono-Field-create-careful-writer 
                 ,class-variable ',fieldname ) )) ) ) ) )

(define (Poly-Field-generate-MeroonV2-accessors field class)
  (let* ((classname (careless-Class-name class))
         ;; UMB Scheme does not accept symbols starting with a minus sign.
         (class-variable (symbol-concatenate classname '- 'class))
         (fieldname (careless-Field-name field)) )
    `(begin 
       ;; UMB Scheme does not accept symbols starting with a minus sign.
       (meroon-define ,(symbol-concatenate classname '- fieldname '- 'length)
         (Poly-Field-create-careful-lengther ,class-variable ',fieldname) )
       (meroon-define ,(symbol-concatenate classname '- fieldname)
         (Poly-Field-create-careful-reader ,class-variable ',fieldname) )
       ,@(if (Field-immutable? field)
             `()
             `((meroon-define
                ,(symbol-concatenate 'set- classname '- fieldname "!")
                 (Poly-Field-create-careful-writer 
                  ,class-variable ',fieldname ) )) ) ) ) )

;;; end of handy.scm
;;; $Id: definers-gsc.scm,v 1.1 2008/03/02 03:02:17 lucier Exp lucier $ 
;;; Copyright (c) 1990-2000 Christian Queinnec. All rights reserved.

;;;                ***********************************************
;;;                    Small, Efficient and Innovative Class System
;;;                                    Meroon 
;;;                              Christian Queinnec  
;;;                    Ecole Polytechnique & INRIA--Rocquencourt
;;;                ************************************************

;;; This file defines the defining forms of Meroon:
;;;    define-{class,generic,method,handy-method}.
;;; Syntax checking functions also appear here.

;;;=============================================================== define-class
;;; Check a little the syntax of a class definition then destructure the 
;;; definition to finally process it.

(define (check-and-process-class-definition definition)
  (unless (fx>= (length definition) 3)
    (report-meroon-error 
     'Syntax 'define-class "Incomplete definition" definition ) )
  (let ((name (car definition))
        (super-name (cadr definition))
        (own-field-descs (caddr definition))
        (class-options (cdddr definition)) )
    (unless (symbol? name) 
      (report-meroon-error 
       'Syntax 'define-class "Incorrect name for a class" name ) )
    ;; Provision for multiple inheritance.
    (unless (or (symbol? super-name) 
                (and (pair? super-name)
                     (symbol? (car super-name))
                     (null? (cdr super-name)) ) )
      (report-meroon-error 
       'Syntax 'define-class "Incorrect name for a super class" super-name ) )
    ;; check that the super exists at class definition time
    (let* ((sn (if (pair? super-name) (car super-name) super-name))
           (super-class
            (symbol->class sn complain-if-missing-super-class) ))
      (process-class-definition
       name super-class own-field-descs class-options ) ) )  )

;;; Complain in case of a missing (super of meta) class.
;;; Share code and do not rebuild useless closures.

(define (make-complainer-if-missing-class msg)
  (lambda (name) 
    (report-meroon-error 'Syntax 'define-class msg name) ) )

(define complain-if-missing-super-class
  (make-complainer-if-missing-class "No such super class") )

(define complain-if-missing-meta-class
  (make-complainer-if-missing-class "No such meta class") )

(define complain-if-missing-super-view
  (lambda (name) 
    (report-meroon-error 'Syntax 'define-view "No such super view" name) ) )

;;; Some options can be set by default, for instance :immutable and
;;; :maybe-uninitialized (which characterize the fields that can be left
;;; uninitialized). This variable is only used for internal tests.

(define *default-class-options* '())

;;; This variable defines the default metaclass for classes.
;;; It is actually set, for compatibility with older Meroon, to
;;; MeroonV2-Class so that when a class is defined a bunch of
;;; functions is altogether defined with the old conventions. You can
;;; also use Handy-Class if you want the new naming scheme for
;;; accessors. You can also use Class-class to get rid of accompanying
;;; functions.

(define *standard-class-metaclass-name* 'MeroonV2-Class)

;;; These are the names of the default classes for Fields.

(define *standard-mono-field-metaclass-name* 'Mono-Field)
(define *standard-poly-field-metaclass-name* 'Poly-Field)

;;; To process the definition of a class, one must give its name, the 
;;; name of its super-class (that must exists at this time), the
;;; description of its fields and some class-options as well. It
;;; creates a class object and generates code from it. Two cases are
;;; distinguished whether the class is being defined as a prototype or
;;; not. If it is a prototype then the class is only used for
;;; compile-time and only code that will check that the compilation
;;; hypotheses are respected at load time is issued.

(define (process-class-definition 
         name super-class own-field-descs class-options )
  (let* ((metaclass-options (find-option-values 
                             'metaclass class-options
                             (lambda (kw) 
                               (list *standard-class-metaclass-name*) ) ))
         (metaclass-name (car metaclass-options))
         ;; Attention: metaclass-allocation-args should be immediate constants
         (metaclass-allocation-args (cdr metaclass-options))
         (metaclass (symbol->class 
                     metaclass-name complain-if-missing-meta-class ))
         (depth (fx+ 1 (careless-Class-depth super-class)))
         (class (oo-apply (Class-allocator metaclass)
                          (fx+ 1 (fx* 2 depth)) metaclass-allocation-args )) )
    ;; initialize name and super-class immediately
    (careless-initialize-Class-name class name)
    (careless-initialize-Class-super-number
     class (careless-Class-number super-class) )
    (Class-parse! class 
                  own-field-descs 
                  (append class-options *default-class-options*) ) ) )

;;; As soon as a Class instance is created, it is asked to parse the
;;; rest of its definition. It creates as soon as possible instances
;;; of Field representing its fields and ask them to parse their
;;; definition. Every determining fact is used to update the
;;; class or field instances (by side-effect) then code is generated.

(define (Class-parse! class own-fields-desc class-options)
  ;; UMB Scheme does not accept symbols starting with a minus sign.
  (let* ((class-variable (symbol-concatenate (Class-name class) '- 'class))
         (super-class (Class-super-class class))
         (super-fields (Class-fields (Class-super-class class)))
         (preceding-fields 
          (cons 'fields (append (oo-map instance-clone super-fields)
                                '() )) ) ; false head
         (own-fields-code
          (let scan ((descs own-fields-desc))
            (if (pair? descs)
                (let ((field-code (parse-field! (car descs) 
                                                preceding-fields 
                                                class-options )))
                  (cons field-code (scan (cdr descs))) )
                '() ) ) ) )
    ;; set up all (inherited and proper) field descriptors. Also
    ;; remove the false head!
    (careless-initialize-Class-fields class (cdr preceding-fields))
    ;; Registering a prototype is done only if not already present
    (cond ((find-option-present? 'prototype class-options)
           (unless (symbol->class (careless-Class-name class)
                                  (lambda (name) #f) )
             ;; register the class so it can be subclassed at expand-time:
             (register-class meroon-revision class (Class-name class) 
                             super-class 'void ) )
           (generate-class-prototype-check class class-options) )
          (else 
           ;; register the class so it can be subclassed at expand-time:
           (register-class meroon-revision class (Class-name class) 
                           super-class 'void )
           (generate-class-registration 
            class own-fields-code class-options ) ) ) ) )

;;; Generate code to register, at load time, a class accompanied
;;; by all the necessary functions to access slots.
;;; Generate all the names that accompany a class. They are all formed
;;; on the name of the class. Accessors to fields are also generated.
;;; CLASS is the class object at generation-time, CLASS-VARIABLE is the name
;;; of the variable that contain the associated load-time class. It
;;; currently defines many names:
;;;       <class>-class to hold the class object (this eliminates
;;;              the :eponymous option of Meroon V2).
;;;       <class>-<field> for functions (readers) to read fields.
;;;       set-<class>-<field>! for functions to write fields.
;;;       <class>? to recognize instances (direct or indirect) of <class>.
;;;       make-<class> to allocate fully initialized instances of <class>.
;;;       allocate-<class> to allocate uninitialized instances.
;;;       -><class> a generic function to coerce things into <class> instances.
;;; Remark: The great problem is that this consumes many names. Some of which
;;; may be eliminated in favor of more general (not necessarily
;;; generic) functions. For instance, is-a? and <class>-class can be
;;; used instead if <class>? and field-value instead of
;;; <class>-<field>. On the other hand, all these substitutions are
;;; slower, require the user to accompany the definition of classes
;;; with many other functions to recognize, access instances. Another
;;; interesting problem is that the choice to use <class>-<field> to
;;; access instances, if handy, is costly. It requires a lot of
;;; variables, a lot of functions as well as a lot of redundancy.
;;; Suppose that we have:
;;;          (define-class Point         Object (x y))
;;;          (define-class Colored-Point Point  (color))
;;; Then Point-x and Colored-Point-x are equivalent except that the
;;; latter only accepts Colored-Points instead of Points. Another solution
;;; (adopted by Dylan, for instance) is to have a generic function
;;; named x to access this field. In CLOS, acccessors are methods that
;;; are added to a user-specified generic function. We do not want to
;;; have this property since it is then difficult to compile well at
;;; the first place since the generic function can be added new
;;; methods that violate the compilation hypotheses. This is lessened
;;; if there is dynamic recompilation and/or sealing mechanisms.

(define (generate-class-registration class own-fields-code class-options)
  (let* ((name (Class-name class))
         ;; UMB Scheme does not accept symbols starting with a minus sign.
         (class-variable (symbol-concatenate name '- 'class)) )
    `(begin
       ,(if-meroon-feature bootstrap
            `#t 
            (generate-class-definition class own-fields-code class-options) )
       ;; Generates the names of associated functions
       ,(generate-accompanying-functions class class-options)
       ',name ) ) )

(define-temporary (generate-accompanying-functions class class-options)
  (cond ((Handy-Class? class)
         (Handy-Class-generate-accompanying-functions class class-options) )
        ((Class? class)
         (Class-generate-accompanying-functions class class-options) )
        (else (report-meroon-error 'internal 'domain
               "No such method" 'generate-accompanying-functions )) ) )

(define (Class-generate-accompanying-functions class class-options)
  `'**no-accompanying-functions** )

(define (generate-class-definition class own-fields-code class-options)
  (let* ((name (Class-name class))
         ;; UMB Scheme does not accept symbols starting with a minus sign.
         (class-variable (symbol-concatenate name '- 'class))
         (metaclass-name (symbol-concatenate (Class-name (object->class class))
                                             '- 'class ))
         (super-class (Class-super-class class))
         (super-name (Class-name super-class))
         (super-variable (symbol-concatenate super-name '- 'class))
         (metaclass-options (find-option-values 
                             'metaclass class-options
                             (lambda (kw) '(Class)) ))
         (metaclass-allocation-args (cdr metaclass-options)) )
    ;; classes are regular instances that must be given to initialize!
    `(meroon-define
      ,class-variable
      (initialize!
       (fill-other-fields!
       (register-class 
	',meroon-revision
	((Class-allocator ,metaclass-name)
	 ,(Class-suprel-length class) ,@metaclass-allocation-args )
	',name
	,super-variable
	((let ()(declare (standard-bindings))list) ,@own-fields-code) ) ) ) ) ) )

;;; Generate code from the prototypical CLASS. This poses some
;;; interesting problems of compatibility. Should the metaclass be the
;;; same ? etc. 
;;; Note: There is a problem discovered by Lance Norskog <thinman@netcom.com>
;;; whenever a file contains (define-class x object () :prototype) followed by
;;; (define-class x object () ). In this case, Meroon V3 does not check 
;;; anything and hope it is well.

(define (generate-class-prototype-check class class-options)
  (let ((name (Class-name class))
        (super-name (Class-name (Class-super-class class))) )
    ;; Check compatibility between a prototype and the real class,
    ;; but only if the class already exists. If it does not exist then
    ;; cross your fingers and hope anything is well.
    `(let ((c (symbol->class ',name (lambda (name) #f))))
       (if c
           (check-fields 
            c
            ',super-name
            ',(oo-map (lambda (f)
                        (cons (Field-name f) (Class-name (object->class f))) )
                      (Class-fields class) ) ) )
       ',name ) ) )

;;;======================================================= define field
;;; Parse descriptions of fields, create Field instances, insert them
;;; physically in the list of preceding-fields and return equivalent
;;; code. The field-description is either 
;;;    a name                            describing a Mono-Field 
;;;    (= name other-field-options...)   describing a Mono-Field 
;;;    (* name other-field-options...)   describing a Poly-Field
;;;    (name other-field-options... :metaclass classname sizes...)
;;;                                      describing a specific Field 

(define (parse-field! field-desc preceding-fields class-options)
  (let ((path-length (fx+ 1 (count-Poly-Fields preceding-fields))))
    (cond
     ((symbol? field-desc)
      (let* ((field-class (symbol->class *standard-mono-field-metaclass-name*))
             (field ((Class-allocator field-class) path-length) ))
        (Field-parse! field (cons field-desc class-options) 
                      preceding-fields ) ) )
     ((and (pair? field-desc)           ; at least a field name
           (pair? (cdr field-desc)) )
      (case (car field-desc)
        ((= mono) 
         ;; When the = abbreviation is used, :metaclass should not occur.
         ;; Thanks to Josep Riverola (Sep 1st, 1995).
         (if (find-option-values 'metaclass field-desc (lambda ignore #f))
             (report-meroon-error
              'Syntax 'define-class
              "Superfluous (or contradictory) :metaclass option" field-desc) )
         (let* ((field-class 
                 (symbol->class *standard-mono-field-metaclass-name*) )
                (field ((Class-allocator field-class) path-length) ))
           (Field-parse! field (append (cdr field-desc) class-options)
                         preceding-fields ) ) )
        ((* poly)
         ;; When the = abbreviation is used, :metaclass should not occur.
         (if (find-option-values 'metaclass field-desc (lambda ignore #f))
             (report-meroon-error
              'Syntax 'define-class
              "Superfluous (or contradictory) :metaclass option" field-desc) )
         (let* ((field-class 
                 (symbol->class *standard-poly-field-metaclass-name*) )
                (field ((Class-allocator field-class) path-length) ))
           (Field-parse! field (append (cdr field-desc) class-options)
                         preceding-fields ) ) )
        (else (let ((values (find-option-values 
                             'metaclass field-desc
                             option-not-there )))
                (if (pair? values)
                    (let ((field (oo-apply (Class-allocator 
                                            (symbol->class (car values)) )
                                           path-length
                                           (cdr values) )))
                      (Field-parse! field 
                                    ;; leave the metaclass options.
                                    (append field-desc class-options) 
                                    preceding-fields ) )
                    (report-meroon-error 
                     'Syntax 'define-class
                     "Invalid field descriptor" field-desc ) ) ) ) ) )
     (else (report-meroon-error 
            'Syntax 'define-class
            "Invalid field description" field-desc ) ) ) ) )

;;; Ask FIELD to parse the rest of its definition, update the field
;;; and generate the appropriate code tot regenerate this object at
;;; load-time. Note that the initializer is the result of a
;;; computation and will only appear in the generated field and not
;;; the one which is current at expand-time. The other uninitialized
;;; fields will be setup by fill-other-fields!

;;; FUTURE: Does not check for unknown options. It is not possible to
;;; use a metaclass with more than one Poly-Field other than path.

(define (Field-parse! field field-options preceding-fields)
  (let ((name (car field-options)))
    (unless (symbol? name)
      (report-meroon-error 
       'Syntax 'define-class "Not a field name" name ) )
    ;; check if the field is not already defined. Remember that
    ;; preceding-fields has a false head!
    (when (sequence-find name (cdr preceding-fields) Field-name 
                         (lambda (field) #t)
                         (lambda (name) #f) )
      (report-meroon-error 
       'Syntax 'define-class "Field redefinition" name ) )
    ;; set the name
    (careless-initialize-Field-name field name)
    ;; set offsets (before appending field to preceding-fields)
    (set-important-offsets! field (cdr preceding-fields))
    ;; append (physically) this field to the preceding fields
    (append! preceding-fields (list field))
    ;; set the mutability 
    (when (and (find-option-present? 'immutable field-options) 
               (find-option-present? 'mutable field-options) )
      (report-meroon-error 'Syntax 'define-class 
                           "Incompatible mutability options" field-options ) )
    (unless (field-defined? field 'immutable?)
      (careless-initialize-Field-immutable?
       field (find-option-present? 'immutable field-options) ) )
    ;; This means that this field may be uninitialized
    (let ((uninit (find-option-present? 'maybe-uninitialized field-options))
          (initer (find-option-single-value 'initializer field-options
                                            (lambda (kw) #f) )) )
      (careless-initialize-Field-initialized?
       ;; when there is an initializer, the field is always initialized
       ;; it is also the case when :maybe-uninitialized is not specified.
       ;; So if the value of the initialized? field is true, then the field
       ;; CANNOT contain meroon-uninitialized.
       field (or initer (not uninit)) )
      ;; The initializer field must be correctly set since
      ;; macroexpansion of instantiate forms requires it. But due to
      ;; the lack of eval, we can only set it to an error-producer.
      ;; This code is related to a bug found by riverola@iese.es
      ;; (Josep Riverola): see oo3.tst.
      (when initer
        (careless-initialize-Field-initializer
         field (lambda ignore 
                 (report-meroon-error
                  'Syntax 'instantiate
                  "Unavailable initializer for this field"
                  field ) ) ) )
      ;; generate final code
      `(instantiate ,(Class-name (object->class field))
         ,(make-meroon-keyword 'name) ',name
         ,(make-meroon-keyword 'immutable?) ,(Field-immutable? field)
         ,(make-meroon-keyword 'initialized?) ,(Field-initialized? field)
         ,@(if initer
               `(,(make-meroon-keyword 'initializer) ,initer)
               `() )
         ,(make-meroon-keyword 'path)
         ,@(let ((limit (Field-path-length field)))
                   (let enum ((i 0))
                     (if (fx< i limit)
                         (cons (Field-path field i)
                               (enum (fx+ i 1)) )
                         '() ) ) ) ) ) ) )

;;;============================================================= Define-generic
;;; There are some optimizations here. 
;;; For (define-generic (foo (x) y) body), 
;;;       foo will be bound to (lambda (x y) ((compute-method) x y))
;;;       default method will be (lambda (x y) body)
;;; For (define-generic (foo (x) y . z) body), 
;;;       foo will be bound to (lambda (x y . z) ((compute-method) x y z))
;;;       default method will be (lambda (x y z) body)
;;; This avoids consing again and again as well as using the slow apply. 
;;; For (define-generic (foo (x) y #!kw ...) body), 
;;;       foo will be bound to (lambda (x y . z) (apply (compute-method) x y z))
;;;       default method will be (lambda (x y #!kw ...) body)
;;; Here we pay a high price but who cares now ?

;;; Find discrimination specifications (aka pairs of (variable-name
;;; [class])) in SPECS. Useful for parsing define-generic and
;;; define-method. Discriminating variables must appear before DSSSL
;;; keywords if any. 

(if-meroon-feature (or DSSSL)
  (define (extract-discriminant specs)
    (if (pair? specs)
        (cond 
         ((or (dsssl-optional? (car specs))
              (dsssl-rest? (car specs))
              (dsssl-key? (car specs)) )
          ;; stop parsing at first DSSSL key, discriminant
          ;; variables should appear before.
          '() )
         (else (if (pair? (car specs))
                   (cons (car specs) (extract-discriminant (cdr specs)))
                   (extract-discriminant (cdr specs)) )) )
        '() ) )
  ;; Regular R4RS rules
  (define (extract-discriminant specs)
    (if (pair? specs)
        (if (pair? (car specs))
            (cons (car specs) (extract-discriminant (cdr specs)))
            (extract-discriminant (cdr specs)) )
        '() ) )
  )

;;; Convert a list of variables into a reduced list of variables. This
;;; latter will be used to bind arguments to the variables of the
;;; generic function. DSSSL keywords if any are converted into a
;;; dotted variable.

(if-meroon-feature (or DSSSL)
  (define (reduce-variables specs)
    (if (pair? specs)
        (cond 
         ((or (dsssl-optional? (car specs))
              (dsssl-rest? (car specs))
              (dsssl-key? (car specs)) )
          ;; Handle all DSSSL keywords as a dotted variable.
          (if (pair? (cdr specs))
              (let search-symbol ((s (cadr specs)))
                (if (symbol? s) 
                    s
                    (if (pair? s) 
                        (search-symbol (car s))
                        (report-meroon-error 'Syntax 'define-generic
                                             "Bad variable list" specs ) ) ) )
              (report-meroon-error 'Syntax 'define-generic
                                   "Very bad variable list" specs ) ) )
         (else (if (pair? (car specs))
                   (cons (caar specs) (reduce-variables (cdr specs)))
                   (cons (car specs) (reduce-variables (cdr specs))) )) )
        specs ) )
  ;; Regular R4RS
  (define (reduce-variables specs)
    (if (pair? specs)
        (if (pair? (car specs))
            (cons (caar specs) (reduce-variables (cdr specs)))
            (cons (car specs) (reduce-variables (cdr specs))) ) 
        specs ) )
)

;;; Flat formals to handle the dotted variable if any. This is needed
;;; to rebuild a correct lambda-list.

(define (flat-formals formals)
  (if (pair? formals)
      (cons (car formals) (flat-formals (cdr formals)))
      (if (null? formals)
          formals
          (list formals) ) ) )

;;; Extract the list of all variables that appear in a lambda list.
;;; Take care of DSSSL keywords. This is needed to build a list of
;;; bound variables to code-walk the body of handy methods.

(if-meroon-feature (or DSSSL)
  (define (extract-formals formals)
    (if (pair? formals)
        (cond
         ((or (dsssl-optional? (car formals))
              (dsssl-rest? (car formals))
              (dsssl-key? (car formals)) )
          (extract-formals (cdr formals)))
         (else (if (pair? (car formals))
                   (cons (caar formals) (extract-formals (cdr formals)) )
                   (cons (car formals) (extract-formals (cdr formals)) ) )) )
        (if (null? formals) formals (list formals)) ) )
  ;; R4RS regular rules
  (define (extract-formals formals)
    (flat-formals formals) )
)

;;; Extract the list of variables for a method. This only flattens
;;; discriminant variables.

(if-meroon-feature (or DSSSL)
  (define (rebuild-method-variable-list specs)
    (if (pair? specs)
        (cond
         ((or (dsssl-optional? (car specs))
              (dsssl-rest? (car specs))
              (dsssl-key? (car specs)) )
          specs )
         (else (if (pair? (car specs))
                   (cons (caar specs) 
                         (rebuild-method-variable-list (cdr specs)) )
                   (cons (car specs)
                         (rebuild-method-variable-list (cdr specs)) ) )) )
        specs ) )
  ;; Regular R4RS
  (define (rebuild-method-variable-list specs)
    (flat-formals (reduce-variables specs)) )
)

;;; Check a little the syntax of a define-generic form.

(define (check-generic-definition call body)
  (unless (and (pair? call)
               (symbol? (car call))
               (pair? (cdr call)) )
    (report-meroon-error 
     'Syntax 'define-generic "Incorrect definition" call ) ) )

;;; Syntax: (define-generic (foo x (y) . z) [ . default-body])
;;; Methods as well as the default method are represented by fixed
;;; arity functions. For instance, the previous default method is
;;; (lambda (x y z) default-body). To try to lessen the size of the
;;; expanded code, anything that can be factorized is put in a
;;; register-* function. The behavior part is no longer needed.

(define (process-define-generic-form call body)
  (check-generic-definition call body)
  (let ((disc-specs (extract-discriminant (cdr call)))
        (variables-for-generic (reduce-variables (cdr call)))
        (dsssl? #f)
        (variables-for-default-method (cdr call))
        (generic (gensym))
        (rest-var (gensym)) )
    (when (fx< (length disc-specs) 1)
      (report-meroon-error 'Syntax 'define-generic
                           "No discriminating variable" call ) )
    ;; The generation depends on the use of DSSSL keywords.
    (if-meroon-feature (or DSSSL)
      (set! dsssl? (or (any? dsssl-optional? (cdr call))
                       (any? dsssl-rest? (cdr call))
                       (any? dsssl-key? (cdr call)) ))
      'nothing )
    `(begin
       (meroon-define ,(car call)
         (let ((,generic 'wait))
           ;; The real generic instance:
           (set! ,generic 
                 (,(case (length disc-specs)
                     ((1)  `register-Generic-1)
                     (else `register-Generic-N) )
                  ',meroon-revision     ; Meroon revision
                  ',(car call)          ; generic name
                  ,(if (pair? body)     ; default method
                       `(lambda ,(if dsssl?
                                     (rebuild-method-variable-list (cdr call))
                                     (flat-formals variables-for-generic) )
                          ,@body )
                       `meroon-uninitialized )
                  ',(cdr call)          ; signature 
                  ,@(oo-map (lambda (spec) ; top-class-names
                              (if (pair? (cdr spec)) 
                                  (list 'quote (cadr spec)) 
                                  ;; #f means anything rather than Object.
                                  `#f ) )
                            disc-specs ) ) )
           ;; Variant for Scheme implementations with applyable objects.
           ,(if-meroon-feature applyable-object
              generic
              ;; the equivalent functional image for Scheme
              `(lambda ,variables-for-generic
                 ;; This is used to count the number of generic calls.
                 ,@(if-meroon-feature profile
                     `((set! *generic-call-counter* 
                             (fx+ 1 *generic-call-counter*) ))
                     `() )
                 (,@(if dsssl? '(apply) '())
                  (,(case (length disc-specs)
                      ((1)  `careless-determine-method1)
                      (else `determine-method) )
                   ,generic ,@(oo-map car disc-specs) )
                  ,@(flat-formals variables-for-generic) ) ) ) ) ) ) ) )

;;; Changed expansion of define-generic into two independent
;;; definitions. The interest is to gain a static definition of the
;;; generic image but which requires a global variable to hold the
;;; associated generic instance. That is because most compilers do not
;;; recognize that (define f (letrec (...) ... (lambda ...))) to be a
;;; function with a known arity. We therefore pollute global space
;;; with new generated name prefixed by meroon.

;;; This variables counts the number of generic functions. To profile
;;; your code, redefine the define-generic macro with the `profile'
;;; feature added.

(define *generic-call-counter* 0)

;;;============================================================== Define-method

;;; Check a little the syntax of a define-method form.

(define (check-method-definition call body)
  (unless (and (pair? call)
               (symbol? (car call)) )
    (report-meroon-error 
     'Syntax 'define-method "Incorrect definition" call ) )
  (unless (pair? body)
    (report-meroon-error
     'Syntax 'define-method "Empty body" call ) ) )

;;; Syntax: (define-method (foo x (y class) . z) . body)
;;; The current scheme transforms methods into fix arity functions so
;;; the dotted previous method is represented by 
;;;     (lambda (x y z) . body)
;;; In the expansion of a method definition, a pre-method is generated that
;;; will accept, at run-time, two arguments: the generic function and
;;; the class on which it will be added. As before a call to a
;;; register-* function is generated. The version of Meroon under
;;; which this is generated will be checked against the version uder
;;; which it will be loaded.

(define (process-define-method-form call body)
  (check-method-definition call body)
  (let ((disc-specs (extract-discriminant (cdr call)))
        (variables-for-generic (reduce-variables (cdr call)))
        (variables-for-method (cdr call))
        (dsssl? #f)
        (g (gensym)) 
        (c (gensym))
        (d (gensym)) )
    (unless (fx>= (length disc-specs) 1)
      (report-meroon-error 'Syntax 'define-method
                           "No discriminating variables" call ) )
    (for-each (lambda (spec) 
                (unless (pair? (cdr spec))
                  (report-meroon-error 
                   'Syntax 'define-method 
                   "No discriminating class" disc-specs ) ) )
               disc-specs )
    ;; The generation depends on the use of DSSSL keywords.
    (if-meroon-feature (or DSSSL)
      (set! dsssl? (or (any? dsssl-optional? (cdr call))
                       (any? dsssl-rest? (cdr call))
                       (any? dsssl-key? (cdr call)) ))
      'nothing )
    `(register-method
      ',(car call)                      ; generic name
      ',(cdr call)                      ; generic signature
      ',meroon-revision                 ; Meroon revision
      (lambda (,g ,c ,d)                ; pre-method
        (lambda ,(if dsssl? 
                     (rebuild-method-variable-list variables-for-method)
                     (flat-formals variables-for-generic) )
          ,@(next-method-code-if-needed 
             dsssl? (cdr call) body g c d disc-specs )
          ,@body ) )
      ,@(oo-map (lambda (spec)          ; discriminating class names 
                  `(quote ,(cadr spec)) ) 
                disc-specs ) ) ) )

;;; The defining macro define-handy-method defines a method with an
;;; implicit with-access form around its body. The variables that
;;; appear in the variable list are left as they are even if there are
;;; fields with the same name. It is thus possible to write:
;;;   (define-handy-method (move-to (o Point) x)
;;;      (set! y (+ y x)) )
;;; This is equivalent to (x is not converted into (Point- o)):
;;;   (define-method (move-to (o Point) x)
;;;      (set-Point-y! o (+ (Point-y o) x)) )
;;; But in fact it is expanded into (implementation dependent):
;;;   (define-method (move-to (o Point) x)
;;;      (instance-set! o 2 (+ (instance-ref o 2) x)) )

;;; define-handy-method only handles mono-discriminating methods since
;;; there might be some clashes between the names of the fields of the
;;; classes of the multiple discriminating variables [thinman@netcom.com].

(define (process-define-handy-method call body)
  (check-method-definition call body)
  (let ((disc-specs (extract-discriminant (cdr call)))
        (variables-for-generic (reduce-variables (cdr call)))
        (variables-for-method (cdr call))
        (dsssl? #f)
        (g (gensym)) 
        (c (gensym))
        (d (gensym)) )
    (unless (fx= (length disc-specs) 1)
      (report-meroon-error 'Syntax 'define-handy-method
            "Only one discriminating variable is supported" call ) )
    (for-each (lambda (spec) 
                (unless (pair? (cdr spec))
                  (report-meroon-error 
                   'Syntax 'define-method 
                   "No discriminating class" disc-specs ) ) )
              disc-specs )
    ;; The generation depends on the use of DSSSL keywords.
    (if-meroon-feature (or DSSSL)
      (set! dsssl? (or (any? dsssl-optional? (cdr call))
                       (any? dsssl-rest? (cdr call))
                       (any? dsssl-key? (cdr call)) ))
      'nothing )
    `(register-method
      ',(car call)                      ; generic name
      ',(cdr call)                      ; generic signature
      ',meroon-revision                 ; Meroon revision
      (lambda (,g ,c ,d)                ; pre-method
        (lambda ,(if dsssl? 
                     (rebuild-method-variable-list variables-for-method)
                     (flat-formals variables-for-generic) )
          ,@(next-method-code-if-needed 
             dsssl? (cdr call) body g c d disc-specs )
          ,(generate-with-access 
            #f 
            (car (car disc-specs))
            (->Class (cadr (car disc-specs)))
            (Class-fields (->Class (cadr (car disc-specs))))
            body
            (extract-formals (cdr call)) ) ) )
      ,@(oo-map (lambda (spec)          ; discriminating class names 
                  `(quote ,(cadr spec)) ) 
                disc-specs ) ) ) )

;;; Generate the code for (call-next-method) and/or (next-method?)
;;; if needed. Just check the presence of these words somehere in the
;;; body of the method. DISC-SPECS is given to this function so it can
;;; adapt the generated code to find-method or find-multi-method which
;;; computes differently their next methods.

(define (next-method-code-if-needed dsssl? variables body g c d disc-specs)
  (define csn (gensym))
  (case (length disc-specs)
    ((1)                                ; mono-methods
     (append
      (if (appear? 'call-next-method body)
          (list
           ;; Call the super-method with the same arguments.
           `(define (call-next-method)
              (let ((,csn (Class-super-number ,c)))
                (,@(if dsssl? '(apply) '())
                 (or (and ,csn
                          (find-method1 (careless-Generic-dispatcher ,g)
                                        (number->class ,csn) ) )
                     (Generic-default ,g) )
                 ;; This code allows to assign these variables which is
                 ;; unsafe, fortunately users do not know that !?
                 ,@(reduce-variables variables) ) ) ) )
          '() )
      (if (appear? 'next-method? body)
          (list
           `(define (next-method?)
              (let ((,csn (Class-super-number ,c)))
                ;; returns T if there is another method other than default.
                (and ,csn
                     (find-method1 (careless-Generic-dispatcher ,g)
                                   (number->class ,csn) ) ) ) ) )
          '() ) ) )
    (else                               ; multi-methods
     (append
      (if (appear? 'call-next-method body)
          (list
           ;; Call the super-method with the same arguments.
           `(define (call-next-method)
              (,@(if dsssl? '(apply) '())
               (or (find-multi-method 
                    ;; Avoid retraversing the whole dispatcher
                    (Linear-Dispatcher-no ,d)
                    (Linear-Dispatcher-signature ,d) )
                   (Generic-default ,g) )
               ,@(reduce-variables variables) ) ) )
          '() )
      (if (appear? 'next-method? body)
          (list
           `(define (next-method?)
              (find-multi-method (Linear-Dispatcher-no ,d)
                                 (Linear-Dispatcher-signature ,d) )) )
          '() ) ) ) ) )

;;; Roughly check if a word appears in a definition:

(define (appear? kw e)
  (define (appear e)
    (or (eq? e kw)
        (and (pair? e)
             (or (appear (car e))
                 (appear (cdr e)) ) ) ) )
  (appear e) )

;;; end of definers.scm
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
;;; $Id: genes1.scm,v 1.1 2005/02/25 22:19:37 lucier Exp $ 
;;; Copyright (c) 1990-97 by Christian Queinnec. All rights reserved.

;;;                ***********************************************
;;;                    Small, Efficient and Innovative Class System
;;;                                    Meroon 
;;;                              Christian Queinnec  
;;;                    Ecole Polytechnique & INRIA--Rocquencourt
;;;                ************************************************

;;; This file defines the initial net of classes, generics and methods
;;; that are needed by Meroon to run. It is a delicate piece of
;;; bootstrap.

;;; Build by hand the initial net of classes with all the relationship
;;; needed to bootstrap the rest of the file. These are the numbers
;;; associated to the first classes to be created. The function
;;; get-new-class-number returns sequential numbers from 0.

(define original-Object-class-number                (get-new-class-number))
(define original-Pre-Class-class-number             (get-new-class-number))
(define original-Class-class-number                 (get-new-class-number))
(define original-Handy-Class-class-number           (get-new-class-number))
(define original-MeroonV2-Class-class-number        (get-new-class-number))
(define original-Applyable-Object-class-number      (get-new-class-number))
(define original-Generic-class-number               (get-new-class-number))
(define original-Generic-1-class-number             (get-new-class-number))
(define original-Generic-N-class-number             (get-new-class-number))
(define original-Pre-Field-class-number             (get-new-class-number))
(define original-Field-class-number                 (get-new-class-number))
(define original-Mono-Field-class-number            (get-new-class-number))
(define original-Poly-Field-class-number            (get-new-class-number))
(define original-Dispatcher-class-number            (get-new-class-number))
(define original-Immediate-Dispatcher-class-number  (get-new-class-number))
(define original-Subclass-Dispatcher-class-number   (get-new-class-number))
(define original-Indexed-Dispatcher-class-number    (get-new-class-number))
(define original-Linear-Dispatcher-class-number     (get-new-class-number))
(define original-Global-Dispatcher-class-number     (get-new-class-number))
(define original-Tracing-Dispatcher-class-number    (get-new-class-number))
(define original-Anomaly-class-number               (get-new-class-number))
(define original-Warning-class-number               (get-new-class-number))
(define original-View-class-number                  (get-new-class-number))
(define original-Virtual-Field-class-number         (get-new-class-number))

;;;================================================================ creator

;;; create a Mono-Field (by hand) 
(define (create-Mono-Field immutable? name cn . path)
  (create-some-field original-Mono-Field-class-number immutable? name cn path) )

;;; create a Poly-Field (by hand) 
(define (create-Poly-Field immutable? name cn . path)
  (create-some-field original-Poly-Field-class-number immutable? name cn path) )

(define (create-some-field tcn immutable? name cn path)
  (let ((f (allocate-empty-instance tcn (fx+ 5 (fx+ 1 (length path))))))
    (instance-set! f 0 name)            ; name
    (instance-set! f 1 immutable?)      ; immutable?
    (instance-set! f 2 cn)              ; class-number
    (instance-set! f 3 #f)              ; initialized?
                                        ; initializer
    (instance-set! f 5 (length path))   ; path
    (let stuff ((args path)
                (i 6) )
      (if (pair? args)
          (begin (instance-set! f i (car args))
                 (stuff (cdr args) (fx+ i 1)) )
          f ) ) ) )

;;; Concatenate two list of fields. Fields in FIELDS1 are restricted
;;; to be Mono-Fields with a path-length of 1.

(define (simple-append-fields fields1 fields2)
  (let ((start (length fields1)))
    (append (oo-map instance-clone fields1)
            (oo-map (lambda (field index)
                      ;; (set-Field-path! field 0 index)
                      (instance-set! field (fx+ 5 1) index)
                      field )
                    fields2
                    (iota start (fx+ start (length fields2))) ) ) ) )

;;;========================================================= Object
(define Object-class 
  (instance
   original-Class-class-number          ; internal index (Object is a Class)
   'Object                              ; name
   original-Object-class-number         ; number
   (list)                               ; fields
   0                                    ; depth
   #f                                   ; super-number
   (list                                ; subclass-numbers
    original-Class-class-number
    original-Applyable-Object-class-number
    original-Pre-Field-class-number
    original-Dispatcher-class-number 
    original-Anomaly-class-number
    original-View-class-number )
   (fx+ 1 original-Virtual-Field-class-number)      ; next
   meroon-uninitialized                 ; allocator
   #t                                   ; immutable?
   (list)                               ; views
   1                                    ; supers and relative numbers
   original-Object-class-number         ; super[0] = relnum[0]
   ) )

;;;========================================================= Class
(define Pre-Class-class
  (instance
   original-Class-class-number          ; internal index (Class is a Class)
   'Pre-Class                           ; name
   original-Pre-Class-class-number      ; number
   (list                                ; fields
    (create-Mono-Field #t 'name             original-Pre-Class-class-number 0)
    (create-Mono-Field #t 'number           original-Pre-Class-class-number 1)
    (create-Mono-Field #t 'fields           original-Pre-Class-class-number 2)
    )
   1                                    ; depth
   original-Object-class-number         ; super-number
   (list                                ; subclasses
    original-Class-class-number
    original-View-class-number )
   5                                    ; next
   meroon-uninitialized                 ; allocator
   #f                                   ; immutable?
   (list)                               ; views
   3
   original-Object-class-number         ; super[0]
   original-Pre-Class-class-number      ; super[1] = relnum[0]
   0                                    ; relnum[1]
   ) )

(define Class-class
  (instance
   original-Class-class-number          ; internal index (Class is a Class)
   'Class                               ; name
   original-Class-class-number          ; number
   (simple-append-fields                ; fields
    (careless-Class-fields Pre-Class-class)
    (list
     (create-Mono-Field #t 'depth            original-Class-class-number 3)
     (create-Mono-Field #t 'super-number     original-Class-class-number 4)
     (create-Mono-Field #f 'subclass-numbers original-Class-class-number 5)
     (create-Mono-Field #f 'next             original-Class-class-number 6)
     (create-Mono-Field #t 'allocator        original-Class-class-number 7)
     (create-Mono-Field #t 'immutable?       original-Class-class-number 8)
     (create-Mono-Field #t 'views            original-Class-class-number 9)
     (create-Poly-Field #t 'suprel           original-Class-class-number 10) ) )
   2                                    ; depth
   original-Pre-Class-class-number      ; super-number
   (list                                ; subclasses
    original-Handy-Class-class-number )
   3                                    ; next
   meroon-uninitialized                 ; allocator
   #f                                   ; immutable?
   (list)                               ; views
   5
   original-Object-class-number         ; super[0]
   original-Pre-Class-class-number      ; super[1] 
   original-Class-class-number          ; super[2] = relnum[0]
   1                                    ; relnum[1]
   0                                    ; relnum[2]
   ) )

;;;========================================================= Class
(define Handy-Class-class
  (instance
   original-Class-class-number          ; internal index 
   'Handy-Class                         ; name
   original-Handy-Class-class-number    ; number
   (simple-append-fields                ; fields
    (careless-Class-fields Class-class)
    (list) )
   3                                    ; depth
   original-Class-class-number          ; super-number
   (list                                ; subclasses
    original-MeroonV2-Class-class-number )
   2                                    ; next
   meroon-uninitialized                 ; allocator
   #f                                   ; immutable?
   (list)                               ; views
   7
   original-Object-class-number         ; super[0]
   original-Pre-Class-class-number      ; super[1]
   original-Class-class-number          ; super[2]
   original-Handy-Class-class-number    ; super[3] = relnum[0]
   2                                    ; relnum[1]
   1                                    ; relnum[2]
   0                                    ; relnum[3]
   ) )

;;;========================================================= Meroon V2 Class
(define MeroonV2-Class-class
  (instance
   original-Class-class-number          ; internal index 
   'MeroonV2-Class                      ; name
   original-MeroonV2-Class-class-number ; number
   (simple-append-fields                ; fields
    (careless-Class-fields Class-class)
    (list) )
   4                                    ; depth
   original-Handy-Class-class-number    ; super-number
   (list)                               ; subclasses
   1                                    ; next
   meroon-uninitialized                 ; allocator
   #f                                   ; immutable?
   (list)                               ; views
   9
   original-Object-class-number         ; super[0]
   original-Pre-Class-class-number      ; super[1]
   original-Class-class-number          ; super[2]
   original-Handy-Class-class-number    ; super[3]
   original-MeroonV2-Class-class-number ; super[4] = relnum[0]
   3                                    ; relnum[1]
   2                                    ; relnum[2]
   1                                    ; relnum[3]
   0                                    ; relnum[4]
   ) )

;;;========================================================= Applyable-Object
;;; This is the Class intended for objects that can act simultaneously
;;; as an object and a function. This is not possible in portable Scheme.
;;; This class defines pseudo-fields just to skip over the header of
;;; functional objects.

(define Applyable-Object-class
  (instance
   original-Class-class-number          ; internal index 
   'Applyable-Object                    ; name
   original-Applyable-Object-class-number ; number
   (if-meroon-feature applyable-object  ; fields
     (list    )
     (list) )
   1                                    ; depth
   original-Object-class-number         ; super-number
   (list                                ; subclasses
    original-Generic-class-number)
   4                                    ; next
   meroon-uninitialized                 ; allocator
   #t                                   ; immutable?
   (list)                               ; views
   3
   original-Object-class-number         ; super[0]
   original-Applyable-Object-class-number ; super[1] = relnum[0]
   0                                    ; relnum[1]
   ) )

;;;========================================================= Generic
;;; Since Generic inherits from Applyable-Object, the offsets for fields
;;; that appear in this definition may be translated by a fix offset.

(define Generic-class
  (instance
   original-Class-class-number          ; internal index 
   'Generic                             ; name
   original-Generic-class-number        ; number
   (simple-append-fields                ; fields
    (careless-Class-fields Applyable-Object-class)
    (list
     (create-Mono-Field #t 'name         original-Generic-class-number 0)
     (create-Mono-Field #f 'default      original-Generic-class-number 1)
     (create-Mono-Field #t 'variables    original-Generic-class-number 2)
     (create-Mono-Field #f 'dispatcher   original-Generic-class-number 3)
     (create-Mono-Field #t 'top-classes  original-Generic-class-number 4) ) )
   2                                    ; depth
   original-Applyable-Object-class-number ; super-number
   (list                                ; subclasses
    original-Generic-1-class-number
    original-Generic-N-class-number )
   3                                    ; next
   meroon-uninitialized                 ; allocator
   #f                                   ; immutable?
   (list)                               ; views
   5
   original-Object-class-number         ; super[0]
   original-Applyable-Object-class-number ; super[1]
   original-Generic-class-number        ; super[2] = relnum[0]
   1                                    ; relnum[1]
   0                                    ; relnum[2]
   ) )

;;;========================================================= Generic-1
(define Generic-1-class
  (instance
   original-Class-class-number          ; internal index 
   'Generic-1                           ; name
   original-Generic-1-class-number      ; number
   (simple-append-fields                ; fields
    (careless-Class-fields Generic-class)
    (list) )
   3                                    ; depth
   original-Generic-class-number        ; super-number
   (list)                               ; subclasses
   1                                    ; next
   meroon-uninitialized                 ; allocator
   #f                                   ; immutable?
   (list)                               ; views
   7
   original-Object-class-number         ; super[0]
   original-Applyable-Object-class-number ; super[1]
   original-Generic-class-number        ; super[2]
   original-Generic-1-class-number      ; super[3] = relnum[0]
   2                                    ; relnum[1]
   1                                    ; relnum[2]
   0                                    ; relnum[3]
   ) )

;;;========================================================= Generic-N
(define Generic-N-class
  (instance
   original-Class-class-number          ; internal index 
   'Generic-N                           ; name
   original-Generic-N-class-number      ; number
   (simple-append-fields                ; fields
    (careless-Class-fields Generic-class)
    (list) )
   3                                    ; depth
   original-Generic-class-number        ; super-number
   (list)                               ; subclasses
   1                                    ; next
   meroon-uninitialized                 ; allocator
   #f                                   ; immutable?
   (list)                               ; views
   7
   original-Object-class-number         ; super[0]
   original-Applyable-Object-class-number ; super[1]
   original-Generic-class-number        ; super[2]
   original-Generic-N-class-number      ; super[3] = relnum[0]
   3                                    ; relnum[1]
   2                                    ; relnum[2]
   0                                    ; relnum[3]
   ) )

;;; end of genes1.scm
;;; $Id: genes2.scm,v 1.1 2005/02/25 22:19:37 lucier Exp lucier $ 
;;; Copyright (c) 1990-97 by Christian Queinnec. All rights reserved.

;;;                ***********************************************
;;;                    Small, Efficient and Innovative Class System
;;;                                    Meroon 
;;;                              Christian Queinnec  
;;;                    Ecole Polytechnique & INRIA--Rocquencourt
;;;                ************************************************

;;; This file defines the end of the initial net of classes, generics
;;; and methods that are needed by Meroon to run. It is a delicate
;;; piece of bootstrap.

;;;========================================================= Field
;;; The initializer field is now mutable.

(define Pre-Field-class
  (instance
   original-Class-class-number          ; internal index 
   'Pre-Field                           ; name
   original-Pre-Field-class-number      ; number
   (list                                ; fields
    (create-Mono-Field #t 'name         original-Pre-Field-class-number 0) )
   1                                    ; depth
   original-Object-class-number         ; super-number
   (list                                ; subclasses
    original-Field-class-number )
   4                                    ; next
   meroon-uninitialized                 ; allocator
   #f                                   ; immutable?
   (list)                               ; views
   3
   original-Object-class-number         ; super[0]
   original-Pre-Field-class-number       ; super[1] = relnum[0]
   0                                    ; relnum[1]
   ) )

(define Field-class
  (instance
   original-Class-class-number          ; internal index 
   'Field                               ; name
   original-Field-class-number          ; number
   (simple-append-fields                ; fields
    (careless-Class-fields Pre-Field-class)
    (list                     
     (create-Mono-Field #t 'immutable?   original-Field-class-number 1)
     (create-Mono-Field #f 'class-number original-Field-class-number 2)
     (create-Mono-Field #t 'initialized? original-Field-class-number 3)
     (create-Mono-Field #f 'initializer  original-Field-class-number 4)
     (create-Poly-Field #t 'path         original-Field-class-number 5) ))
   2                                    ; depth
   original-Pre-Field-class-number      ; super-number
   (list                                ; subclasses
    original-Mono-Field-class-number
    original-Poly-Field-class-number )
   3                                    ; next
   meroon-uninitialized                 ; allocator
   #f                                   ; immutable?
   (list)                               ; views
   5
   original-Object-class-number         ; super[0]
   original-Pre-Field-class-number       ; super[1] 
   original-Field-class-number          ; super[2] = relnum[0]
   1                                    ; relnum[1]
   0                                    ; relnum[2]
   ) )

;;;========================================================= Mono-Field
(define Mono-Field-class
  (instance
   original-Class-class-number          ; internal index 
   'Mono-Field                          ; name
   original-Mono-Field-class-number     ; number
   (simple-append-fields                ; fields
    (careless-Class-fields Field-class)
    (list) )
   3                                    ; depth
   original-Field-class-number          ; super-number
   (list)                               ; subclasses
   1                                    ; next
   meroon-uninitialized                 ; allocator
   #f                                   ; immutable?
   (list)                               ; views
   7
   original-Object-class-number         ; super[0]
   original-Pre-Field-class-number       ; super[1]
   original-Field-class-number          ; super[2]
   original-Mono-Field-class-number     ; super[3] = relnum[0]
   2                                    ; relnum[1]
   1                                    ; relnum[2]
   0                                    ; relnum[3]
   ) )

;;;========================================================= Poly-Field
(define Poly-Field-class
  (instance
   original-Class-class-number          ; internal index 
   'Poly-Field                          ; name
   original-Poly-Field-class-number     ; number
   (simple-append-fields                ; fields
    (careless-Class-fields Field-class)
    (list) )
   3                                    ; depth
   original-Field-class-number          ; super-number
   (list)                               ; subclasses
   1                                    ; next
   meroon-uninitialized                 ; allocator
   #f                                   ; immutable?
   (list)                               ; views
   7
   original-Object-class-number         ; super[0]
   original-Pre-Field-class-number       ; super[1]
   original-Field-class-number          ; super[2]
   original-Poly-Field-class-number     ; super[3] = relnum[0]
   3                                    ; relnum[1]
   2                                    ; relnum[2]
   0                                    ; relnum[3]
   ) )

;;;========================================================= Dispatcher
(define Dispatcher-class
  (instance
   original-Class-class-number          ; internal index 
   'Dispatcher                          ; name
   original-Dispatcher-class-number     ; number
   (list                                ; fields
    (create-Mono-Field #t 'method-finder original-Dispatcher-class-number 0) )
   1                                    ; depth
   original-Object-class-number         ; super-number
   (list                                ; subclasses
    original-Immediate-Dispatcher-class-number
    original-Subclass-Dispatcher-class-number
    original-Indexed-Dispatcher-class-number
    original-Linear-Dispatcher-class-number
    original-Global-Dispatcher-class-number
    original-Tracing-Dispatcher-class-number
    )
   7                                    ; next
   meroon-uninitialized                 ; allocator
   #t                                   ; immutable?
   (list)                               ; views
   3
   original-Object-class-number         ; super[0]
   original-Dispatcher-class-number     ; super[1] = relnum[0]
   0                                    ; relnum[1]
   ) )

;;;======================================================= Immediate-Dispatcher
(define Immediate-Dispatcher-class
  (instance
   original-Class-class-number          ; internal index 
   'Immediate-Dispatcher                ; name
   original-Immediate-Dispatcher-class-number ; number
   (simple-append-fields                ; fields
    (careless-Class-fields Dispatcher-class)
    (list
     (create-Mono-Field #t 'method  
                        original-Immediate-Dispatcher-class-number 1 ) ) )
   2                                    ; depth
   original-Dispatcher-class-number     ; super-number
   (list)                               ; subclasses
   1                                    ; next
   meroon-uninitialized                 ; allocator
   #t                                   ; immutable?
   (list)                               ; views
   5
   original-Object-class-number         ; super[0]
   original-Dispatcher-class-number     ; super[1]
   original-Immediate-Dispatcher-class-number ; super[2] = relnum[0]
   1                                    ; relnum[1]
   0                                    ; relnum[2]
   ) )

;;;======================================================= Subclass-Dispatcher
(define Subclass-Dispatcher-class
  (instance
   original-Class-class-number          ; internal index 
   'Subclass-Dispatcher                 ; name
   original-Subclass-Dispatcher-class-number ; number
   (simple-append-fields                ; fields
    (careless-Class-fields Dispatcher-class)
    (list
     (create-Mono-Field #t 'class-number 
                        original-Subclass-Dispatcher-class-number 1)
     (create-Mono-Field #t 'class-depth
                        original-Subclass-Dispatcher-class-number 2)
     (create-Mono-Field #f 'no
                        original-Subclass-Dispatcher-class-number 3)
     (create-Mono-Field #f 'yes
                        original-Subclass-Dispatcher-class-number 4) ) )
   2                                    ; depth
   original-Dispatcher-class-number     ; super-number
   (list)                               ; subclasses
   1                                    ; next
   meroon-uninitialized                 ; allocator
   #t                                   ; immutable?
   (list)                               ; views
   5
   original-Object-class-number         ; super[0]
   original-Dispatcher-class-number     ; super[1]
   original-Subclass-Dispatcher-class-number ; super[2] = relnum[0]
   2                                    ; relnum[1]
   0                                    ; relnum[2]
   ) )

;;;======================================================= Indexed-Dispatcher
(define Indexed-Dispatcher-class
  (instance
   original-Class-class-number          ; internal index 
   'Indexed-Dispatcher                  ; name
   original-Indexed-Dispatcher-class-number ; number
   (simple-append-fields                ; fields
    (careless-Class-fields Dispatcher-class)
    (list
     (create-Mono-Field #t 'class-number
                        original-Indexed-Dispatcher-class-number 1)
     (create-Mono-Field #t 'class-depth
                        original-Indexed-Dispatcher-class-number 2)
     (create-Mono-Field #f 'no
                        original-Indexed-Dispatcher-class-number 3)
     (create-Poly-Field #f 'method
                        original-Indexed-Dispatcher-class-number 4) ) )
   2                                    ; depth
   original-Dispatcher-class-number     ; super-number
   (list)                               ; subclasses
   1                                    ; next
   meroon-uninitialized                 ; allocator
   #t                                   ; immutable?
   (list)                               ; views
   5
   original-Object-class-number         ; super[0]
   original-Dispatcher-class-number     ; super[1]
   original-Indexed-Dispatcher-class-number ; super[2] = relnum[0]
   3                                    ; relnum[1]
   0                                    ; relnum[2]
   ) )

;;;======================================================= Linear-Dispatcher
(define Linear-Dispatcher-class
  (instance
   original-Class-class-number          ; internal index 
   'Linear-Dispatcher                   ; name
   original-Linear-Dispatcher-class-number ; number
   (simple-append-fields                ; fields
    (careless-Class-fields Dispatcher-class)
    (list
     (create-Mono-Field #f 'no
                        original-Linear-Dispatcher-class-number 1)
     (create-Mono-Field #f 'method
                        original-Linear-Dispatcher-class-number 2)
     (create-Mono-Field #f 'signature
                        original-Linear-Dispatcher-class-number 3) ) )
   2                                    ; depth
   original-Dispatcher-class-number     ; super-number
   (list)                               ; subclasses
   1                                    ; next
   meroon-uninitialized                 ; allocator
   #t                                   ; immutable?
   (list)                               ; views
   5
   original-Object-class-number         ; super[0]
   original-Dispatcher-class-number     ; super[1]
   original-Linear-Dispatcher-class-number ; super[2] = relnum[0]
   4                                    ; relnum[1]
   0                                    ; relnum[2]
   ) )

;;;======================================================= Global-Dispatcher
(define Global-Dispatcher-class
  (instance
   original-Class-class-number          ; internal index 
   'Global-Dispatcher                   ; name
   original-Global-Dispatcher-class-number ; number
   (simple-append-fields                ; fields
    (careless-Class-fields Dispatcher-class)
    (list
     (create-Poly-Field #f 'method
                        original-Global-Dispatcher-class-number 1) ) )
   2                                    ; depth
   original-Dispatcher-class-number     ; super-number
   (list)                               ; subclasses
   1                                    ; next
   meroon-uninitialized                 ; allocator
   #t                                   ; immutable?
   (list)                               ; views
   5
   original-Object-class-number         ; super[0]
   original-Dispatcher-class-number     ; super[1]
   original-Global-Dispatcher-class-number ; super[2] = relnum[0]
   5                                    ; relnum[1]
   0                                    ; relnum[2]
   ) )

;;;======================================================= Tracing-Dispatcher
(define Tracing-Dispatcher-class
  (instance
   original-Class-class-number          ; internal index 
   'Tracing-Dispatcher                  ; name
   original-Tracing-Dispatcher-class-number ; number
   (simple-append-fields                ; fields
    (careless-Class-fields Dispatcher-class)
    (list
     ;; adding methods to a traced dispatcher requires this field to be mutable.
     (create-Mono-Field #f 'dispatcher 
                        original-Tracing-Dispatcher-class-number 1 )
     (create-Mono-Field #t 'default
                        original-Tracing-Dispatcher-class-number 2 ) ) )
   2                                    ; depth
   original-Dispatcher-class-number     ; super-number
   (list)                               ; subclasses
   1                                    ; next
   meroon-uninitialized                 ; allocator
   #t                                   ; immutable?
   (list)                               ; views
   5
   original-Object-class-number         ; super[0]
   original-Dispatcher-class-number     ; super[1]
   original-Tracing-Dispatcher-class-number ; super[2] = relnum[0]
   6                                    ; relnum[1]
   0                                    ; relnum[2]
   ) )

;;;======================================================= Anomaly
(define Anomaly-class
  (instance
   original-Class-class-number          ; internal index 
   'Anomaly                             ; name
   original-Anomaly-class-number        ; number
   (list                                ; fields
    (create-Mono-Field #t 'category    original-Anomaly-class-number 0)
    (create-Mono-Field #t 'operator    original-Anomaly-class-number 1)
    (create-Mono-Field #t 'message     original-Anomaly-class-number 2)
    (create-Poly-Field #t 'hint        original-Anomaly-class-number 3)
    )
   1                                    ; depth
   original-Object-class-number         ; super-number
   (list                                ; subclasses
    original-Warning-class-number )
   2                                    ; next
   meroon-uninitialized                 ; allocator
   #t                                   ; immutable?
   (list)                               ; views
   3
   original-Object-class-number         ; super[0]
   original-Anomaly-class-number        ; super[1] = relnum[0]
   0                                    ; relnum[1]
   ) )

(define Warning-class
  (instance
   original-Class-class-number          ; internal index 
   'Warning                             ; name
   original-Warning-class-number        ; number
   (simple-append-fields                ; fields
    (careless-Class-fields Anomaly-class)
    (list) )
   2                                    ; depth
   original-Anomaly-class-number        ; super-number
   (list)                               ; subclasses
   1                                    ; next
   meroon-uninitialized                 ; allocator
   #t                                   ; immutable?
   (list)                               ; views
   5
   original-Object-class-number         ; super[0]
   original-Anomaly-class-number        ; super[1]
   original-Warning-class-number        ; super[2] = relnum[0]
   1                                    ; relnum[1]
   0                                    ; relnum[0]
   ) )

;;;======================================================== View

(define View-class
  (instance
   original-Class-class-number          ; internal index 
   'View                                ; name
   original-View-class-number           ; number
   (simple-append-fields                ; fields
    (careless-Class-fields Pre-Class-class)
    (list
     (create-Poly-Field #t 'super         original-View-class-number 3)
     ) )
   2                                    ; depth
   original-Pre-Class-class-number      ; super-number
   (list                                ; subclasses
     )
   1                                    ; next
   meroon-uninitialized                 ; allocator
   #t                                   ; immutable?
   (list)                               ; views
   5
   original-Object-class-number         ; super[0]
   original-Pre-Class-class-number      ; super[1]
   original-View-class-number           ; super[2] = relnum[0]
   0                                    ; relnum[1]
   0                                    ; relnum[2]
   ) )

(define Virtual-Field-class
  (instance
   original-Class-class-number          ; internal index 
   'Virtual-Field                       ; name
   original-Virtual-Field-class-number  ; number
   (simple-append-fields                ; fields
    (careless-Class-fields Pre-Field-class)
    (list                     
     (create-Mono-Field #t 'view   original-Virtual-Field-class-number 1)
     (create-Mono-Field #f 'index  original-Virtual-Field-class-number 2) ))
   2                                    ; depth
   original-Pre-Field-class-number      ; super-number
   (list)                               ; subclasses
   1                                    ; next
   meroon-uninitialized                 ; allocator
   #f                                   ; immutable?
   (list)                               ; views
   5
   original-Object-class-number         ; super[0]
   original-Pre-Field-class-number      ; super[1] 
   original-Virtual-Field-class-number  ; super[2] = relnum[0]
   4                                    ; relnum[1]
   0                                    ; relnum[2]
   ) )

;;;oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; Register all these classes in the sequence of all classes.

(for-each 
 (lambda (class) 
   (vector-set! *classes* (careless-Class-number class) class) )
 (let ((classes 
        (list 
         Object-class                   ; 0
         Pre-Class-class                ; 1
         Class-class                    ; 2
         Handy-Class-class              ; 3
         MeroonV2-Class-class           ; 4
         Applyable-Object-class         ; 5
         Generic-class                  ; 6
         Generic-1-class                ; 7
         Generic-N-class                ; 8
         Pre-Field-class                ; 9
         Field-class                    ; 10
         Mono-Field-class               ; 11
         Poly-Field-class               ; 12
         Dispatcher-class               ; 13
         Immediate-Dispatcher-class     ; 14
         Subclass-Dispatcher-class      ; 15
         Indexed-Dispatcher-class       ; 16
         Linear-Dispatcher-class        ; 17
         Global-Dispatcher-class        ; 18
         Tracing-Dispatcher-class       ; 19
         Anomaly-class                  ; 20
         Warning-class                  ; 21
         View-class                     ; 22
         Virtual-Field-class            ; 23
         ) ))
   (unless (fx= *class-number* (length classes))
     (oo-error 'genesis "Missed some classes") )
   classes ) )

;;; end of genes2.scm
;;; File automatically generated (Don't touch).

(meroon-define Pre-Class? (create-predicate Pre-Class-class))

(meroon-define Class? (create-predicate Class-class))

(meroon-define Handy-Class? (create-predicate Handy-Class-class))

(meroon-define MeroonV2-Class? (create-predicate MeroonV2-Class-class))

(meroon-define Applyable-Object? (create-predicate Applyable-Object-class))

(meroon-define Generic? (create-predicate Generic-class))

(meroon-define Generic-1? (create-predicate Generic-1-class))

(meroon-define Generic-N? (create-predicate Generic-N-class))

(meroon-define Pre-Field? (create-predicate Pre-Field-class))

(meroon-define Field? (create-predicate Field-class))

(meroon-define Mono-Field? (create-predicate Mono-Field-class))

(meroon-define Poly-Field? (create-predicate Poly-Field-class))

(meroon-define Dispatcher? (create-predicate Dispatcher-class))

(meroon-define Immediate-Dispatcher? (create-predicate Immediate-Dispatcher-class))

(meroon-define Subclass-Dispatcher? (create-predicate Subclass-Dispatcher-class))

(meroon-define Indexed-Dispatcher? (create-predicate Indexed-Dispatcher-class))

(meroon-define Linear-Dispatcher? (create-predicate Linear-Dispatcher-class))

(meroon-define Global-Dispatcher? (create-predicate Global-Dispatcher-class))

(meroon-define Tracing-Dispatcher? (create-predicate Tracing-Dispatcher-class))

(meroon-define Anomaly? (create-predicate Anomaly-class))

(meroon-define Warning? (create-predicate Warning-class))

(meroon-define View? (create-predicate View-class))

(meroon-define Virtual-Field? (create-predicate Virtual-Field-class))

;;; File automatically generated (Don't touch).





(meroon-define Pre-Class-name (Mono-Field-create-careful-reader Pre-Class-class 'name))

(meroon-define Pre-Class-number (Mono-Field-create-careful-reader Pre-Class-class 'number))

(meroon-define Pre-Class-fields (Mono-Field-create-careful-reader Pre-Class-class 'fields))




(meroon-define Class-name (Mono-Field-create-careful-reader Class-class 'name))

(meroon-define Class-number (Mono-Field-create-careful-reader Class-class 'number))

(meroon-define Class-fields (Mono-Field-create-careful-reader Class-class 'fields))

(meroon-define Class-depth (Mono-Field-create-careful-reader Class-class 'depth))

(meroon-define Class-super-number (Mono-Field-create-careful-reader Class-class 'super-number))

(meroon-define Class-subclass-numbers (Mono-Field-create-careful-reader Class-class 'subclass-numbers))
(meroon-define set-Class-subclass-numbers! (Mono-Field-create-careful-writer Class-class 'subclass-numbers))

(meroon-define Class-next (Mono-Field-create-careful-reader Class-class 'next))
(meroon-define set-Class-next! (Mono-Field-create-careful-writer Class-class 'next))

(meroon-define Class-allocator (Mono-Field-create-careful-reader Class-class 'allocator))

(meroon-define Class-immutable? (Mono-Field-create-careful-reader Class-class 'immutable?))

(meroon-define Class-views (Mono-Field-create-careful-reader Class-class 'views))

(meroon-define Class-suprel-length (Poly-Field-create-careful-lengther Class-class 'suprel))
(meroon-define Class-suprel (Poly-Field-create-careful-reader Class-class 'suprel))




(meroon-define Handy-Class-name (Mono-Field-create-careful-reader Handy-Class-class 'name))

(meroon-define Handy-Class-number (Mono-Field-create-careful-reader Handy-Class-class 'number))

(meroon-define Handy-Class-fields (Mono-Field-create-careful-reader Handy-Class-class 'fields))

(meroon-define Handy-Class-depth (Mono-Field-create-careful-reader Handy-Class-class 'depth))

(meroon-define Handy-Class-super-number (Mono-Field-create-careful-reader Handy-Class-class 'super-number))

(meroon-define Handy-Class-subclass-numbers (Mono-Field-create-careful-reader Handy-Class-class 'subclass-numbers))
(meroon-define set-Handy-Class-subclass-numbers! (Mono-Field-create-careful-writer Handy-Class-class 'subclass-numbers))

(meroon-define Handy-Class-next (Mono-Field-create-careful-reader Handy-Class-class 'next))
(meroon-define set-Handy-Class-next! (Mono-Field-create-careful-writer Handy-Class-class 'next))

(meroon-define Handy-Class-allocator (Mono-Field-create-careful-reader Handy-Class-class 'allocator))

(meroon-define Handy-Class-immutable? (Mono-Field-create-careful-reader Handy-Class-class 'immutable?))

(meroon-define Handy-Class-views (Mono-Field-create-careful-reader Handy-Class-class 'views))

(meroon-define Handy-Class-suprel-length (Poly-Field-create-careful-lengther Handy-Class-class 'suprel))
(meroon-define Handy-Class-suprel (Poly-Field-create-careful-reader Handy-Class-class 'suprel))




(meroon-define MeroonV2-Class-name (Mono-Field-create-careful-reader MeroonV2-Class-class 'name))

(meroon-define MeroonV2-Class-number (Mono-Field-create-careful-reader MeroonV2-Class-class 'number))

(meroon-define MeroonV2-Class-fields (Mono-Field-create-careful-reader MeroonV2-Class-class 'fields))

(meroon-define MeroonV2-Class-depth (Mono-Field-create-careful-reader MeroonV2-Class-class 'depth))

(meroon-define MeroonV2-Class-super-number (Mono-Field-create-careful-reader MeroonV2-Class-class 'super-number))

(meroon-define MeroonV2-Class-subclass-numbers (Mono-Field-create-careful-reader MeroonV2-Class-class 'subclass-numbers))
(meroon-define set-MeroonV2-Class-subclass-numbers! (Mono-Field-create-careful-writer MeroonV2-Class-class 'subclass-numbers))

(meroon-define MeroonV2-Class-next (Mono-Field-create-careful-reader MeroonV2-Class-class 'next))
(meroon-define set-MeroonV2-Class-next! (Mono-Field-create-careful-writer MeroonV2-Class-class 'next))

(meroon-define MeroonV2-Class-allocator (Mono-Field-create-careful-reader MeroonV2-Class-class 'allocator))

(meroon-define MeroonV2-Class-immutable? (Mono-Field-create-careful-reader MeroonV2-Class-class 'immutable?))

(meroon-define MeroonV2-Class-views (Mono-Field-create-careful-reader MeroonV2-Class-class 'views))

(meroon-define MeroonV2-Class-suprel-length (Poly-Field-create-careful-lengther MeroonV2-Class-class 'suprel))
(meroon-define MeroonV2-Class-suprel (Poly-Field-create-careful-reader MeroonV2-Class-class 'suprel))







(meroon-define Generic-name (Mono-Field-create-careful-reader Generic-class 'name))

(meroon-define Generic-default (Mono-Field-create-careful-reader Generic-class 'default))
(meroon-define set-Generic-default! (Mono-Field-create-careful-writer Generic-class 'default))

(meroon-define Generic-variables (Mono-Field-create-careful-reader Generic-class 'variables))

(meroon-define Generic-dispatcher (Mono-Field-create-careful-reader Generic-class 'dispatcher))
(meroon-define set-Generic-dispatcher! (Mono-Field-create-careful-writer Generic-class 'dispatcher))

(meroon-define Generic-top-classes (Mono-Field-create-careful-reader Generic-class 'top-classes))




(meroon-define Generic-1-name (Mono-Field-create-careful-reader Generic-1-class 'name))

(meroon-define Generic-1-default (Mono-Field-create-careful-reader Generic-1-class 'default))
(meroon-define set-Generic-1-default! (Mono-Field-create-careful-writer Generic-1-class 'default))

(meroon-define Generic-1-variables (Mono-Field-create-careful-reader Generic-1-class 'variables))

(meroon-define Generic-1-dispatcher (Mono-Field-create-careful-reader Generic-1-class 'dispatcher))
(meroon-define set-Generic-1-dispatcher! (Mono-Field-create-careful-writer Generic-1-class 'dispatcher))

(meroon-define Generic-1-top-classes (Mono-Field-create-careful-reader Generic-1-class 'top-classes))




(meroon-define Generic-N-name (Mono-Field-create-careful-reader Generic-N-class 'name))

(meroon-define Generic-N-default (Mono-Field-create-careful-reader Generic-N-class 'default))
(meroon-define set-Generic-N-default! (Mono-Field-create-careful-writer Generic-N-class 'default))

(meroon-define Generic-N-variables (Mono-Field-create-careful-reader Generic-N-class 'variables))

(meroon-define Generic-N-dispatcher (Mono-Field-create-careful-reader Generic-N-class 'dispatcher))
(meroon-define set-Generic-N-dispatcher! (Mono-Field-create-careful-writer Generic-N-class 'dispatcher))

(meroon-define Generic-N-top-classes (Mono-Field-create-careful-reader Generic-N-class 'top-classes))




(meroon-define Pre-Field-name (Mono-Field-create-careful-reader Pre-Field-class 'name))




(meroon-define Field-name (Mono-Field-create-careful-reader Field-class 'name))

(meroon-define Field-immutable? (Mono-Field-create-careful-reader Field-class 'immutable?))

(meroon-define Field-class-number (Mono-Field-create-careful-reader Field-class 'class-number))
(meroon-define set-Field-class-number! (Mono-Field-create-careful-writer Field-class 'class-number))

(meroon-define Field-initialized? (Mono-Field-create-careful-reader Field-class 'initialized?))

(meroon-define Field-initializer (Mono-Field-create-careful-reader Field-class 'initializer))
(meroon-define set-Field-initializer! (Mono-Field-create-careful-writer Field-class 'initializer))

(meroon-define Field-path-length (Poly-Field-create-careful-lengther Field-class 'path))
(meroon-define Field-path (Poly-Field-create-careful-reader Field-class 'path))




(meroon-define Mono-Field-name (Mono-Field-create-careful-reader Mono-Field-class 'name))

(meroon-define Mono-Field-immutable? (Mono-Field-create-careful-reader Mono-Field-class 'immutable?))

(meroon-define Mono-Field-class-number (Mono-Field-create-careful-reader Mono-Field-class 'class-number))
(meroon-define set-Mono-Field-class-number! (Mono-Field-create-careful-writer Mono-Field-class 'class-number))

(meroon-define Mono-Field-initialized? (Mono-Field-create-careful-reader Mono-Field-class 'initialized?))

(meroon-define Mono-Field-initializer (Mono-Field-create-careful-reader Mono-Field-class 'initializer))
(meroon-define set-Mono-Field-initializer! (Mono-Field-create-careful-writer Mono-Field-class 'initializer))

(meroon-define Mono-Field-path-length (Poly-Field-create-careful-lengther Mono-Field-class 'path))
(meroon-define Mono-Field-path (Poly-Field-create-careful-reader Mono-Field-class 'path))




(meroon-define Poly-Field-name (Mono-Field-create-careful-reader Poly-Field-class 'name))

(meroon-define Poly-Field-immutable? (Mono-Field-create-careful-reader Poly-Field-class 'immutable?))

(meroon-define Poly-Field-class-number (Mono-Field-create-careful-reader Poly-Field-class 'class-number))
(meroon-define set-Poly-Field-class-number! (Mono-Field-create-careful-writer Poly-Field-class 'class-number))

(meroon-define Poly-Field-initialized? (Mono-Field-create-careful-reader Poly-Field-class 'initialized?))

(meroon-define Poly-Field-initializer (Mono-Field-create-careful-reader Poly-Field-class 'initializer))
(meroon-define set-Poly-Field-initializer! (Mono-Field-create-careful-writer Poly-Field-class 'initializer))

(meroon-define Poly-Field-path-length (Poly-Field-create-careful-lengther Poly-Field-class 'path))
(meroon-define Poly-Field-path (Poly-Field-create-careful-reader Poly-Field-class 'path))




(meroon-define Dispatcher-method-finder (Mono-Field-create-careful-reader Dispatcher-class 'method-finder))




(meroon-define Immediate-Dispatcher-method-finder (Mono-Field-create-careful-reader Immediate-Dispatcher-class 'method-finder))

(meroon-define Immediate-Dispatcher-method (Mono-Field-create-careful-reader Immediate-Dispatcher-class 'method))




(meroon-define Subclass-Dispatcher-method-finder (Mono-Field-create-careful-reader Subclass-Dispatcher-class 'method-finder))

(meroon-define Subclass-Dispatcher-class-number (Mono-Field-create-careful-reader Subclass-Dispatcher-class 'class-number))

(meroon-define Subclass-Dispatcher-class-depth (Mono-Field-create-careful-reader Subclass-Dispatcher-class 'class-depth))

(meroon-define Subclass-Dispatcher-no (Mono-Field-create-careful-reader Subclass-Dispatcher-class 'no))
(meroon-define set-Subclass-Dispatcher-no! (Mono-Field-create-careful-writer Subclass-Dispatcher-class 'no))

(meroon-define Subclass-Dispatcher-yes (Mono-Field-create-careful-reader Subclass-Dispatcher-class 'yes))
(meroon-define set-Subclass-Dispatcher-yes! (Mono-Field-create-careful-writer Subclass-Dispatcher-class 'yes))




(meroon-define Indexed-Dispatcher-method-finder (Mono-Field-create-careful-reader Indexed-Dispatcher-class 'method-finder))

(meroon-define Indexed-Dispatcher-class-number (Mono-Field-create-careful-reader Indexed-Dispatcher-class 'class-number))

(meroon-define Indexed-Dispatcher-class-depth (Mono-Field-create-careful-reader Indexed-Dispatcher-class 'class-depth))

(meroon-define Indexed-Dispatcher-no (Mono-Field-create-careful-reader Indexed-Dispatcher-class 'no))
(meroon-define set-Indexed-Dispatcher-no! (Mono-Field-create-careful-writer Indexed-Dispatcher-class 'no))

(meroon-define Indexed-Dispatcher-method-length (Poly-Field-create-careful-lengther Indexed-Dispatcher-class 'method))
(meroon-define Indexed-Dispatcher-method (Poly-Field-create-careful-reader Indexed-Dispatcher-class 'method))
(meroon-define set-Indexed-Dispatcher-method! (Poly-Field-create-careful-writer Indexed-Dispatcher-class 'method))




(meroon-define Linear-Dispatcher-method-finder (Mono-Field-create-careful-reader Linear-Dispatcher-class 'method-finder))

(meroon-define Linear-Dispatcher-no (Mono-Field-create-careful-reader Linear-Dispatcher-class 'no))
(meroon-define set-Linear-Dispatcher-no! (Mono-Field-create-careful-writer Linear-Dispatcher-class 'no))

(meroon-define Linear-Dispatcher-method (Mono-Field-create-careful-reader Linear-Dispatcher-class 'method))
(meroon-define set-Linear-Dispatcher-method! (Mono-Field-create-careful-writer Linear-Dispatcher-class 'method))

(meroon-define Linear-Dispatcher-signature (Mono-Field-create-careful-reader Linear-Dispatcher-class 'signature))
(meroon-define set-Linear-Dispatcher-signature! (Mono-Field-create-careful-writer Linear-Dispatcher-class 'signature))




(meroon-define Global-Dispatcher-method-finder (Mono-Field-create-careful-reader Global-Dispatcher-class 'method-finder))

(meroon-define Global-Dispatcher-method-length (Poly-Field-create-careful-lengther Global-Dispatcher-class 'method))
(meroon-define Global-Dispatcher-method (Poly-Field-create-careful-reader Global-Dispatcher-class 'method))
(meroon-define set-Global-Dispatcher-method! (Poly-Field-create-careful-writer Global-Dispatcher-class 'method))




(meroon-define Tracing-Dispatcher-method-finder (Mono-Field-create-careful-reader Tracing-Dispatcher-class 'method-finder))

(meroon-define Tracing-Dispatcher-dispatcher (Mono-Field-create-careful-reader Tracing-Dispatcher-class 'dispatcher))
(meroon-define set-Tracing-Dispatcher-dispatcher! (Mono-Field-create-careful-writer Tracing-Dispatcher-class 'dispatcher))

(meroon-define Tracing-Dispatcher-default (Mono-Field-create-careful-reader Tracing-Dispatcher-class 'default))




(meroon-define Anomaly-category (Mono-Field-create-careful-reader Anomaly-class 'category))

(meroon-define Anomaly-operator (Mono-Field-create-careful-reader Anomaly-class 'operator))

(meroon-define Anomaly-message (Mono-Field-create-careful-reader Anomaly-class 'message))

(meroon-define Anomaly-hint-length (Poly-Field-create-careful-lengther Anomaly-class 'hint))
(meroon-define Anomaly-hint (Poly-Field-create-careful-reader Anomaly-class 'hint))




(meroon-define Warning-category (Mono-Field-create-careful-reader Warning-class 'category))

(meroon-define Warning-operator (Mono-Field-create-careful-reader Warning-class 'operator))

(meroon-define Warning-message (Mono-Field-create-careful-reader Warning-class 'message))

(meroon-define Warning-hint-length (Poly-Field-create-careful-lengther Warning-class 'hint))
(meroon-define Warning-hint (Poly-Field-create-careful-reader Warning-class 'hint))




(meroon-define View-name (Mono-Field-create-careful-reader View-class 'name))

(meroon-define View-number (Mono-Field-create-careful-reader View-class 'number))

(meroon-define View-fields (Mono-Field-create-careful-reader View-class 'fields))

(meroon-define View-super-length (Poly-Field-create-careful-lengther View-class 'super))
(meroon-define View-super (Poly-Field-create-careful-reader View-class 'super))




(meroon-define Virtual-Field-name (Mono-Field-create-careful-reader Virtual-Field-class 'name))

(meroon-define Virtual-Field-view (Mono-Field-create-careful-reader Virtual-Field-class 'view))

(meroon-define Virtual-Field-index (Mono-Field-create-careful-reader Virtual-Field-class 'index))
(meroon-define set-Virtual-Field-index! (Mono-Field-create-careful-writer Virtual-Field-class 'index))



;;; File automatically generated (Don't touch).

(meroon-define make-Object (make-maker Object-class))

(meroon-define make-Pre-Class (make-maker Pre-Class-class))

(meroon-define make-Class (make-maker Class-class))

(meroon-define make-Handy-Class (make-maker Handy-Class-class))

(meroon-define make-MeroonV2-Class (make-maker MeroonV2-Class-class))

(meroon-define make-Applyable-Object (make-maker Applyable-Object-class))

(meroon-define make-Generic (make-maker Generic-class))

(meroon-define make-Generic-1 (make-maker Generic-1-class))

(meroon-define make-Generic-N (make-maker Generic-N-class))

(meroon-define make-Pre-Field (make-maker Pre-Field-class))

(meroon-define make-Field (make-maker Field-class))

(meroon-define make-Mono-Field (make-maker Mono-Field-class))

(meroon-define make-Poly-Field (make-maker Poly-Field-class))

(meroon-define make-Dispatcher (make-maker Dispatcher-class))

(meroon-define make-Immediate-Dispatcher (make-maker Immediate-Dispatcher-class))

(meroon-define make-Subclass-Dispatcher (make-maker Subclass-Dispatcher-class))

(meroon-define make-Indexed-Dispatcher (make-maker Indexed-Dispatcher-class))

(meroon-define make-Linear-Dispatcher (make-maker Linear-Dispatcher-class))

(meroon-define make-Global-Dispatcher (make-maker Global-Dispatcher-class))

(meroon-define make-Tracing-Dispatcher (make-maker Tracing-Dispatcher-class))

(meroon-define make-Anomaly (make-maker Anomaly-class))

(meroon-define make-Warning (make-maker Warning-class))

(meroon-define make-View (make-maker View-class))

(meroon-define make-Virtual-Field (make-maker Virtual-Field-class))

;;; end of Basics.scm
;;; $Id: fill.scm,v 1.1 2005/02/25 22:19:37 lucier Exp $
;;; Copyright (c) 1990-96 by Christian Queinnec. All rights reserved.

;;;                ***********************************************
;;;                    Small, Efficient and Innovative Class System
;;;                                    Meroon 
;;;                              Christian Queinnec  
;;;                    Ecole Polytechnique & INRIA--Rocquencourt
;;;                ************************************************

;;; This file defines the instantiate special form which is the general
;;; form to instantiate instances. The major feature is that the class
;;; must be statically known ie cannot be computed. The instantiate
;;; form is analyzed and converted into a direct allocation if
;;; possible otherwise to an allocation followed by various
;;; initializations. For example suppose that we have a class defined as:
;;;    (define-class ColoredPoint Object 
;;;          ((= x :maybe-uninitialized :initializer (lambda () 0))
;;;           (= y :initializer (lambda () 100))
;;;           (= color) ) )
;;; The form (instantiate ColoredPoint :y 33 :color 'pink :x 44) will
;;; generate the equivalent of: (make-ColoredPoint 44 33 'pink).
;;; The form (instantiate ColoredPoint :y 33 :color 'pink) will
;;; generate the equivalent of: 
;;;   (fill-other-fields! (make-ColoredPoint meroon-uninitialized 33 'pink)).

;;; This file is the first to be loaded after genesis.scm, so we must
;;; finish the bootstrap and setup a default allocator for al
;;; predefined classes.

(sequence-map (lambda (class)
                (when class
                  (careless-initialize-Class-allocator
                   class (create-allocator class) ) ) )
              *classes* )

;;; The initialize! generic function is invoked on any freshly created
;;; instance. This function can be customized by the user to count
;;; instances, memoize them etc. The function fill-other-fields! is invoked
;;; on every fresh instance that is statically known to contain at
;;; least one uninitialized field.

(define (process-instantiate-form class-name parms)
  (process-instantiation 
   (symbol->class class-name
                  (lambda (name) 
                    (report-meroon-error 
                     'Syntax 'instantiate "Unknown class" name ) ) )
   parms ) )

;;; If it is possible to allocate the instance in one functional call,
;;; do it but only if the number of arguments does not exceed this
;;; number (check it with your native Scheme). For instance, if you
;;; have a class C with more than *maximal-explicit-allocation-size*
;;; Mono-fields then a totally explicit form as (instance C :f1 v1 :f2
;;; v2 ...  ... :fn vn) will not be turned into (make-C v1 v2 ... vn)
;;; but into (let ((i (allocate-C))) (instance-set! i 0 v1) ... i)

(define *maximal-explicit-allocation-size* 25)

;;; This function generates an allocation form. Two main cases are
;;; recognized whether it is possible to create directly the instance
;;; with a single call to vector or if it has to be allocated then
;;; patched for every specified field.
;;; A bug was discovered by <riverola@iese.es>: instances were not
;;; initialized with default-initializer when their length was specified
;;; if that indexed field was the only one to be mentioned in an instantiate
;;; form.

(define (process-instantiation class parms)
  (let* ((fields (careless-Class-fields class))
         (class-name (symbol-concatenate (careless-Class-name class) '-class))
         ;; content is a list of couples (field-size . field-content),
         ;; the process-initialization function just splits the parameters 
         ;; for each field:
         (contents (process-initialization 
                    fields parms find-initialization )) )  
    (if (and (every? (lambda (content) (fixnum? (car content)))
                     contents )
             (fx<= (meroon-reduce (lambda (sum content) (fx+ sum (car content)))
                                0
                                contents )
                 *maximal-explicit-allocation-size* ) )
        ;; The total size of the instance to allocate is known
        (let ((filling (generate-whole-content fields contents)))
          ;; Check statically if all fields that should be initialized
          ;; really are.
          (if (memq 'meroon-uninitialized filling)
              ;; some fields are not yet initialized, so generate a call
              ;; to fill-other-fields!.
              `(initialize!
                (fill-other-fields!
                 (instance (careless-Class-number ,class-name)
                           ,@filling ) ) )
              ;; all fields are known
              `(initialize!
                (instance (careless-Class-number ,class-name)
                          ,@filling ) ) ) )
        ;; the total size is unknown (or too big): allocate then patch fields
        (let ((g (gensym))
              (index (gensym)) )
          `(let ((,index 0)
                 (,g ((careless-Class-allocator ,class-name)
                      ,@(generate-whole-size fields contents) )))
             ,@(generate-whole-initialization g index fields contents)
             (initialize! (fill-other-fields! ,g)) ) ) ) ) )

;;; Take all fields and contents and generate code for various purposes.

(define (generate-whole-content fields contents)
  (meroon-reduce (lambda (forms field content)
                   (generate-content field content forms) )
                 '()
                 fields
                 contents ) )

(define (generate-whole-size fields contents)
  (meroon-reduce (lambda (forms field content)
                   (generate-size field content forms) )
                 '()
                 fields
                 contents ) )

(define (generate-whole-initialization ovar index fields contents)
  (meroon-reduce (lambda (forms field content)
                   (generate-initialization field content ovar index forms) )
                 '()
                 fields
                 contents ) )

;;; This function processes all initializations to find these that are 
;;; related to each field, it also checks that there is no superfluous
;;; initialization. It returns a list:
;;;     ( (size init-form ...) ...)
;;; where size is an integer or a form followed by the forms that initialize
;;; the field (a Mono- or a Poly-Field). A field specified as
;;;    :field form1 form2        yields (2 form1 form2)
;;;    :field-length form        yields (form)

(define (process-initialization fields parms finder)
  (let iterate ((fields fields)(parms parms))
    (if (pair? fields)
        (finder 
         (car fields) 
         parms
         (lambda (init other-parms)
           ;; other-parms are the parms not taken into account by (car fields)
           (cons init (iterate (cdr fields) other-parms)) ) )
        (begin 
          ;; If there remain parms then they are superfluous
          (when (pair? parms)
            (report-meroon-error 'Syntax 'instantiate
                                 "Unused options" parms ) )             
          '() ) ) ) )

;;; This function looks in the given parameters PARMS for the size
;;; needed by each field. It returns the list of the forms that will
;;; be used to initialize the fields prefixed by their size. If the
;;; first term is not an integer then the total size of the instance
;;; to be allocated is not static. It also checks if a field that must
;;; be initialized really is.

(define-generic (find-initialization (f Field) parms k))

(define-method (find-initialization (f Mono-Field) parms k)
  (let* ((form (find-option-single-value 
                (careless-Field-name f) parms option-not-there ))
         (other-parms (remove-option (careless-Field-name f) parms)) )
    (if (absent-option? form)
        (if (and (careless-Field-initialized? f) ; means "must be initialized"
                 (not (field-defined? f 'initializer)) )
            (report-meroon-error
             'Syntax 'instantiate
             "This field should be initialized" f )             
            (k (list 1 'meroon-uninitialized) other-parms) )
        (k (list 1 form) other-parms) ) ) )

(define-method (find-initialization (f Poly-Field) parms k)
  ;; check if the field is specified by comprehension
  (let* ((content (find-option-values
                   (careless-Field-name f) parms option-not-there ))
         (parms (remove-option (careless-Field-name f) parms))
         (kw (list (careless-Field-name f) '-length)) )
    (if (absent-option? content)
        ;; check if the length of the field is specified.
        (let* ((size (find-option-single-value kw parms option-not-there))
               (parms (remove-option kw parms)) )
          (if (absent-option? size)
              (report-meroon-error
               'Syntax 'instantiate
               "This field has no size specified" f )
              (k (list size) parms) ) )
        (begin 
          (unless (absent-option?
                   (find-option-single-value kw parms option-not-there) )
            (report-meroon-error
             'Syntax 'instantiate
             "Simultaneously incompatible options" 
             (careless-Field-name f) kw ) )
          (k (cons (length content) content) parms) ) ) ) )

;;; This function returns the exact content of an initialization into
;;; a vector form. The content variable already contains what was
;;; parsed from the initial parameters.

(define-generic (generate-content (field Field) content forms))

(define-method (generate-content (field Mono-Field) content forms)
  (cons (cadr content) forms) )

(define-method (generate-content (field Poly-Field) content forms)
  (let ((size       (car content))
        (init-forms (cdr content)) )
    (if (fx= size (length init-forms))
        (append content forms)
        (cons size
              (let make-list ((size size))
                (if (fx> size 0)
                    (cons `meroon-uninitialized (make-list (fx- size 1)))
                    forms ) ) ) ) ) )

;;; Extract from contents the sizes that are necessary to allocate the
;;; instance.

(define-generic (generate-size (field Field) content forms))

(define-method (generate-size (field Mono-Field) content forms)
  forms )

(define-method (generate-size (field Poly-Field) content forms)
  (cons (car content) forms) )

;;; Generate explicit initializations. OVAR is the name of the variable
;;; that holds the instance, INDEXVAR is the name of the variable that
;;; contains the current offset. Every initialization leaves the
;;; offset correctly positioned on the next field.

(define-generic (generate-initialization (field Field) 
                                         content ovar indexvar forms))

(define-method (generate-initialization (field Mono-Field) 
                                        content ovar indexvar forms )
  (if (eq? 'meroon-uninitialized (cadr content))
      ;; position indexvar on the next field
      (adjust-index `(set! ,indexvar (fx+ 1 ,indexvar)) forms)
      (cons `(instance-set! ,ovar ,indexvar ,(cadr content))
            (adjust-index `(set! ,indexvar (fx+ 1 ,indexvar))
                          forms ) ) ) )

(define-method (generate-initialization (field Poly-Field) 
                                        content ovar indexvar forms )
  (if (null? (cdr content))
      (if (fixnum? (car content))
          (adjust-index `(set! ,indexvar (fx+ ,(fx+ 1 (car content)) ,indexvar))
                        forms )
          ;; Attention: three arguments in the next addition (see adjust-index)
          (adjust-index `(set! ,indexvar (fx+ 1 
                                            (instance-ref ,ovar ,indexvar)
                                            ,indexvar ))
                        forms ) )
      (cons `(instance-set! ,ovar ,indexvar ,(length (cdr content)))
            (adjust-index 
             `(set! ,indexvar (fx+ 1 ,indexvar))
             (let enum ((contents (cdr content)))
               (if (pair? contents)
                   (cons `(instance-set! ,ovar ,indexvar ,(car contents))
                         (adjust-index `(set! ,indexvar (fx+ 1 ,indexvar))
                                       (enum (cdr contents)) ) )
                   forms ) ) ) ) ) )

;;; It is not necessary to ajust the index if no one uses it.
;;; For interpreters speed, coalesce incrementations.

(define (adjust-index expression forms)
  (if (null? forms)
      forms
      ;; assume expression == (set! ,indexvar (+ ,n ... ,indexvar))
      ;; but coalesce only when expression is (set! ,indexvar (+ ,n ,indexvar))
      ;; and if the first form of forms has the same shape.
      (if (and (pair? (car forms))
               (eq? (car (car forms)) 'set!)
               (eq? (car (caddr (car forms))) '+)
               (fx= (length (caddr (car forms))) 3)
               (fx= (length (caddr expression)) 3) )
          (let ((n1 (cadr (caddr (car forms))))
                (n2 (cadr (caddr expression))) )
            (cons `(set! ,(cadr expression)
                         (fx+ ,(fx+ n1 n2) 
                            ,(caddr (caddr (car forms))) ) )
                  (cdr forms) ) )
          (cons expression forms) ) ) )

;;; This function finishes the initialization of a freshly
;;; created instance. It successively check all the fields and for
;;; each field of the fresh instance which is not initialized, it
;;; tries to find the associate initializer and uses it.
;;; NOTE: fill-other-fields! is not the default method of initialize!
;;; so that the implementation can ensure that it will always be
;;; called. So, all fields that must be initialized (whether
;;; explicitely or by default) really are.

(define-generic (fill-uninitialized-field! o (field Field)))

(define-method (fill-uninitialized-field! o (field Mono-Field))
  (unless (field-defined? o field)
    (if (field-defined? field 'initializer)
        (initialize-field-value!
         o ((careless-Field-initializer field)) field )
        (when (careless-Field-initialized? field)
          ;; this field should be initialized but there is no initializer
          (report-meroon-error
           'Access 'fill-uninitialized-field!
           "No initializer" field ) ) ) ) )

(define-method (fill-uninitialized-field! o (field Poly-Field))
  (let ((len (field-length o field)))
    (unless (and (fx> len 0)
                 (field-defined? o field 0) )
      (if (field-defined? field 'initializer)
          (do ((i 0 (fx+ 1 i)))
              ((fx>= i len))
            (initialize-field-value!
             o ((careless-Field-initializer field) i) field i ) )
          (when (careless-Field-initialized? field)
            ;; this field should be initialized but there is no initializer
            (report-meroon-error
             'Access 'fill-uninitialized-field!
             "No initializer" field ) ) ) ) ) )

;;; Definite definition of fill-other-fields!

(set! fill-other-fields! 
      (lambda (o)
        (for-each (lambda (field) (fill-uninitialized-field! o field))
                  (careless-Class-fields (object->class o)) )
        o ) )

;;; end of fill.scm
;;; $Id: coinst.scm,v 1.1 2005/02/25 22:19:37 lucier Exp lucier $
;;; Copyright (c) 1990-96 by Christian Queinnec. All rights reserved.

;;;                ***********************************************
;;;                    Small, Efficient and Innovative Class System
;;;                                    Meroon 
;;;                              Christian Queinnec  
;;;                    Ecole Polytechnique & INRIA--Rocquencourt
;;;                ************************************************

;;; This file defines how to coinstantiate multiple objects at the same
;;; time with mutual references (a little like a letrec form).
;;; For instance, you can say:
;;;    (co-instantiate
;;;       (x Pair :left y :right x)
;;;       (y Point) )
;;; x and y are allocated, then x fields are initialized with the given
;;; definitions (x may use x and y), then y fields are initialized, then
;;; default initializations for x and y take place. Finally the first 
;;; object, here x, is returned as value of the whole form. Note that
;;; initializations are processed from left to right so you can write
;;;    (co-instantiate
;;;       (x Pair :left y :right x)
;;;       (y Point)
;;;       (z Pair :right (Pair-left x)) )
;;;
;;;  The co-instantiate form is expanded into a begin form containing
;;; set! forms so you cannot use it at toplevel unless the variables are 
;;; already globally defined. If you want them locally as with a letrec
;;; form, use the cousin macro:
;;;    (with-co-instantiation
;;;       ((x Pair :left y :right x)
;;;        (y Point :x 33) )
;;;     body )

(define (process-co-instantiate-form definitions definer)
  (let* ((forms (map (lambda (def)
                      (if (and (pair? def) (pair? (cdr def)))
                          (process-co-instantiation-form 
                           (car def) (cadr def) (cddr def) )
                          (report-meroon-error
                           'Syntax 'co-instantiate
                           "Missing parameters" def ) ) )
                    definitions ))
         (names (map car definitions)) )
    `(begin
       ,@(map (lambda (name alloc+init)
                `(,definer ,name ,(car alloc+init)) )
              names
              forms )
       ,@(map cdr forms)
       ,(if (pair? names) (car names) #f) ) ) )

;;; This function returns a pair of two forms: a raw allocation form
;;; and an initialization form.

(define (process-co-instantiation-form name class-name parms)
  (process-co-instantiation 
   name
   (symbol->class class-name
                  (lambda (name) 
                    (report-meroon-error 
                     'Syntax 'co-instantiate "Unknown class" name ) ) )
   parms ) )

;;; This is where the real work is done. This function is inspired from
;;; the process-instantiation function.

(define (process-co-instantiation name class parms)
  (let* ((fields (careless-Class-fields class))
         (class-name (symbol-concatenate (careless-Class-name class) '-class))
         ;; content is a list of couples (field-size . field-content),
         ;; the process-initialization function just splits the parameters 
         ;; for each field:
         (contents (process-initialization 
                    fields parms find-initialization )) )
    (let ((index (gensym)))
      (cons `((careless-Class-allocator ,class-name)
              ,@(generate-whole-size fields contents) )
            `(let ((,index 0))
               ,@(generate-whole-initialization name index fields contents)
               (initialize! (fill-other-fields! ,name)) ) ) ) ) )

;;; Make the co-defined objects local to a let form so the body can be
;;; evaluated in the right environment. 

(define (process-with-co-instantiation-form definitions body)
  (let ((initforms (process-co-instantiate-form definitions 'define)))
    `(let ()
       ,@(cdr initforms)
       . ,body ) ) )

;;; end of coinst.scm
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
;;; $Id: view.scm,v 1.1 2005/02/25 22:19:37 lucier Exp lucier $
;;; Copyright (c) 1990-97 by Christian Queinnec. All rights reserved.

;;;                ***********************************************
;;;                    Small, Efficient and Innovative Class System
;;;                                    Meroon 
;;;                              Christian Queinnec  
;;;                    Ecole Polytechnique & INRIA--Rocquencourt
;;;                ************************************************

;;;  ---  NOT YET FINISHED  ---  NOT YET FINISHED  ---  NOT YET FINISHED ---

;;; This file implements views (aka interfaces) for Meroon. A view
;;; defines a number of virtual fields. A class (or a view) implements
;;; a view if it provides fields for the virtual fields. A generic
;;; function may be defined on a view V and may use the accessors V-f,
;;; V-f-length or set-V-f! in its default body. Methods may be added
;;; on classes that implement views. These methods may use their own
;;; accessors or the virtual accessors inherited from the view. A
;;; problem exists with call-next-method in that world.

'''
(define (check-and-process-view-definition definition)
  (unless (fx>= (length definition) 3)
    (report-meroon-error 
     'Syntax 'define-view "Incomplete definition" definition ) )
  (let ((name            (car definition))
        (super-names     (cadr definition))
        (own-field-descs (caddr definition))
        (view-options    (cdddr definition)) )
    (unless (symbol? name) 
      (report-meroon-error 
       'Syntax 'define-view "Incorrect name for a class" name ) )
    (unless (list? super-names)
      (report-meroon-error 'Syntax 'define-view
                           "Bad super views" definition ) )
    ;; Check all super-views to be views
    (let ((super-views 
           (map (lambda (name) 
                  (symbol->class name complain-if-missing-super-view) )
                super-names )))
      ;; No MOP options for now                    FUTURE
      (when (pair? view-options)
        (report-meroon-error 'Syntax 'define-view
                             "MOP options not supported" definition ) )
      (process-view-definition
       name super-views own-field-descs view-options ) ) ) )

'''
(define (process-view-definition name super-views own-field-descs view-options)
  (let ((view (oo-apply (Class-allocator View-class) (length super-views))))
    (initialize-Pre-Class-name view name)
    ;; allocate a number to the view:
    (let ((cn (if already-there? 
                  (View-number old-view) 
                  (get-new-class-number) )))
      (initialize-Pre-Class-number view cn)
      ;; install the view in the vector of all classes
      (vector-set! *classes* cn view) )
    ;; Record every super-views
    ;; TO BE DONE (initialize-View-super view super-views)
    ;;
    (report-meroon-error
     'Syntax 'define-view "Not yet implemented" name) ) )

(define (View-generate-accessors view view-options)
  `'#f )

(define (Virtual-Field-generate-MeroonV2-accessors field view)
  `'#f )

(define (Virtual-Field-generate-Handy-accessors field view)
  `'#f )

(define (View-generate-predicate view view-options)
  `'#f )

(define (View-generate-accompanying-functions view view-options)
  `'#f )

;;; end of view.scm
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
;;; File automatically generated (Don't touch).

(define-generic (->Pre-Class (o)))
(define-method (->Pre-Class (o Pre-Class)) o)


(define-generic (->Handy-Class (o)))
(define-method (->Handy-Class (o Handy-Class)) o)


(define-generic (->MeroonV2-Class (o)))
(define-method (->MeroonV2-Class (o MeroonV2-Class)) o)


(define-generic (->Applyable-Object (o)))
(define-method (->Applyable-Object (o Applyable-Object)) o)


(define-generic (->Generic-1 (o)))
(define-method (->Generic-1 (o Generic-1)) o)


(define-generic (->Generic-N (o)))
(define-method (->Generic-N (o Generic-N)) o)


(define-generic (->Pre-Field (o)))
(define-method (->Pre-Field (o Pre-Field)) o)


(define-generic (->Field (o)))
(define-method (->Field (o Field)) o)


(define-generic (->Mono-Field (o)))
(define-method (->Mono-Field (o Mono-Field)) o)


(define-generic (->Poly-Field (o)))
(define-method (->Poly-Field (o Poly-Field)) o)


(define-generic (->Dispatcher (o)))
(define-method (->Dispatcher (o Dispatcher)) o)


(define-generic (->Immediate-Dispatcher (o)))
(define-method (->Immediate-Dispatcher (o Immediate-Dispatcher)) o)


(define-generic (->Subclass-Dispatcher (o)))
(define-method (->Subclass-Dispatcher (o Subclass-Dispatcher)) o)


(define-generic (->Indexed-Dispatcher (o)))
(define-method (->Indexed-Dispatcher (o Indexed-Dispatcher)) o)


(define-generic (->Linear-Dispatcher (o)))
(define-method (->Linear-Dispatcher (o Linear-Dispatcher)) o)


(define-generic (->Global-Dispatcher (o)))
(define-method (->Global-Dispatcher (o Global-Dispatcher)) o)


(define-generic (->Tracing-Dispatcher (o)))
(define-method (->Tracing-Dispatcher (o Tracing-Dispatcher)) o)


(define-generic (->Anomaly (o)))
(define-method (->Anomaly (o Anomaly)) o)


(define-generic (->Warning (o)))
(define-method (->Warning (o Warning)) o)


(define-generic (->View (o)))
(define-method (->View (o View)) o)


(define-generic (->Virtual-Field (o)))
(define-method (->Virtual-Field (o Virtual-Field)) o)


;;; end of Coercers.scm
(declare (not inline))
;;; $Id: anomaly.scm,v 1.1 2005/02/25 22:19:37 lucier Exp $ 
;;; Copyright (c) 1990-96 by Christian Queinnec. All rights reserved.

;;;                ***********************************************
;;;                    Small, Efficient and Innovative Class System
;;;                                    Meroon 
;;;                              Christian Queinnec  
;;;                    Ecole Polytechnique & INRIA--Rocquencourt
;;;                ************************************************

;;; This file defines anomalies which is the name used by Meroon to
;;; reify exceptions, errors etc. Abnormal situations are reified into
;;; instances of Anomaly then the generic function meroon-error
;;; is called on them. It is possible to define new classes of
;;; anomalies (which do not need to be instances of Anomaly, any
;;; objects can work though to confine anomalies to Anomaly is advised).
;;; The meroon-error function is generic and can be customised. 

;;; There are different categories of anomalies though not
;;; individualized in different classes (to spare the number of
;;; classes). These are syntax or runtime anomalies: runtime anomalies
;;; are further divided into domain, access or allocation anomalies.

;;; This function is used throughout Meroon to report anomalies.
;;; CATEGORY gives the overall nature of the anomaly (something like
;;; Syntax, Access, Domain). OPERATOR is the name of the function that
;;; discovered the anomaly, MESSAGE is a string explaining the problem
;;; and HINTS are hints (or culprits) that might help to grasp the problem.

(define (report-meroon-error category operator message . hints)
  (meroon-error
   (oo-apply make-Anomaly category operator message 
             (length hints) hints ) ) )

;;; All these functions report some precise type of anomalies.

(define (report-bad-index field-or-field-name o index)
  (report-meroon-error 
   'Domain (if (Field? field-or-field-name)
               field-or-field-name
               (retrieve-named-field (object->class o) field-or-field-name) )
   "Index out of bounds" o index ) )

(define (report-uninitialized-field field o . index)
  (oo-apply report-meroon-error 
            'Uninitialized field "Uninitialized field" o index ) )

(define (report-immutable-field field o . index)
  (oo-apply report-meroon-error
            'Access field "Immutable field" o index ) )

(define (report-missing-index field o)
  (report-meroon-error 'Access field "Missing index" o) )

(define (report-already-initialized field o value . index)
  (oo-apply report-meroon-error 
            'initialize! field "Already initialized" o value index ) )
  
;;; Sometime warnings can be issued for the user to improve the speed of
;;; some internal aspects of Meroon. Give them to meroon-error which has
;;; a specific method to display them.

(define (report-meroon-warning operator message . hints)
  (meroon-error
   (oo-apply make-Warning 'Warning operator message
             (length hints) hints ) ) )

;;; Anomalies are reported with a generic function so users can add
;;; their proper errors. Oo-error is a special function defined in the
;;; various prologues that calls the underlying error system.

(define-generic (meroon-error (anomaly))
  (oo-error 'meroon "Error" anomaly) )

;;; Record the last occurred anomaly.

(define-method (meroon-error (o Anomaly))
  (set! *last-meroon-anomaly* o)
  (display-meroon-anomaly o)
  (call-next-method) )

;;; Just display the anomaly, don't call (call-next-method).

(define-method (meroon-error (o Warning))
  (set! *last-meroon-anomaly* o)
  (display-meroon-anomaly o) )

;;; The universal function to display anomalies (or warnings)..

(define (display-meroon-anomaly o)
  (newline)
  (display "******************* Meroon ")
  (display (Anomaly-category o))
  (display " *********************")(newline)
  (display "Occurred in: ")
  (show (Anomaly-operator o))
  (newline)
  (display "Reason: ")
  (show (Anomaly-message o))
  (newline)
  (do ((i 0 (fx+ 1 i)))
      ((fx= i (Anomaly-hint-length o)) #f)
    (display "Hint[")
    (display i)
    (display "]: ")
    (show (Anomaly-hint o i))
    (newline) ) )

;;; end of anomaly.scm
(declare (inline))
;;; $Id: multi.scm,v 1.1 2005/02/25 22:19:37 lucier Exp lucier $ 
;;; Copyright (c) 1990-96 by Christian Queinnec. All rights reserved.

;;;                ***********************************************
;;;                    Small, Efficient and Innovative Class System
;;;                                    Meroon 
;;;                              Christian Queinnec  
;;;                    Ecole Polytechnique & INRIA--Rocquencourt
;;;                ************************************************

;;; This file defines how are handled multi-methods. The programmation
;;; is actually quite naive since multi-methods are rare according to
;;; my own habit. This is also accounted by good authors (see
;;; Kiczales and Rodriguez LFP90). Multi-methods can only appear on a 
;;; special class: Generic-N. They are implemented by a linear search.

;;; Comparing signatures might produce one of four results, this
;;; function combines these status:
;;;        =           means equal,
;;;        <           means strictly-less,
;;;        >           means strictly-more,
;;;        unrelated   means unrelated (they are nor =, nor <, nor >),
;;;        clash       means incompatible.
;;; Two different methods clash if they can be applied to similar
;;; objects. Suppose that ColoredPoint is a subclass of Point and
;;; suppose a method M1 is defined on Point*ColoredPoint, then it is
;;; not possible to define a different method M2 on ColoredPoint*Point
;;; since M1 and M2 are both the most specialized method that can be
;;; applied on a couple of ColoredPoints. We see no reason to adopt an
;;; order of discrimination so we report an error if such a method is
;;; defined. Given the same M1, observe that it is correct to define
;;; M3 on ColoredPoint*ColoredPoint: M1 is the call-next-method of M3.
;;; It is nevertheless still not possible to define M2 since that
;;; would make the call-next-method of M3 ambiguous.

;;; FUTURE: Were it possible to insert the SAME method in two
;;; different places the notion of ambiguity would have to be refined !?

;;; Insert a method in a multi-method generic functions. 
;;; Methods and their associated signature are gathered in a sorted
;;; list of Linear-Dispatchers: most general methods appear at the
;;; end. This makes call-next-method easier. When the exact dispatcher
;;; where to put the method is found, invoke method-maker to get the
;;; exact method, this allows a method to know the rest of the more
;;; general methods that can be searched via (call-next-method).

(define (add-N-method! g cl* method-maker)
  (careless-set-Generic-dispatcher!
   g (insert-N-method! (careless-Generic-dispatcher g) g cl* method-maker) ) )

(define-generic (insert-N-method! (d Dispatcher) g cl* method-maker))

(define-method (insert-N-method! (d Linear-Dispatcher) g cl* method-maker)
  (let ((r (compare-signatures cl* (Linear-Dispatcher-signature d))))
    (case r
      ;; patch the current dispatcher
      ((=) (set-Linear-Dispatcher-method!
            d (method-maker g cl* d) )
           d )
      ;; insert method before the current dispatcher 
      ((<) (let ((d (make-Linear-Dispatcher
                     Linear-Dispatcher-find-multi-method 
                     d                  ; no
                     meroon-uninitialized ; method
                     cl* )))            ; signature
             (careless-initialize-Linear-Dispatcher-method
              d (method-maker g cl* d) )
             d ))
      ;; continue to scan the list of methods
      ((> unrelated) 
       (set-Linear-Dispatcher-no!
        d (insert-N-method! (Linear-Dispatcher-no d) g cl* method-maker) )
       d )
      ((clash)
       (report-meroon-error
        'Signature 'insert-N-method! "Conflicting signatures"
        cl*
        (Linear-Dispatcher-signature d) ) )
      (else (report-meroon-error 
             'Domain 'insert-N-method! "Not a status" r)) ) ) )

(define-method (insert-N-method! (d Immediate-Dispatcher) g cl* method-maker)
  (let ((d (make-Linear-Dispatcher
            Linear-Dispatcher-find-multi-method
            d                           ; no
            meroon-uninitialized        ; method
            cl* )))                     ; signature
    (careless-initialize-Linear-Dispatcher-method
     d (method-maker g cl* d) )
    d ) )

(define-method (insert-N-method! (d Tracing-Dispatcher) g cl* method-maker)
  (set-Tracing-Dispatcher-dispatcher!
   d (insert-N-method! (Tracing-Dispatcher-dispatcher d) g cl* method-maker) )
  d )

;;;========================================================= find-multi-method
;;; Scan linearly the sorted list of dispatchers to find the most 
;;; precise method. There are only Linear- or Immediate- Dispatchers.
;;; Each element of CLASSES is a class or #f to specify a non-Meroon Scheme
;;; value. 

(define (find-multi-method d classes)
  ((careless-Dispatcher-method-finder d) d classes) )

(define (Linear-Dispatcher-find-multi-method d classes)
  (if (let ((r (compare-signatures classes (Linear-Dispatcher-signature d))))
        (or (eq? r '<) (eq? r '=)) )
      (Linear-Dispatcher-method d)
      (find-multi-method (Linear-Dispatcher-no d) classes) ) )

(define (Immediate-Dispatcher-find-multi-method d classes)
  (Immediate-Dispatcher-method d) )

;;; end of multi.scm
;;; $Id: show.scm,v 1.1 2005/02/25 22:19:37 lucier Exp $ 
;;; Copyright (c) 1990-96 by Christian Queinnec. All rights reserved.

;;;                ***********************************************
;;;                    Small, Efficient and Innovative Class System
;;;                                    Meroon 
;;;                              Christian Queinnec  
;;;                    Ecole Polytechnique & INRIA--Rocquencourt
;;;                ************************************************

;;; Show printed images of Meroon (or non-Meroon) objects. The generic
;;; function show ignores cycles and might loop, use unveil instead.
;;; If you add your proper methods on show, make sure that show does
;;; not return the instance to be shown to avoid printing circular
;;; entities by the native toplevel printer of Scheme.

(define-generic (show (o) . stream)
  (let ((stream (if (pair? stream) (car stream) (current-output-port))))
    (cond ((boolean? o) (display (if o "#T" "#F") stream))
          ((null? o) (display "()" stream))
          ((pair? o) (show-list o stream))
          ((vector? o) (show-vector o stream))
          ;;  by default, use display
          (else (display o stream)) ) ) )

(define (show-list o . stream)
  (let ((stream (if (pair? stream) (car stream) (current-output-port))))
    (define (show-content o)
      (show (car o) stream)
      (cond ((null? (cdr o)) #t)
            ((pair? (cdr o)) (display " " stream)
                             (show-content (cdr o)) )
            (else (display " . " stream)
                  (show (cdr o) stream) ) ) )
    (display "(" stream)
    (show-content o)
    (display ")" stream) ) )

(define (show-vector o . stream)
  (let ((stream (if (pair? stream) (car stream) (current-output-port)))
        (n (vector-length o)) )
    (define (show-content i)
      (when (fx< i n) 
            (show (vector-ref o i) stream)
            (display " " stream)
            (show-content (fx+ 1 i)) ) )
    (display "#(" stream)
    (show-content 0)
    (display ")" stream) ) )

(define-method (show (o Object) . stream)
  (let ((stream (if (pair? stream) (car stream) (current-output-port)))
        (name (Class-name (object->class o))) )
    (display "#<a" stream)
    ;; a french misinterpretation of english pronunciation
    ;;(case (string-ref (symbol->string name) 0)
    ;;  ((#\a #\e #\i #\o #\u #\y) (write-char #\n stream))
    ;;  (else #f) )
    (write-char #\space stream)
    (display name stream)
    (display ">" stream) ) )

(define-method (show (o Class) . stream)
  (let ((stream (if (pair? stream) (car stream) (current-output-port))))
    (display "#<Class: " stream)
    (display (Class-name o) stream)
    (display ">" stream) ) )

(define-method (show (o Generic) . stream)
  (let ((stream (if (pair? stream) (car stream) (current-output-port))))
    (display "#<Generic: " stream)
    (display (Generic-name o) stream)
    (display ">" stream) ) )

;;; A * or = follows the word Field to indicate if it is a mono or
;;; a Poly-Field. A ! is also inserted if the field is mutable.
(define (show-field status o stream)
  (display "#<" stream)
  (display status stream)
  (if (Field-immutable? o) (display " " stream) (display "! " stream))
  (display (Class-name (number->class (Field-class-number o))) stream)
  (display "." stream)
  (display (Field-name o) stream)
  (display ">" stream) )

(define-method (show (o Mono-Field) . stream)
  (let ((stream (if (pair? stream) (car stream) (current-output-port))))
    (show-field "Mono-Field" o stream) ) )

(define-method (show (o Poly-Field) . stream)
  (let ((stream (if (pair? stream) (car stream) (current-output-port))))
    (show-field "Poly-Field" o stream) ) )

(define-method (show (o Anomaly) . stream)
  (let ((stream (if (pair? stream) (car stream) (current-output-port))))
    (display "#<" stream)
    (display (Anomaly-category o) stream)
    (display "-Anomaly:" stream)
    (display (Anomaly-message o) stream)
    (display ">" stream) ) )

(define-method (show (o Subclass-Dispatcher) . stream)
  (let ((stream (if (pair? stream) (car stream) (current-output-port))))
    (display "#<" stream)
    (display (Class-name (object->class o)) stream)
    (display " " stream)
    (show (number->class (Subclass-Dispatcher-class-number o)) stream)
    (display " No: " stream)
    (show (Subclass-Dispatcher-no o) stream)
    (display " Yes: " stream)
    (show (Subclass-Dispatcher-yes o) stream)
    (display ">" stream) ) )

(define-method (show (o Indexed-Dispatcher) . stream)
  (let ((stream (if (pair? stream) (car stream) (current-output-port))))
    (display "#<" stream)
    (display (Class-name (object->class o)) stream)
    (display "/" stream)
    (show (number->class (Indexed-Dispatcher-class-number o)) stream)
    (display ", no:" stream)
    (show (Indexed-Dispatcher-no o) stream)
    (display ">" stream) ) )

(define-method (show (o Linear-Dispatcher) . stream)
  (let ((stream (if (pair? stream) (car stream) (current-output-port))))
    (display "#<" stream)
    (display (Class-name (object->class o)) stream)
    (show (Linear-Dispatcher-signature o) stream)
    (display ", no:" stream)
    (show (Linear-Dispatcher-no o) stream)
    (display ">" stream) ) )

(define-method (show (o Immediate-Dispatcher) . stream)
  (let ((stream (if (pair? stream) (car stream) (current-output-port))))
    (display "#<" stream)
    (display (Class-name (object->class o)) stream)
    (display ":" stream)
    (show (Immediate-Dispatcher-method o) stream)
    (display ">" stream) ) )

(define-method (show (o Tracing-Dispatcher) . stream)
  (let ((stream (if (pair? stream) (car stream) (current-output-port))))
    (display "#<" stream)
    (display (Class-name (object->class o)) stream)
    (display ":" stream)
    (show (Tracing-Dispatcher-dispatcher o) stream)
    (display ">" stream) ) )

;;;============================================ New library of useful functions
;;; This function displays the inheritance tree of a class.
;;; (show-hierarchy) displays all classes.

(define (show-hierarchy . args)
  (let* ((arg (if (pair? args) (car args) 'Object))
         (class (->Class arg))
         (str (if (pair? args) (cdr args) '()))
         (stream (if (pair? str) (car str) (current-output-port))) )
    (define (show-class c indent)
      (do ((i 0 (fx+ 1 i)))
          ((fx>= i indent))
        (display " " stream) )
      (show c stream)
      (newline stream)
      (for-each (lambda (c) (show-class c (fx+ indent 1)))
                (Class-subclasses c) ) )
    (display "Subclass tree of " stream)
    (display (Class-name class) stream)
    (newline stream)
    (show-class class 0)
    #t ) )

;;; Show all different methods attached to a generic function.
;;; (show-generic) displays all generic functions.

(define (show-generic . args)
  (if (pair? args)
      (oo-apply show-generic-function args)
      (sequence-map (lambda (generic)
                      (when generic (show-generic-function generic)) )
                    *generics* ) ) )

(define (show-generic-function name . stream)
  (let* ((stream (if (pair? stream) (car stream) (current-output-port)))
         (generic (->Generic name))
         (name (Generic-name generic))
         (dispatcher (Generic-dispatcher generic)) )
    (define (show-1-method c super-method indent)
      (let ((current-method (find-method1 dispatcher c)))
        (unless (eq? super-method current-method)
          (do ((i 0 (fx+ 1 i)))
              ((fx>= i indent))
            (display " " stream) )
          (show c stream)
          (newline stream) )
        (for-each (lambda (c) (show-1-method 
                               c current-method (fx+ indent 1) ))
                  (Class-subclasses c) ) ) )
    (define (show-N-method d)
      (cond ((Linear-Dispatcher? d)
             (for-each (lambda (c) 
                         (display " " stream)
                         (show c) )
                       (Linear-Dispatcher-signature d) )
             (newline stream)
             (show-N-method (Linear-Dispatcher-no d)) )
            (else (newline stream)) ) )
    (display "Methods on " stream)
    (display name stream)
    (newline stream)
    (cond ((Generic-1? generic) (show-1-method Object-class #f 0))
          ((Generic-N? generic) (show-N-method (Generic-dispatcher generic))) )
    #t ) )

;;; Show dispatchers of a generic function.
;;; (show-dispatcher) displays all dispatchers.

(define (show-dispatcher . g)
  (if (pair? g)
      (let* ((generic (->Generic (car g))))
        (show (Generic-dispatcher generic)) )
      (sequence-map (lambda (g) (when g 
                                  (display "Dispatcher of ")
                                  (display (Generic-name g))
                                  (newline)
                                  (show-dispatcher g)
                                  (newline) ))
                    *generics* ) ) )

;;; Prints the methods (on mono- or poly- generic) that have class in
;;; their signature. Asked for by Josep Riverola riverola@iese.es

(define (show-methods-for-class class . stream)
  (let* ((stream (if (pair? stream) (car stream) (current-output-port)))
         (class (->Class class)) )
    (sequence-map (lambda (g)
                    (when g (search-methods-for-class g class stream)) )
                  *generics* ) ) )

(define-generic (search-methods-for-class (o) c stream))

(define-method (search-methods-for-class (g Generic-1) c stream)
  (let ((super (Class-super-class c))
        (d (Generic-dispatcher g)) )
    (when super
      (let ((superm (find-method1 d super))
            (m (find-method1 d c)) )
        (unless (eq? m superm)
          (show g stream)
          (display " has a method for " stream)
          (show c stream)
          (newline stream) ) ) ) ) )

(define-method (search-methods-for-class (g Generic-N) c stream)
  (let scan ((d (Generic-dispatcher g)))
    (when (Linear-Dispatcher? d)
      (let ((sig (Linear-Dispatcher-signature d)))
        (when (member c sig)
          (show g stream)
          (display " has a method for " stream)
          (show sig stream)
          (newline stream) ) )
      (scan (Linear-Dispatcher-no d)) ) ) )

;;;=================================================== Debugging
;;; Show a detailed view of the Class hierarchy to check the
;;; renumbering process.

(define (show-detailed-hierarchy . message)
  (let ((class Object-class)
        (stream (current-output-port)) )
    (define (show-class c indent)
      ;;(display c)(newline)                           ; DEBUG
      (do ((i 0 (fx+ 1 i)))
          ((fx>= i indent))
        (display " " stream) )
      (display (careless-Class-name c) stream)
      (display " " stream)
      (display (list (careless-Class-number c)) stream)
      (newline stream)
      (for-each (lambda (cn) (show-class (number->class cn) (fx+ indent 1)))
                (Class-subclass-numbers c) ) )
    (display "Detailed Class tree of " stream)
    (display (careless-Class-name class) stream)
    (when (pair? message) 
      (display " " stream)
      (display (car message) stream) )
    (newline stream)
    (show-class class 0)
    #t ) )

;;; end of show.scm
;;; $Id: trace.scm,v 1.1 2005/02/25 22:19:37 lucier Exp lucier $ 
;;; Copyright (c) 1990-96 by Christian Queinnec. All rights reserved.

;;;                ***********************************************
;;;                    Small, Efficient and Innovative Class System
;;;                                    Meroon 
;;;                              Christian Queinnec  
;;;                    Ecole Polytechnique & INRIA--Rocquencourt
;;;                ************************************************

;;; This file defines the tracing facility. Two levels are provided: a
;;; low level one which installs hook-functions to be called before
;;; and after a generic call and a high level one which by default
;;; shows the arguments and the result of a generic call. The high
;;; level just uses the low level one.

;;; The list of traced generic functions paired with their original
;;; default behaviour (which must be restored after untracing).

(define *traced-generics* (list))

;;;============================================================== Low level
;;; This function installs two functions named BEFORE and AFTER
;;; that are called on the arguments and the result of a generic call.
;;; It is a sort of old-style advise feature. 

(define (generic-trace generic before after)
  (let ((generic (->Generic generic)))
    (unless (procedure? before)
      (report-meroon-error 
       'Domain 'generic-trace "Not a procedure" before ) )
    (unless (procedure? after)
      (report-meroon-error 
       'Domain 'generic-trace "Not a procedure" after ) )
    (unless (assq generic *traced-generics*)
      (let ((default (Generic-default generic)))
        (set! *traced-generics* 
              (cons (cons generic default) *traced-generics*) )
        (set-Generic-default!
         generic
         (lambda all-arguments
           ;; the BEFORE function is only is only called for side-effects.
           (oo-apply before all-arguments)
           ;; the AFTER function can modify the result.
           (after (oo-apply default all-arguments)) ) )
        (set-Generic-dispatcher!
         generic
         (make-Tracing-Dispatcher
          (build-Tracing-Dispatcher-find-method before after)
          (Generic-dispatcher generic)
          default ) ) ) )
    generic ) )

(define (build-Tracing-Dispatcher-find-method before after)
  ;; args can be cn for mono-method or classes for poly-method but in
  ;; any case, it is the same arity for method-finder.
  (lambda (d args)
    (lambda all-arguments
      (let* ((nextd (Tracing-Dispatcher-dispatcher d))
             (method ((careless-Dispatcher-method-finder nextd)
                      nextd args )) )
        ;; the BEFORE function is only is only called for side-effects.
        (oo-apply before all-arguments)
        ;; the AFTER function can modify the result.
        (after (oo-apply (or method (Tracing-Dispatcher-default d))
                         all-arguments )) ) ) ) )

;;; remove the tracing dispatchers associated to a generic instance.

(define (generic-untrace generic)
  (let* ((generic (->Generic generic))
         (name (Generic-name generic))
         (p (assq generic *traced-generics*)) )
    (when (pair? p)
      (set! *traced-generics*
            (let remq ((g* *traced-generics*))
              (if (pair? g*)
                  (if (eq? (car g*) p)
                      (cdr g*)
                      (cons (car g*) (remq (cdr g*))) )
                  g* ) ) )
      ;; this works till default of generic functions are immutable.
      (set-Generic-default! generic (cdr p))
      (set-Generic-dispatcher!
       generic (remove-Tracing-Dispatcher! 
                (Generic-dispatcher generic) ) )
      generic ) ) )

(define-generic (remove-Tracing-Dispatcher! (d Dispatcher)))

(define-method (remove-Tracing-Dispatcher! (d Immediate-Dispatcher))
  d )

(define-method (remove-Tracing-Dispatcher! (d Indexed-Dispatcher))
  (set-Indexed-Dispatcher-no!
   d (remove-Tracing-Dispatcher! (Indexed-Dispatcher-no d)) )
  d )

(define-method (remove-Tracing-Dispatcher! (d Global-Dispatcher))
  d )

(define-method (remove-Tracing-Dispatcher! (d Subclass-Dispatcher))
  (set-Subclass-Dispatcher-no! 
   d (remove-Tracing-Dispatcher! (Subclass-Dispatcher-no d)) )
  (set-Subclass-Dispatcher-yes! 
   d (remove-Tracing-Dispatcher! (Subclass-Dispatcher-yes d)) )
  d )

(define-method (remove-Tracing-Dispatcher! (d Linear-Dispatcher))
  (set-Linear-Dispatcher-no!
   d (remove-Tracing-Dispatcher! (Linear-Dispatcher-no d)) )
  d )

(define-method (remove-Tracing-Dispatcher! (d Tracing-Dispatcher))
  (remove-Tracing-Dispatcher! (Tracing-Dispatcher-dispatcher d)) )  

;;; Other methods.

(define-method (enlarge-dispatcher! (d Tracing-Dispatcher))
  (set-Tracing-Dispatcher-dispatcher! 
   d (enlarge-dispatcher! (Tracing-Dispatcher-dispatcher d)) )
  d )

;;;========================================================== Enhanced tracing
;;; A better tracing facility with meaningful defaults. Arguments are
;;; displayed with the generic function show. It is actually coded with
;;; generic-trace which is the lower level instrumenting facility.

(define (show-generic-trace . names)
  (for-each (lambda (name)
              (generic-trace 
               (->Generic name)
               (lambda args
                 (show name)
                 (display "<<")
                 (for-each (lambda (arg) 
                             (display " ")
                             (show arg) )
                           args )
                 (newline) )
               (lambda (result)
                 (show name)
                 (display ">> ")
                 (show result)
                 (newline)
                 result ) ) )
            names ) )

;;; (show-generic-untrace) untraces all traced generic.

(define (show-generic-untrace . names)
  (for-each (lambda (o) (generic-untrace (->Generic o)))
            (if (pair? names) names
                (oo-map car *traced-generics*) ) ) )

;;; end of trace.scm
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
;;; $Id: unveil.scm,v 1.1 2005/02/25 22:19:37 lucier Exp $ 
;;; Copyright (c) 1990-96 by Christian Queinnec. All rights reserved.

;;;                ***********************************************
;;;                    Small, Efficient and Innovative Class System
;;;                                    Meroon 
;;;                              Christian Queinnec  
;;;                    Ecole Polytechnique & INRIA--Rocquencourt
;;;                ************************************************

;;; This file defines the unveil function that displays all details
;;; about any Meroon object. It also knows how to display cyclic
;;; structures and non-Meroon objects such as lists or vectors. It
;;; runs slowly. All Meroon are displayed with a generic function that
;;; can be customized. By default, it invokes a meta-method that uses
;;; the structure of the object and the nature of its fields.

;;; The exported function. Takes an object and possibly a stream and
;;; output O onto STREAM.

(define (unveil o . stream)
  (let ((stream (if (pair? stream) (car stream) (current-output-port))))
    (set! *meroon-already-unveiled-objects* '())
    (set! *first-time* #t)
    (show-unveiled o 0 stream)
    (newline stream)
    (set! *meroon-already-unveiled-objects* '()) ; GC: dont keep this !
    #t ) )

;;; This is the deepest unveiling possible. Feel free to adapt it to
;;; your needs.

(define *unveil-maximal-indent* 10)

;;; Output a newline, then indent. Also mark vertical bars to explicit
;;; alignment. These bars and relative indentations can be set via the
;;; global variables:

(define *unveil-step* 2)
(define *unveil-bigstep* 6)

;;; Make these variables mutable so they are not inlined by block
;;; compilation.

(set! *unveil-maximal-indent* *unveil-maximal-indent*)
(set! *unveil-step* *unveil-step*)
(set! *unveil-bigstep* *unveil-bigstep*)

;;; Brad Lucier <lucier@MATH.purdue.EDU> prefers unveil not to start
;;; with a newline. I added this ugly global mutable variable to handle 
;;; this.

(define *first-time* #t)

(define (goto-margin indent stream)
  (let* ((step *unveil-step*)
         (bigstep *unveil-bigstep*) )
    (if *first-time*
        (set! *first-time* #f)
        (newline stream) )
    (do ((i 0 (fx+ 1 i)))
        ((fx= i indent) #f)
      (write-char (cond ((fx= (fx- bigstep 1) (fxmodulo i bigstep)) #\+)
                        ((fx= (fx- step 1) (fxmodulo i step)) #\|)
                        (else #\space) )
                  stream ) ) ) )

;;; The list of already unveiled objects to detect cycles. Should ideally
;;; be a variable local to unveil, not a global one.

(define *meroon-already-unveiled-objects* '())

;;; Check if an object is already unveiled and returns its
;;; index. Otherwise record it and return false (in that case, use
;;; (get-current-index) IMMEDIATELY after to know the index which is
;;; associated to the newly recorded object). As a matter of fact, the
;;; object is always inserted in the list of already seen objects.

(define (already-unveiled? o)
  (let* ((already-unveiled (memq o *meroon-already-unveiled-objects*))
         (index (if (pair? already-unveiled)
                    (length already-unveiled)
                    (set! *meroon-already-unveiled-objects*
                          (cons o *meroon-already-unveiled-objects*) ) )) )
    (if already-unveiled index #f) ) )

(define (get-current-index)
  (length *meroon-already-unveiled-objects*) )
  
;;; Pretty print O, INDENT is the current indentation. Handle
;;; specially lists and vectors to detect possible cycles through
;;; them.

(define (show-unveiled o indent stream)
  (cond ((Object? o) (show-object-unveiled o indent stream))
        ((vector? o) (show-unveiled-vector-content o indent stream))
        ((pair? o)   (show-unveiled-list-content o indent stream))
        (else        (show o stream)) ) )

;;; The general way to display acyclic Meroon objects. It is a generic
;;; function so it can be customized.  Display O with INDENT on
;;; STREAM.

(define-generic (show-object-unveiled (o) indent stream))

(define-method (show-object-unveiled (o Object) indent stream)
  (let ((class (object->class o))
        (index (already-unveiled? o)) )
    (if index
        (show-already-unveiled-object (Class-name class) index stream)
        (begin
          (goto-margin indent stream)
          (if (fx> indent *unveil-maximal-indent*)
              (display "<details omitted...>" stream)
              (let ((index (get-current-index)))
                ;;(format stream "(a ~A <------------- [Id: ~A]"
                ;;        (Class-name class) index )
                (display "(a " stream)
                (display (Class-name class) stream)
                (show-index index stream)
                (for-each (lambda (field) 
                            (show-unveiled-field-content
                             o (fx+ 1 indent) stream field ) )
                          (Class-fields class) )
                ;;(format stream " end ~A)" (Class-name class))
                (display " end " stream)
                (display (Class-name class) stream)
                (display ")" stream) ) ) ) ) ) )

;;; All objects are referenced with integers.

(define (show-index index stream)
  (display " <------------- [Id: " stream)
  (display index stream)
  (display "]" stream) )

;;; Display a reference to an already displayed object.

(define (show-already-unveiled-object class-name index stream)
  ;;(format stream "<the ~A referred above as ~A>"
  ;;        (Class-name class) index )
  (display "<the " stream)
  (display class-name stream)
  (display " referred above as " stream)
  (display index stream)
  (display ">" stream) )

;;; Display the content of a field according to the type of the field.
;;; This is a generic function you can extend on new types of fields
;;; if any.

(define-generic (show-unveiled-field-content o indent stream (field)))

;;; Show a Mono-Field value.

(define-method (show-unveiled-field-content 
                 o indent stream (field Mono-Field) )
  (goto-margin indent stream)
  (display (Field-name field) stream)
  (display ": " stream)
  (if (field-defined? o field)
      (show-unveiled (field-value o field) indent stream)
      (display "#<Uninitialized>" stream) ) )

;;; Show a Poly-Field value

(define-method (show-unveiled-field-content 
                 o indent stream (field Poly-Field) )
  (let loop ((n (field-length o field))
             (i 0) )
    (when (fx< i n)
      (goto-margin indent stream)
      (display (Field-name field) stream)
      (display "[" stream)
      (display i stream)
      (display "]: " stream)
      (if (field-defined? o field i)
          (show-unveiled (field-value o field i) indent stream)
          (display "#<Uninitialized>" stream) )
      (loop n (fx+ 1 i)) ) ) )

;;;========================================================== Native showers
;;; Show the content of a vector (or a list) taking care to detect cycles.
;;; The presentation of vectors and lists mimics the presentation of a
;;; Poly-Field.

(define (show-unveiled-vector-content o indent stream)
  (let ((class-name "Vector")
        (index (already-unveiled? o)) )
    (if index
        (show-already-unveiled-object class-name index stream)
        (let ((index (get-current-index)))
          (goto-margin indent stream)
          (display "(a Vector " stream)
          (show-index index stream)
          (let ((indent (fx+ 1 indent)))
            (goto-margin indent stream)
            (let loop ((n (vector-length o))
                       (i 0) )
              (when (fx< i n)
                (display "Vector[" stream)
                (display i stream)
                (display "]: " stream)
                (show-unveiled (vector-ref o i) indent stream)
                (goto-margin indent stream)
                (loop n (fx+ 1 i)) ) ) )
          (display " end " stream)
          (display class-name stream)
          (display ")" stream) ) ) ) )

(define (show-unveiled-list-content o indent stream)
  (let* ((class-name "List")
         (index (already-unveiled? o)) )
    (if index
        (show-already-unveiled-object class-name index stream)
        (let ((index (get-current-index)))
          (goto-margin indent stream)
          (display "(a List " stream)
          (show-index index stream)
          (let ((indent (fx+ 1 indent)))
            (goto-margin indent stream)
            (let loop ((o o)
                       (i 0) )
              (cond ((pair? o)
                     (display "List[" stream)
                     (display i stream)
                     (display "]: " stream)
                     (show-unveiled (car o) indent stream)
                     (goto-margin indent stream)
                     (cond ((pair? (cdr o))
                            (let ((index (already-unveiled? (cdr o))))
                              (if index
                                  (begin (display " . " stream)
                                         (show-already-unveiled-object 
                                          class-name index stream) )
                                  (loop (cdr o) (fx+ 1 i)) ) ) )
                           ((null? (cdr o)) 'nothing)
                           (else (display " . " stream)
                                 (show-unveiled (cdr o) indent stream) ) ) )
                    ((null? o) #f)
                    (else (display " . " stream)
                          (show-unveiled o indent stream)
                          (goto-margin indent stream) ) ) ) )
          (display " end " stream)
          (display class-name stream)
          (display ")" stream) ) ) ) )

;;; end of unveil.scm
;;; $Id: egal.scm,v 1.1 2005/02/25 22:19:37 lucier Exp $ 
;;; Copyright (c) 1990-96 by Christian Queinnec. All rights reserved.

;;;                ***********************************************
;;;                    Small, Efficient and Innovative Class System
;;;                                    Meroon 
;;;                              Christian Queinnec  
;;;                    Ecole Polytechnique & INRIA--Rocquencourt
;;;                ************************************************

;;; This file defines general predicates that compares Meroon. Two
;;; Meroon objects are substituable if one cannot write a Meroon
;;; program to distinguish them. Of course, since Meroon objects are
;;; done with vectors, one can use that to side-effect instances in an
;;; unseen manner. Two Meroon objects are substituable if they are
;;; immutable and equal (in Lisp sense) or mutable and eq (still in
;;; Lisp sense). This is yet another metaprogrammming example.

;;; CAUTION: This predicate only works on Meroon objects.
;;; See also Henry Baker's paper "Equal Rights for Functional Objects
;;; or, The More Things Change, The More They Are the Same".

;;; Takes care of cycles. The list has the structure 
;;;   ( ( o1 o2 . result ) ...) where result is the result of the
;;; comparison of o1 an o.
(define *already-compared-objects* '())

(define (substituable? o1 o2)
  (set! *already-compared-objects* '())
  (let ((result (compare4egal o1 o2)))
    (set! *already-compared-objects* '()) ;; make GC happier
    result ) )

(define (compare4egal o1 o2)
  ;; look in a cache if an appropriate entry exists and return it or #f.
  (define (look cache)
    (if (pair? cache)
        (let ((entry (car cache)))
          (if (or (and (eq? (car entry) o1) (eq? (cadr entry) o2))
                  (and (eq? (car entry) o2) (eq? (cadr entry) o1)) )
              (begin
                ;; reorganize the cache                      FUTURE
                entry ) 
              (look (cdr cache)) ) )
        #f ) )
  ;; if o1 and o2 are the same, do not record an entry for it.
  (or (eq? o1 o2)
      (and (Object? o1) 
           (Object? o2)
           (fx= (object->class-number o1) (object->class-number o2))
           ;; We already know that o1 is not physically the same as o2
           ;; (on a mono-processor) so if these objects are mutable
           ;; then they differ.
           (Class-immutable? (object->class o1))
           ;; Add an entry and recursively compare their fields.
           (let ((entry (look *already-compared-objects*)))
             (if (pair? entry) (cddr entry)
                 (let ((entry (cons o1 (cons o2 #t)))
                       (c (object->class o1)) )
                   ;; suppose the comparison yields true
                   (set! *already-compared-objects*
                         (cons entry *already-compared-objects*) )
                   ;; then check whether it is true
                   (let ((result (every? (lambda (field) 
                                           (Field-compare field o1 o2) )
                                         (Class-fields c) )))
                     (set-cdr! (cdr entry) result)
                     result ) ) ) ) ) ) )

;;; This generic function compares the content of a similar (possibly
;;; indexed) field in two objects.

(define-generic (Field-compare (field Field) o1 o2))

(define-method (Field-compare (field Mono-Field) o1 o2)
  (compare4egal (field-value o1 field)
                (field-value o2 field) ) )

(define-method (Field-compare (field Poly-Field) o1 o2)
  (let ((len1 (field-length o1 field))
        (len2 (field-length o1 field)) )
    (and (fx= len1 len2)
         (let iter ((i 0))
           (or (fx>= i len1)
               (and (compare4egal (field-value o1 field i)
                                  (field-value o2 field i))
                    (iter (fx+ i 1)) ) ) ) ) ) )

;;; faire table de hash pour objets immuables                        FUTURE

;;; end of egal.scm
;;; $Id: modify.scm,v 1.1 2005/02/25 22:19:37 lucier Exp $
;;; Copyright (c) 1990-96 by Christian Queinnec. All rights reserved.

;;;                ***********************************************
;;;                    Small, Efficient and Innovative Class System
;;;                                    Meroon 
;;;                              Christian Queinnec  
;;;                    Ecole Polytechnique & INRIA--Rocquencourt
;;;                ************************************************

;;; This file introduces the 
;;;       (modify (o Class) parameters-with-keywords) macro. 
;;; It is superfically similar to duplicate or instantiate except that
;;; it does not modify an object but modify it. Of course, it is not
;;; possible to change the sizes of poly-fields nor it is possible to
;;; mutate immutable fields.

;;; parse the modify form.

(define (process-modify-form desc parms)
  (unless (and (pair? desc)
               (pair? (cdr desc))
               (null? (cddr desc)) )
    (report-meroon-error
     'Syntax 'modify "Incorrect specification" desc ) )
  (process-modification
   (car desc)
   (symbol->class (cadr desc)
                  (lambda (name)
                    (report-meroon-error
                     'Syntax 'modify "No such class" name ) ) )
   parms ) )

;;; The following functions are very close from the ones of clone.scm

(define (process-modification ins-tance class parms)
  (let* ((fields (careless-Class-fields class))
         (o      (gensym))
         (index  (gensym)) )
    `(let ((,o     ,ins-tance)
           (,index 0) )
       (check-class ,o (symbol->class ',(careless-Class-name class)) 'modify)
       ,@(generate-whole-modification
          o index fields
          (arrange-whole-modification
           fields (process-initialization fields parms find-modification) ) )
       ,o ) ) )

;;; These are the methods that parses the modification parameters.
;;; They return a list (length . values) where length is 1 for a Mono-field
;;; followed by meroon-unitialized if the field is not specified or by the
;;; form whose value will be the new content of the Mono-Field. Length may
;;; be another number (or a form whose value will be a length) followed by
;;; the forms forming the values of this indexed field. 

(define-generic (find-modification (field Field) parms k))

(define-method (find-modification (f Mono-Field) parms k)
  (let* ((form (find-option-single-value 
                (careless-Field-name f) parms option-not-there ))
         (other-parms (remove-option (careless-Field-name f) parms)) )
    (if (absent-option? form)
        ;; means that the real value should be copied from the original.
        (k (list) other-parms)
        (k (list 1 form) other-parms) ) ) )

(define-method (find-modification (f Poly-Field) parms k)
  ;; check if the field is specified by comprehension
  (let* ((content (find-option-values
                   (careless-Field-name f) parms option-not-there ))
         (parms (remove-option (careless-Field-name f) parms)) )
    (if (absent-option? content)
        ;; means that the size should be copied from the original instance
        (let* ((kw (list (careless-Field-name f) '-length))
               (size (find-option-single-value kw parms option-not-there)) )
          (if (absent-option? size)
              ;; means that the real values should be copied from the original.
              (k (list) parms)
              (report-meroon-error 
               'Syntax 'modify
               "No :kw-length keyword possible in a modify form"
               (careless-Field-name f) ) ) )
        ;; Of course, here the length will be checked wrt original.
        (k (cons (length content) content) parms) ) ) )

;;; Parse the modification parameters and produce a list of values for
;;; any fields. 

(define (arrange-whole-modification fields contents)
  (oo-map (lambda (field content)
            (if (pair? content)
                ;; a new content is specified, check its validity.
                (check-modification field content)
                ;; use the old content.
                (arrange-modification field content) ) )
          fields
          contents ) )

;;; Complements the modification parameters with the default values
;;; from the original instance. These cases correspond in the
;;; generate-whole-initialization function to code that will skip the
;;; corresponding fields in the original instance.

(define-generic (arrange-modification (field Field) content))

(define-method (arrange-modification (field Mono-Field) content)
  (list 1 'meroon-uninitialized) )

(define-method (arrange-modification (field Poly-Field) content)
 (list) )

;;; Check if the explicitly given parameters are correct wrt original
;;; instance. 

(define-generic (check-modification (field Field) content))

(define-method (check-modification (field Mono-Field) content)
  (if (Field-mutable? field)
      content
      (report-meroon-error
       'Syntax 'modify
       "Immutable field" (careless-Field-name field) ) ) )

(define-method (check-modification (field Poly-Field) content)
  (if (Field-mutable? field)
      content 
      (report-meroon-error
       'Syntax 'modify
       "Immutable field" (careless-Field-name field) ) ) )

;;; Generate the modifications.

(define (generate-whole-modification o index fields contents)
  (meroon-reduce (lambda (forms field content)
                   (generate-modification field content o index forms) )
                 '()
                 fields
                 contents ) )

(define-generic (generate-modification (field Field) content o index forms))

(define-method (generate-modification (field Mono-Field) content o index forms)
  (if (eq? 'meroon-uninitialized (cadr content))
      ;; position index on the next field
      (adjust-index `(set! ,index (fx+ 1 ,index)) forms)
      (cons `(instance-set! ,o ,index ,(cadr content))
            (adjust-index `(set! ,index (fx+ 1 ,index))
                          forms ) ) ) )

(define-method (generate-modification (field Poly-Field) content o index forms)
  (if (and (pair? content) (fixnum? (car content)))
      ;; The content of the poly-field must be modified, check that it keeps
      ;; the same size. 
      (cons `(check-same-size ,(car content) ,o ,index)
            (adjust-index 
             `(set! ,index (fx+ 1 ,index))
             (let enum ((contents (cdr content)))
               (if (pair? contents)
                   (cons `(instance-set! ,o ,index ,(car contents))
                         (adjust-index `(set! ,index (fx+ 1 ,index))
                                       (enum (cdr contents)) ) )
                   forms ) ) ) )
      ;; Attention: three arguments in the next addition (see adjust-index)
      ;; Just skip the poly-field.
      (adjust-index `(set! ,index (fx+ 1
				       (fx+ (instance-ref ,o ,index)
					    ,index )))
                    forms ) ) )

;;; Check that 

(define (check-same-size size o offset)
  (if (not (fx= size (instance-ref o offset)))
      (report-meroon-error 
       'Domain 'modify "Lengths may not be modified" o ) ) )

;;; end of modify.scm
;;; $Id: clone.scm,v 1.1 2001/08/31 01:34:48 lucier Exp $
;;; Copyright (c) 1990-96 by Christian Queinnec. All rights reserved.

;;;                ***********************************************
;;;                    Small, Efficient and Innovative Class System
;;;                                    Meroon 
;;;                              Christian Queinnec  
;;;                    Ecole Polytechnique & INRIA--Rocquencourt
;;;                ************************************************

;;; This file defines the (duplicate (<instance> class-name) :kw ... :kw
;;; ...) special form.  This form creates a new instance with explicit
;;; fields stated as in the instantiate form by keyords and other
;;; fields taken from <instance>.

;;; Parse the duplicate form. 

(define (process-duplicate-form desc parms)
  (unless (and (pair? desc)
               (pair? (cdr desc))
               (null? (cddr desc)) )
    (report-meroon-error
     'Syntax 'duplicate "Incorrect specification" desc ) )
  (process-duplication
   (car desc)
   (symbol->class (cadr desc)
                  (lambda (name)
                    (report-meroon-error
                     'Syntax 'duplicate "No such class" name ) ) )
   parms ) )

;;; Parse the instantiate-from form. 

(define (process-instantiation-from-form desc parms)
  (unless (and (pair? desc)
               (pair? (cdr desc))
               (null? (cddr desc)) )
    (report-meroon-error
     'Syntax 'instantiate-from "Incorrect specification" desc ) )
  (process-instantiation-from
   (car desc)
   (symbol->class (cadr desc)
                  (lambda (name)
                    (report-meroon-error
                     'Syntax 'instantiate-from "No such class" name ) ) )
   parms ) )

;;; The process-duplication function mimics the process-instantiation
;;; form.  Due to demand of Frey and Riverola, it is possible to
;;; restrict or extend an instance. That is: if we have the following 
;;; tree of classes:
;;;                         A (x)
;;;                        / \
;;;                 (x u) B   C (x y)
;;;                          / \
;;;                (x y t)  D   E (x y z)
;;; then it is possible to ask:
;;;   (duplicate (a B) :u ? :x ?) ; extension of a into a B
;;;          The 'u' field must be given via an initializer in the definition
;;;;         of the class B or by an explicit keyword ':u'.
;;;   (duplicate (b A) :x ?)      ; restriction of a B into a A
;;;          No restriction at all. No argument required.
;;;   (duplicate (b C) :y ?)      ; cousinage: coercion into a C.
;;;          The 'y' field must be given via an initializer in the definition
;;;          of the class B or by an explicit keyword ':y'. The value of b
;;;          should be at least an A to provide a value for 'x'.
;;;   (duplicate (b C) :y ? :x ?) ; cousinage: coercion into a C.
;;;          Here b may have whatever value since it is a disguised 
;;;          allocate form.

(define (process-duplication ins-tance class parms)
  (let* ((fields (careless-Class-fields class))
         (class-name (symbol-concatenate (careless-Class-name class) '-class))
         (g (gensym))
         (org (gensym))
         (index (gensym))
         (contents (arrange-whole-initialization 
                    org
                    fields
                    (process-initialization fields parms find-duplication) )) )
    `(let ((,index 0)
           (,org ,ins-tance))
       ;; class-name is too restrictive: should be the highest class
       ;; ensuring that all always-uninitialized fields are given.
       (if (is-a? ,org ,(find-highest-class-name class parms))
           (let ((,g ((careless-Class-allocator ,class-name)
                      ,@(generate-whole-size fields contents) )))
             ,@(generate-whole-initialization g index fields contents)
             (initialize! 
              (fill-other-fields-from-instance! 
               ,g ,org ) ) )
           (report-meroon-error
            'Syntax 'duplicate
            "Missing fields from original"
            ,org ,class-name ) ) ) ) )

;;; An attempt to provide a new special form asked for by Brad Lucier.
;;; (instantiate-from (x ClassName) field: value ...)
;;; This form instantiates an object with the class of x, initializes its
;;; fields according to the keywords then fills the rest of the new instance
;;; with the fields of x. The class of x is of course a subclass of ClassName.

(define (process-instantiation-from ins-tance class parms)
  (let* ((fields (careless-Class-fields class))
         (class-name (symbol-concatenate (careless-Class-name class) '-class))
         (g (gensym))
         (org (gensym))
         (index (gensym))
         (lnew (gensym))
         (llnew (gensym))
         (lorg (gensym))
         (llorg (gensym))
         (contents (arrange-whole-initialization 
                    org
                    fields
                    (process-initialization 
                     fields parms find-instantiation-from ) )) )
    `(let ((,index 0)
           (,org ,ins-tance))
       (if (is-a? ,org ,class-name)
           (let ((,g (apply 
                      (careless-Class-allocator (object->class ,org))
                      (let* ((,lnew  (list . ,(generate-whole-size 
                                               fields contents ) ))
                             (,llnew (length ,lnew))
                             (,lorg  (find-repeated-fields-lengths ,org))
                             (,llorg (length ,lorg)) )
                        ;; combine the lengths of the polyfields of org
                        ;; that are not superseded for the new g:
                        (if (fx< ,llnew ,llorg)
                            (append ,lnew (list-tail ,lorg ,llnew))
                            ,lnew ) ) )))
             (fill-other-fields-from-instance! ,g ,org)
             ,@(generate-whole-initialization g index fields contents)
             (initialize! ,g) )
           (report-meroon-error
            'Syntax 'instantiate-from
            "Wrong class for original"
            ,org ,class-name ) ) ) ) )

;;; Return the lengths of all the polyfields of o.

(define (find-repeated-fields-lengths o)
  (let* ((class (object->class o))
         (fields (careless-Class-fields class)) )
    (meroon-reduce 
     (lambda (sizes field)
       (if (Poly-Field? field)
           (cons (field-length o field)
                 sizes )
           sizes ) )
     '()
     fields ) ) )                   

;;; Find the deepest superclass of class so that every mandatory field
;;; is ensured to be defined in parms.

(define (find-highest-class-name class parms)
  (let scan ((fields (reverse (Class-fields class))))
    (if (pair? fields)
        (let ((field (car fields)))
          (if (find-if-initialized field parms)
              (scan (cdr fields))
              (symbol-concatenate (careless-Class-name
                                   (number->class
                                    (careless-Field-class-number field) ) )
                                  '-class ) ) )
        'Object-class ) ) )

;;; Test if the field is initialized or not.

(define-generic (find-if-initialized (f Field) parms)
  #f )

(define-method (find-if-initialized (f Mono-Field) parms)
  (let ((form (find-option-single-value 
               (careless-Field-name f) parms option-not-there) ))
    (not (and (absent-option? form)
              (careless-Field-initialized? f) ; means "must be initialized"
              (not (field-defined? f 'initializer)) )) ) )

(define-method (find-if-initialized (f Poly-Field) parms)
  (let ((form (find-option-values
               (careless-Field-name f) parms option-not-there) ))
    (not (and (absent-option? form)
              (careless-Field-initialized? f) ; means "must be initialized"
              (not (field-defined? f 'initializer)) )) ) )

;;; This set of functions take a description of the content of an
;;; instance to be allocated and guesses the missing components based
;;; on the instance which is to be duplicated.

(define (arrange-whole-initialization org fields contents)
  (oo-map (lambda (field content)
            (if (pair? content)
                content
                ;; This means take the content of the original instance
                (arrange-initialization org field content) ) )
          fields
          contents ) )

(define-generic (arrange-initialization org (field Field) content))

(define-method (arrange-initialization org (field Mono-Field) content)
  (list 1 'meroon-uninitialized) )

(define-method (arrange-initialization org (field Poly-Field) content)
  (let* ((fname (Field-name field)))
    (list `(field-length ,org ',fname)) ) )

;;; This generic function parses the parameters of the duplicate form
;;; and returns a content specification.

(define-generic (find-duplication (f Field) parms k))

(define-method (find-duplication (f Mono-Field) parms k)
  (let* ((form (find-option-single-value 
                (careless-Field-name f) parms option-not-there ))
         (other-parms (remove-option (careless-Field-name f) parms)) )
    (if (absent-option? form)
        ;; means that the real value will be copied from the original instance
        (k (list) other-parms)
        (k (list 1 form) other-parms) ) ) )

;;; This function does not recognize the :kw-length specification
;;; since it is not clear what it means. If the new size is greater
;;; and if there is an initializer: fine! If the new size is shorter,
;;; do we truncate the content of the field ?

(define-method (find-duplication (f Poly-Field) parms k)
  ;; check if the field is specified by comprehension
  (let* ((content (find-option-values
                   (careless-Field-name f) parms option-not-there ))
         (parms (remove-option (careless-Field-name f) parms)) )
    (if (absent-option? content)
        ;; means that the size should be copied from the original instance
        (let* ((kw (list (careless-Field-name f) '-length))
               (size (find-option-single-value kw parms option-not-there)) )
          (if (absent-option? size)
              (k (list) parms)
              (report-meroon-error 
               'Syntax 'duplicate
               "No :kw-length keyword possible in a duplicate form"
               (careless-Field-name f) ) ) )
        (k (cons (length content) content) parms) ) ) )

;;; TO BE FIXED LATER (at least for the error message).

(define-generic (find-instantiation-from (f Field) parms k)
  (find-duplication f parms k) )

;;; Fill uninitialized fields of O from ORIGINAL. Take only into
;;; account the fields of O that belong to ORIGINAL and ignore the
;;; others. Also fills other initialized fields.

(define (fill-other-fields-from-instance! o original)
  (let ((original-class (object->class original)))
    (for-each
     (lambda (field)
       (if (careless-subclass? 
            original-class
            (number->class (careless-Field-class-number field)) )
           (fill-uninitialized-field-from-instance! o field original) )
       ;; fill also otherwise uninitialized fields.
       (fill-uninitialized-field! o field) )
     (careless-Class-fields (object->class o)) )
    o ) )

(define-generic (fill-uninitialized-field-from-instance! 
                 o (field Field) original))

(define-method (fill-uninitialized-field-from-instance! 
                o (field Mono-Field) original )
  (let ((offset (compute-value-offset o field)))
    (when (uninitialized? (instance-ref o offset))
      (let ((orgoffset (compute-value-offset original field)))
        (instance-set! o offset (instance-ref original orgoffset)) ) ) ) )

;;; Only the first fields of the duplicated instance are copied from
;;; the original. Should share better offsets computations    FUTURE

(define-method (fill-uninitialized-field-from-instance! 
                o (field Poly-Field) original )
  (let ((len (fxmin (field-length o field)
                    (field-length original field) )))
    (do ((i 0 (fx+ i 1)))
        ((fx>= i len) o)
      (unless (field-defined? o field i)
        (let ((offset (compute-value-offset o field i))
              (orgoffset (compute-value-offset original field i)) )
          (instance-set! o offset (instance-ref original orgoffset)) ) ) ) ) )
  
;;; end of clone.scm
