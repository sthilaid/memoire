;;; $Id: definers.scm,v 1.1 2005/02/25 22:19:37 lucier Exp $ 
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
    `(meroon-define ,class-variable
       (initialize!
        (fill-other-fields!
         (register-class 
          ',meroon-revision
          ((Class-allocator ,metaclass-name)
           ,(Class-suprel-length class) ,@metaclass-allocation-args )
          ',name
          ,super-variable
          (list ,@own-fields-code) ) ) ) ) ) )

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
