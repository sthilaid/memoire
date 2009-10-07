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
