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
