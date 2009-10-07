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
