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
