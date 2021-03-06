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
