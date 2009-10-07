;;; $Id: filer.scm,v 1.1 2005/02/25 22:19:37 lucier Exp $
;;; Copyright (c) 1990-93 by Christian Queinnec. All rights reserved.

;;;                ***********************************************
;;;                    Small, Efficient and Innovative Class System
;;;                                    Meroon 
;;;                              Christian Queinnec  
;;;                   \'Ecole Polytechnique & INRIA--Rocquencourt
;;;                ************************************************

;;; This file defines the functions that construct the files necessary
;;; for the bootstrap. Once the net of classes is loaded, they can be
;;; used to generate the code that will rebuild them. 

;;; This is the ordered list of all the classes that are in the initial net.

(define initial-classes
  (let ((result '()))
    (do ((i (fx- *class-number* 1) (fx- i 1)))
        ((fx< i 0))
      (let ((cl (vector-ref *classes* i)))
        (set! result (cons cl result) ) ) )
    result ) )

;;; Try to simplify an expression
(define (simplify-bootstrap-expression e)
  e )

;;; Write in a file all the accessors to the initial classes.

(define (write-accessor-file out) 
  (display-header out)
  (for-each 
   (lambda (class)
     (simplified-display 
      (simplify-bootstrap-expression 
       (MeroonV2-Class-generate-accessors class '()) )
      out ) 
     (newline out) )
   initial-classes ) )

;;; Write in a file all the coercers towards the initial classes. Skip
;;; those that are redefined later (see libgen.scm)

(define (write-coercer-file out) 
  (display-header out)
  (for-each 
   (lambda (class)
     (unless (memq (Class-name class) '(Object Class Generic))
       (simplified-display 
        (simplify-bootstrap-expression 
         (generate-coercer class '()) )
        out )
       (newline out) ) )
   initial-classes ) )

;;; Write in a file all the makers for the initial classes. 

(define (write-maker-file out) 
  (display-header out)
  (for-each 
   (lambda (class)
     (unless (memq (Class-name class) '())
       (simplified-display 
        (simplify-bootstrap-expression 
         (generate-maker class '()) )
        out )
       (newline out) ) )
   initial-classes ) )

;;; Write in a file all the predicate associated to the initial classes.

(define (write-predicate-file out)
  (display-header out)
  (for-each 
   (lambda (class)
     (unless (memq (Class-name class) '(Object))
       (simplified-display 
        (simplify-bootstrap-expression 
         (generate-predicate class '()) )
        out )
       (newline out) ) )
   initial-classes ) )

;;; Display a definition avoiding useless forms.

(define (simplified-display def out)
  (when (pair? def)
    (case (car def)
      ((begin) (for-each (lambda (def) (simplified-display def out))
                         (cdr def) ))
      ((quote) #f)
      (else (write def out)) ) )
  (newline out) )

;;; Just say that generated files should not be touched.

(define (display-header out)
  (display ";;; File automatically generated (Don't touch)." out)
  (newline out)
  (newline out) )

;;; Generate all the needed files. They are two:
;;;    Basics.scm contains predicates, accessors and makers. It must be
;;;               loaded before genesis.scm
;;;    Coercers.scm contains coercers. This must be loaded after genesis.scm

(define (generate-all-files)
  (call-with-output-file "Basics.scm"
    (lambda (out)
      (write-predicate-file out)
      (write-accessor-file  out)
      (write-maker-file     out)
      (display ";;; end of Basics.scm" out)
      (newline out) ) )
  (call-with-output-file "Coercers.scm"
    (lambda (out)
      (write-coercer-file   out)
      (display ";;; end of Coercers.scm" out)
      (newline out) ) )
  #t )

;;; end of filer.scm
