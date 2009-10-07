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
