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
