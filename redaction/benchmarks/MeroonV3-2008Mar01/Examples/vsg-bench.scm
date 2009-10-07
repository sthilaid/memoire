;;; $Id: vsg-bench.scm,v 1.3 1995/02/28 14:01:10 queinnec Exp $

;;; Copyright (c) 1990-94 by Christian Queinnec. All rights reserved.

;;;                ***********************************************
;;;                    Small, Efficient and Innovative Class System
;;;                                    Meroon 
;;;                              Christian Queinnec  
;;;                    Ecole Polytechnique & INRIA--Rocquencourt
;;;                ************************************************

;;; This is another bench for Meroon. It uses quite a lot of memory.
;;; It defines 12 classes, 5 generic functions and 27 methods.

;;; Define the stream of Scheme programs.

(define Scheme-CStream
  (build-Recursive-CStream 
   self
   (let ((var (enum 'x 'y 't 'car 'set-car! 'cons)))
     (++ var
         (+ (Kleene self)
            (letrec ((body self))
              `(begin . ,body) )
            (letrec ((form self))
              `(quote ,form) )
            (letrec ((c self)
                     (th self)
                     (el self) )
              `(if ,c ,th ,el) )
            (letrec ((v var)
                     (form self) )
              `(set! ,v ,form) )
            (letrec ((variables (Kleene var))
                     (body self) )
              `(lambda ,variables ,body) ) ) ) ) ) )

;;; Explore this stream to compute how many programs there are with
;;; increasing number of symbols.

(define (start-bench)
  (show-meroon)
  ;; transform the stream so it begins with all programs with five symbols
  ;; There are 0 terms of cost 0.
  ;; 5 terms of cost 1.
  ;; 17 terms of cost 2.
  ;; 89 terms of cost 3.   <- immediate
  ;; 665 terms of cost 4.  <- roughly 30 seconds
  ;; 4805 terms of cost 5. <- roughly 2 minutes if compiled or 8 if interpreted.
  (count-terms/cost Scheme-CStream 5)
  ;; Now this is quite immediate 
  (count-terms/cost Scheme-CStream 5) )

;;; end of vsg-bench.scm
