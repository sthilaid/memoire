;;; $Id: dsssl.scm,v 1.1 2005/02/25 22:19:37 lucier Exp lucier $
;;; Copyright (c) 1990-97 by Christian Queinnec. All rights reserved.

;;;                ***********************************************
;;;                    Small, Efficient and Innovative Class System
;;;                                    Meroon 
;;;                              Christian Queinnec  
;;;                    Ecole Polytechnique & INRIA--Rocquencourt
;;;                ************************************************

;;; These are the functions to recognize DSSSL keywords. This file is
;;; not loaded by Scheme implementations that do not know how to read
;;; these keywords.

(define (dsssl-optional? thing)
  (eq? thing '#!optional) )

(define (dsssl-rest? thing)
  (eq? thing '#!rest) )

(define (dsssl-key? thing)
  (eq? thing '#!key) )

;;; end of dsssl.scm
