;;; $Id: nodsssl.scm,v 1.1 2005/02/25 22:19:37 lucier Exp lucier $
;;; Copyright (c) 1990-97 by Christian Queinnec. All rights reserved.

;;;                ***********************************************
;;;                    Small, Efficient and Innovative Class System
;;;                                    Meroon 
;;;                              Christian Queinnec  
;;;                    Ecole Polytechnique & INRIA--Rocquencourt
;;;                ************************************************

;;; This file is loaded by Scheme implementations that do not know how to
;;; read DSSSL keywords.

(define (dsssl-optional? thing) #f)

(define (dsssl-rest? thing) #f)

(define (dsssl-key? thing) #f)

;;; end of nodsssl.scm
