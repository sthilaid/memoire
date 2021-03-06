;;; $Id: meroon.gsc,v 1.1 2005/02/25 22:20:34 lucier Exp lucier $
;;; Copyright (c) 1990-97 by Christian Queinnec. All rights reserved.

;;;                            *******************************
;;;                            Gambit-C Adaptation for Meroon
;;;                            *******************************

;;; This file contains the necessary adaptation to be interpreted by Gambit-C.
;;; This prologue now works with Gambit-C 2.7.
;;;  (load "meroon.gsc")
;;;  (load "meroon.scm")

;;; This program is distributed in the hope that it will be useful.
;;; Use and copying of this software and preparation of derivative works
;;; based upon this software are permitted, so long as the following
;;; conditions are met:
;;;      o credit to the authors is acknowledged following current
;;;        academic behaviour
;;;      o no fees or compensation are charged for use, copies, or
;;;        access to this software
;;;      o this copyright notice is included intact.
;;; This software is made available AS IS, and no warranty is made about 
;;; the software or its performance. 

;;; Bug descriptions, use reports, comments or suggestions are welcome.
;;; Send them to    
;;;   <queinnec@polytechnique.fr>   or to:   <Christian.Queinnec@inria.fr>
;;;                                 or to:
;;;   Christian Queinnec                     Christian Queinnec
;;;   Laboratoire d'Informatique de l'X      INRIA -- Rocquencourt
;;;   Ecole Polytechnique                    Domaine de Voluceau, BP 105
;;;   91128 Palaiseau                        78153 Le Chesnay Cedex
;;;   France                                 France

;;;************************************************
;;;    Small, Efficient and Innovative Class System
;;;       Christian Queinnec  
;;;   \'Ecole Polytechnique & INRIA-Rocquencourt
;;;************************************************

;;; The first macro is a macro definer that allows these macros to
;;; remain in the compiled image. The second just defines a macro
;;; local to the sources but normally not visible from the user.

(define-macro (define-meroon-macro call . body)
  `(begin (eval '(define-macro ,call . ,body))
          (define-macro ,call . ,body) ) )

(define-macro (define-internal-meroon-macro call . body)
  `(define-macro ,call . ,body) )

;;; Programs of the Meroon repository use define*-meroon-macro and
;;; users' programs may also want to use portable macros.  Make these
;;; macros themselves pervasive. [Thanks to Brad Lucier
;;; <lucier@MATH.Purdue.EDU> for that nice suggestion].

;;; The problem are (1) the current file is not compiled so there is
;;; no associated .o file (2) how to make define-meroon-macro
;;; pervasive without using define-meroon-macro and thus stumbling
;;; into regression problems (3) how to make define-meroon-macro
;;; portably pervasive with a unique code (that must not depend then
;;; on the particular implementation of define-meroon-macro). 
;;; The simplest thing seems to require that each prologue defines
;;; a (make-meroon-macro-pervasive) macro that will be called in a file
;;; to be compiled (ie utils.scm).

(define-meroon-macro (make-meroon-macro-pervasive)
  (list 
   'eval 
   ''(begin 
       (define-macro (define-meroon-macro call . body)
          `(begin (eval '(define-macro ,call . ,body))
                  (define-macro ,call . ,body) ) )
       (define-macro (define-internal-meroon-macro call . body)
          `(define-macro ,call . ,body) )
       #t ) ) )

;;; Conditional compilation: The variable *meroon-features* defines
;;; the features of Meroon to be taken into account when loading (or
;;; compiling Meroon). 

(define *meroon-features* 
  '( gambit C 2.7 DSSSL
     pervasive-meroon-macros
     ;; Used by Brad Lucier to speed up Meroon:
     inline-predicate
     ) )

;;; Local Variables:
;;; mode: scheme
;;; End:

;;; End of adaptation of Meroon for Gambit-C.
