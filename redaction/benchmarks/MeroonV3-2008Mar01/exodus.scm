;;; $Id: exodus.scm,v 1.1 2005/02/25 22:19:37 lucier Exp lucier $ 
;;; Copyright (c) 1990-97 by Christian Queinnec. All rights reserved.

;;;                ***********************************************
;;;                    Small, Efficient and Innovative Class System
;;;                                    Meroon 
;;;                              Christian Queinnec  
;;;                    Ecole Polytechnique & INRIA--Rocquencourt
;;;                ************************************************

;;; This file (exodus follows genesis) finishes to build the initial
;;; and internal classes of Meroon. Classes can now be defined more
;;; handily with the define-class macro. 

;;;======================================================== fundamental classes
;;; All the fundamental classes were already built by hand but only a
;;; handful of accessors were defined. We can now generate all of them
;;; since all the machinery is present. Don't redefine Object which is 
;;; deeply built-in.

;;; 0
;;;(define-class Object #f ())

;;; 1
(define-class Pre-Class Object
  ((= name              :immutable) ; 0
   (= number            :immutable) ; 1
   (= fields            :immutable) ; 2
   ) )

;;; 2
(define-class Class Pre-Class
  ((= depth             :immutable) ; 3
   (= super-number      :immutable) ; 4
   (= subclass-numbers  :mutable)   ; 5
   (= next              :mutable)   ; 6
   (= allocator         :immutable) ; 7
   (= immutable?        :immutable) ; 8
   (= views		:mutable)   ; 9
   (* suprel            :immutable) ; 10
   ) )

;;; 3
(define-class Handy-Class Class ())

;;; 4
(define-class MeroonV2-Class Handy-Class ())

;;; 5                                                  FUTURE
(define-class Applyable-Object Object
  () )

;;; 6
(define-class Generic Applyable-Object
  ((= name              :immutable)  ;0
   (= default           :mutable)    ;1
   (= variables         :immutable)  ;2
   (= dispatcher        :mutable)    ;3
   (= top-classes       :immutable)  ;4
   ) )

;;; 7
(define-class Generic-1 Generic
  ( ) )

;;; 8
(define-class Generic-N Generic 
  () )

;;; 9
(define-class Pre-Field Object
  ((= name              :immutable) ;0
   ) )

;;; 10
(define-class Field Pre-Field
  ((= immutable?        :immutable) ;1
   (= class-number      :mutable)   ;2
   (= initialized?      :immutable) ;3
   (= initializer       :mutable)   ;4
   (* path              :immutable) ;5
   ) )

;;; 11
(define-class Mono-Field Field ())

;;; 12
(define-class Poly-Field Field ())

;;;=============================================================== Dispatchers
;;; Generic functions contain a dispatcher field, instances of the
;;; following classes:

;;; 13
(define-class Dispatcher Object 
  ((= method-finder     :immutable)
   ) )

;;; 14
(define-class Immediate-Dispatcher Dispatcher 
  ((= method            :immutable)
   ) )

;;; 15
(define-class Subclass-Dispatcher Dispatcher 
  ((= class-number      :immutable) ; 1
   (= class-depth       :immutable) ; 2
   (= no                :mutable)   ; 3
   (= yes               :mutable)   ; 4
   ) )

;;; 16
(define-class Indexed-Dispatcher Dispatcher 
  ((= class-number      :immutable) ; 1
   (= class-depth       :immutable) ; 2
   (= no                :mutable)   ; 3
   (* method            :mutable)   ; 4
   ) )

;;; 17
(define-class Linear-Dispatcher Dispatcher
  ((= no                :mutable)
   (= method            :mutable)
   (= signature         :mutable)
   ) )

;;; 18
(define-class Global-Dispatcher Dispatcher
  ( (* method           :mutable)
    ) )

;;; 19
(define-class Tracing-Dispatcher Dispatcher
  ( (= dispatcher       :mutable)
    (= default          :immutable)
    ) )

;;;=============================================================== Anomaly
;;; Finally all errors are handled through this class.

;;; 20
(define-class Anomaly Object 
  ( (= category         :immutable)
    (= operator         :immutable)
    (= message          :immutable)
    (* hint             :immutable)
    ) )

;;; 21
(define-class Warning Anomaly
  ( ) )

;;;=============================================================== View

;;; 22
(define-class View Pre-Class
  ((* super             :immutable) ; 3
   ) )

;;; 23
(define-class Virtual-Field Pre-Field
  ((= view              :immutable) ;1
   (= index             :immutable) ;2
   ) )

;;; end of exodus.scm
