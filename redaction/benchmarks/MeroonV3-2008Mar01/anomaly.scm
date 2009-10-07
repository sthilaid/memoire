;;; $Id: anomaly.scm,v 1.1 2005/02/25 22:19:37 lucier Exp $ 
;;; Copyright (c) 1990-96 by Christian Queinnec. All rights reserved.

;;;                ***********************************************
;;;                    Small, Efficient and Innovative Class System
;;;                                    Meroon 
;;;                              Christian Queinnec  
;;;                    Ecole Polytechnique & INRIA--Rocquencourt
;;;                ************************************************

;;; This file defines anomalies which is the name used by Meroon to
;;; reify exceptions, errors etc. Abnormal situations are reified into
;;; instances of Anomaly then the generic function meroon-error
;;; is called on them. It is possible to define new classes of
;;; anomalies (which do not need to be instances of Anomaly, any
;;; objects can work though to confine anomalies to Anomaly is advised).
;;; The meroon-error function is generic and can be customised. 

;;; There are different categories of anomalies though not
;;; individualized in different classes (to spare the number of
;;; classes). These are syntax or runtime anomalies: runtime anomalies
;;; are further divided into domain, access or allocation anomalies.

;;; This function is used throughout Meroon to report anomalies.
;;; CATEGORY gives the overall nature of the anomaly (something like
;;; Syntax, Access, Domain). OPERATOR is the name of the function that
;;; discovered the anomaly, MESSAGE is a string explaining the problem
;;; and HINTS are hints (or culprits) that might help to grasp the problem.

(define (report-meroon-error category operator message . hints)
  (meroon-error
   (oo-apply make-Anomaly category operator message 
             (length hints) hints ) ) )

;;; All these functions report some precise type of anomalies.

(define (report-bad-index field-or-field-name o index)
  (report-meroon-error 
   'Domain (if (Field? field-or-field-name)
               field-or-field-name
               (retrieve-named-field (object->class o) field-or-field-name) )
   "Index out of bounds" o index ) )

(define (report-uninitialized-field field o . index)
  (oo-apply report-meroon-error 
            'Uninitialized field "Uninitialized field" o index ) )

(define (report-immutable-field field o . index)
  (oo-apply report-meroon-error
            'Access field "Immutable field" o index ) )

(define (report-missing-index field o)
  (report-meroon-error 'Access field "Missing index" o) )

(define (report-already-initialized field o value . index)
  (oo-apply report-meroon-error 
            'initialize! field "Already initialized" o value index ) )
  
;;; Sometime warnings can be issued for the user to improve the speed of
;;; some internal aspects of Meroon. Give them to meroon-error which has
;;; a specific method to display them.

(define (report-meroon-warning operator message . hints)
  (meroon-error
   (oo-apply make-Warning 'Warning operator message
             (length hints) hints ) ) )

;;; Anomalies are reported with a generic function so users can add
;;; their proper errors. Oo-error is a special function defined in the
;;; various prologues that calls the underlying error system.

(define-generic (meroon-error (anomaly))
  (oo-error 'meroon "Error" anomaly) )

;;; Record the last occurred anomaly.

(define-method (meroon-error (o Anomaly))
  (set! *last-meroon-anomaly* o)
  (display-meroon-anomaly o)
  (call-next-method) )

;;; Just display the anomaly, don't call (call-next-method).

(define-method (meroon-error (o Warning))
  (set! *last-meroon-anomaly* o)
  (display-meroon-anomaly o) )

;;; The universal function to display anomalies (or warnings)..

(define (display-meroon-anomaly o)
  (newline)
  (display "******************* Meroon ")
  (display (Anomaly-category o))
  (display " *********************")(newline)
  (display "Occurred in: ")
  (show (Anomaly-operator o))
  (newline)
  (display "Reason: ")
  (show (Anomaly-message o))
  (newline)
  (do ((i 0 (fx+ 1 i)))
      ((fx= i (Anomaly-hint-length o)) #f)
    (display "Hint[")
    (display i)
    (display "]: ")
    (show (Anomaly-hint o i))
    (newline) ) )

;;; end of anomaly.scm
