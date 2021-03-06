;;; $Id: revision.scm,v 1.1 2005/02/25 22:19:37 lucier Exp lucier $ 
;;; Copyright (c) 1990-2000 Christian Queinnec. All rights reserved.

;;;                ***********************************************
;;;                    Small, Efficient and Innovative Class System
;;;                                    Meroon 
;;;                              Christian Queinnec  
;;;                    Ecole Polytechnique & INRIA--Rocquencourt
;;;                ************************************************

;;; This variable nicknames the current distribution. It is set up by 
;;; Imakefile when producing a new release.

(define meroon-distribution-name "Paques2001+1")

;;; A set of strings to identify the current release. Also shows the
;;; interesting features.

(define meroon-version
  (list "Meroon V3"
        meroon-distribution-name
        "$Revision: 1.1 $" ) )

;;; This couple of numbers is used to check compatibility between various
;;; versions of Meroon. Whenever a class, a generic function or a
;;; method previously compiled with some Meroon version is loaded into a
;;; different one, an anomaly will be signalled.
;;; This huge code only extracts the revision number from the string.

(define meroon-revision
  (let* ((rev "$Revision: 1.1 $")
         (length (string-length rev)) )
    (let scan ((i 0)
               (r 0) )
      (if (fx>= i length) r
          (let ((ch (string-ref rev i)))
            (case ch
              ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
               (scan (fx+ 1 i) (fx+ (fx* 10 r)
                                (fx- (char->integer ch)
                                   (char->integer #\0) ) )) )
              ((#\.) (cons r (scan (fx+ i 1) 0)))
              (else (scan (fx+ 1 i) r)) ) ) ) ) ) )

;;; This function is used to check a number against the current
;;; revision number. It appears in the expansion of define-class,
;;; define-generic and define-method macros.

(define (check-revision rev)
  (unless (equal? rev meroon-revision)
    (report-meroon-error
     'Syntax 'define-generic
     "Compiled with incompatible versions of Meroon" 
     rev meroon-revision ) ) )

;;; Display a banner

(define (display-meroon-banner out)
  (display "[ " out)
  (for-each (lambda (x) 
              (display x out)
              (display " " out) )
            meroon-version )
  (display "]" out)
  (newline out) )

(if-meroon-feature (or gambit)
  (display-meroon-banner (current-output-port))
  #f )

;;; end of revision.scm
