;;; $Id: computl.scm,v 1.1 2005/02/25 22:19:37 lucier Exp $
;;; Copyright (c) 1990-96 by Christian Queinnec. All rights reserved.

;;;                ***********************************************
;;;                    Small, Efficient and Innovative Class System
;;;                                    Meroon 
;;;                              Christian Queinnec  
;;;                    Ecole Polytechnique & INRIA--Rocquencourt
;;;                ************************************************

;;; This file contains various utilites that are necessary to compile
;;; Meroon, expand files, prepare module headers and so forth.

;;;oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; Read a whole file into a list of expressions.

(define (file->list filename)
  (call-with-input-file filename
    (lambda (in)
      (let loop ((e (read in)))
        (if (eof-object? e)
            '()
            (cons e (loop (read in))) ) ) ) ) )

;;; Test: (file->list "revision.scm")

;;;oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; Expand filename into target. Attention, the expand function does
;;; not exist everywhere (for example in Gambit).

(define (expand-file filename target)
  (let ((content `(begin ,@(file->list filename))))
    (call-with-output-file target
      (lambda (out)
        (display ";;; Automatically expanded file " out)
        (display filename out)
        (display " -- Don't touch!!! " out)
        (newline out) (newline out)
        ;;  DEBUG
        ;;(write `(begin (display ',filename)(newline)) out)
        ;;(newline out)(newline out)
        (write (expand content) out)
        (newline out) ) ) ) )

;;; Test: (expand-file "revision.scm" "/dev/tty")

;;;oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; As the Unix utility: strip the suffix from the string or leave the string
;;; as it is if it does not have the right suffix.

(define (basename string suffix)
  (call/cc
    (lambda (abort)
      (let* ((length-of-string (string-length string))
             (length-of-suffix (string-length suffix))
             (index length-of-string) )
        (do ((n (fx- length-of-string 1) (fx- n 1))
             (m (fx- length-of-suffix 1) (fx- m 1)) )
            ((or (fx< n 0) (fx< m 0))
             (substring string 0 index) )
          (if (char=? (string-ref string n) (string-ref suffix m))
            (set! index (fx- index 1))
            (abort string) ) ) ) )) )
; "foo" <- (basename "foo.x" ".x")
; "f.x" <- (basename "f.x" ".y")
; "foo" <- (basename "foo" "")
; "f"   <- (basename "f" "")
; ""    <- (basename "" "")

;;;oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; Find the files that comprising Meroon. This is done by evaluating
;;; the meroon.scm file with a redefinition of load. Hope there is no
;;; problem while computing this since load is redefined. 
;;; NOTE: the current if-meroon-feature macro must be appropriately set.

(define (meroon-files)
  (define *files* '())
  (define native-load load)
  (define (new-load filename)
    (set! *files* (append *files* (list (basename filename ".scm"))))
    filename )
  (set! load new-load)
  (native-load "meroon.scm")
  (set! load native-load)
  *files* )

;;; end of computl.scm
