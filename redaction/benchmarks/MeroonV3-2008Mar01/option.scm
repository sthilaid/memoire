;;; $Id: option.scm,v 1.1 2005/02/25 22:19:37 lucier Exp $
;;; Copyright (c) 1990-97 by Christian Queinnec. All rights reserved.

;;;                ***********************************************
;;;                    Small, Efficient and Innovative Class System
;;;                                    Meroon 
;;;                              Christian Queinnec  
;;;                    Ecole Polytechnique & INRIA--Rocquencourt
;;;                ************************************************

;;; This file contains functions that deal with options for class or
;;; field definitions and for instantiations. Options are prefixed by
;;; a keyword (a symbol with a starting colon) and followed by a
;;; number of expressions (zero, one or many).

;;; Keywords (ie symbols beginning with a colon) can appear in Meroon
;;; special forms. Unlike Common Lisp there might be zero or more than
;;; one value associated to a keyword. So the following is a legal list
;;; of options:               :opt1 123 :opt2 :opt3 a b c
;;; Brad Lucier wanted Meroon to use Gambit keywords (ie foo: instead
;;; of :foo). The solution is to support the two kinds since some 
;;; pseudo-keywords appear here and there within Meroon source files.

;;; The function string->meroon-keyword is needed to expand previous files
;;; so it appears in utils.scm.

(if-meroon-feature (and guile (or ii iii))
  ;; Guile has a special <keyword> type 
  ;; Noticed by boyd@crl.com (Bonnie L. Boyd)
  ;; This has changed with Guile 1.2.
  (define meroon-keyword? keyword?)
  (if-meroon-feature (or DSSSL)
    ;; DSSSL also defines keywords.
    (define (meroon-keyword? e)
      (or (keyword? e)
          ;; Keep these keywords since some of them appear in exodus.scm,
          ;; fill.scm and other files.
          (and (symbol? e)
               (char=? (string-ref (symbol->string e) 0) #\:) ) ) )
    (define (meroon-keyword? e)
      (and (symbol? e)
           (char=? (string-ref (symbol->string e) 0) #\:) ) ) ) )

;;; Check if a keyword has a given name. The name can be a symbol or
;;; list of symbols and does not contain the initial colon character.
;;; Pay attention not to create temporary strings or symbols.

(define (meroon-keyword-eq? thing symbols)
  ;; (assume (meroon-keyword? thing))
  (define starting-index 1)
  (define limit #f)
  (if-meroon-feature (or DSSSL)
    (if (keyword? thing)
        (begin
          (set! thing (keyword->string thing))
          (if-meroon-feature (and bigloo 2.1)
            ;; Bigloo 2.1a converts :foo into ":foo"
            (set! starting-index 1)
            (set! starting-index 0) ) ) )
    ;; This is not a DSSSL keyword.
    #f )
  ;; This may be an old Guile keyword
  (if-meroon-feature (and guile (or ii iii))
    (begin
      (set! thing (keyword->symbol thing))
      (if-meroon-feature (and guile ii)
        ;; Guile-ii converts :foo into -foo
        (set! starting-index 1)
        ;; while guile-iii converts it into foo.
        (set! starting-index 0) ) )
    #f )
  ;; Now the real comparison:
  (let* ((thing (if (string? thing) thing (symbol->string thing)))
         (limit (or limit (string-length thing)))
         (symbols (cond ((pair? symbols) symbols)
                        ((symbol? symbols) (list symbols))
                        (else (report-meroon-error 
                               'Domain 'meroon-keyword-eq?
                               "Not a keyword specification" symbols )) )) )
    (let compare ((i starting-index)(symbols symbols))
      (if (fx< i limit)
          (and (pair? symbols)
               (let* ((sym (symbol->string (car symbols)))
                      (len (string-length sym)) )
                 (let comp ((i i)(j 0))
                   (if (fx>= j len)
                       (compare i (cdr symbols))
                       (and (fx< i limit)
                            (char=? (string-ref thing i) 
                                    (string-ref sym j) )
                            (comp (fx+ 1 i) (fx+ 1 j)) ) ) ) ) )
          (null? symbols) ) ) ) )
;;; Tests: 
;;; #t = (meroon-keyword-eq? ':foo 'foo)
;;; #f = (meroon-keyword-eq? ':foo 'ffoo)
;;; #t = (meroon-keyword-eq? ':foo '(f oo))
;;; #f = (meroon-keyword-eq? ':foo '(fo oo))
;;; #t = (meroon-keyword-eq? ':foo '(f o o))
;;; #f = (meroon-keyword-eq? ':foo '(ff o o))
;;; #f = (meroon-keyword-eq? ':foo 'fooo)
;;; #f = (meroon-keyword-eq? ':foo '(fooo ooo))
;;; #f = (meroon-keyword-eq? ':foo 'f)
;;; #f = (meroon-keyword-eq? ':foo '(f o))

;;; This is the internal function used to scan list of options.  It
;;; returns the list starting with the searched keyword followed by
;;; the associated values. This is so since there are still some
;;; Scheme implementations that have problems with #f and ().

(define (find-option-plus-values keyword options)
  (define (get-values options)
    (if (or (null? options)
            (and (pair? options)(meroon-keyword? (car options))) )
        '()
        (cons (car options) (get-values (cdr options))) ) )
  (let ((options (look4keyword keyword (skip2next-keyword options))))
    (if (pair? options)
        (cons (car options) (get-values (cdr options)))
        '() ) ) )

;;; Look for the values that are prefixed by a KEYWORD in a list of
;;; OPTIONS, if no such options can be found invoke DEFAULT on the
;;; keyword.

(define (find-option-values keyword options default)
  (let ((kw+values (find-option-plus-values keyword options)))
    (if (pair? kw+values)
        (cdr kw+values)
        (default keyword) ) ) )

;;; Look for exacty one value associated with a KEYWORD in a list of
;;; OPTIONS otherwise invoke default on the keyword. It is syntactical
;;; error to not associate the keyword to a single value.

(define (find-option-single-value keyword options default)
  (let ((kw+values (find-option-plus-values keyword options)))
    (if (pair? kw+values)
        (let ((values (cdr kw+values)))
          (if (and (pair? values) (null? (cdr values)))
              (car values)
              (report-meroon-error 
               'Syntax 'find-option
               "This option expects a single value"
               keyword values ) ) )
        (default keyword) ) ) )

;;; Looks for the presence of a keyword in a list of options. The keyword
;;; should not be associated to any values.

(define (find-option-present? keyword options)
  (let ((kw+values (find-option-plus-values keyword options)))
    (if (pair? kw+values)
        (if (null? (cdr kw+values))
            #t
            (report-meroon-error
             'Syntax 'find-option
             "this option should not have associated values" 
             keyword (cdr kw+values) ) )
        #f ) ) )
;;; Tests:
;;; #f = (find-option-present? 'a '(2 3 :c :d 4 :e))
;;; #t = (find-option-present? 'a '(2 3 :c :d 4 :a))
;;; #t = (find-option-present? 'a '(2 3 :c :a :e))

;;; Check that an option occurs at most once.

(define (check-option-present-once? keyword options)
  (let ((options (look4keyword keyword (skip2next-keyword options))))
    (if (pair? options)
        (null? (look4keyword keyword (skip2next-keyword (cdr options))))
        #t ) ) )

;;; remove an option and its associated values from a list of options.
;;; Only remove the first occurrence.

(define (remove-option keyword options)
  (let skip ((options options))
    (if (pair? options)
        (if (and (meroon-keyword? (car options))
                 (meroon-keyword-eq? (car options) keyword) )
            (skip2next-keyword (cdr options))
            (cons (car options) (skip (cdr options))) )
        '() ) ) )

;;; chop the list of options until empty or beginning with a keyword.

(define (skip2next-keyword options)
  (if (or (null? options)
          (and (pair? options)
               (meroon-keyword? (car options)) ) )
      options
      (skip2next-keyword (cdr options)) ) )

;;; chop the list of options until empty or beginning y keyword.

(define (look4keyword keyword options)
  ;; (assume (meroon-keyword? (car options)))
  (if (pair? options)
      (if (meroon-keyword-eq? (car options) keyword)
          options
          (look4keyword keyword (skip2next-keyword (cdr options))) )
      '() ) )

;;; This function may be used as a default when an option is not found.
;;; For example to distinguish a Field initialized with an explicit #f.

(define meroon-option-not-there
  (list 'meroon-option-not-there) )

(define (option-not-there kw) 
  meroon-option-not-there )

(define (absent-option? e)
  (eq? e meroon-option-not-there) )
  
;;; end of option.scm
