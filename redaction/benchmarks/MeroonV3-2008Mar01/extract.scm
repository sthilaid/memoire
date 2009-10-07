;;; $Id: extract.scm,v 1.1 2005/02/25 22:19:37 lucier Exp lucier $
;;; Copyright (c) 1990-97 by Christian Queinnec. All rights reserved.

;;;                ***********************************************
;;;                    Small, Efficient and Innovative Class System
;;;                                    Meroon 
;;;                              Christian Queinnec  
;;;                    Ecole Polytechnique & INRIA--Rocquencourt
;;;                ************************************************

;;; This file extracts headers of definition out of Meroon sources. It
;;; should be run with Elk to respect case sensitivity and as such it
;;; might use some features of Elk. It can also be run with sci as
;;; well as other Schemes but we lose case.

;;; The goal is to extract all define forms out of a file to produce a
;;; new file with headers. Care must be taken to analyze all toplevel
;;; forms ie begin, if-meroon-feature, define-temporary, define-inline
;;; and define-internal-meroon-macro. 

;;; Load some utilities from Meroon to understand Meroon macros.

(cond ((equal? boot-scm "elk -h 10000")
       (define (extract-load f)
         (native-load f top-level-environment) )
       (load "meroon.elk") )
      ((equal? boot-scm "mzscheme")
       ;; Do not use, the format function is different.
       (define (extract-load f)
         (native-load f) )
       (load "meroon.mzscheme")
       (set! read-case-sensitive #t) )
      ;; Force an error
      (else (display "Not a recognized boot scheme")(newline)
            (/ "Not a recognized boot scheme" 0)) )

(load "macros.scm")
(load "utils.scm")
(load "computl.scm")

;;; Define the implementation wrt which headers must be extracted.

(set! *meroon-features*
      '( ;; renumber-classes
         ;; Scheme->C
         ;; bigloo
         ) )

;;;ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo Extraction
;;; Process a file, enriching a global variable with all found headers
;;; and return it at the end.

(define *defined-names* '())

(define (adjoin! info)
  (set! *defined-names* (cons info *defined-names*)) )

(define (process-file file-in features)
  (set! *meroon-features* features)
  (call-with-input-file file-in
    (lambda (in)
      (set! *defined-names* '())
      (let loop ((e (read in)))
        (if (eof-object? e)
            (reverse! *defined-names*)
            (begin 
              (analyze-toplevel-form! e)
              (loop (read in)) ) ) ) ) ) )

;;; extract the name to be defined.

(define (first-name call)
  (if (pair? call)
      (first-name (car call))
      call ) )

;;; Take care of temporary definitions. The define-temporary form
;;; defines the location, the definite define will be turned into a set!.

(define *temporary-names*
  '( (define-temporary (compute-value-offset o field . index))
     (define-temporary (field-value o field . index))
     (define-temporary (set-field-value! o value field . index))
     (define-temporary (initialize-field-value! o value field . index))
     (define-temporary (field-defined? o field . index))
     (define-temporary (field-length o field))
     (define-temporary (Field-generate-next-offset! field last-field))
     (define-temporary (generate-fast-careful-reader field class-variable))
     (define-temporary (generate-fast-careful-lengther field class-variable))
     (define-temporary (generate-fast-careful-writer field class-variable))
     (define-temporary (generate-offset ovar field fieldvar . indexvar))
     (define-temporary (generate-accompanying-functions class class-options))
     (define-temporary (enlarge-dispatcher! d))
     (define-temporary (Generic-update! generic class))
     (define-temporary (update-dispatcher! d class))
     (define-temporary (augment-dispatcher! d class method))
     (define-temporary (rebalance-dispatcher! d dno))
     (define-temporary (compress-dispatcher! d level top-class))
     (define-temporary generate-predicate)
     (define-temporary generate-coercer) 
     (define-temporary (generate-allocator class class-options))
     (define-temporary generate-maker)
     (define-temporary (Field-generate-Handy-accessors field class))
     (define-temporary (generate-accessors class class-options))
     (define-temporary (Field-generate-MeroonV2-accessors field class))
     (define-internal-meroon-macro (define-temporary call . body))
     (define-temporary (is-a? o class))
     (define-temporary add-subclass)
     (define-temporary (fill-other-fields! o))
     (define-temporary (initialize! o))
     ) )

;;; This predicate recognizes if a definition is temporary or not.

(define (temporary? name)
  (let scan ((l *temporary-names*))
    (and (pair? l)
         (or (and (eq? (caar l) 'define-temporary)
                  (eq? name (first-name (cadr (car l)))) )
             (scan (cdr l)) ) ) ) )

;;; Analyze toplevel forms.

(define (analyze-toplevel-form! e)
  (if (pair? e)
      (case (car e)
        ((begin)
         (for-each analyze-toplevel-form! (cdr e)) )
        ((quote) e)
        ((if-meroon-feature)
         (let ()
           (define (feature-present? condition)
               (if (pair? condition) 
                 (case (car condition)
                   ((and) (every? feature-present? (cdr condition)))
                   ((or)  (any? feature-present? (cdr condition)))
                   ((not) (not (feature-present? (cadr condition))))
                   (else (member condition *meroon-features*)) )
                 (member condition *meroon-features*) ) )
           (analyze-toplevel-form!
            (if (feature-present? (cadr e)) (caddr e) (cadddr e)) ) ) )
        ((define meroon-define)
         (let ((name (first-name (cadr e))))
           (if (temporary? name)
               'nothing
               (adjoin! `(define ,(cadr e))) ) ) )
        ((define-temporary)
         (let ((name (first-name (cadr e))))
           (if (temporary? name)
               (adjoin! `(,(car e) ,name)) 
               (error "Not known to be temporary ~a" name) ) ) )
        ((define-generic)
         (let ((name (first-name (cadr e))))
           (if (temporary? name)
               'nothing
               (adjoin! `(,(car e) ,name)) ) ) )
        ;; Ignore these cases:
        ((define-inline 
           define-meroon-macro
           define-definite-generic ) 
         e )
        (else e) ) 
      e ) )

;;; Convert a call description to a generic into a normal interface.
;;; The expansion of a define-generic actually do not allow compilers to
;;; infer the correct signature so do not use this function.

(define (protoize call)
  (if (pair? call)
      (if (pair? (car call))
          (cons (caar call) (protoize (cdr call)))
          (cons (car call) (protoize (cdr call))) )
      call ) )

;;; Test: (pp (process-file "utils.scm" '(Scheme->C)))

;;; Extract headers from filein and output them (with the indication of
;;; the source file) to fileout.

(define (extract-headers filein fileout features)
  (let ((headers (process-file filein features))
        (from (list filein)) )
    (call-with-output-file fileout
      (lambda (out)
        (format out ";;; (Automatically generated, don't touch)
;;;                Headers of ~a ~%~%" filein )
        (for-each (lambda (header)
                    (format out "~s~%" (append header from)) )
                  headers )
        (format out "~%;;; end of headers of ~a~%" filein) ) ) ) )

;;; Test: (extract-headers "utils.scm" "/dev/tty" '(bigloo))

;;;oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo Synthesis
;;; Extract the order of dependency of files.

;;; Find the exact order of the files to be loaded. For that, intercept all
;;; load forms from meroon.scm file.

(define *files* '())

(define native-load load)

(define (determine-files features)
  (let ((old-features *meroon-features*))
    (set! *meroon-features* features)
    (set! load
          (lambda (filename)
            (set! *files* (append *files* (list (basename filename ".scm"))))
            filename ) )
    (set! *files* '())
    (extract-load "meroon.scm")
    (set! load native-load)
    (set! *meroon-features* old-features)
    *files* ) )

(set! *files* (determine-files '()))
;;;(display `(*files* are ,*files*))(newline)

;;;ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; Generate a Scheme->C interface.sch file. 
;;; Generate an extra function that will just serve to force the order
;;; of evaluation, this is a hack since I do not know how to force it by 
;;; declaration within Scheme->C.

(define (generate-sch-interface filein fileout)
  (let* ((features '( Scheme->C ))
         (headers (process-file filein features))
         (basefile (basename filein ".scm")) )
    (call-with-output-file fileout
      (lambda (out)
        (format out ";;; (Automatically generated, don't touch)
;;;                      Interface of ~a for Scheme->C.~%~%" filein )
        (for-each (lambda (header)
                    (display `(define-external ,(cadr header) ,basefile) out)
                    (newline out) )
                  headers )
        (newline out)
        ;; generate the extra entry point:
        (format out "~%;;; end of interface of ~a~%" filein) ) ) ) )

;;; Test: (generate-sch-interface "utils.scm" "/dev/tty")

;;; Generate all interfaces. The macros.scm file only contains macros
;;; and as such does not have an associated interface.

(define (generate-all-sch-interfaces dir)
  (for-each (lambda (file)
              (format #t ";;; Produce ~a.sch ...~%" file)
              (generate-sch-interface 
               (string-append file ".scm")
               (string-append dir file ".sch") ) )
            *files* ) )

;;; Generate a Scheme->C module.sc file. This is complex since we must
;;; take care of the dependency order. We must also not include the
;;; proper interface of the file. There is also a huge inclusion of
;;; the Meroon prologue as far as enough libraries are loaded.

(define (generate-scc-module filein fileout)
  (call-with-output-file fileout
    (lambda (out)
      (let* ((basefile (basename filein ".scm"))
             (other-files *files*)
             (before-files 
              (reverse (cdr (member basefile (reverse other-files)))) ) )
        (format out ";;; (Automatically generated, don't touch)
;;;                      Module of ~a for Scheme->C.~%~%" filein )
        (display `(module ,basefile 
                          ,@(if (pair? before-files)
                                `((with ,@before-files))
                                `() ) )
                 out )
        (newline out)
        (format out "
;;; This is a hack to load Meroon into the compiler to expand its own sources.
(define-macro hack (begin
  (loadq \"../../../meroon.s2c\")
  (set! *meroon-features*  
        (cons 'enough-libraries *meroon-features*) )
  (set! load (lambda (file)
               (loadq (string-append \"../../../\" file)) ))
  (load \"meroon.scm\") 
  (lambda (e m) e) ))
;;; DEBUG: this hack tells which Meroon module is loaded.
(set-top-level-value! 'Loading-meroon-module \"~a\")~%~%" basefile )
        (for-each (lambda (file)
                    (format out "(include \"~a.sch\")~%" file) )
                  (remove basefile other-files) )
        (newline out) 
        (write `(include ,filein) out)
        (newline out) 
        ;(let ((previous (let scan ((previous #f)
        ;                           (files other-files) )
        ;                  (if (or (null? files)
        ;                          (equal? (car files) basefile) )
        ;                      previous
        ;                      (scan (car files) (cdr files)) ) )))
        ;  (display (if previous
        ;               `(define ,(symbol-concatenate 'meroon-load- basefile)
        ;                  (,(symbol-concatenate 'meroon-load- previous)) )
        ;               `(define ,(symbol-concatenate 'meroon-load- basefile)
        ;                  (lambda () #t) ) )
        ;           out )
        ;  (newline out) )
        (format out "~%;;; end of module of ~a.~%" filein) ) ) ) )

;;; Test: (generate-scc-module "utils.scm" "/dev/tty")
;;; Test: (generate-scc-module "revision.scm" "/dev/tty")

;;; Generate all modules for Scheme->C.

(define (generate-all-scc-modules dir)
  (for-each (lambda (file)
              (format #t ";;; Produce ~a.sc ...~%" file)
              (generate-scc-module
               (string-append file ".scm")
               (string-append dir file ".sc") ) )
            *files* ) )

;;;ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; Generate a Bigloo module

(define *meroon-features* '( bigloo 2.1 DSSSL ))

(define (generate-bgl-module filein fileout)
  (call-with-output-file fileout
    (lambda (out)
      (let* ((features *meroon-features*)
             (basefile (basename filein ".scm"))
             (other-files (determine-files features))
             (tail (member basefile (reverse other-files))) )
        ;; With the creation of (no)dsssl.scm, it is possible that tail is #f
        (if (not tail)
            (begin
              (format out ";;; (Automatically generated, don't touch)
;;;                      Module of ~a for Bigloo.~%~%" filein )
              (format out "(module ~a)~%" basefile)
              (format out "~%;;; end of module ~a~%" filein) )
            (let ((before-files
                   (reverse (cdr (member basefile (reverse other-files)))) )
                  (other-basefiles 
                   (remove "macros" (remove basefile other-files)) )
                  (headers (process-file filein features)) )
              (format out ";;; (Automatically generated, don't touch)
;;;                      Module of ~a for Bigloo.~%~%" filein )
              (format out "(module ~a
  (include ~s)~%" basefile (string-append "./" filein) )
              ;(format out "  (load (meroonV3 \"meroon.bgl\"))~%")
              ;(for-each (lambda (f) 
              ;            (format out "  (load (~a ~s))~%"
              ;                    f (string-append f ".bgl") ))
              ;          (append before-files (list basefile)) )
              (when (pair? other-basefiles)
                (format out "  (use")
                (for-each (lambda (file)
                            (format out "~%    (~a ~s)" 
                                    (string->symbol file)
                                    (string-append file ".bgl") ) )
                          other-basefiles )
                (format out " )~%") )
              ;(when (pair? before-files)
              ;  (format out "  (force")
              ;  (for-each (lambda (file)
              ;              (format out "~%  ~a" (string->symbol file)) )
              ;            before-files )
              ;  (format out " )~%") )
              (when (pair? headers)
                (format out "  (export")
                (for-each (lambda (header)
                            (format out "~%    ~a" (cadr header)) )
                          headers )
                (format out " )") )
              (format out " )~%")
              ;;(format out "~%(eval ''this-is-a-hack)~%")
              (format out "~%;;; end of module ~a~%" filein) ) ) ) ) ) )

;;; Test: (generate-bgl-module "utils.scm" "/dev/tty")

;;; Generate all modules for Bigloo.

(define (generate-all-bgl-modules dir)
  (for-each (lambda (file)
              (format #t ";;; Produce ~a.bgl ...~%" file)
              (generate-bgl-module
               (string-append file ".scm")
               (string-append dir file ".bgl") ) )
            (determine-files *meroon-features*) ) )

;;; Test: (load "extract.scm")
;;; Test: (generate-all-sch-interfaces "o/news_mips/SCC/")
;;; Test: (generate-all-scc-modules "o/news_mips/SCC/")
;;; Test: (generate-all-bgl-modules "o/news_mips/BGL/")

;;; end of extract-headers.scm
