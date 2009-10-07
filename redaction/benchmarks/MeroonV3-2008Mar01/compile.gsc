;;; $Id: compile.gsc,v 3.3 1997/02/12 16:31:04 queinnec Exp $
;;; Copyright (c) 1990-97 by Christian Queinnec. All rights reserved.

;;;                ***********************************************
;;;                    Small, Efficient and Innovative Class System
;;;                                   Meroon 
;;;                              Christian Queinnec  
;;;                   \'Ecole Polytechnique & INRIA--Rocquencourt
;;;                ************************************************

;;; This file explains how to compile Meroon for Gambit-C (version for Unix).
;;; It has been improved for gsc 2.4

;;; INSTRUCTIONS:

;;;   Check the Makefile of the distribution to see if all the variables 
;;;   related to GAMBIT are appropriately setup.

;;;   Run
;;;           make SCM=GSC o/${HOSTTYPE}/GSC/gsi+
;;;   This will generate a Gambit interpreter with compiled Meroon.

;;;   If you want to have a Gambit interpreter+compiler with again a compiled
;;;   Meroon then run
;;;           make SCM=GSC o/${HOSTTYPE}/GSC/gsc++


;;;ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;;                       End of Public part.

;;; Meroon V3 sources are assumed to be already present. They should be
;;; included by hand from the toplevel rather than loaded here.

;;; This is where generated files will go. This directory must exist.
;;; In fact, it is set up by the Imakefile (to avoid decoding the
;;; shell variable HOSTTYPE) see o/${HOSTTYPE}/GSC/all.c dependence.

;(define compiled-meroon-subpath "o/${HOSTTYPE}/GSC/")

;;; Load some common utilities needed for compilation.

(load "computl.scm")

;;; Redefine meroon-files from computl.scm to keep extensions.

(define (meroon-files)
  (define *files* '())
  (define native-load load)
  (define (new-load filename)
    (set! *files* (cons filename *files*))
    filename )
  (set! load new-load)
  (native-load "meroon.scm")
  (set! load native-load)
  (reverse *files*) )

;;; These are the files to process. 

(define *files* (meroon-files))

;;; Compile all files into C code.

(define (compile-meroon-file-to-c filename)
  (compile-file-to-c 
   filename
   (list 'verbose)
   (string-append compiled-meroon-subpath
                  (basename (basename filename ".scm") ".gsc")
                  ".c" ) ) )

(for-each compile-meroon-file-to-c *files*)

;;; Compile all files into .o files.  Unfortunately, the .o file (as
;;; well as the .c file) appears in the current directory. [No chdir
;;; in Gambit ?]

(define (compile-meroon-file filename)
  (compile-file
   filename
   (list 'verbose) ) )

;(for-each compile-meroon-file *files*)

;;; These are the basenames of the compiled files.

(define *basefiles*
  (map (lambda (filename) 
         (string-append compiled-meroon-subpath
                        (basename (basename filename ".scm") ".gsc") ) )
       *files* ) )

;;; Prepare the flat link file, they use the value of GSC_MODULES from
;;; the Makefile. NOTE: should be included automatically ????]
; They also uses the value of GAMBIT_GSC_DIR.

'''
(link-flat (append (map (lambda (f)
                          (string-append "/usr/local/src/gambc241/gsc/" f) )
                        '("_host" "_utils" "_parms" "_source"
                          "_env" "_ptree1" "_ptree2" "_gvm" "_back"
                          "_front" "_prims" "_t-c-1" "_t-c-2" "_t-c-3" ) )
                   *basefiles* )
           (string-append compiled-meroon-subpath "MeroonV3.c") )

;;; Local Variables:
;;; mode: scheme
;;; End:

;;; end of compile.gsc
