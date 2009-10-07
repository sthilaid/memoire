;;; $Id: meroon.scm,v 1.1 2005/02/25 22:19:37 lucier Exp lucier $ 
;;; Copyright (c) 1990-97 by Christian Queinnec. All rights reserved.

;;;                ***********************************************
;;;                    Small, Efficient and Innovative Class System
;;;                                    Meroon 
;;;                              Christian Queinnec  
;;;                    Ecole Polytechnique & INRIA--Rocquencourt
;;;                ************************************************
;;; Additional documentation can be found in the README, meroon.ps or
;;; meroon.dvi files that are packaged with this file. Users may also
;;; get some information on Meroon with the following mailing list:
;;;	meroon-info@cornas.inria.fr
;;; or subscribe to the previous one using:
;;;	meroon-request@cornas.inria.fr

;;;==================================================== Legalistic introduction
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
;;; Send them to:
;;;       Christian Queinnec
;;;       <Christian.Queinnec@inria.fr>
;;;       INRIA -- Rocquencourt
;;;       Domaine de Voluceau, BP 105
;;;       78153 Le Chesnay Cedex
;;;       France

;;; Load the appropriate prologue (uncomment the appropriate load form
;;; if you want to only load one file). This prologue defines how to
;;; define macros as well as those features that will conditionalize
;;; the loading of the other files.

;;; Possible features are:
;;;    bootstrap           for bootstrapping. Do not use it !!!!
;;;    <scheme> <version>  to identify the Scheme system. Used to 
;;;                        conditionalize macros.scm and utils.scm.
;;;    enough-library      Do not load the additional libraries that are
;;;                        not needed to expand modules using Meroon.

;(load "meroon.bigloo")
;(load "meroon.elk")
;(load "meroon.slib")
;(load "meroon.s2c")
;(load "meroon.gam")
;(load "meroon.mac")
;(load "meroon.pcs")
;(load "meroon.pcg")
;(load "meroon.mit")
;;; and others, see README file.

;;; Then load, in order, these files:

(load "macros.scm")
(if-meroon-feature DSSSL
  (load "dsssl.scm")
  (load "nodsssl.scm") )
(if-meroon-feature bigloo
  (load "macinst.bigloo")
  (if-meroon-feature (and gambit C)
    (load "macinst.gsc")
    (load "macinst.scm") ) )
;;; Altering exodus.scm (the predefined classes of Meroon) implies to
;;; adjust careless.scm since it contains the hand-made (direct,
;;; unsafe) field accessors.
(load "careless.scm")
;;; Normally there are no more macros from here.
(load "utils.scm")
(if-meroon-feature bigloo
  (load "instance.bigloo")
  (if-meroon-feature (and gambit C)
    (load "instance.gsc")
    (load "instance.scm") ) )
(load "revision.scm")
(load "walker.scm")
(load "runtime.scm")
(load "option.scm")
(load "access.scm")
(load "dispatch.scm")
(load "handy.scm")
(load "definers.scm")
(load "alloc.scm")
;;; These are hand-made classes corresponding to exodus.scm.
(load "genes1.scm")
(load "genes2.scm")
(if-meroon-feature bootstrap
  (load "interm.scm")
  (load "Basics.scm") )
(load "fill.scm")
(load "coinst.scm")
(load "maker.scm")
(if-meroon-feature bootstrap
  ;; regenerate the Basics.scm and Coercers.scm files.
  (begin (load "exodus.scm")
         (load "filer.scm")
         (generate-all-files) 
         "Bootstrap files regenerated." )
  ;; Finish loading the libraries.
  (begin (load "view.scm")
         (load "libgen.scm")
         (load "Coercers.scm")
         (load "anomaly.scm")
         (load "multi.scm")
         ;; The above set of files is sufficient to expand Meroon sources.
         ;; The following form a library of convenient forms or functions.
         (if-meroon-feature (not enough-libraries)
           (begin (load "show.scm")
                  (load "trace.scm")
                  (load "size.scm")
                  (load "unveil.scm")
                  (load "egal.scm")
                  (load "modify.scm")
                  (load "clone.scm")
                  ;; Load some MOP modifications for Brad Lucier:
                  (if-meroon-feature (lucier)
                    (load "postset.scm")
                    #f )
                  "Meroon V3 loaded." )
           "bare Meroon V3 loaded." ) ) )

;;; end of meroon.scm 
