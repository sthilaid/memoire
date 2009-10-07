;;; $Id: dispatch.scm,v 1.1 2005/02/25 22:19:37 lucier Exp $ 
;;; Copyright (c) 1990-97 by Christian Queinnec. All rights reserved.

;;;                ***********************************************
;;;                    Small, Efficient and Innovative Class System
;;;                                    Meroon 
;;;                              Christian Queinnec  
;;;                    Ecole Polytechnique & INRIA--Rocquencourt
;;;                ************************************************

;;; This file contains the run-time functions that allows generic
;;; dispatching. Many functions are generic so one can add new kind of
;;; dispatchers. In that case, one has to extend the find-method
;;; function which is not generic since it must runs fast. 

;;; The predefined dispatchers classes appear in genesis.scm. Some
;;; types exist:
;;;   Immediate-Dispatcher does not check the class of the
;;;     discriminating variable and imposes the method to be found.
;;;   Subclass-Dispatcher tests the class of the discriminating variable 
;;;     and proposes two dispatchers whether the discriminating variable is
;;;     an instance or not of this class.
;;;   Indexed-Dispatcher finds a method in a vector indexed by the class-number
;;;     of the discriminating instance.
;;;   Global-Dispatcher uses a classes-wide Indexed-Dispatcher.
;;;   Linear-Dispatcher uses an Alist-like structure for multi-methods.

;;; Dispatchers in generic functions look like a decision tree that cannot
;;; be deeper than the following depth. When a method is added, the
;;; whole dispatcher is compressed to respect this depth. A zero depth
;;; means that all generic functions dispatch automatically with an
;;; Indexed-Dispatcher, a high number favours the elaboration of a
;;; decision tree which a high number of run-time tests. 
;;; A negative value is a special case meaning "dont try to compress
;;; dispatchers". It is used for debug purposes.

(define *dispatcher-max-depth* 
  (if-meroon-feature use-global-dispatchers
      -1    ; Use Global-Dispatchers
      3     ; compress dispatchers into less than 3 levels
      ) )

;;; extend the vector of classes as well as the dispatch tables of all
;;; generic functions (whether traced or not) that need it. This is
;;; necessary for indexed dispatchers that might need to be extended
;;; to take the new classes into account. Global-Dispatchers always
;;; have the size of vector *classes*. Whenever this latter grows, the
;;; formers grow similarly.

(define (extend-classes-number!)
  (set! *classes* (vector-extend *classes*))
  (sequence-map 
   (lambda (g)
     (when g (careless-set-Generic-dispatcher! 
              g (enlarge-dispatcher! 
                 (careless-Generic-dispatcher g) ) )) )
   *generics* ) )

;;; Global-Dispatchers have to be enlarged and only them. They
;;; generally appear at the top position of dispatchers except if the
;;; generic function is traced (see trace.scm). As other generic
;;; functions of that file, it will be redefined in the libgen.scm
;;; file.

(define-temporary (enlarge-dispatcher! d)
  (cond ((Global-Dispatcher? d) (Global-Dispatcher-enlarge-dispatcher! d))
        (else                   d) ) )

(define (Global-Dispatcher-enlarge-dispatcher! d)
  (let* ((size (vector-length *classes*))
         (oldn (careless-Global-Dispatcher-method-length d))
         (newd ((Class-allocator Global-Dispatcher-class) size)) )
    (careless-initialize-Dispatcher-method-finder
     newd Global-Dispatcher-find-method )
    (do ((i 0 (fx+ i 1)))
        ((fx>= i oldn))
      (careless-set-Global-Dispatcher-method!
       newd i (careless-Global-Dispatcher-method d i) ) )
    newd ) )

;; A method to fill holes in Indexed-Dispatcher. It is a sure sign of
;;; some internal error if it is ever invoked.

(define (barfing-method . arguments)
  (oo-apply report-meroon-error 'Domain 'barfing-method 
            "Absent method" arguments ) )

;;; Report that no method has been found for this GENERIC function and
;;; this set of ARGUMENTS. To increase code sharing, this is default
;;; method generator: see the expansion of define-generic.

(define (create-default-generic-method generic)
  (lambda arguments
    (oo-apply report-meroon-error 'Domain generic
              "No method for" arguments ) ) )

;;; Create an indexed dispatcher. NO is the default dispatcher and CN
;;; is the number of the class on which to index the methods.

(define (create-Indexed-Dispatcher cn . no)
  (let ((cl (number->class cn)))
    (let* ((no (if (pair? no) 
                   (car no) 
                   (make-Immediate-Dispatcher 
                    Immediate-Dispatcher-find-method
                    #f ) ))
           (sz (careless-Class-next cl))
           (size (fx+ sz (fx+ 1 (fxquotient sz 2)))) ; extra room
           (d ((Class-allocator Indexed-Dispatcher-class) size)) )
      (careless-initialize-Dispatcher-method-finder
       d Indexed-Dispatcher-find-method )
      (careless-set-Indexed-Dispatcher-class-number! d cn)
      (careless-set-Indexed-Dispatcher-class-depth! d (careless-Class-depth cl))
      (set-Indexed-Dispatcher-no! d no)
      (do ((i 0 (fx+ 1 i)))
          ((fx>= i size))
        (set-Indexed-Dispatcher-method! d i barfing-method) )
      (initialize! d) ) ) )

;;; Takes an Indexed-Dispatcher and extends it.

(define (extend-Indexed-Dispatcher d)
  (let ((old-size (Indexed-Dispatcher-method-length d))
        (newd (create-Indexed-Dispatcher 
               (Indexed-Dispatcher-class-number d)
               (Indexed-Dispatcher-no d) )))
    (do ((i 0 (fx+ 1 i)))
        ((fx= i old-size) newd)
      (set-Indexed-Dispatcher-method! 
       newd i (Indexed-Dispatcher-method d i) ) ) ) )

;;; Create an alternative dispatcher annd fill dependent slots.

(define (create-Subclass-Dispatcher cn no yes)
  (let ((cl (number->class cn)))
    (make-Subclass-Dispatcher 
     Subclass-Dispatcher-find-method
     cn (careless-Class-depth cl) no yes ) ) )

;;;============================================================= create generic
;;; Create a new Generic instance, also check version compatibility.
;;; Try to reuse the number associated to a generic having the same
;;; name if any.

(define register-Generic-1 
  (let ()
    (meroon-declare-not-inlinable)
    (lambda (rev name default-method signature top-class-name)
      (check-revision rev)
      (let ((g (make-Generic-1
                name                    ; name
                default-method          ; default
                signature               ; variables
                meroon-uninitialized    ; dispatcher
                meroon-uninitialized ) )) ; top-classes
        ;; set up a default method if there is none
        (unless (field-defined? g 'default)
          (careless-initialize-Generic-default
           g (create-default-generic-method g) ) )
        ;; set up the top-classes
        (careless-initialize-Generic-top-classes
         g (list (if top-class-name 
                     (symbol->class top-class-name 
                                    (lambda (name)
                                      (report-meroon-error
                                       'Syntax 'define-generic
                                       "No such class" name ) ) )
                     #f )) )
        ;; create the dispatcher
        (careless-set-Generic-dispatcher! 
         g (if-meroon-feature use-global-dispatchers
             (let* ((size (vector-length *classes*))
                    (d ((Class-allocator Global-Dispatcher-class) size)) )
               (careless-initialize-Dispatcher-method-finder
                d Global-Dispatcher-find-method ) 
               d )
             (let ((defdispatcher (make-Immediate-Dispatcher 
                                   Immediate-Dispatcher-find-method
                                   #f ))
                   (top-class (car (careless-Generic-top-classes g))) )
               (if (and (Class? top-class)
                        (not (eq? top-class Object-class)) )
                   (create-Indexed-Dispatcher 
                    (careless-Class-number top-class)
                    defdispatcher )
                   defdispatcher ) ) ) )
        ;; Propagate default-method to Global-Dispatcher or Indexed-Dispatcher
        (let ((d (careless-Generic-dispatcher g)))
          (when (Global-Dispatcher? d)
            (let ((size (Global-Dispatcher-method-length d))
                  (default (careless-Generic-default g)) )
              (do ((i 0 (fx+ 1 i)))
                  ((fx>= i size))
                (careless-set-Global-Dispatcher-method! d i default) ) ) )
          (when (Indexed-Dispatcher? d)
            (let ((size (Indexed-Dispatcher-method-length d))
                  (default (careless-Generic-default g)) )
              (do ((i 0 (fx+ 1 i)))
                  ((fx>= i size))
                (careless-set-Indexed-Dispatcher-method! d i default) ) ) ) )
        ;; register in the sequence of all generic functions
        (let* ((i (or (look-generic (Generic-name g))
                      (get-new-generic-number) )))
          (vector-set! *generics* i g) )
        ;; generic functions are regular instances so submit them to initialize!
        (initialize! (fill-other-fields! g)) ) ) ) )

(define register-Generic-N 
  (let ()
     (meroon-declare-not-inlinable)
     (lambda (rev name default-method signature . top-class-names)
       (check-revision rev)
       (let ((g (make-Generic-N
                 name                   ; name
                 default-method         ; default
                 signature              ; variables
                 meroon-uninitialized   ; dispatcher
                 meroon-uninitialized ) )) ; top-classes
         ;; set up a default method if there is none
         (unless (field-defined? g 'default)
           (careless-initialize-Generic-default
            g (create-default-generic-method g) ) )
         ;; set up the top-classes
         (careless-initialize-Generic-top-classes
          g (oo-map (lambda (name)
                      (if name (symbol->class name 
                                              (lambda (name)
                                                (report-meroon-error
                                                 'Syntax 'define-generic
                                                 "No such class" name ) ) )
                          #f ) )
                    top-class-names ) )
         ;; create the dispatcher
         (careless-set-Generic-dispatcher! 
          g (make-Immediate-Dispatcher 
             Immediate-Dispatcher-find-multi-method ; <- multi-method
             #f ) )
         ;; register in the sequence of all generic functions
         (let* ((i (or (look-generic (Generic-name g))
                       (get-new-generic-number) )))
           (vector-set! *generics* i g) )
         ;; generic functions are regular instances so submit them to initialize!
         (initialize! (fill-other-fields! g)) ) ) ) )

;;; Find if a previous generic function with the same name is
;;; registered, return its number if it exists.

(define (look-generic name)
  (define (look i)
    (if (fx< i *generic-number*)
        (if (eq? name (careless-Generic-name (vector-ref *generics* i)))
            i (look (fx+ 1 i)) )
        #f ) )
  (look 0) )

;;; The behavior of a generic function with only one discriminating
;;; variable. If you cannot find a method, then try to see if the
;;; object has an obsolete class-number, update it and retry to find a
;;; method.  This time if such a method cannot be found then default
;;; to the Generic default.

(define (careless-determine-method1 g o1)
  (if (Object? o1)
      (let ((d (careless-Generic-dispatcher g)))
        (let ((method (find-method1 d (object->class o1))))
          (or method
              (careless-Generic-default g) ) ) )
      (careless-Generic-default g) ) )

;;; The general function to determine a method. O* are the discriminating
;;; instances, since to discriminate on more than one argument is
;;; rare, the search through linear dispatchers expensive, update
;;; class-numbers immediately. Due to the integration with Scheme,
;;; there is a problem since Object is not the top-class ie there are
;;; Scheme values that are not objects. This might be the case of:
;;;         (define-generic (foo (o) (v)))
;;;         (define-method  (foo (o <some-class>>) (v)) ...)
;;; The actual value for v does not need to be an object at all.

(define (determine-method g . o*)
  ;; there is always at least one discriminating variable
  (if (null? (cdr o*))
      (careless-determine-method1 g (car o*))
      (let ((method (find-multi-method 
                     (careless-Generic-dispatcher g)
                     (oo-map (lambda (o) 
                               (if (Object? o) (object->class o) #f) )
                             o* ) )))
        (or method
            (careless-Generic-default g) ) ) ) )

;;;========================================================== find-method
;;; find-method works for generic functions with a single discriminating
;;; variable. It takes a dispatcher and a class then extracts from 
;;; the dispatcher, the appropriate method. Note that no method is
;;; coded as #f so you might have to fetch the default method in the
;;; generic instance if you get such a result.
;;; find-method is not generic since that would be unbearably slow.

(define (Immediate-Dispatcher-find-method d cl)
  (careless-Immediate-Dispatcher-method d) )

(define (Subclass-Dispatcher-find-method d cl)
  (if (let ((depth (careless-Subclass-Dispatcher-class-depth d))
            (number (careless-Subclass-Dispatcher-class-number d)) )
        (and (fx>= (careless-Class-depth cl) depth)
             (fx= (careless-Class-super cl depth) number) ) )
      (find-method1 (careless-Subclass-Dispatcher-yes d) cl)
      (find-method1 (careless-Subclass-Dispatcher-no d) cl) ) )

(define (Indexed-Dispatcher-find-method d cl)
  (let ((depth  (careless-Indexed-Dispatcher-class-depth d))
        (number (careless-Indexed-Dispatcher-class-number d)) )
    (if (and (fx>= (careless-Class-depth cl) depth)
             (fx= (careless-Class-super cl depth) number) )
        (careless-Indexed-Dispatcher-method
         d (careless-Class-relnum cl depth) )
        (find-method1 (careless-Indexed-Dispatcher-no d) cl) ) ) )

(define (Global-Dispatcher-find-method d cl)
  (careless-Global-Dispatcher-method d (careless-Class-number cl)) )

;;; (define (Tracing-Dispatcher-find-method d cl)...) see trace.scm

;;; These counters are used when profiling. With the first benchmark
;;; (Examples/interp.scm) results are (with Ida+4):
;;; ((dispatch index= 77758 subclass= 815590 immediate= 491882 
;;;     global= 0 else= 0) (generic-calls= 569565) )

(define *indexed-dispatch* 	0)
(define *subclass-dispatch* 	0)
(define *immediate-dispatch* 	0)
(define *global-dispatch* 	0)
(define *else-dispatch* 	0)

;;; General method for finding a method on Generic-1.
;;; The original definition was:
;;;    (define (find-method1 d cl)
;;;      ((careless-Dispatcher-method-finder d) d cl) )
;;; This was involving too much invokations. So I open-code the usual
;;; dispatcher cases. This allows to have a recursion which is better
;;; compiled. Why I did not think to it before ?! The reason is that
;;; it is no longer possible to change the find method of these usual
;;; dispatchers since they now are inlined.

(define (find-method1 d cl)
  (let ((cn (object->class-number d)))
    (cond 
     ((fx= cn original-Subclass-Dispatcher-class-number)
      (if-meroon-feature profile
        (set! *subclass-dispatch* (fx+ *subclass-dispatch* 1))
        #t )
      (if (let ((depth  (careless-Subclass-Dispatcher-class-depth d))
                (number (careless-Subclass-Dispatcher-class-number d)) )
            (and (fx>= (careless-Class-depth cl) depth)
                 (fx= (careless-Class-super cl depth) number) ) )
          (find-method1 (careless-Subclass-Dispatcher-yes d) cl)
          (find-method1 (careless-Subclass-Dispatcher-no d) cl) ) )
     ((fx= cn original-Immediate-Dispatcher-class-number)
      (if-meroon-feature profile
        (set! *immediate-dispatch* (fx+ *immediate-dispatch* 1))
        #t )
      (careless-Immediate-Dispatcher-method d) )
     ((fx= cn original-Indexed-Dispatcher-class-number)
      (if-meroon-feature profile
        (set! *indexed-dispatch* (fx+ *indexed-dispatch* 1))
        #t )
      (let ((depth  (careless-Indexed-Dispatcher-class-depth d))
            (number (careless-Indexed-Dispatcher-class-number d)) )
        (if (and (fx>= (careless-Class-depth cl) depth)
                 (fx= (careless-Class-super cl depth) number) )
            (careless-Indexed-Dispatcher-method
             d (careless-Class-relnum cl depth) )
            (find-method1 (careless-Indexed-Dispatcher-no d) cl) ) ) )
     ((fx= cn original-Global-Dispatcher-class-number)
      (if-meroon-feature profile
        (set! *global-dispatch* (fx+ *global-dispatch* 1))
        #t )
      (careless-Global-Dispatcher-method d (careless-Class-number cl)) )
     ;; Generic method for all other cases.
     (else 
      (if-meroon-feature profile
        (set! *else-dispatch* (fx+ *else-dispatch* 1))
        #t )
      ((careless-Dispatcher-method-finder d) d cl) ) ) ) )

;;;========================================================= create method
;;; This function is called at run-time to officially register the method.
;;; It also check the revision version of Meroon. This is just an
;;; entry point that is specialized on the number of discriminating
;;; variables.

(define register-method 
  (let ()
    (meroon-declare-not-inlinable)
    (lambda (generic-name
             variable-list
             rev
             method-maker
             . discriminating-class-names )
      (check-revision rev)
      (let ((g (symbol->generic generic-name
                                (lambda (name)
                                  (report-meroon-error 
                                   'Domain 'define-method
                                   "No such generic function"
                                   generic-name discriminating-class-names
                                   variable-list method-maker ) ) ))
            (classes (oo-map (lambda (name)
                               (symbol->class name
                                              (lambda (name)
                                                (report-meroon-error 
                                                 'Domain 'define-method
                                                 "No such class" 
                                                 generic-name name ) ) ) )
                             discriminating-class-names )) )
        (unless (coherent-variables? (Generic-variables g) variable-list)
          (report-meroon-error
           'Syntax 'define-method "Non congruent lambda-lists" 
           (Generic-variables g) variable-list ) )
        ;; only accepts methods that are compatible with the definition of
        ;; the generic function.
        (unless (memq (compare-signatures classes (Generic-top-classes g))
                      '(< =) )
          (report-meroon-error
           'Syntax 'define-method "method prohibited by the generic declaration"
           (Generic-top-classes g) classes ) )
        (case (length discriminating-class-names)
          ((1) (let ((c (car classes)))
                 (add-1-method! g c (method-maker g c 'void)) ))
          (else (add-N-method! g classes method-maker)) )
        generic-name ) ) ) )

;;; Test the congruence of two variable lists.  They may both contain
;;; a final n-ary (dotted) variable (which cannot be a discriminating
;;; variable (of course)). This predicate is used to test if a
;;; method is compatible with a generic function.

(if-meroon-feature (or DSSSL)
  (define (coherent-variables? generic-vars method-vars)
    (define dsssl? #f)
    (define rest? #f)
    (define (extract-keywords formals)
      ;; #!key appear at the end of the formals in DSSSL
      (if (pair? formals)
          (if (pair? (car formals))
              (cons (caar formals) (extract-keywords (cdr formals)))
              (cons (car formals) (extract-keywords (cdr formals))) )
          '() ) )
    (define (set-included? set1 set2)
      (if (pair? set1)
          (and (memq (car set1) set2)
               (set-included? (cdr set1) set2) )
          #t ) )
    (define (set-equal? set1 set2)
      (if (pair? set1)
          (and (memq (car set1) set2)
               (set-equal? (cdr set1) (remove (car set1) set2)) )
          (null? set2) ) )
    (define (cv generic-vars method-vars)
      (if (pair? generic-vars)
          (if (pair? method-vars)
              (if (and (not dsssl?)
                       (pair? (car generic-vars))
                       (pair? (car method-vars)) )
                  ;; similar discriminating variable
                  (cv (cdr generic-vars) (cdr method-vars))
                  ;; not a discriminating variable
                  (cond 
                   ((dsssl-optional? (car generic-vars))
                    (set! dsssl? #t)
                    (and (equal? (car generic-vars) (car method-vars))
                         (cv (cdr generic-vars) (cdr method-vars)) ) )
                   ((dsssl-rest? (car generic-vars))
                    (set! dsssl? #t)
                    (set! rest? #t)
                    (and (equal? (car generic-vars) (car method-vars))
                         (cv (cdr generic-vars) (cdr method-vars)) ) )
                   ((dsssl-key? (car generic-vars))
                    ;; keywords may be unordered
                    (and (equal? (car generic-vars) (car method-vars))
                         ((if rest? set-included? set-equal?)
                          (extract-keywords (cdr generic-vars))
                          (extract-keywords (cdr method-vars)) ) ) )
                   (else
                    (if (not dsssl?)
                        ;; similar non discriminating variables
                        (and (symbol? (car generic-vars))
                             (symbol? (car method-vars))
                             (cv (cdr generic-vars) (cdr method-vars)) )
                        ;; similar optional or rest variables
                        (cv (cdr generic-vars) (cdr method-vars)) ) ) ) )
              #f )
          (or (and (null? generic-vars) (null? method-vars))
              ;; similar dotted variable (meaningless in DSSSL)
              (and (not dsssl?)
                   (symbol? generic-vars)
                   (symbol? method-vars) ) ) ) )
    (cv generic-vars method-vars) )
  ;; R4RS normal comparison
  (define (coherent-variables? la lb)
    (if (pair? la)
        (if (pair? lb)
            (and (or;; similar discriminating variable
                  (and (pair? (car la))
                       (pair? (car lb)) )
                  ;; similar regular variable
                  (and (symbol? (car la))
                       (symbol? (car lb)) ) )
                 (coherent-variables? (cdr la) (cdr lb)) )
            #f )
        (or (and (null? la) (null? lb))
            ;; similar dotted variable
            (and (symbol? la) (symbol? lb)) ) ) )
)

;;; For Brad Lucier, so he may redefine it to support part of
;;; DSSSL-style arguments list. I now think that he does not need it
;;; any longer. This assignment hurts the exportation of this function
;;; for Bigloo so I comment it.
;;;(set! coherent-variables? coherent-variables?)

;;;======================================================== update dispatcher
;;; When a class is created then, for each generic function, propagate
;;; the methods of the superclass towards this class. While scanning
;;; dispatchers, update also the copy of the max fields of
;;; Indexed-Dispatchers of Subclass-Dispatchers [Recall that a fresh
;;; class is created with the highest class-number possible (by
;;; renumbering) so the min field is already correct.]  Another case
;;; is possible when class already existed. In that case, just reset
;;; the methods. The dispatchers are supposed to verify that if C1 is
;;; a super of C2 then C1 cannot occur in the no part of a
;;; Subclass-Dispatcher on C2.

;;; This function confers to GENERIC the method of the super of CLASS.

(define-temporary (Generic-update! generic class)
  (cond ((Generic-1? generic) (Generic-1-update! generic class))
        ((Generic-N? generic) (Generic-N-update! generic class))
        (else (report-meroon-error 'internal 'Generic-update!
                                   "No such method" generic class)) ) )

;;; It is not necessary to update generic-N functions since
;;; multimethods use Linear-Dispatcher that do not require to be
;;; updated.

(define (Generic-N-update! generic class)
  generic )

;;; Walk a Generic-1 dispatcher to update Subclass-Dispatcher and
;;; Indexed-Dispatcher ones. update-dispatcher! is a generic function.

(define (Generic-1-update! generic class)
  (set-Generic-dispatcher! 
   generic (update-dispatcher! (Generic-dispatcher generic) class) ) )

(define-temporary (update-dispatcher! d class)
  ((cond ((Immediate-Dispatcher? d) Immediate-Dispatcher-update-dispatcher!)
         ((Subclass-Dispatcher? d)  Subclass-Dispatcher-update-dispatcher!)
         ((Indexed-Dispatcher? d)   Indexed-Dispatcher-update-dispatcher!)
         ((Global-Dispatcher? d)    Global-Dispatcher-update-dispatcher!)
         ((Tracing-Dispatcher? d)   Tracing-Dispatcher-update-dispatcher!)
         (else                      default-update-dispatcher!) )
   d class ) )

;;; should never be called
(define default-update-dispatcher! internal-meroon-error)

(define (Immediate-Dispatcher-update-dispatcher! d class)
  d )

(define (Subclass-Dispatcher-update-dispatcher! d class)
  (let ((depth (careless-Subclass-Dispatcher-class-depth d))
        (number (careless-Subclass-Dispatcher-class-number d)) )
    (if (and (fx>= (careless-Class-depth class) depth)
             (fx= (careless-Class-super class depth) number) )
        (careless-set-Subclass-Dispatcher-yes! 
         d (update-dispatcher! (careless-Subclass-Dispatcher-yes d) class) )
        (careless-set-Subclass-Dispatcher-no! 
         d (update-dispatcher! (careless-Subclass-Dispatcher-no d) class) ) ) 
    d ) )

(define (Indexed-Dispatcher-update-dispatcher! d class)
  (let* ((depth  (careless-Subclass-Dispatcher-class-depth d))
         (number (careless-Subclass-Dispatcher-class-number d)) )
    (let ((d (if (fx> (careless-Indexed-Dispatcher-method-length d) 
                    (careless-Class-next (number->class number)) )
                 d 
                 (extend-Indexed-Dispatcher d) )))
      (if (and (fx>= (careless-Class-depth class) depth)
               (fx= (careless-Class-super class depth) number) )
          (let* ((supercl (number->class (careless-Class-super-number class)))
                 (method  (find-method1 d supercl))
                 (old-method (find-method1 d class)) )
            (define (fill! cn)
              (let* ((cl (number->class cn))
                     (content (find-method1 d cl)) )
                (when (eq? content old-method)
                  (careless-set-Indexed-Dispatcher-method! 
                   d (careless-Class-relnum cl depth) method )
                  (for-each fill! (careless-Class-subclass-numbers 
                                   (number->class cn) )) ) ) )
            (fill! (careless-Class-number class)) )
          ;; walk the `no' part
          (careless-set-Indexed-Dispatcher-no!
           d (update-dispatcher! (careless-Indexed-Dispatcher-no d) class) ) )
      d ) ) )

(define (Global-Dispatcher-update-dispatcher! d class)
  (let* ((supercl    (number->class (careless-Class-super-number class)))
         (method     (find-method1 d supercl))
         (old-method (find-method1 d class)) )
    (define (fill! cn)
      (let* ((cl      (number->class cn))
             (content (find-method1 d cl)) )
        (when (eq? content old-method)
          (careless-set-Global-Dispatcher-method! d cn method)
          (for-each fill! (careless-Class-subclass-numbers 
                           (number->class cn) )) ) ) )
    (fill! (careless-Class-number class))
    d ) )

;;; Traced generic are transparent for addition of classes.

(define (Tracing-Dispatcher-update-dispatcher! d class)
  (set-Tracing-Dispatcher-dispatcher!
   d (update-dispatcher! (Tracing-Dispatcher-dispatcher d) class) )
  d )

;;;======================================================== augment dispatcher
;;; Add a method to a generic function on a particular class. This is
;;; actually done by assigning the generic instance an augmented
;;; dispatcher. It can also be done with a functional style, in this way
;;; a new generic instance would be synthetized rather than modifying
;;; the old one but this consumes a stupendous amount of dispatchers
;;; when a new class is created.

(define (add-1-method! generic class method)
  (let* ((dd (Generic-dispatcher generic))
         (d (augment-dispatcher! dd class method)) )
    (set! d (compress-dispatcher! d *dispatcher-max-depth* Object-class))
    (set-Generic-dispatcher! generic d)
    (Generic-name generic) ) )

;;; Compute a new dispatcher that provides method on class without
;;; side-effect. Try to compress dispatchers if few methods otherwise
;;; use indexed dispatch tables.

(define-temporary (augment-dispatcher! d class method)
  ((cond ((Immediate-Dispatcher? d) Immediate-Dispatcher-augment-dispatcher!)
         ((Subclass-Dispatcher? d)  Subclass-Dispatcher-augment-dispatcher!)
         ((Indexed-Dispatcher? d)   Indexed-Dispatcher-augment-dispatcher!)
         ((Global-Dispatcher? d)    Global-Dispatcher-augment-dispatcher!)
         ((Tracing-Dispatcher? d)   Tracing-Dispatcher-augment-dispatcher!)
         (else                      default-augment-dispatcher!) )
   d class method ) )

;;; should never be called
(define default-augment-dispatcher! internal-meroon-error)

(define (Immediate-Dispatcher-augment-dispatcher! d class method)
  (cond ((eq? method (Immediate-Dispatcher-method d)) d)
        ;; Often the case:
        ((eq? class Object-class)
         (make-Immediate-Dispatcher 
          Immediate-Dispatcher-find-method
          method ) )
        (else (let ((yes (make-Immediate-Dispatcher 
                          Immediate-Dispatcher-find-method
                          method ))
                    (no d) )
                (create-Subclass-Dispatcher
                 (Class-number class) no yes ) )) ) )

;;; Add a method on a class in an Indexed-Dispatcher. There are two
;;; cases whether the class already exists (then patch the vector
;;; of methods (to avoid copying the Indexed-Dispatcher)) or is
;;; freshly created. In this latter case, a new Indexed-Dispatcher
;;; must be allocated to hold this new method as well as all the old
;;; ones.

(define (Indexed-Dispatcher-augment-dispatcher! d class method)
  (let* ((cn    (Indexed-Dispatcher-class-number d))
         (depth (Indexed-Dispatcher-class-depth d))
         (cl    (number->class cn)) )
    ;; extend the indexed-dispatcher if needed
    (let ((d (if (fx> (Indexed-Dispatcher-method-length d)
                    (careless-Class-next cl) )
                 d 
                 (extend-Indexed-Dispatcher d) )))
      (cond 
       ;; also take care of class = cl:
       ((careless-subclass? class cl)
        (let ((old-method (find-method1 d class)))
          (define (fill! cn)
            (let* ((cl      (number->class cn))
                   (content (find-method1 d cl)) )
              (when (eq? content old-method)
                (set-Indexed-Dispatcher-method! 
                 d (careless-Class-relnum cl depth) method )
                (for-each fill! (Class-subclass-numbers 
                                 (number->class cn) )) ) ) )
          (fill! (Class-number class))
          d ) )
       ;; Add a Subclass-Dispatcher before theIndexed-Dispatcher
       ((careless-subclass? cl class)
        (let ((no-d (Indexed-Dispatcher-no d)))
          (set-Indexed-Dispatcher-no! 
           d (make-Immediate-Dispatcher
              Immediate-Dispatcher-find-method
              method ) )
          (make-Subclass-Dispatcher
           Subclass-Dispatcher-find-method
           (careless-Class-number class)
           (careless-Class-depth class)
           no-d
           d ) ) )
       ;; Augment (physically) the no part of this Indexed-Dispatcher
       (else
        (let ((no-d (augment-dispatcher! (Indexed-Dispatcher-no d) 
                                         class method )))
          (set-Indexed-Dispatcher-no! d no-d)
          d ) ) ) ) ) )

(define (Global-Dispatcher-augment-dispatcher! d class method)
  (let ((old-method (find-method1 d class)))
    (define (fill! cn)
      (let* ((cl      (number->class cn))
             (content (find-method1 d cl)) )
        (when (eq? content old-method)
          (set-Global-Dispatcher-method! d cn method)
          (for-each fill! (Class-subclass-numbers cl)) ) ) )
    (fill! (careless-Class-number class)) )
  d )

;;; Traced dispatchers are transparent for method addition.

(define (Tracing-Dispatcher-augment-dispatcher! d class method)
  (set-Tracing-Dispatcher-dispatcher!
   d (augment-dispatcher! (Tracing-Dispatcher-dispatcher d) class method) )
  d )

;;; Three cases depending on the relation between CLASS and the class
;;; on which the dispatcher dispatches: CL. One invariant that must be
;;; respected is: if C1 is a superclass of C2 then a
;;; Subclass-Dispatcher on C2 cannot have a dispatcher for C1 in its
;;; no part.

(define (Subclass-Dispatcher-augment-dispatcher! d class method)
  (let ((cl (number->class (Subclass-Dispatcher-class-number d))))
    (cond 
     ;; augment the `yes' part
     ((eq? cl class)
      (let ((yes-d (augment-dispatcher! (Subclass-Dispatcher-yes d) 
                                        class method )))
        (if (and (Subclass-Dispatcher? yes-d)
                 (fx= (Subclass-Dispatcher-class-number d)
                    (Subclass-Dispatcher-class-number yes-d) ) )
            ;; remove one level
            (set-Subclass-Dispatcher-yes! d (Subclass-Dispatcher-yes yes-d))
            ;; keep it
            (set-Subclass-Dispatcher-yes! d yes-d) )
        d ) )            
     ;; also augment the `yes' part
     ((careless-subclass? class cl)
      (set-Subclass-Dispatcher-yes!
       d (augment-dispatcher! (Subclass-Dispatcher-yes d) class method) )
      d )
     ;; insert a new level of Subclass-Dispatcher. Extract from the no part
     ;; the dispatchers inferior to class.
     ((careless-subclass? cl class)
      (let ((newd (create-Subclass-Dispatcher
                   (Class-number class)
                   (Subclass-Dispatcher-no d)
                   (create-Subclass-Dispatcher
                    (Subclass-Dispatcher-class-number d)
                    (make-Immediate-Dispatcher
                     Immediate-Dispatcher-find-method
                     method )
                    (Subclass-Dispatcher-yes d) ) )))
        (set-Subclass-Dispatcher-no!
         newd (rebalance-dispatcher! newd (Subclass-Dispatcher-no newd)) )
        newd ) )
     ;; augment the `no' part
     (else (set-Subclass-Dispatcher-no! 
            d (augment-dispatcher! (Subclass-Dispatcher-no d) class method) )
           d )) ) )

;;; Note (*): This must avoid the following bug: suppose you have
;;; classes A with subclass B and C.  You define mb as a method for B,
;;; then mc for C, you thus have a Subclass-Dispatcher on B with mb as
;;; yes and in the no part a Subclass-Dispatcher with mc as yes and #f
;;; as no.  Suppose now that you define ma for A then you must not put
;;; mc in the no part of a Subclass-Dispatcher for A nor you can put
;;; ma in the no part of the Subclass-Dispatcher for B. You have to
;;; rebalance the tree of dispatchers.

(define-temporary (rebalance-dispatcher! d dno)
  ((cond 
    ((Subclass-Dispatcher? dno)  Subclass-Dispatcher-rebalance-dispatcher!)
    ((Immediate-Dispatcher? dno) Immediate-Dispatcher-rebalance-dispatcher!)
    ((Indexed-Dispatcher? dno)   Indexed-Dispatcher-rebalance-dispatcher!)
    ((Global-Dispatcher? dno)    Global-Dispatcher-rebalance-dispatcher!)
    ((Tracing-Dispatcher? dno)   Tracing-Dispatcher-rebalance-dispatcher!)
    (else                        internal-meroon-error) )
   d dno ) )

(define (Subclass-Dispatcher-rebalance-dispatcher! d dno)          
  (let ((cl (number->class (Subclass-Dispatcher-class-number d)))
        (clno (number->class (Subclass-Dispatcher-class-number dno))) )
    (if (careless-subclass? clno cl)
        (let ((no (Subclass-Dispatcher-no dno)))
          (set-Subclass-Dispatcher-no!
           dno (Subclass-Dispatcher-yes d) )
          (set-Subclass-Dispatcher-yes! d dno)
          (rebalance-dispatcher! d no) )
        ;; Due to the invariant, clno and cl are unrelated
        (set-Subclass-Dispatcher-no!
         dno (rebalance-dispatcher! 
              d (Subclass-Dispatcher-no dno) ) ) ) ) )

(define (Immediate-Dispatcher-rebalance-dispatcher! d dno)
  dno )

(define (Indexed-Dispatcher-rebalance-dispatcher! d dno)
  (let ((cl (number->class (Subclass-Dispatcher-class-number d)))
        (clno (number->class (Indexed-Dispatcher-class-number dno))) )
    (if (careless-subclass? clno cl)
        (let ((no (Indexed-Dispatcher-no dno)))
          (set-Indexed-Dispatcher-no!
           dno (Subclass-Dispatcher-yes d) )
          (set-Subclass-Dispatcher-yes! d dno)
          (rebalance-dispatcher! d no) )
        ;; Due to the invariant, clno and cl are unrelated
        (set-Indexed-Dispatcher-no!
         dno (rebalance-dispatcher! 
              d (Indexed-Dispatcher-no dno) ) ) ) ) )

(define (Global-Dispatcher-rebalance-dispatcher! d dno)
  dno )

(define (Tracing-Dispatcher-rebalance-dispatcher! d dno)
  dno )

;;;======================================================== compress dispatcher
;;; Convert a dispatcher into an Indexed-Dispatcher if too bushy. This
;;; will speed-up method looking. Compressing dispatchers use LEVEL
;;; to record the depth of the dispatching tree. Whenever too deep,
;;; access is done through an indexing scheme. TOP-CLASS is the
;;; greatest possible class for an instance reaching the current D.

(define-temporary (compress-dispatcher! d level top-class)
  ((cond ((Immediate-Dispatcher? d) Immediate-Dispatcher-compress-dispatcher!)
         ((Indexed-Dispatcher? d)   Indexed-Dispatcher-compress-dispatcher!)
         ((Subclass-Dispatcher? d)  Subclass-Dispatcher-compress-dispatcher!)
         ((Global-Dispatcher? d)    Global-Dispatcher-compress-dispatcher!)
         ((Tracing-Dispatcher? d)   Tracing-Dispatcher-compress-dispatcher!)
         (else                      internal-meroon-error) )
   d level top-class ) )

;;; Converts a dispatcher into an indexed dispatcher. It will be
;;; accessed by all instances of CLASS. If the class is the Object class
;;; then use a Global-Dispatcher instead. This is slightly faster for the
;;; default method and also when walking dispatchers.

(define (indexize-dispatcher dispatcher class)
  (if (eq? class Object-class)
      (globalize-dispatcher dispatcher)
      (really-indexize-dispatcher dispatcher class) ) )

(define (really-indexize-dispatcher dispatcher class)
  ;;(display `(indexize-dispatcher ,dispatcher ,class))(newline) ; DEBUG
  (let* ((depth  (careless-Class-depth class))
         (number (careless-Class-number class))
         (newd   (create-Indexed-Dispatcher 
                  number 
                  (make-Immediate-Dispatcher 
                   Immediate-Dispatcher-find-method 
                   #f ) )) )
    (let fill! ((cn number))
      (let ((cl (number->class cn)))
        (set-Indexed-Dispatcher-method! 
         newd (careless-Class-relnum cl depth) (find-method1 dispatcher cl) )
        (for-each fill! (Class-subclass-numbers cl)) ) )
    ;;(display `(result is ,newd))(newline) ; DEBUG
    newd ) )

;;; Should be faster to insert the default method instead of #f but
;;; this will prevent changing the default method of generic
;;; functions.

(define (globalize-dispatcher dispatcher)
  ;;(display `(globalize-dispatcher ,dispatcher))(newline) ; DEBUG
  (let* ((size (vector-length *classes*))
         (newd ((Class-allocator Global-Dispatcher-class) size)) )
    (careless-initialize-Dispatcher-method-finder
     newd Global-Dispatcher-find-method )
    (do ((i 0 (fx+ i 1)))
        ((fx>= i size))
      (set-Global-Dispatcher-method! newd i #f) )
    (let fill! ((cn (careless-Class-number Object-class)))
      (let ((cl (number->class cn)))
        (set-Global-Dispatcher-method! 
         newd cn (find-method1 dispatcher cl) )
        (for-each fill! (Class-subclass-numbers cl)) ) )
    ;;(display `(result is ,newd))(newline) ; DEBUG
    newd ) )

(define (Global-Dispatcher-compress-dispatcher! d level top-class)
  d )

(define (Immediate-Dispatcher-compress-dispatcher! d level top-class)
  d )

(define (Indexed-Dispatcher-compress-dispatcher! d level top-class)
  d )

(define (Tracing-Dispatcher-compress-dispatcher! d level top-class)
  d )

;;; When compressing a Subclass-Dispatcher and if the dispatcher
;;; corresponding to the NO part is indexed then there is no point to
;;; not indexize also the YES part since they are accessed by the same
;;; TOP-CLASS. This version of compress-dispatcher! is no longer
;;; purely functional to lessen data consumption.

(define (Subclass-Dispatcher-compress-dispatcher! d level top-class)
  (if (fx<= level 0) 
      (indexize-dispatcher d top-class)
      (let ((no-d (compress-dispatcher! 
                   (Subclass-Dispatcher-no d) (fx- level 1) top-class )))
        (if (or (Indexed-Dispatcher? no-d)
                (Global-Dispatcher? no-d) )
            (indexize-dispatcher d top-class)
            (let ((yes-d (compress-dispatcher! 
                          (Subclass-Dispatcher-yes d)
                          (fx- level 1)
                          (number->class 
                           (Subclass-Dispatcher-class-number d) ) )))
              (if (and (Indexed-Dispatcher? yes-d)
                       (fx= (Subclass-Dispatcher-class-number d)
                          (Indexed-Dispatcher-class-number yes-d) ) )
                  ;; remove the Subclass-Dispatcher level
                  (begin 
                    (set-Indexed-Dispatcher-no! yes-d no-d)
                    yes-d )
                  ;; keep the Subclass-Dispatcher level
                  (begin
                    (set-Subclass-Dispatcher-yes! d yes-d)
                    (set-Subclass-Dispatcher-no! d no-d)
                    d ) ) ) ) ) ) )

;;; NOTE: This compaction is too simple-minded, should be improved!!!

;;; end of dispatch.scm
