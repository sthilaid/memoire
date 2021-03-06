;;; $Id: genes2.scm,v 1.1 2005/02/25 22:19:37 lucier Exp lucier $ 
;;; Copyright (c) 1990-97 by Christian Queinnec. All rights reserved.

;;;                ***********************************************
;;;                    Small, Efficient and Innovative Class System
;;;                                    Meroon 
;;;                              Christian Queinnec  
;;;                    Ecole Polytechnique & INRIA--Rocquencourt
;;;                ************************************************

;;; This file defines the end of the initial net of classes, generics
;;; and methods that are needed by Meroon to run. It is a delicate
;;; piece of bootstrap.

;;;========================================================= Field
;;; The initializer field is now mutable.

(define Pre-Field-class
  (instance
   original-Class-class-number          ; internal index 
   'Pre-Field                           ; name
   original-Pre-Field-class-number      ; number
   (list                                ; fields
    (create-Mono-Field #t 'name         original-Pre-Field-class-number 0) )
   1                                    ; depth
   original-Object-class-number         ; super-number
   (list                                ; subclasses
    original-Field-class-number )
   4                                    ; next
   meroon-uninitialized                 ; allocator
   #f                                   ; immutable?
   (list)                               ; views
   3
   original-Object-class-number         ; super[0]
   original-Pre-Field-class-number       ; super[1] = relnum[0]
   0                                    ; relnum[1]
   ) )

(define Field-class
  (instance
   original-Class-class-number          ; internal index 
   'Field                               ; name
   original-Field-class-number          ; number
   (simple-append-fields                ; fields
    (careless-Class-fields Pre-Field-class)
    (list                     
     (create-Mono-Field #t 'immutable?   original-Field-class-number 1)
     (create-Mono-Field #f 'class-number original-Field-class-number 2)
     (create-Mono-Field #t 'initialized? original-Field-class-number 3)
     (create-Mono-Field #f 'initializer  original-Field-class-number 4)
     (create-Poly-Field #t 'path         original-Field-class-number 5) ))
   2                                    ; depth
   original-Pre-Field-class-number      ; super-number
   (list                                ; subclasses
    original-Mono-Field-class-number
    original-Poly-Field-class-number )
   3                                    ; next
   meroon-uninitialized                 ; allocator
   #f                                   ; immutable?
   (list)                               ; views
   5
   original-Object-class-number         ; super[0]
   original-Pre-Field-class-number       ; super[1] 
   original-Field-class-number          ; super[2] = relnum[0]
   1                                    ; relnum[1]
   0                                    ; relnum[2]
   ) )

;;;========================================================= Mono-Field
(define Mono-Field-class
  (instance
   original-Class-class-number          ; internal index 
   'Mono-Field                          ; name
   original-Mono-Field-class-number     ; number
   (simple-append-fields                ; fields
    (careless-Class-fields Field-class)
    (list) )
   3                                    ; depth
   original-Field-class-number          ; super-number
   (list)                               ; subclasses
   1                                    ; next
   meroon-uninitialized                 ; allocator
   #f                                   ; immutable?
   (list)                               ; views
   7
   original-Object-class-number         ; super[0]
   original-Pre-Field-class-number       ; super[1]
   original-Field-class-number          ; super[2]
   original-Mono-Field-class-number     ; super[3] = relnum[0]
   2                                    ; relnum[1]
   1                                    ; relnum[2]
   0                                    ; relnum[3]
   ) )

;;;========================================================= Poly-Field
(define Poly-Field-class
  (instance
   original-Class-class-number          ; internal index 
   'Poly-Field                          ; name
   original-Poly-Field-class-number     ; number
   (simple-append-fields                ; fields
    (careless-Class-fields Field-class)
    (list) )
   3                                    ; depth
   original-Field-class-number          ; super-number
   (list)                               ; subclasses
   1                                    ; next
   meroon-uninitialized                 ; allocator
   #f                                   ; immutable?
   (list)                               ; views
   7
   original-Object-class-number         ; super[0]
   original-Pre-Field-class-number       ; super[1]
   original-Field-class-number          ; super[2]
   original-Poly-Field-class-number     ; super[3] = relnum[0]
   3                                    ; relnum[1]
   2                                    ; relnum[2]
   0                                    ; relnum[3]
   ) )

;;;========================================================= Dispatcher
(define Dispatcher-class
  (instance
   original-Class-class-number          ; internal index 
   'Dispatcher                          ; name
   original-Dispatcher-class-number     ; number
   (list                                ; fields
    (create-Mono-Field #t 'method-finder original-Dispatcher-class-number 0) )
   1                                    ; depth
   original-Object-class-number         ; super-number
   (list                                ; subclasses
    original-Immediate-Dispatcher-class-number
    original-Subclass-Dispatcher-class-number
    original-Indexed-Dispatcher-class-number
    original-Linear-Dispatcher-class-number
    original-Global-Dispatcher-class-number
    original-Tracing-Dispatcher-class-number
    )
   7                                    ; next
   meroon-uninitialized                 ; allocator
   #t                                   ; immutable?
   (list)                               ; views
   3
   original-Object-class-number         ; super[0]
   original-Dispatcher-class-number     ; super[1] = relnum[0]
   0                                    ; relnum[1]
   ) )

;;;======================================================= Immediate-Dispatcher
(define Immediate-Dispatcher-class
  (instance
   original-Class-class-number          ; internal index 
   'Immediate-Dispatcher                ; name
   original-Immediate-Dispatcher-class-number ; number
   (simple-append-fields                ; fields
    (careless-Class-fields Dispatcher-class)
    (list
     (create-Mono-Field #t 'method  
                        original-Immediate-Dispatcher-class-number 1 ) ) )
   2                                    ; depth
   original-Dispatcher-class-number     ; super-number
   (list)                               ; subclasses
   1                                    ; next
   meroon-uninitialized                 ; allocator
   #t                                   ; immutable?
   (list)                               ; views
   5
   original-Object-class-number         ; super[0]
   original-Dispatcher-class-number     ; super[1]
   original-Immediate-Dispatcher-class-number ; super[2] = relnum[0]
   1                                    ; relnum[1]
   0                                    ; relnum[2]
   ) )

;;;======================================================= Subclass-Dispatcher
(define Subclass-Dispatcher-class
  (instance
   original-Class-class-number          ; internal index 
   'Subclass-Dispatcher                 ; name
   original-Subclass-Dispatcher-class-number ; number
   (simple-append-fields                ; fields
    (careless-Class-fields Dispatcher-class)
    (list
     (create-Mono-Field #t 'class-number 
                        original-Subclass-Dispatcher-class-number 1)
     (create-Mono-Field #t 'class-depth
                        original-Subclass-Dispatcher-class-number 2)
     (create-Mono-Field #f 'no
                        original-Subclass-Dispatcher-class-number 3)
     (create-Mono-Field #f 'yes
                        original-Subclass-Dispatcher-class-number 4) ) )
   2                                    ; depth
   original-Dispatcher-class-number     ; super-number
   (list)                               ; subclasses
   1                                    ; next
   meroon-uninitialized                 ; allocator
   #t                                   ; immutable?
   (list)                               ; views
   5
   original-Object-class-number         ; super[0]
   original-Dispatcher-class-number     ; super[1]
   original-Subclass-Dispatcher-class-number ; super[2] = relnum[0]
   2                                    ; relnum[1]
   0                                    ; relnum[2]
   ) )

;;;======================================================= Indexed-Dispatcher
(define Indexed-Dispatcher-class
  (instance
   original-Class-class-number          ; internal index 
   'Indexed-Dispatcher                  ; name
   original-Indexed-Dispatcher-class-number ; number
   (simple-append-fields                ; fields
    (careless-Class-fields Dispatcher-class)
    (list
     (create-Mono-Field #t 'class-number
                        original-Indexed-Dispatcher-class-number 1)
     (create-Mono-Field #t 'class-depth
                        original-Indexed-Dispatcher-class-number 2)
     (create-Mono-Field #f 'no
                        original-Indexed-Dispatcher-class-number 3)
     (create-Poly-Field #f 'method
                        original-Indexed-Dispatcher-class-number 4) ) )
   2                                    ; depth
   original-Dispatcher-class-number     ; super-number
   (list)                               ; subclasses
   1                                    ; next
   meroon-uninitialized                 ; allocator
   #t                                   ; immutable?
   (list)                               ; views
   5
   original-Object-class-number         ; super[0]
   original-Dispatcher-class-number     ; super[1]
   original-Indexed-Dispatcher-class-number ; super[2] = relnum[0]
   3                                    ; relnum[1]
   0                                    ; relnum[2]
   ) )

;;;======================================================= Linear-Dispatcher
(define Linear-Dispatcher-class
  (instance
   original-Class-class-number          ; internal index 
   'Linear-Dispatcher                   ; name
   original-Linear-Dispatcher-class-number ; number
   (simple-append-fields                ; fields
    (careless-Class-fields Dispatcher-class)
    (list
     (create-Mono-Field #f 'no
                        original-Linear-Dispatcher-class-number 1)
     (create-Mono-Field #f 'method
                        original-Linear-Dispatcher-class-number 2)
     (create-Mono-Field #f 'signature
                        original-Linear-Dispatcher-class-number 3) ) )
   2                                    ; depth
   original-Dispatcher-class-number     ; super-number
   (list)                               ; subclasses
   1                                    ; next
   meroon-uninitialized                 ; allocator
   #t                                   ; immutable?
   (list)                               ; views
   5
   original-Object-class-number         ; super[0]
   original-Dispatcher-class-number     ; super[1]
   original-Linear-Dispatcher-class-number ; super[2] = relnum[0]
   4                                    ; relnum[1]
   0                                    ; relnum[2]
   ) )

;;;======================================================= Global-Dispatcher
(define Global-Dispatcher-class
  (instance
   original-Class-class-number          ; internal index 
   'Global-Dispatcher                   ; name
   original-Global-Dispatcher-class-number ; number
   (simple-append-fields                ; fields
    (careless-Class-fields Dispatcher-class)
    (list
     (create-Poly-Field #f 'method
                        original-Global-Dispatcher-class-number 1) ) )
   2                                    ; depth
   original-Dispatcher-class-number     ; super-number
   (list)                               ; subclasses
   1                                    ; next
   meroon-uninitialized                 ; allocator
   #t                                   ; immutable?
   (list)                               ; views
   5
   original-Object-class-number         ; super[0]
   original-Dispatcher-class-number     ; super[1]
   original-Global-Dispatcher-class-number ; super[2] = relnum[0]
   5                                    ; relnum[1]
   0                                    ; relnum[2]
   ) )

;;;======================================================= Tracing-Dispatcher
(define Tracing-Dispatcher-class
  (instance
   original-Class-class-number          ; internal index 
   'Tracing-Dispatcher                  ; name
   original-Tracing-Dispatcher-class-number ; number
   (simple-append-fields                ; fields
    (careless-Class-fields Dispatcher-class)
    (list
     ;; adding methods to a traced dispatcher requires this field to be mutable.
     (create-Mono-Field #f 'dispatcher 
                        original-Tracing-Dispatcher-class-number 1 )
     (create-Mono-Field #t 'default
                        original-Tracing-Dispatcher-class-number 2 ) ) )
   2                                    ; depth
   original-Dispatcher-class-number     ; super-number
   (list)                               ; subclasses
   1                                    ; next
   meroon-uninitialized                 ; allocator
   #t                                   ; immutable?
   (list)                               ; views
   5
   original-Object-class-number         ; super[0]
   original-Dispatcher-class-number     ; super[1]
   original-Tracing-Dispatcher-class-number ; super[2] = relnum[0]
   6                                    ; relnum[1]
   0                                    ; relnum[2]
   ) )

;;;======================================================= Anomaly
(define Anomaly-class
  (instance
   original-Class-class-number          ; internal index 
   'Anomaly                             ; name
   original-Anomaly-class-number        ; number
   (list                                ; fields
    (create-Mono-Field #t 'category    original-Anomaly-class-number 0)
    (create-Mono-Field #t 'operator    original-Anomaly-class-number 1)
    (create-Mono-Field #t 'message     original-Anomaly-class-number 2)
    (create-Poly-Field #t 'hint        original-Anomaly-class-number 3)
    )
   1                                    ; depth
   original-Object-class-number         ; super-number
   (list                                ; subclasses
    original-Warning-class-number )
   2                                    ; next
   meroon-uninitialized                 ; allocator
   #t                                   ; immutable?
   (list)                               ; views
   3
   original-Object-class-number         ; super[0]
   original-Anomaly-class-number        ; super[1] = relnum[0]
   0                                    ; relnum[1]
   ) )

(define Warning-class
  (instance
   original-Class-class-number          ; internal index 
   'Warning                             ; name
   original-Warning-class-number        ; number
   (simple-append-fields                ; fields
    (careless-Class-fields Anomaly-class)
    (list) )
   2                                    ; depth
   original-Anomaly-class-number        ; super-number
   (list)                               ; subclasses
   1                                    ; next
   meroon-uninitialized                 ; allocator
   #t                                   ; immutable?
   (list)                               ; views
   5
   original-Object-class-number         ; super[0]
   original-Anomaly-class-number        ; super[1]
   original-Warning-class-number        ; super[2] = relnum[0]
   1                                    ; relnum[1]
   0                                    ; relnum[0]
   ) )

;;;======================================================== View

(define View-class
  (instance
   original-Class-class-number          ; internal index 
   'View                                ; name
   original-View-class-number           ; number
   (simple-append-fields                ; fields
    (careless-Class-fields Pre-Class-class)
    (list
     (create-Poly-Field #t 'super         original-View-class-number 3)
     ) )
   2                                    ; depth
   original-Pre-Class-class-number      ; super-number
   (list                                ; subclasses
     )
   1                                    ; next
   meroon-uninitialized                 ; allocator
   #t                                   ; immutable?
   (list)                               ; views
   5
   original-Object-class-number         ; super[0]
   original-Pre-Class-class-number      ; super[1]
   original-View-class-number           ; super[2] = relnum[0]
   0                                    ; relnum[1]
   0                                    ; relnum[2]
   ) )

(define Virtual-Field-class
  (instance
   original-Class-class-number          ; internal index 
   'Virtual-Field                       ; name
   original-Virtual-Field-class-number  ; number
   (simple-append-fields                ; fields
    (careless-Class-fields Pre-Field-class)
    (list                     
     (create-Mono-Field #t 'view   original-Virtual-Field-class-number 1)
     (create-Mono-Field #f 'index  original-Virtual-Field-class-number 2) ))
   2                                    ; depth
   original-Pre-Field-class-number      ; super-number
   (list)                               ; subclasses
   1                                    ; next
   meroon-uninitialized                 ; allocator
   #f                                   ; immutable?
   (list)                               ; views
   5
   original-Object-class-number         ; super[0]
   original-Pre-Field-class-number      ; super[1] 
   original-Virtual-Field-class-number  ; super[2] = relnum[0]
   4                                    ; relnum[1]
   0                                    ; relnum[2]
   ) )

;;;oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; Register all these classes in the sequence of all classes.

(for-each 
 (lambda (class) 
   (vector-set! *classes* (careless-Class-number class) class) )
 (let ((classes 
        (list 
         Object-class                   ; 0
         Pre-Class-class                ; 1
         Class-class                    ; 2
         Handy-Class-class              ; 3
         MeroonV2-Class-class           ; 4
         Applyable-Object-class         ; 5
         Generic-class                  ; 6
         Generic-1-class                ; 7
         Generic-N-class                ; 8
         Pre-Field-class                ; 9
         Field-class                    ; 10
         Mono-Field-class               ; 11
         Poly-Field-class               ; 12
         Dispatcher-class               ; 13
         Immediate-Dispatcher-class     ; 14
         Subclass-Dispatcher-class      ; 15
         Indexed-Dispatcher-class       ; 16
         Linear-Dispatcher-class        ; 17
         Global-Dispatcher-class        ; 18
         Tracing-Dispatcher-class       ; 19
         Anomaly-class                  ; 20
         Warning-class                  ; 21
         View-class                     ; 22
         Virtual-Field-class            ; 23
         ) ))
   (unless (fx= *class-number* (length classes))
     (oo-error 'genesis "Missed some classes") )
   classes ) )

;;; end of genes2.scm
