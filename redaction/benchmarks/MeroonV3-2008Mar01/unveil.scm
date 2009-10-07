;;; $Id: unveil.scm,v 1.1 2005/02/25 22:19:37 lucier Exp $ 
;;; Copyright (c) 1990-96 by Christian Queinnec. All rights reserved.

;;;                ***********************************************
;;;                    Small, Efficient and Innovative Class System
;;;                                    Meroon 
;;;                              Christian Queinnec  
;;;                    Ecole Polytechnique & INRIA--Rocquencourt
;;;                ************************************************

;;; This file defines the unveil function that displays all details
;;; about any Meroon object. It also knows how to display cyclic
;;; structures and non-Meroon objects such as lists or vectors. It
;;; runs slowly. All Meroon are displayed with a generic function that
;;; can be customized. By default, it invokes a meta-method that uses
;;; the structure of the object and the nature of its fields.

;;; The exported function. Takes an object and possibly a stream and
;;; output O onto STREAM.

(define (unveil o . stream)
  (let ((stream (if (pair? stream) (car stream) (current-output-port))))
    (set! *meroon-already-unveiled-objects* '())
    (set! *first-time* #t)
    (show-unveiled o 0 stream)
    (newline stream)
    (set! *meroon-already-unveiled-objects* '()) ; GC: dont keep this !
    #t ) )

;;; This is the deepest unveiling possible. Feel free to adapt it to
;;; your needs.

(define *unveil-maximal-indent* 10)

;;; Output a newline, then indent. Also mark vertical bars to explicit
;;; alignment. These bars and relative indentations can be set via the
;;; global variables:

(define *unveil-step* 2)
(define *unveil-bigstep* 6)

;;; Make these variables mutable so they are not inlined by block
;;; compilation.

(set! *unveil-maximal-indent* *unveil-maximal-indent*)
(set! *unveil-step* *unveil-step*)
(set! *unveil-bigstep* *unveil-bigstep*)

;;; Brad Lucier <lucier@MATH.purdue.EDU> prefers unveil not to start
;;; with a newline. I added this ugly global mutable variable to handle 
;;; this.

(define *first-time* #t)

(define (goto-margin indent stream)
  (let* ((step *unveil-step*)
         (bigstep *unveil-bigstep*) )
    (if *first-time*
        (set! *first-time* #f)
        (newline stream) )
    (do ((i 0 (fx+ 1 i)))
        ((fx= i indent) #f)
      (write-char (cond ((fx= (fx- bigstep 1) (fxmodulo i bigstep)) #\+)
                        ((fx= (fx- step 1) (fxmodulo i step)) #\|)
                        (else #\space) )
                  stream ) ) ) )

;;; The list of already unveiled objects to detect cycles. Should ideally
;;; be a variable local to unveil, not a global one.

(define *meroon-already-unveiled-objects* '())

;;; Check if an object is already unveiled and returns its
;;; index. Otherwise record it and return false (in that case, use
;;; (get-current-index) IMMEDIATELY after to know the index which is
;;; associated to the newly recorded object). As a matter of fact, the
;;; object is always inserted in the list of already seen objects.

(define (already-unveiled? o)
  (let* ((already-unveiled (memq o *meroon-already-unveiled-objects*))
         (index (if (pair? already-unveiled)
                    (length already-unveiled)
                    (set! *meroon-already-unveiled-objects*
                          (cons o *meroon-already-unveiled-objects*) ) )) )
    (if already-unveiled index #f) ) )

(define (get-current-index)
  (length *meroon-already-unveiled-objects*) )
  
;;; Pretty print O, INDENT is the current indentation. Handle
;;; specially lists and vectors to detect possible cycles through
;;; them.

(define (show-unveiled o indent stream)
  (cond ((Object? o) (show-object-unveiled o indent stream))
        ((vector? o) (show-unveiled-vector-content o indent stream))
        ((pair? o)   (show-unveiled-list-content o indent stream))
        (else        (show o stream)) ) )

;;; The general way to display acyclic Meroon objects. It is a generic
;;; function so it can be customized.  Display O with INDENT on
;;; STREAM.

(define-generic (show-object-unveiled (o) indent stream))

(define-method (show-object-unveiled (o Object) indent stream)
  (let ((class (object->class o))
        (index (already-unveiled? o)) )
    (if index
        (show-already-unveiled-object (Class-name class) index stream)
        (begin
          (goto-margin indent stream)
          (if (fx> indent *unveil-maximal-indent*)
              (display "<details omitted...>" stream)
              (let ((index (get-current-index)))
                ;;(format stream "(a ~A <------------- [Id: ~A]"
                ;;        (Class-name class) index )
                (display "(a " stream)
                (display (Class-name class) stream)
                (show-index index stream)
                (for-each (lambda (field) 
                            (show-unveiled-field-content
                             o (fx+ 1 indent) stream field ) )
                          (Class-fields class) )
                ;;(format stream " end ~A)" (Class-name class))
                (display " end " stream)
                (display (Class-name class) stream)
                (display ")" stream) ) ) ) ) ) )

;;; All objects are referenced with integers.

(define (show-index index stream)
  (display " <------------- [Id: " stream)
  (display index stream)
  (display "]" stream) )

;;; Display a reference to an already displayed object.

(define (show-already-unveiled-object class-name index stream)
  ;;(format stream "<the ~A referred above as ~A>"
  ;;        (Class-name class) index )
  (display "<the " stream)
  (display class-name stream)
  (display " referred above as " stream)
  (display index stream)
  (display ">" stream) )

;;; Display the content of a field according to the type of the field.
;;; This is a generic function you can extend on new types of fields
;;; if any.

(define-generic (show-unveiled-field-content o indent stream (field)))

;;; Show a Mono-Field value.

(define-method (show-unveiled-field-content 
                 o indent stream (field Mono-Field) )
  (goto-margin indent stream)
  (display (Field-name field) stream)
  (display ": " stream)
  (if (field-defined? o field)
      (show-unveiled (field-value o field) indent stream)
      (display "#<Uninitialized>" stream) ) )

;;; Show a Poly-Field value

(define-method (show-unveiled-field-content 
                 o indent stream (field Poly-Field) )
  (let loop ((n (field-length o field))
             (i 0) )
    (when (fx< i n)
      (goto-margin indent stream)
      (display (Field-name field) stream)
      (display "[" stream)
      (display i stream)
      (display "]: " stream)
      (if (field-defined? o field i)
          (show-unveiled (field-value o field i) indent stream)
          (display "#<Uninitialized>" stream) )
      (loop n (fx+ 1 i)) ) ) )

;;;========================================================== Native showers
;;; Show the content of a vector (or a list) taking care to detect cycles.
;;; The presentation of vectors and lists mimics the presentation of a
;;; Poly-Field.

(define (show-unveiled-vector-content o indent stream)
  (let ((class-name "Vector")
        (index (already-unveiled? o)) )
    (if index
        (show-already-unveiled-object class-name index stream)
        (let ((index (get-current-index)))
          (goto-margin indent stream)
          (display "(a Vector " stream)
          (show-index index stream)
          (let ((indent (fx+ 1 indent)))
            (goto-margin indent stream)
            (let loop ((n (vector-length o))
                       (i 0) )
              (when (fx< i n)
                (display "Vector[" stream)
                (display i stream)
                (display "]: " stream)
                (show-unveiled (vector-ref o i) indent stream)
                (goto-margin indent stream)
                (loop n (fx+ 1 i)) ) ) )
          (display " end " stream)
          (display class-name stream)
          (display ")" stream) ) ) ) )

(define (show-unveiled-list-content o indent stream)
  (let* ((class-name "List")
         (index (already-unveiled? o)) )
    (if index
        (show-already-unveiled-object class-name index stream)
        (let ((index (get-current-index)))
          (goto-margin indent stream)
          (display "(a List " stream)
          (show-index index stream)
          (let ((indent (fx+ 1 indent)))
            (goto-margin indent stream)
            (let loop ((o o)
                       (i 0) )
              (cond ((pair? o)
                     (display "List[" stream)
                     (display i stream)
                     (display "]: " stream)
                     (show-unveiled (car o) indent stream)
                     (goto-margin indent stream)
                     (cond ((pair? (cdr o))
                            (let ((index (already-unveiled? (cdr o))))
                              (if index
                                  (begin (display " . " stream)
                                         (show-already-unveiled-object 
                                          class-name index stream) )
                                  (loop (cdr o) (fx+ 1 i)) ) ) )
                           ((null? (cdr o)) 'nothing)
                           (else (display " . " stream)
                                 (show-unveiled (cdr o) indent stream) ) ) )
                    ((null? o) #f)
                    (else (display " . " stream)
                          (show-unveiled o indent stream)
                          (goto-margin indent stream) ) ) ) )
          (display " end " stream)
          (display class-name stream)
          (display ")" stream) ) ) ) )

;;; end of unveil.scm
