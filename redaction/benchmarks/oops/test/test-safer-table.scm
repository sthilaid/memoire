;;;===================================================
;;;===================================================
;;; Table Testing

(define (make-symbol-table)
  (<ts-table> key-ok?: symbol? key=?: eq? hash: symbol-hash))


(define st (make-symbol-table))

;; Populate the table.

(let loop ( (n (char->integer #\a)) )
  (let ( (str (string (integer->char n) (integer->char (+ 1 n)))) )
    (elt-set! st (string->symbol str) str)
    (if (< n (char->integer #\y)) (loop (+ 1 n)))))

(let loop ( (n (char->integer #\A)) )
  (let ( (str (string (integer->char n) (integer->char (+ 1 n)))) )
    (elt-set! st (string->symbol str) str)
    (if (< n (char->integer #\Y)) (loop (+ 1 n)))))

(let loop ( (n (char->integer #\b)) )
  (let ( (str (string (integer->char n) (integer->char (- n 1)))) )
    (elt-set! st (string->symbol str) str)
    (if (< n (char->integer #\z)) (loop (+ 1 n)))))

(let loop ( (n (char->integer #\B)) )
  (let ( (str (string (integer->char n) (integer->char (- n 1)))) )
    (elt-set! st (string->symbol str) str)
    (if (< n (char->integer #\Z)) (loop (+ 1 n)))))

#| ;; check basics (single-threaded)
(elt-ref st 'ab)
"ab"
""
(num-elts st)
100
""
(elt-set! st 'cd "CeeDee")
'cd
""
(elt-ref st 'cd)
"CeeDee"
""
(remove-key! st 'uv)
#t
""
(elt-ref-or st 'uv 'not-found)
'not-found
""
(num-elts st)
99
""
|#

;; (require 'format)

(call-with-output-file "test-table.txt"
  (lambda (p)
    (format p "====PRE-MUTATION TABLE====~%")
    (for-each-key-elt (lambda (key val) (format p "~%key=~a val=~s" key val)) st)
    (format p "~%====done====~%")))

(define (noisily-set! key new-val table-walker)
  (elt-set! st key new-val)
  (format #t "~%(elt-set! st ~s ~s) set! => ~s"
          key new-val
          (%table-walker-overrides-alist table-walker))
)

(define (noisily-add! key new-val table-walker)
  (elt-set! st key new-val)
  (format #t "~%(elt-set! st ~s ~s) add! => ~s"
          key new-val
          (%table-walker-elides-list table-walker))
)

(define (noisily-remove! key table-walker)
  (remove-key! st key)
  (format #t "~%(table-remove! st ~s) rem! => ~s"
          key
          (%table-walker-removes-alist table-walker))
)

(define (walker-thunk)
  (with-exception-catcher
   (lambda (exec) (pp exec))
   (lambda ()
     (call-with-output-file "table-test.out"
       (lambda (outport)
         (for-each-key-elt
          (lambda (key val)
            (format outport "~%key=~a val=~s" key val)
            (format #t          "~%key=~a val=~s" key val)
            (thread-sleep! 0.25))
          st)
         (format outport "~%complete~%")
         (format #t      "~%complete~%")))
     'walker-is-complete
) ) )

(define (mutator-add!-thunk)
  (with-exception-catcher
   (lambda (exec) (pp exec))
   (thread-sleep! 5)
   (let init ( (walker-info (walkers st)) )
     (if (null? walker-info)
         (begin (thread-sleep! 3)
                (init (walkers st)))
         (let loop ( (n (char->integer #\c)) )
           (let ( (str (string (integer->char n) (integer->char (- n 1)) (integer->char (- n 2)))) )
             (noisily-add! (string->symbol str) str (car walker-info))
             (thread-sleep! 0.35)
             (if (< n (char->integer #\z)) (loop (+ 1 n)) 'add!-is-done)
) ) ) ) ) )

(define (mutator-rm!-thunk)
  (with-exception-catcher
   (lambda (exec) (pp exec))
   (thread-sleep! 5)
   (let init ( (walker-info (walkers st)) )
     (if (null? walker-info)
         (begin (thread-sleep! 3)
                (init (walkers st)))
         (let loop ( (n (char->integer #\A)) )
           (let ( (str (string (integer->char n) (integer->char (+ 1 n)))) )
             (noisily-remove! (string->symbol str) (car walker-info))
             (thread-sleep! 0.3)
             (if (< n (char->integer #\Y)) (loop (+ 1 n)) 'rm!-is-done)
) ) ) ) ) )

(define (mutator-set!-thunk)
  (with-exception-catcher
   (lambda (exec) (pp exec))
   (thread-sleep! 5)
   (let init ( (walker-info (walkers st)) )
     (if (null? walker-info)
         (begin (thread-sleep! 3)
                (init (walkers st)))
         (let loop ( (n (char->integer #\B)) )
           (let ( (str (string (integer->char n) (integer->char (- n 1)))) )
             (noisily-set! (string->symbol str) n (car walker-info))
             (thread-sleep! 0.28)
             (if (< n (char->integer #\Z)) (loop (+ 1 n)) 'set!-is-done)
) ) ) ) ) )

;;(define walker-group (make-thread-group 'walker-test))

(define threads
  (list (make-thread walker-thunk       'walker-thunk       );;walker-group)
        (make-thread mutator-add!-thunk 'mutator-add!-thunk );;walker-group)
        (make-thread mutator-rm!-thunk  'mutator-rm!-thunk  );;walker-group)
        (make-thread mutator-set!-thunk 'mutator-set!-thunk );;walker-group)))
) )

(define (run-threads)
  (map thread-start! threads))

(define (suspend-threads)
  (map thread-suspend! threads))

(define (resume-threads)
  (map thread-resume! threads))

(define (stop)   (suspend-threads));; (thread-group-suspend!   walker-group))
(define (resume) (resume-threads));; (thread-group-resume!    walker-group))
;;(define (kill)   (thread-group-terminate! walker-group))


;;;   --- E O F ---   ;;;
