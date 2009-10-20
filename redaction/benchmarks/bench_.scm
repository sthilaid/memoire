(define-macro (time-expr . exprs)
  (let ((time (gensym 'time)))
    `(let ((,time (time->seconds (current-time))))
       ,@ exprs
       (- (time->seconds (current-time)) ,time))))

(define-macro (define-bench id env . exprs)
  (define n 500000)
  `(define (,id)
     (let ,env
       (let ((t (time-expr (do ((i 0 (+ i 1)))
                               ((= i benchmark-limit) 'ok)
                             ,@exprs))))
         (list (quote ,id) t)))))

(define benchmark-limit 1000000)
