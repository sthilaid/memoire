(define (old-load filename)
  (call-with-input-file
    filename
    (lambda (port)
      (let loop ((exp (read port)))
        (if (not (eof-object? exp))
            (begin
              (eval exp)
              (loop (read port))))))))
(old-load "pre_meroon.scm")
(compile-file "_meroon.scm")
