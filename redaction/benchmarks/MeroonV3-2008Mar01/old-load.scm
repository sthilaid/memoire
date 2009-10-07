;;; $Id: old-load.scm,v 1.1 2005/02/25 22:19:37 lucier Exp lucier $

;;; This is useful to compile Meroon with Gambit:

(define (old-load filename)
  (call-with-input-file
    filename
    (lambda (port)
      (let loop ((exp (read port)))
        (if (not (eof-object? exp))
            (begin
              (eval exp)
              (loop (read port))))))))

;;; end of old-load.scm
