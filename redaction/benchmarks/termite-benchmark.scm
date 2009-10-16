(include "bench_.scm")
(include "~~/lib/gambit#.scm")
(include "~~/lib/termite/termite#.scm")
(load "~~/lib/termite/termite")

(define (bench-send-receive)
  (time-expr (do ((i 0 (+ i 1)))
                 ((= i 500000) 'ok)
               (! (self) 'ping)
               (?))))

(define (bench-recv)
  (time-expr (do ((i 0 (+ i 1)))
                 ((= i 500000) 'ok)
               (! (self) (list (self) 'ping))
               (recv ((pid 'ping) 'ok)))))

(pp `(termite send-receive: ,(bench-send-receive)))
(pp `(termite recv: ,(bench-recv)))
