(include "bench_.scm")
(include "~~/lib/gambit#.scm")
(include "~~/lib/termite/termite#.scm")
(load "~~/lib/termite/termite")

(define (bench-!)
  (let ((main (self)))
    (spawn
     (lambda ()
       (! main
          (time-expr (do ((i 0 (+ i 1)))
                 ((= i benchmark-limit) 'ok)
               (! (self) 'ping))))))
    (?)))

(define (bench-?)
  (let ((main (self)))
    (spawn
     (lambda ()
       (do ((i 0 (+ i 1)))
           ((= i benchmark-limit) 'ok)
         (! (self) 'ping))
       (! main (time-expr (do ((i 0 (+ i 1)))
                              ((= i benchmark-limit) 'ok)
                            (?))))))
    (?)))

(define (bench-?-timeout)
  (let ((main (self)))
    (spawn
     (lambda ()
       (! main (time-expr (do ((i 0 (+ i 1)))
                              ((= i benchmark-limit) 'ok)
                            (? 0. 'ok))))))
    (?)))

(define (bench-send-receive)
  (let ((main (self)))
    (spawn
     (lambda ()
       (! main
          (time-expr (let ((p (spawn (lambda () (let loop () (?) (loop))))))
                       (do ((i 0 (+ i 1)))
                           ((= i benchmark-limit) 'ok)
                         (! p 'allo))))
        )))
    (?)))

(define (bench-recv)
  (let ((main (self)))
    (spawn
     (lambda ()
       (do ((i 0 (+ i 1)))
           ((= i benchmark-limit) 'ok)
         (! (self) 'pong))
       (! main
          (time-expr (do ((i 0 (+ i 1)))
                         ((= i benchmark-limit) 'ok)
                       (recv ('pong 'ok)))))))
    (?)))

(define (bench-ping-server)
  (let ((main (self)))
    (spawn
     (lambda ()
       (! main
          (time-expr (let ((pong-server
                            (spawn (lambda ()
                                     (let loop ()
                                       (recv ((from 'ping) (! from 'pong)))
                                       (loop))))))
                       (do ((i 0 (+ i 1)))
                           ((= i benchmark-limit) 'ok)
                         (! pong-server (list (self) 'ping))
                         (recv ('pong 'ok))))))))
    (?)))

(define (bench-recv-timeout)
  (let ((main (self)))
    (spawn
     (lambda ()
       (! main (time-expr (do ((i 0 (+ i 1)))
                              ((= i benchmark-limit) 'ok)
                            (recv (after 0 'ok)))))))
    (?)))

(pp `(termite !: ,(bench-!)))
(pp `(termite ?: ,(bench-?)))
(pp `(termite ?-timeout: ,(bench-?)))
(pp `(termite send-receive: ,(bench-send-receive)))
(pp `(termite recv: ,(bench-recv)))
(pp `(termite recv-timeout: ,(bench-recv-timeout)))
(pp `(termite ping-server: ,(bench-ping-server)))
