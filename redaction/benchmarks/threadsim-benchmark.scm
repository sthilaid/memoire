(include "thread-simulation/include/scm-lib_.scm")
(include "thread-simulation/include/thread-simulation_.scm")
(include "thread-simulation/include/match.scm")
(include "bench_.scm")

(define (bench-yield)
  (let ((c1 (new-corout
             'c1 (lambda ()
                   (do ((i 0 (+ i 1)))
                       ((= i benchmark-limit) (kill-all! 'done))
                     (yield)))))
        (c2 (new-corout 'c2 (lambda () (let loop () (yield) (loop))))))
    (time-expr (boot (list c1 c2)))))

(define (bench-send-receive)
  (let* ((c1 (new-corout
              'c1 (lambda ()
                    (do ((i 0 (+ i 1)))
                        ((= i benchmark-limit) (kill-all! 'done))
                      (! (current-corout) 'ping)
                      (?))))))
    (time-expr (boot (list c1)))))

(define (bench-recv)
  (let* ((c2 (new-corout 'c2 (lambda ()
                               (let loop ()
                                 (recv ((,sender ping) (! sender 'pong)))
                                 (loop)))))
         (c1 (new-corout
              'c1 (lambda ()
                    (do ((i 0 (+ i 1)))
                        ((= i benchmark-limit) (kill-all! 'done))
                      (! c2 (list (current-corout) 'ping))
                      (recv (pong 'ok)))))))
    (time-expr (boot (list c1 c2)))))

(pp `(thread-simulation yield: ,(bench-yield)))
(pp `(thread-simulation send-receive: ,(bench-send-receive)))
(pp `(thread-simulation recv: ,(bench-recv)))
