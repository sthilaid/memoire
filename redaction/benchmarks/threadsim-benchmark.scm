(include "thread-simulation/include/scm-lib_.scm")
(include "thread-simulation/include/thread-simulation_.scm")
(include "thread-simulation/include/match.scm")
(include "bench_.scm")

(define (bench-yield)
  (let ((c1 (new-corout
             'c1 (lambda ()
                   (do ((i 0 (+ i 1)))
                       ((= i (/ benchmark-limit 2)) (kill-all! 'done))
                     (yield)))))
        (c2 (new-corout 'c2 (lambda () (let loop () (yield) (loop)))))
        )
    (time-expr (boot (list c1 c2)))))

(define (bench-!)
  (let* ((c1 (new-corout
              'c1 (lambda ()
                    (do ((i 0 (+ i 1)))
                        ((= i benchmark-limit) (kill-all! 'done))
                      (! (current-corout) 'ping))))))
    (time-expr (boot (list c1)))))

(define (bench-?)
  (let* ((c1 (new-corout
              'c1 (lambda ()
                    (do ((i 0 (+ i 1)))
                          ((= i benchmark-limit) 'ok)
                      (! (current-corout) 'allo))
                    (let ((dt (time-expr
                               (do ((i 0 (+ i 1)))
                                   ((= i benchmark-limit) 'ok)
                                 (?)))))
                      dt)))))
    (boot (list c1))
    (corout-get-result c1)))

(define (bench-?-timeout)
  (let* ((c1 (new-corout
              'c1 (lambda ()
                    (do ((i 0 (+ i 1)))
                        ((= i benchmark-limit) (kill-all! 'done))
                      (? timeout: 0 timeout-val: 'ok))))))
    (time-expr (boot (list c1)))))

(define (bench-!-?)
  (let* ((c1 (new-corout
              'c1 (lambda ()
                    (do ((i 0 (+ i 1)))
                        ((= i benchmark-limit) (kill-all! 'done))
                      (?)
                      (yield)))))
         (c2 (new-corout
              'c2 (lambda ()
                    (let loop () (! c1 'allo) (yield) (loop))))))
    (time-expr (boot (list c2 c1)))))

(define (bench-recv)
  (let* ((c1 (new-corout
              'c1 (lambda ()
                    (do ((i 0 (+ i 1)))
                          ((= i benchmark-limit) 'ok)
                      (! (current-corout) '(allo 12)))
                    (time-expr
                     (do ((i 0 (+ i 1)))
                         ((= i benchmark-limit) 'done)
                       (recv (salut 'non)
                             ((allo 12) 'ok))))))))
    (boot (list c1))
    (corout-get-result c1)))

(define (bench-recv-timeout)
  (let* ((c1 (new-corout
              'c1 (lambda ()
                    (do ((i 0 (+ i 1)))
                        ((= i benchmark-limit) (kill-all! 'done))
                      (recv (after 0 'ok)))))))
    (time-expr (boot (list c1)))))

(define (bench-ping-server)
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
(pp `(thread-simulation !: ,(bench-!)))
(pp `(thread-simulation ?: ,(bench-?)))
(pp `(thread-simulation ?-timeout: ,(bench-?-timeout)))
(pp `(thread-simulation !-?: ,(bench-!-?)))
(pp `(thread-simulation recv: ,(bench-recv)))
(pp `(thread-simulation recv-timeout: ,(bench-recv-timeout)))
(pp `(thread-simulation ping-server: ,(bench-ping-server)))
