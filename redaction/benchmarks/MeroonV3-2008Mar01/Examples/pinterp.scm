;;; Automatically generated from "Examples/bench.scm"
;;; Changed object into Object for the sake of case-sensitivity.

(define-class value Object ())

(define-class function value (variables body env))

(define-class primitive value (name address))

(define-class environ Object ())

(define-class null-env environ ())

(define-class full-env environ (others name))

(define-class variable-env full-env (value))

(define-class continuation Object (k))

(define-class if-cont continuation (et ef r))

(define-class set!-cont continuation (n r))

(define-class begin-cont continuation (e* r))

(define-class evfun-cont continuation (e* r))

(define-class apply-cont continuation (f r))

(define-class argument-cont continuation (e* r))

(define-class gather-cont continuation (v))

(define-class bottom-cont continuation (f))

(define (evaluate e r k) (if (not (pair? e)) (cond ((symbol? e) (evaluate-variable e r k)) (else (evaluate-quote e r k))) (case (car e) ((quote) (evaluate-quote (cadr e) r k)) ((if) (evaluate-if (cadr e) (caddr e) (cadddr e) r k)) ((begin) (evaluate-begin (cdr e) r k)) ((set!) (evaluate-set! (cadr e) (caddr e) r k)) ((lambda) (evaluate-lambda (cadr e) (cddr e) r k)) (else (evaluate-application (car e) (cdr e) r k)))))

(define (evaluate-variable n r k) (lookup r n k))

(define (evaluate-quote v r k) (resume k v))

(define (evaluate-if ec et ef r k) (evaluate ec r (make-if-cont k et ef r)))

(define (evaluate-set! n e r k) (evaluate e r (make-set!-cont k n r)))

(define (evaluate-lambda n* e* r k) (resume k (make-function n* e* r)))

(define (evaluate-begin e* r k) (if (pair? e*) (if (pair? (cdr e*)) (evaluate (car e*) r (make-begin-cont k e* r)) (evaluate (car e*) r k)) (resume k empty-begin-value)))

(define (evaluate-application e e* r k) (evaluate e r (make-evfun-cont k e* r)))

(define-generic (invoke (f) v* r k) (newline) (display "error in invoke : cannot apply : ") (show f) (wrong "not a function" f r k))

(define-method (invoke (f primitive) v* r k) ((primitive-address f) v* r k))

(define-method (invoke (f function) v* r k) (let ((env (extend-env (function-env f) (function-variables f) v*))) (evaluate-begin (function-body f) env k)))

(define-method (invoke (f continuation) v* r k) (if (= 1 (length v*)) (resume f (car v*)) (wrong "Continuations expect one argument" v* r k)))

(define (evaluate-arguments e* r k) (if (pair? e*) (evaluate (car e*) r (make-argument-cont k e* r)) (resume k no-more-arguments)))

(define-generic (resume (k) v) (newline) (display "error in resume : Not a continuation : ") (show k) (wrong "Unknown continuation" k))

(define-method (resume (k if-cont) v) (evaluate (if v (if-cont-et k) (if-cont-ef k)) (if-cont-r k) (if-cont-k k)))

(define-method (resume (k begin-cont) v) (evaluate-begin (cdr (begin-cont-e* k)) (begin-cont-r k) (begin-cont-k k)))

(define-method (resume (k evfun-cont) f) (evaluate-arguments (evfun-cont-e* k) (evfun-cont-r k) (make-apply-cont (evfun-cont-k k) f (evfun-cont-r k))))

(define-method (resume (k apply-cont) v) (invoke (apply-cont-f k) v (apply-cont-r k) (apply-cont-k k)))

(define-method (resume (k argument-cont) v) (evaluate-arguments (cdr (argument-cont-e* k)) (argument-cont-r k) (make-gather-cont (argument-cont-k k) v)))

(define-method (resume (k gather-cont) v*) (resume (gather-cont-k k) (cons (gather-cont-v k) v*)))

(define-method (resume (k bottom-cont) v) ((bottom-cont-f k) v))

(define-generic (lookup (r) n k) (newline) (display "error in lookup : Not an environment : ") (show r) (wrong "not an environment" r n k))

(define-method (lookup (r null-env) n k) (wrong "Unknown variable" n r k))

(define-method (lookup (r variable-env) n k) (if (eq? n (variable-env-name r)) (resume k (variable-env-value r)) (lookup (variable-env-others r) n k)))

(define-method (resume (k set!-cont) v) (update! (set!-cont-r k) (set!-cont-n k) (set!-cont-k k) v))

(define-generic (update! (r) n k v) (newline) (display "error in lookup : Not an environment : ") (show r) (wrong "not an environment" r n k))

(define-method (update! (r null-env) n k v) (wrong "Unknown variable" n r k))

(define-method (update! (r variable-env) n k v) (if (equal? n (variable-env-name r)) (begin (set-variable-env-value! r v) (resume k v)) (update! (variable-env-others r) n k v)))

(define (extend-env env names values) (cond ((pair? names) (if (pair? values) (make-variable-env (extend-env env (cdr names) (cdr values)) (car names) (car values)) (wrong "too less values" names))) ((null? names) (if (pair? values) (wrong "Too much values" values) env)) (else (make-variable-env env names values))))

(define empty-begin-value '())

(define no-more-arguments '())

(define (internal-error msg . culprits) (wrong "Internal error" msg culprits))

(define r.init (make-null-env))

(set! r.init (make-variable-env r.init 'cons (make-primitive 'cons (lambda (v* r k) (if (= 2 (length v*)) (resume k (apply cons v*)) (wrong "Incorrect arity" 'cons v*))))))

(set! r.init (make-variable-env r.init 'car (make-primitive 'car (lambda (v* r k) (if (= 1 (length v*)) (resume k (apply car v*)) (wrong "Incorrect arity" 'car v*))))))

(set! r.init (make-variable-env r.init 'cdr (make-primitive 'cdr (lambda (v* r k) (if (= 1 (length v*)) (resume k (apply cdr v*)) (wrong "Incorrect arity" 'cdr v*))))))

(set! r.init (make-variable-env r.init 'pair? (make-primitive 'pair? (lambda (v* r k) (if (= 1 (length v*)) (resume k (apply pair? v*)) (wrong "Incorrect arity" 'pair? v*))))))

(set! r.init (make-variable-env r.init 'symbol? (make-primitive 'symbol? (lambda (v* r k) (if (= 1 (length v*)) (resume k (apply symbol? v*)) (wrong "Incorrect arity" 'symbol? v*))))))

(set! r.init (make-variable-env r.init 'eq? (make-primitive 'eq? (lambda (v* r k) (if (= 2 (length v*)) (resume k (apply eq? v*)) (wrong "Incorrect arity" 'eq? v*))))))

(set! r.init (make-variable-env r.init 'null? (make-primitive 'null? (lambda (v* r k) (if (= 1 (length v*)) (resume k (apply null? v*)) (wrong "Incorrect arity" 'null? v*))))))

(set! r.init (make-variable-env r.init 'set-car! (make-primitive 'set-car! (lambda (v* r k) (if (= 2 (length v*)) (resume k (apply set-car! v*)) (wrong "Incorrect arity" 'set-car! v*))))))

(set! r.init (make-variable-env r.init 'set-cdr! (make-primitive 'set-cdr! (lambda (v* r k) (if (= 2 (length v*)) (resume k (apply set-cdr! v*)) (wrong "Incorrect arity" 'set-cdr! v*))))))

(set! r.init (make-variable-env r.init '+ (make-primitive '+ (lambda (v* r k) (if (= 2 (length v*)) (resume k (apply + v*)) (wrong "Incorrect arity" '+ v*))))))

(set! r.init (make-variable-env r.init '- (make-primitive '- (lambda (v* r k) (if (= 2 (length v*)) (resume k (apply - v*)) (wrong "Incorrect arity" '- v*))))))

(set! r.init (make-variable-env r.init '= (make-primitive '= (lambda (v* r k) (if (= 2 (length v*)) (resume k (apply = v*)) (wrong "Incorrect arity" '= v*))))))

(set! r.init (make-variable-env r.init '< (make-primitive '< (lambda (v* r k) (if (= 2 (length v*)) (resume k (apply < v*)) (wrong "Incorrect arity" '< v*))))))

(set! r.init (make-variable-env r.init '> (make-primitive '> (lambda (v* r k) (if (= 2 (length v*)) (resume k (apply > v*)) (wrong "Incorrect arity" '> v*))))))

(set! r.init (make-variable-env r.init '* (make-primitive '* (lambda (v* r k) (if (= 2 (length v*)) (resume k (apply * v*)) (wrong "Incorrect arity" '* v*))))))

(set! r.init (make-variable-env r.init '<= (make-primitive '<= (lambda (v* r k) (if (= 2 (length v*)) (resume k (apply <= v*)) (wrong "Incorrect arity" '<= v*))))))

(set! r.init (make-variable-env r.init '>= (make-primitive '>= (lambda (v* r k) (if (= 2 (length v*)) (resume k (apply >= v*)) (wrong "Incorrect arity" '>= v*))))))

(set! r.init (make-variable-env r.init 'remainder (make-primitive 'remainder (lambda (v* r k) (if (= 2 (length v*)) (resume k (apply remainder v*)) (wrong "Incorrect arity" 'remainder v*))))))

(set! r.init (make-variable-env r.init 'call/cc (make-primitive 'call/cc (lambda (v* r k) (if (= 1 (length v*)) (invoke (car v*) (list k) r k) (wrong "Incorrect arity" 'call/cc v*))))))

(set! r.init (make-variable-env r.init 'apply (make-primitive 'apply (lambda (v* r k) (if (>= (length v*) 2) (if (or (function? (car v*)) (primitive? (car v*)) (continuation? (car v*))) (invoke (car v*) (let conc ((args (cdr v*))) (if (pair? (cdr args)) (cons (car args) (conc (cdr args))) (car args))) r k) (wrong "Not a function" 'apply (car v*))) (wrong "Incorrect arity" 'apply v*))))))

(set! r.init (make-variable-env r.init 'list (make-primitive 'list (lambda (v* r k) (resume k v*)))))

(set! r.init (make-variable-env r.init 't #t))

(set! r.init (make-variable-env r.init 'f #f))

(set! r.init (make-variable-env r.init 'nil '()))

(set! r.init (make-variable-env r.init 'x 'void))

(set! r.init (make-variable-env r.init 'y 'void))

(set! r.init (make-variable-env r.init 'z 'void))

(set! r.init (make-variable-env r.init 'a 'void))

(set! r.init (make-variable-env r.init 'b 'void))

(set! r.init (make-variable-env r.init 'c 'void))

(set! r.init (make-variable-env r.init 'foo 'void))

(set! r.init (make-variable-env r.init 'bar 'void))

(set! r.init (make-variable-env r.init 'hux 'void))

(set! r.init (make-variable-env r.init 'fib 'void))

(set! r.init (make-variable-env r.init 'fact 'void))

(set! r.init (make-variable-env r.init 'visit 'void))

(set! r.init (make-variable-env r.init 'length 'void))

(set! r.init (make-variable-env r.init 'primes 'void))

(set! r.init (make-variable-env r.init 'qsort 'void))

(set! r.init (make-variable-env r.init 'merge 'void))

(set! r.init (make-variable-env r.init 'separate 'void))

(set! r.init (make-variable-env r.init 'kappend 'void))

(set! r.init (make-variable-env r.init 'wait 'void))

(set! r.init (make-variable-env r.init 'queens 'void))

(set! r.init (make-variable-env r.init 'foreach 'void))

(set! r.init (make-variable-env r.init 'check 'void))

(set! r.init (make-variable-env r.init 'check-others 'void))

(set! r.init (make-variable-env r.init 'search-one 'void))

(set! r.init (make-variable-env r.init 'iota 'void))

(set! r.init (make-variable-env r.init 'memq 'void))

(define (bench-oo times e) (do ((i times (- i 1))) ((<= i 0) #f) (evaluate e r.init (make-bottom-cont 'void (lambda (v) (display v) (newline))))))

(define *iterations* 10)

(define *bench* '((lambda (fib fact primes) (set! fib (lambda (n) (if (<= n 2) 1 (+ (fib (- n 1)) (fib (- n 2)))))) (set! fact (lambda (n) ((lambda (factint) (begin (set! factint (lambda (n f) (if (< n 2) 1 (* n (f (- n 1) f))))) (factint n factint))) 'wait))) (set! primes (lambda (n f max) ((lambda (filter) (begin (set! filter (lambda (p) (lambda (n) (= 0 (remainder n p))))) (if (> n max) '() (if (f n) (primes (+ n 1) f max) (cons n ((lambda (ff) (primes (+ n 1) (lambda (p) (if (f p) t (ff p))) max)) (filter n))))))) 'wait))) (fib 10) (fact 20) (primes 2 (lambda (x) f) 40)) 'fib 'fact 'primes))

(define (start-bench) (write `(*** meroon benchmark *iterations* = ,*iterations*)) (newline) (show-meroon) (bench-oo *iterations* *bench*) 'done)

;;; end of Examples/pbench.scm
