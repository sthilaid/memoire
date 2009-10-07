Expansion:

(define value-class
  (initialize!
   (fill-other-fields!
    (register-class
     '(1 . 1)
     ((Class-allocator Inlinable-Class-class) 3)
     'value
     Object-class
     ('#<procedure #2 ##list>)))))

#t

(define value?
  (lambda (#:g282)
    (and ('#<procedure #3 ##meroon?> #:g282)
         (careless-subclass?
          ('#<procedure #4 ##vector-ref>
           *classes*
           ('#<procedure #4 ##vector-ref> #:g282 0))
          value-class))))

(define make-value
  (lambda ()
    (initialize!
     ('#<procedure #5 ##subtype-set!>
      ('#<procedure #6 ##vector> ('#<procedure #4 ##vector-ref> value-class 2))
      6))))

(define allocate-value (Class-allocator value-class))

(define ->value
  (let ((#:g283 ('#<procedure #7 ##box> 'wait)))
    (let ((begin-temp.0
           ('#<procedure #8 ##set-box!>
            #:g283
            (register-Generic-1
             '(1 . 1)
             '->value
             meroon-uninitialized
             '((o))
             #f))))
      (lambda (o)
        ((careless-determine-method1 ('#<procedure #9 ##unbox> #:g283) o)
         o)))))

(register-method
 '->value
 '((o value))
 '(1 . 1)
 (lambda (#:g285 #:g286 #:g287) (lambda (o) o))
 'value)

'value

(define function-class
  (initialize!
   (fill-other-fields!
    (register-class
     '(1 . 1)
     ((Class-allocator Inlinable-Class-class) 5)
     'function
     value-class
     ('#<procedure #2 ##list>
      (initialize!
       (fill-other-fields!
        ('#<procedure #5 ##subtype-set!>
         ('#<procedure #6 ##vector>
          (let ((o Inlinable-Mono-Field-class))
            ('#<procedure #4 ##vector-ref> o 2))
          'variables
          #f
          meroon-uninitialized
          #t
          meroon-uninitialized
          1
          0)
         6)))
      (initialize!
       (fill-other-fields!
        ('#<procedure #5 ##subtype-set!>
         ('#<procedure #6 ##vector>
          (let ((o Inlinable-Mono-Field-class))
            ('#<procedure #4 ##vector-ref> o 2))
          'body
          #f
          meroon-uninitialized
          #t
          meroon-uninitialized
          1
          1)
         6)))
      (initialize!
       (fill-other-fields!
        ('#<procedure #5 ##subtype-set!>
         ('#<procedure #6 ##vector>
          (let ((o Inlinable-Mono-Field-class))
            ('#<procedure #4 ##vector-ref> o 2))
          'env
          #f
          meroon-uninitialized
          #t
          meroon-uninitialized
          1
          2)
         6))))))))

#t

(define function-variables
  (lambda (#:g289)
    (let ((begin-temp.1 (check-class #:g289 function-class 'variables)))
      ('#<procedure #4 ##vector-ref> #:g289 1))))

(define function-variables-set!
  (lambda (#:g290 #:g291)
    (let ((begin-temp.2 (check-class #:g290 function-class 'variables)))
      ('#<procedure #10 ##vector-set!> #:g290 1 #:g291))))

(define function-body
  (lambda (#:g292)
    (let ((begin-temp.3 (check-class #:g292 function-class 'body)))
      ('#<procedure #4 ##vector-ref> #:g292 2))))

(define function-body-set!
  (lambda (#:g293 #:g294)
    (let ((begin-temp.4 (check-class #:g293 function-class 'body)))
      ('#<procedure #10 ##vector-set!> #:g293 2 #:g294))))

(define function-env
  (lambda (#:g295)
    (let ((begin-temp.5 (check-class #:g295 function-class 'env)))
      ('#<procedure #4 ##vector-ref> #:g295 3))))

(define function-env-set!
  (lambda (#:g296 #:g297)
    (let ((begin-temp.6 (check-class #:g296 function-class 'env)))
      ('#<procedure #10 ##vector-set!> #:g296 3 #:g297))))

(define function?
  (lambda (#:g298)
    (and ('#<procedure #3 ##meroon?> #:g298)
         (careless-subclass?
          ('#<procedure #4 ##vector-ref>
           *classes*
           ('#<procedure #4 ##vector-ref> #:g298 0))
          function-class))))

(define make-function
  (lambda (variables body env)
    (initialize!
     ('#<procedure #5 ##subtype-set!>
      ('#<procedure #6 ##vector>
       ('#<procedure #4 ##vector-ref> function-class 2)
       variables
       body
       env)
      6))))

(define allocate-function (Class-allocator function-class))

(define ->function
  (let ((#:g299 ('#<procedure #7 ##box> 'wait)))
    (let ((begin-temp.7
           ('#<procedure #8 ##set-box!>
            #:g299
            (register-Generic-1
             '(1 . 1)
             '->function
             meroon-uninitialized
             '((o))
             #f))))
      (lambda (o)
        ((careless-determine-method1 ('#<procedure #9 ##unbox> #:g299) o)
         o)))))

(register-method
 '->function
 '((o function))
 '(1 . 1)
 (lambda (#:g301 #:g302 #:g303) (lambda (o) o))
 'function)

'function

(define primitive-class
  (initialize!
   (fill-other-fields!
    (register-class
     '(1 . 1)
     ((Class-allocator Inlinable-Class-class) 5)
     'primitive
     value-class
     ('#<procedure #2 ##list>
      (initialize!
       (fill-other-fields!
        ('#<procedure #5 ##subtype-set!>
         ('#<procedure #6 ##vector>
          (let ((o Inlinable-Mono-Field-class))
            ('#<procedure #4 ##vector-ref> o 2))
          'name
          #f
          meroon-uninitialized
          #t
          meroon-uninitialized
          1
          0)
         6)))
      (initialize!
       (fill-other-fields!
        ('#<procedure #5 ##subtype-set!>
         ('#<procedure #6 ##vector>
          (let ((o Inlinable-Mono-Field-class))
            ('#<procedure #4 ##vector-ref> o 2))
          'address
          #f
          meroon-uninitialized
          #t
          meroon-uninitialized
          1
          1)
         6))))))))

#t

(define primitive-name
  (lambda (#:g305)
    (let ((begin-temp.8 (check-class #:g305 primitive-class 'name)))
      ('#<procedure #4 ##vector-ref> #:g305 1))))

(define primitive-name-set!
  (lambda (#:g306 #:g307)
    (let ((begin-temp.9 (check-class #:g306 primitive-class 'name)))
      ('#<procedure #10 ##vector-set!> #:g306 1 #:g307))))

(define primitive-address
  (lambda (#:g308)
    (let ((begin-temp.10 (check-class #:g308 primitive-class 'address)))
      ('#<procedure #4 ##vector-ref> #:g308 2))))

(define primitive-address-set!
  (lambda (#:g309 #:g310)
    (let ((begin-temp.11 (check-class #:g309 primitive-class 'address)))
      ('#<procedure #10 ##vector-set!> #:g309 2 #:g310))))

(define primitive?
  (lambda (#:g311)
    (and ('#<procedure #3 ##meroon?> #:g311)
         (careless-subclass?
          ('#<procedure #4 ##vector-ref>
           *classes*
           ('#<procedure #4 ##vector-ref> #:g311 0))
          primitive-class))))

(define make-primitive
  (lambda (name address)
    (initialize!
     ('#<procedure #5 ##subtype-set!>
      ('#<procedure #6 ##vector>
       ('#<procedure #4 ##vector-ref> primitive-class 2)
       name
       address)
      6))))

(define allocate-primitive (Class-allocator primitive-class))

(define ->primitive
  (let ((#:g312 ('#<procedure #7 ##box> 'wait)))
    (let ((begin-temp.12
           ('#<procedure #8 ##set-box!>
            #:g312
            (register-Generic-1
             '(1 . 1)
             '->primitive
             meroon-uninitialized
             '((o))
             #f))))
      (lambda (o)
        ((careless-determine-method1 ('#<procedure #9 ##unbox> #:g312) o)
         o)))))

(register-method
 '->primitive
 '((o primitive))
 '(1 . 1)
 (lambda (#:g314 #:g315 #:g316) (lambda (o) o))
 'primitive)

'primitive

(define environ-class
  (initialize!
   (fill-other-fields!
    (register-class
     '(1 . 1)
     ((Class-allocator Inlinable-Class-class) 3)
     'environ
     Object-class
     ('#<procedure #2 ##list>)))))

#t

(define environ?
  (lambda (#:g318)
    (and ('#<procedure #3 ##meroon?> #:g318)
         (careless-subclass?
          ('#<procedure #4 ##vector-ref>
           *classes*
           ('#<procedure #4 ##vector-ref> #:g318 0))
          environ-class))))

(define make-environ
  (lambda ()
    (initialize!
     ('#<procedure #5 ##subtype-set!>
      ('#<procedure #6 ##vector>
       ('#<procedure #4 ##vector-ref> environ-class 2))
      6))))

(define allocate-environ (Class-allocator environ-class))

(define ->environ
  (let ((#:g319 ('#<procedure #7 ##box> 'wait)))
    (let ((begin-temp.13
           ('#<procedure #8 ##set-box!>
            #:g319
            (register-Generic-1
             '(1 . 1)
             '->environ
             meroon-uninitialized
             '((o))
             #f))))
      (lambda (o)
        ((careless-determine-method1 ('#<procedure #9 ##unbox> #:g319) o)
         o)))))

(register-method
 '->environ
 '((o environ))
 '(1 . 1)
 (lambda (#:g321 #:g322 #:g323) (lambda (o) o))
 'environ)

'environ

(define null-env-class
  (initialize!
   (fill-other-fields!
    (register-class
     '(1 . 1)
     ((Class-allocator Inlinable-Class-class) 5)
     'null-env
     environ-class
     ('#<procedure #2 ##list>)))))

#t

(define null-env?
  (lambda (#:g325)
    (and ('#<procedure #3 ##meroon?> #:g325)
         (careless-subclass?
          ('#<procedure #4 ##vector-ref>
           *classes*
           ('#<procedure #4 ##vector-ref> #:g325 0))
          null-env-class))))

(define make-null-env
  (lambda ()
    (initialize!
     ('#<procedure #5 ##subtype-set!>
      ('#<procedure #6 ##vector>
       ('#<procedure #4 ##vector-ref> null-env-class 2))
      6))))

(define allocate-null-env (Class-allocator null-env-class))

(define ->null-env
  (let ((#:g326 ('#<procedure #7 ##box> 'wait)))
    (let ((begin-temp.14
           ('#<procedure #8 ##set-box!>
            #:g326
            (register-Generic-1
             '(1 . 1)
             '->null-env
             meroon-uninitialized
             '((o))
             #f))))
      (lambda (o)
        ((careless-determine-method1 ('#<procedure #9 ##unbox> #:g326) o)
         o)))))

(register-method
 '->null-env
 '((o null-env))
 '(1 . 1)
 (lambda (#:g328 #:g329 #:g330) (lambda (o) o))
 'null-env)

'null-env

(define full-env-class
  (initialize!
   (fill-other-fields!
    (register-class
     '(1 . 1)
     ((Class-allocator Inlinable-Class-class) 5)
     'full-env
     environ-class
     ('#<procedure #2 ##list>
      (initialize!
       (fill-other-fields!
        ('#<procedure #5 ##subtype-set!>
         ('#<procedure #6 ##vector>
          (let ((o Inlinable-Mono-Field-class))
            ('#<procedure #4 ##vector-ref> o 2))
          'others
          #f
          meroon-uninitialized
          #t
          meroon-uninitialized
          1
          0)
         6)))
      (initialize!
       (fill-other-fields!
        ('#<procedure #5 ##subtype-set!>
         ('#<procedure #6 ##vector>
          (let ((o Inlinable-Mono-Field-class))
            ('#<procedure #4 ##vector-ref> o 2))
          'name
          #f
          meroon-uninitialized
          #t
          meroon-uninitialized
          1
          1)
         6))))))))

#t

(define full-env-others
  (lambda (#:g332)
    (let ((begin-temp.15 (check-class #:g332 full-env-class 'others)))
      ('#<procedure #4 ##vector-ref> #:g332 1))))

(define full-env-others-set!
  (lambda (#:g333 #:g334)
    (let ((begin-temp.16 (check-class #:g333 full-env-class 'others)))
      ('#<procedure #10 ##vector-set!> #:g333 1 #:g334))))

(define full-env-name
  (lambda (#:g335)
    (let ((begin-temp.17 (check-class #:g335 full-env-class 'name)))
      ('#<procedure #4 ##vector-ref> #:g335 2))))

(define full-env-name-set!
  (lambda (#:g336 #:g337)
    (let ((begin-temp.18 (check-class #:g336 full-env-class 'name)))
      ('#<procedure #10 ##vector-set!> #:g336 2 #:g337))))

(define full-env?
  (lambda (#:g338)
    (and ('#<procedure #3 ##meroon?> #:g338)
         (careless-subclass?
          ('#<procedure #4 ##vector-ref>
           *classes*
           ('#<procedure #4 ##vector-ref> #:g338 0))
          full-env-class))))

(define make-full-env
  (lambda (others name)
    (initialize!
     ('#<procedure #5 ##subtype-set!>
      ('#<procedure #6 ##vector>
       ('#<procedure #4 ##vector-ref> full-env-class 2)
       others
       name)
      6))))

(define allocate-full-env (Class-allocator full-env-class))

(define ->full-env
  (let ((#:g339 ('#<procedure #7 ##box> 'wait)))
    (let ((begin-temp.19
           ('#<procedure #8 ##set-box!>
            #:g339
            (register-Generic-1
             '(1 . 1)
             '->full-env
             meroon-uninitialized
             '((o))
             #f))))
      (lambda (o)
        ((careless-determine-method1 ('#<procedure #9 ##unbox> #:g339) o)
         o)))))

(register-method
 '->full-env
 '((o full-env))
 '(1 . 1)
 (lambda (#:g341 #:g342 #:g343) (lambda (o) o))
 'full-env)

'full-env

(define variable-env-class
  (initialize!
   (fill-other-fields!
    (register-class
     '(1 . 1)
     ((Class-allocator Inlinable-Class-class) 7)
     'variable-env
     full-env-class
     ('#<procedure #2 ##list>
      (initialize!
       (fill-other-fields!
        ('#<procedure #5 ##subtype-set!>
         ('#<procedure #6 ##vector>
          (let ((o Inlinable-Mono-Field-class))
            ('#<procedure #4 ##vector-ref> o 2))
          'value
          #f
          meroon-uninitialized
          #t
          meroon-uninitialized
          1
          2)
         6))))))))

#t

(define variable-env-others
  (lambda (#:g345)
    (let ((begin-temp.20 (check-class #:g345 variable-env-class 'others)))
      ('#<procedure #4 ##vector-ref> #:g345 1))))

(define variable-env-others-set!
  (lambda (#:g346 #:g347)
    (let ((begin-temp.21 (check-class #:g346 variable-env-class 'others)))
      ('#<procedure #10 ##vector-set!> #:g346 1 #:g347))))

(define variable-env-name
  (lambda (#:g348)
    (let ((begin-temp.22 (check-class #:g348 variable-env-class 'name)))
      ('#<procedure #4 ##vector-ref> #:g348 2))))

(define variable-env-name-set!
  (lambda (#:g349 #:g350)
    (let ((begin-temp.23 (check-class #:g349 variable-env-class 'name)))
      ('#<procedure #10 ##vector-set!> #:g349 2 #:g350))))

(define variable-env-value
  (lambda (#:g351)
    (let ((begin-temp.24 (check-class #:g351 variable-env-class 'value)))
      ('#<procedure #4 ##vector-ref> #:g351 3))))

(define variable-env-value-set!
  (lambda (#:g352 #:g353)
    (let ((begin-temp.25 (check-class #:g352 variable-env-class 'value)))
      ('#<procedure #10 ##vector-set!> #:g352 3 #:g353))))

(define variable-env?
  (lambda (#:g354)
    (and ('#<procedure #3 ##meroon?> #:g354)
         (careless-subclass?
          ('#<procedure #4 ##vector-ref>
           *classes*
           ('#<procedure #4 ##vector-ref> #:g354 0))
          variable-env-class))))

(define make-variable-env
  (lambda (others name value)
    (initialize!
     ('#<procedure #5 ##subtype-set!>
      ('#<procedure #6 ##vector>
       ('#<procedure #4 ##vector-ref> variable-env-class 2)
       others
       name
       value)
      6))))

(define allocate-variable-env (Class-allocator variable-env-class))

(define ->variable-env
  (let ((#:g355 ('#<procedure #7 ##box> 'wait)))
    (let ((begin-temp.26
           ('#<procedure #8 ##set-box!>
            #:g355
            (register-Generic-1
             '(1 . 1)
             '->variable-env
             meroon-uninitialized
             '((o))
             #f))))
      (lambda (o)
        ((careless-determine-method1 ('#<procedure #9 ##unbox> #:g355) o)
         o)))))

(register-method
 '->variable-env
 '((o variable-env))
 '(1 . 1)
 (lambda (#:g357 #:g358 #:g359) (lambda (o) o))
 'variable-env)

'variable-env

(define continuation-class
  (initialize!
   (fill-other-fields!
    (register-class
     '(1 . 1)
     ((Class-allocator Inlinable-Class-class) 3)
     'continuation
     Object-class
     ('#<procedure #2 ##list>
      (initialize!
       (fill-other-fields!
        ('#<procedure #5 ##subtype-set!>
         ('#<procedure #6 ##vector>
          (let ((o Inlinable-Mono-Field-class))
            ('#<procedure #4 ##vector-ref> o 2))
          'k
          #f
          meroon-uninitialized
          #t
          meroon-uninitialized
          1
          0)
         6))))))))

#t

(define continuation-k
  (lambda (#:g361)
    (let ((begin-temp.27 (check-class #:g361 continuation-class 'k)))
      ('#<procedure #4 ##vector-ref> #:g361 1))))

(define continuation-k-set!
  (lambda (#:g362 #:g363)
    (let ((begin-temp.28 (check-class #:g362 continuation-class 'k)))
      ('#<procedure #10 ##vector-set!> #:g362 1 #:g363))))

(define continuation?
  (lambda (#:g364)
    (and ('#<procedure #3 ##meroon?> #:g364)
         (careless-subclass?
          ('#<procedure #4 ##vector-ref>
           *classes*
           ('#<procedure #4 ##vector-ref> #:g364 0))
          continuation-class))))

(define make-continuation
  (lambda (k)
    (initialize!
     ('#<procedure #5 ##subtype-set!>
      ('#<procedure #6 ##vector>
       ('#<procedure #4 ##vector-ref> continuation-class 2)
       k)
      6))))

(define allocate-continuation (Class-allocator continuation-class))

(define ->continuation
  (let ((#:g365 ('#<procedure #7 ##box> 'wait)))
    (let ((begin-temp.29
           ('#<procedure #8 ##set-box!>
            #:g365
            (register-Generic-1
             '(1 . 1)
             '->continuation
             meroon-uninitialized
             '((o))
             #f))))
      (lambda (o)
        ((careless-determine-method1 ('#<procedure #9 ##unbox> #:g365) o)
         o)))))

(register-method
 '->continuation
 '((o continuation))
 '(1 . 1)
 (lambda (#:g367 #:g368 #:g369) (lambda (o) o))
 'continuation)

'continuation

(define if-cont-class
  (initialize!
   (fill-other-fields!
    (register-class
     '(1 . 1)
     ((Class-allocator Inlinable-Class-class) 5)
     'if-cont
     continuation-class
     ('#<procedure #2 ##list>
      (initialize!
       (fill-other-fields!
        ('#<procedure #5 ##subtype-set!>
         ('#<procedure #6 ##vector>
          (let ((o Inlinable-Mono-Field-class))
            ('#<procedure #4 ##vector-ref> o 2))
          'et
          #f
          meroon-uninitialized
          #t
          meroon-uninitialized
          1
          1)
         6)))
      (initialize!
       (fill-other-fields!
        ('#<procedure #5 ##subtype-set!>
         ('#<procedure #6 ##vector>
          (let ((o Inlinable-Mono-Field-class))
            ('#<procedure #4 ##vector-ref> o 2))
          'ef
          #f
          meroon-uninitialized
          #t
          meroon-uninitialized
          1
          2)
         6)))
      (initialize!
       (fill-other-fields!
        ('#<procedure #5 ##subtype-set!>
         ('#<procedure #6 ##vector>
          (let ((o Inlinable-Mono-Field-class))
            ('#<procedure #4 ##vector-ref> o 2))
          'r
          #f
          meroon-uninitialized
          #t
          meroon-uninitialized
          1
          3)
         6))))))))

#t

(define if-cont-k
  (lambda (#:g371)
    (let ((begin-temp.30 (check-class #:g371 if-cont-class 'k)))
      ('#<procedure #4 ##vector-ref> #:g371 1))))

(define if-cont-k-set!
  (lambda (#:g372 #:g373)
    (let ((begin-temp.31 (check-class #:g372 if-cont-class 'k)))
      ('#<procedure #10 ##vector-set!> #:g372 1 #:g373))))

(define if-cont-et
  (lambda (#:g374)
    (let ((begin-temp.32 (check-class #:g374 if-cont-class 'et)))
      ('#<procedure #4 ##vector-ref> #:g374 2))))

(define if-cont-et-set!
  (lambda (#:g375 #:g376)
    (let ((begin-temp.33 (check-class #:g375 if-cont-class 'et)))
      ('#<procedure #10 ##vector-set!> #:g375 2 #:g376))))

(define if-cont-ef
  (lambda (#:g377)
    (let ((begin-temp.34 (check-class #:g377 if-cont-class 'ef)))
      ('#<procedure #4 ##vector-ref> #:g377 3))))

(define if-cont-ef-set!
  (lambda (#:g378 #:g379)
    (let ((begin-temp.35 (check-class #:g378 if-cont-class 'ef)))
      ('#<procedure #10 ##vector-set!> #:g378 3 #:g379))))

(define if-cont-r
  (lambda (#:g380)
    (let ((begin-temp.36 (check-class #:g380 if-cont-class 'r)))
      ('#<procedure #4 ##vector-ref> #:g380 4))))

(define if-cont-r-set!
  (lambda (#:g381 #:g382)
    (let ((begin-temp.37 (check-class #:g381 if-cont-class 'r)))
      ('#<procedure #10 ##vector-set!> #:g381 4 #:g382))))

(define if-cont?
  (lambda (#:g383)
    (and ('#<procedure #3 ##meroon?> #:g383)
         (careless-subclass?
          ('#<procedure #4 ##vector-ref>
           *classes*
           ('#<procedure #4 ##vector-ref> #:g383 0))
          if-cont-class))))

(define make-if-cont
  (lambda (k et ef r)
    (initialize!
     ('#<procedure #5 ##subtype-set!>
      ('#<procedure #6 ##vector>
       ('#<procedure #4 ##vector-ref> if-cont-class 2)
       k
       et
       ef
       r)
      6))))

(define allocate-if-cont (Class-allocator if-cont-class))

(define ->if-cont
  (let ((#:g384 ('#<procedure #7 ##box> 'wait)))
    (let ((begin-temp.38
           ('#<procedure #8 ##set-box!>
            #:g384
            (register-Generic-1
             '(1 . 1)
             '->if-cont
             meroon-uninitialized
             '((o))
             #f))))
      (lambda (o)
        ((careless-determine-method1 ('#<procedure #9 ##unbox> #:g384) o)
         o)))))

(register-method
 '->if-cont
 '((o if-cont))
 '(1 . 1)
 (lambda (#:g386 #:g387 #:g388) (lambda (o) o))
 'if-cont)

'if-cont

(define set!-cont-class
  (initialize!
   (fill-other-fields!
    (register-class
     '(1 . 1)
     ((Class-allocator Inlinable-Class-class) 5)
     'set!-cont
     continuation-class
     ('#<procedure #2 ##list>
      (initialize!
       (fill-other-fields!
        ('#<procedure #5 ##subtype-set!>
         ('#<procedure #6 ##vector>
          (let ((o Inlinable-Mono-Field-class))
            ('#<procedure #4 ##vector-ref> o 2))
          'n
          #f
          meroon-uninitialized
          #t
          meroon-uninitialized
          1
          1)
         6)))
      (initialize!
       (fill-other-fields!
        ('#<procedure #5 ##subtype-set!>
         ('#<procedure #6 ##vector>
          (let ((o Inlinable-Mono-Field-class))
            ('#<procedure #4 ##vector-ref> o 2))
          'r
          #f
          meroon-uninitialized
          #t
          meroon-uninitialized
          1
          2)
         6))))))))

#t

(define set!-cont-k
  (lambda (#:g390)
    (let ((begin-temp.39 (check-class #:g390 set!-cont-class 'k)))
      ('#<procedure #4 ##vector-ref> #:g390 1))))

(define set!-cont-k-set!
  (lambda (#:g391 #:g392)
    (let ((begin-temp.40 (check-class #:g391 set!-cont-class 'k)))
      ('#<procedure #10 ##vector-set!> #:g391 1 #:g392))))

(define set!-cont-n
  (lambda (#:g393)
    (let ((begin-temp.41 (check-class #:g393 set!-cont-class 'n)))
      ('#<procedure #4 ##vector-ref> #:g393 2))))

(define set!-cont-n-set!
  (lambda (#:g394 #:g395)
    (let ((begin-temp.42 (check-class #:g394 set!-cont-class 'n)))
      ('#<procedure #10 ##vector-set!> #:g394 2 #:g395))))

(define set!-cont-r
  (lambda (#:g396)
    (let ((begin-temp.43 (check-class #:g396 set!-cont-class 'r)))
      ('#<procedure #4 ##vector-ref> #:g396 3))))

(define set!-cont-r-set!
  (lambda (#:g397 #:g398)
    (let ((begin-temp.44 (check-class #:g397 set!-cont-class 'r)))
      ('#<procedure #10 ##vector-set!> #:g397 3 #:g398))))

(define set!-cont?
  (lambda (#:g399)
    (and ('#<procedure #3 ##meroon?> #:g399)
         (careless-subclass?
          ('#<procedure #4 ##vector-ref>
           *classes*
           ('#<procedure #4 ##vector-ref> #:g399 0))
          set!-cont-class))))

(define make-set!-cont
  (lambda (k n r)
    (initialize!
     ('#<procedure #5 ##subtype-set!>
      ('#<procedure #6 ##vector>
       ('#<procedure #4 ##vector-ref> set!-cont-class 2)
       k
       n
       r)
      6))))

(define allocate-set!-cont (Class-allocator set!-cont-class))

(define ->set!-cont
  (let ((#:g400 ('#<procedure #7 ##box> 'wait)))
    (let ((begin-temp.45
           ('#<procedure #8 ##set-box!>
            #:g400
            (register-Generic-1
             '(1 . 1)
             '->set!-cont
             meroon-uninitialized
             '((o))
             #f))))
      (lambda (o)
        ((careless-determine-method1 ('#<procedure #9 ##unbox> #:g400) o)
         o)))))

(register-method
 '->set!-cont
 '((o set!-cont))
 '(1 . 1)
 (lambda (#:g402 #:g403 #:g404) (lambda (o) o))
 'set!-cont)

'set!-cont

(define begin-cont-class
  (initialize!
   (fill-other-fields!
    (register-class
     '(1 . 1)
     ((Class-allocator Inlinable-Class-class) 5)
     'begin-cont
     continuation-class
     ('#<procedure #2 ##list>
      (initialize!
       (fill-other-fields!
        ('#<procedure #5 ##subtype-set!>
         ('#<procedure #6 ##vector>
          (let ((o Inlinable-Mono-Field-class))
            ('#<procedure #4 ##vector-ref> o 2))
          'e*
          #f
          meroon-uninitialized
          #t
          meroon-uninitialized
          1
          1)
         6)))
      (initialize!
       (fill-other-fields!
        ('#<procedure #5 ##subtype-set!>
         ('#<procedure #6 ##vector>
          (let ((o Inlinable-Mono-Field-class))
            ('#<procedure #4 ##vector-ref> o 2))
          'r
          #f
          meroon-uninitialized
          #t
          meroon-uninitialized
          1
          2)
         6))))))))

#t

(define begin-cont-k
  (lambda (#:g406)
    (let ((begin-temp.46 (check-class #:g406 begin-cont-class 'k)))
      ('#<procedure #4 ##vector-ref> #:g406 1))))

(define begin-cont-k-set!
  (lambda (#:g407 #:g408)
    (let ((begin-temp.47 (check-class #:g407 begin-cont-class 'k)))
      ('#<procedure #10 ##vector-set!> #:g407 1 #:g408))))

(define begin-cont-e*
  (lambda (#:g409)
    (let ((begin-temp.48 (check-class #:g409 begin-cont-class 'e*)))
      ('#<procedure #4 ##vector-ref> #:g409 2))))

(define begin-cont-e*-set!
  (lambda (#:g410 #:g411)
    (let ((begin-temp.49 (check-class #:g410 begin-cont-class 'e*)))
      ('#<procedure #10 ##vector-set!> #:g410 2 #:g411))))

(define begin-cont-r
  (lambda (#:g412)
    (let ((begin-temp.50 (check-class #:g412 begin-cont-class 'r)))
      ('#<procedure #4 ##vector-ref> #:g412 3))))

(define begin-cont-r-set!
  (lambda (#:g413 #:g414)
    (let ((begin-temp.51 (check-class #:g413 begin-cont-class 'r)))
      ('#<procedure #10 ##vector-set!> #:g413 3 #:g414))))

(define begin-cont?
  (lambda (#:g415)
    (and ('#<procedure #3 ##meroon?> #:g415)
         (careless-subclass?
          ('#<procedure #4 ##vector-ref>
           *classes*
           ('#<procedure #4 ##vector-ref> #:g415 0))
          begin-cont-class))))

(define make-begin-cont
  (lambda (k e* r)
    (initialize!
     ('#<procedure #5 ##subtype-set!>
      ('#<procedure #6 ##vector>
       ('#<procedure #4 ##vector-ref> begin-cont-class 2)
       k
       e*
       r)
      6))))

(define allocate-begin-cont (Class-allocator begin-cont-class))

(define ->begin-cont
  (let ((#:g416 ('#<procedure #7 ##box> 'wait)))
    (let ((begin-temp.52
           ('#<procedure #8 ##set-box!>
            #:g416
            (register-Generic-1
             '(1 . 1)
             '->begin-cont
             meroon-uninitialized
             '((o))
             #f))))
      (lambda (o)
        ((careless-determine-method1 ('#<procedure #9 ##unbox> #:g416) o)
         o)))))

(register-method
 '->begin-cont
 '((o begin-cont))
 '(1 . 1)
 (lambda (#:g418 #:g419 #:g420) (lambda (o) o))
 'begin-cont)

'begin-cont

(define evfun-cont-class
  (initialize!
   (fill-other-fields!
    (register-class
     '(1 . 1)
     ((Class-allocator Inlinable-Class-class) 5)
     'evfun-cont
     continuation-class
     ('#<procedure #2 ##list>
      (initialize!
       (fill-other-fields!
        ('#<procedure #5 ##subtype-set!>
         ('#<procedure #6 ##vector>
          (let ((o Inlinable-Mono-Field-class))
            ('#<procedure #4 ##vector-ref> o 2))
          'e*
          #f
          meroon-uninitialized
          #t
          meroon-uninitialized
          1
          1)
         6)))
      (initialize!
       (fill-other-fields!
        ('#<procedure #5 ##subtype-set!>
         ('#<procedure #6 ##vector>
          (let ((o Inlinable-Mono-Field-class))
            ('#<procedure #4 ##vector-ref> o 2))
          'r
          #f
          meroon-uninitialized
          #t
          meroon-uninitialized
          1
          2)
         6))))))))

#t

(define evfun-cont-k
  (lambda (#:g422)
    (let ((begin-temp.53 (check-class #:g422 evfun-cont-class 'k)))
      ('#<procedure #4 ##vector-ref> #:g422 1))))

(define evfun-cont-k-set!
  (lambda (#:g423 #:g424)
    (let ((begin-temp.54 (check-class #:g423 evfun-cont-class 'k)))
      ('#<procedure #10 ##vector-set!> #:g423 1 #:g424))))

(define evfun-cont-e*
  (lambda (#:g425)
    (let ((begin-temp.55 (check-class #:g425 evfun-cont-class 'e*)))
      ('#<procedure #4 ##vector-ref> #:g425 2))))

(define evfun-cont-e*-set!
  (lambda (#:g426 #:g427)
    (let ((begin-temp.56 (check-class #:g426 evfun-cont-class 'e*)))
      ('#<procedure #10 ##vector-set!> #:g426 2 #:g427))))

(define evfun-cont-r
  (lambda (#:g428)
    (let ((begin-temp.57 (check-class #:g428 evfun-cont-class 'r)))
      ('#<procedure #4 ##vector-ref> #:g428 3))))

(define evfun-cont-r-set!
  (lambda (#:g429 #:g430)
    (let ((begin-temp.58 (check-class #:g429 evfun-cont-class 'r)))
      ('#<procedure #10 ##vector-set!> #:g429 3 #:g430))))

(define evfun-cont?
  (lambda (#:g431)
    (and ('#<procedure #3 ##meroon?> #:g431)
         (careless-subclass?
          ('#<procedure #4 ##vector-ref>
           *classes*
           ('#<procedure #4 ##vector-ref> #:g431 0))
          evfun-cont-class))))

(define make-evfun-cont
  (lambda (k e* r)
    (initialize!
     ('#<procedure #5 ##subtype-set!>
      ('#<procedure #6 ##vector>
       ('#<procedure #4 ##vector-ref> evfun-cont-class 2)
       k
       e*
       r)
      6))))

(define allocate-evfun-cont (Class-allocator evfun-cont-class))

(define ->evfun-cont
  (let ((#:g432 ('#<procedure #7 ##box> 'wait)))
    (let ((begin-temp.59
           ('#<procedure #8 ##set-box!>
            #:g432
            (register-Generic-1
             '(1 . 1)
             '->evfun-cont
             meroon-uninitialized
             '((o))
             #f))))
      (lambda (o)
        ((careless-determine-method1 ('#<procedure #9 ##unbox> #:g432) o)
         o)))))

(register-method
 '->evfun-cont
 '((o evfun-cont))
 '(1 . 1)
 (lambda (#:g434 #:g435 #:g436) (lambda (o) o))
 'evfun-cont)

'evfun-cont

(define apply-cont-class
  (initialize!
   (fill-other-fields!
    (register-class
     '(1 . 1)
     ((Class-allocator Inlinable-Class-class) 5)
     'apply-cont
     continuation-class
     ('#<procedure #2 ##list>
      (initialize!
       (fill-other-fields!
        ('#<procedure #5 ##subtype-set!>
         ('#<procedure #6 ##vector>
          (let ((o Inlinable-Mono-Field-class))
            ('#<procedure #4 ##vector-ref> o 2))
          'f
          #f
          meroon-uninitialized
          #t
          meroon-uninitialized
          1
          1)
         6)))
      (initialize!
       (fill-other-fields!
        ('#<procedure #5 ##subtype-set!>
         ('#<procedure #6 ##vector>
          (let ((o Inlinable-Mono-Field-class))
            ('#<procedure #4 ##vector-ref> o 2))
          'r
          #f
          meroon-uninitialized
          #t
          meroon-uninitialized
          1
          2)
         6))))))))

#t

(define apply-cont-k
  (lambda (#:g438)
    (let ((begin-temp.60 (check-class #:g438 apply-cont-class 'k)))
      ('#<procedure #4 ##vector-ref> #:g438 1))))

(define apply-cont-k-set!
  (lambda (#:g439 #:g440)
    (let ((begin-temp.61 (check-class #:g439 apply-cont-class 'k)))
      ('#<procedure #10 ##vector-set!> #:g439 1 #:g440))))

(define apply-cont-f
  (lambda (#:g441)
    (let ((begin-temp.62 (check-class #:g441 apply-cont-class 'f)))
      ('#<procedure #4 ##vector-ref> #:g441 2))))

(define apply-cont-f-set!
  (lambda (#:g442 #:g443)
    (let ((begin-temp.63 (check-class #:g442 apply-cont-class 'f)))
      ('#<procedure #10 ##vector-set!> #:g442 2 #:g443))))

(define apply-cont-r
  (lambda (#:g444)
    (let ((begin-temp.64 (check-class #:g444 apply-cont-class 'r)))
      ('#<procedure #4 ##vector-ref> #:g444 3))))

(define apply-cont-r-set!
  (lambda (#:g445 #:g446)
    (let ((begin-temp.65 (check-class #:g445 apply-cont-class 'r)))
      ('#<procedure #10 ##vector-set!> #:g445 3 #:g446))))

(define apply-cont?
  (lambda (#:g447)
    (and ('#<procedure #3 ##meroon?> #:g447)
         (careless-subclass?
          ('#<procedure #4 ##vector-ref>
           *classes*
           ('#<procedure #4 ##vector-ref> #:g447 0))
          apply-cont-class))))

(define make-apply-cont
  (lambda (k f r)
    (initialize!
     ('#<procedure #5 ##subtype-set!>
      ('#<procedure #6 ##vector>
       ('#<procedure #4 ##vector-ref> apply-cont-class 2)
       k
       f
       r)
      6))))

(define allocate-apply-cont (Class-allocator apply-cont-class))

(define ->apply-cont
  (let ((#:g448 ('#<procedure #7 ##box> 'wait)))
    (let ((begin-temp.66
           ('#<procedure #8 ##set-box!>
            #:g448
            (register-Generic-1
             '(1 . 1)
             '->apply-cont
             meroon-uninitialized
             '((o))
             #f))))
      (lambda (o)
        ((careless-determine-method1 ('#<procedure #9 ##unbox> #:g448) o)
         o)))))

(register-method
 '->apply-cont
 '((o apply-cont))
 '(1 . 1)
 (lambda (#:g450 #:g451 #:g452) (lambda (o) o))
 'apply-cont)

'apply-cont

(define argument-cont-class
  (initialize!
   (fill-other-fields!
    (register-class
     '(1 . 1)
     ((Class-allocator Inlinable-Class-class) 5)
     'argument-cont
     continuation-class
     ('#<procedure #2 ##list>
      (initialize!
       (fill-other-fields!
        ('#<procedure #5 ##subtype-set!>
         ('#<procedure #6 ##vector>
          (let ((o Inlinable-Mono-Field-class))
            ('#<procedure #4 ##vector-ref> o 2))
          'e*
          #f
          meroon-uninitialized
          #t
          meroon-uninitialized
          1
          1)
         6)))
      (initialize!
       (fill-other-fields!
        ('#<procedure #5 ##subtype-set!>
         ('#<procedure #6 ##vector>
          (let ((o Inlinable-Mono-Field-class))
            ('#<procedure #4 ##vector-ref> o 2))
          'r
          #f
          meroon-uninitialized
          #t
          meroon-uninitialized
          1
          2)
         6))))))))

#t

(define argument-cont-k
  (lambda (#:g454)
    (let ((begin-temp.67 (check-class #:g454 argument-cont-class 'k)))
      ('#<procedure #4 ##vector-ref> #:g454 1))))

(define argument-cont-k-set!
  (lambda (#:g455 #:g456)
    (let ((begin-temp.68 (check-class #:g455 argument-cont-class 'k)))
      ('#<procedure #10 ##vector-set!> #:g455 1 #:g456))))

(define argument-cont-e*
  (lambda (#:g457)
    (let ((begin-temp.69 (check-class #:g457 argument-cont-class 'e*)))
      ('#<procedure #4 ##vector-ref> #:g457 2))))

(define argument-cont-e*-set!
  (lambda (#:g458 #:g459)
    (let ((begin-temp.70 (check-class #:g458 argument-cont-class 'e*)))
      ('#<procedure #10 ##vector-set!> #:g458 2 #:g459))))

(define argument-cont-r
  (lambda (#:g460)
    (let ((begin-temp.71 (check-class #:g460 argument-cont-class 'r)))
      ('#<procedure #4 ##vector-ref> #:g460 3))))

(define argument-cont-r-set!
  (lambda (#:g461 #:g462)
    (let ((begin-temp.72 (check-class #:g461 argument-cont-class 'r)))
      ('#<procedure #10 ##vector-set!> #:g461 3 #:g462))))

(define argument-cont?
  (lambda (#:g463)
    (and ('#<procedure #3 ##meroon?> #:g463)
         (careless-subclass?
          ('#<procedure #4 ##vector-ref>
           *classes*
           ('#<procedure #4 ##vector-ref> #:g463 0))
          argument-cont-class))))

(define make-argument-cont
  (lambda (k e* r)
    (initialize!
     ('#<procedure #5 ##subtype-set!>
      ('#<procedure #6 ##vector>
       ('#<procedure #4 ##vector-ref> argument-cont-class 2)
       k
       e*
       r)
      6))))

(define allocate-argument-cont (Class-allocator argument-cont-class))

(define ->argument-cont
  (let ((#:g464 ('#<procedure #7 ##box> 'wait)))
    (let ((begin-temp.73
           ('#<procedure #8 ##set-box!>
            #:g464
            (register-Generic-1
             '(1 . 1)
             '->argument-cont
             meroon-uninitialized
             '((o))
             #f))))
      (lambda (o)
        ((careless-determine-method1 ('#<procedure #9 ##unbox> #:g464) o)
         o)))))

(register-method
 '->argument-cont
 '((o argument-cont))
 '(1 . 1)
 (lambda (#:g466 #:g467 #:g468) (lambda (o) o))
 'argument-cont)

'argument-cont

(define gather-cont-class
  (initialize!
   (fill-other-fields!
    (register-class
     '(1 . 1)
     ((Class-allocator Inlinable-Class-class) 5)
     'gather-cont
     continuation-class
     ('#<procedure #2 ##list>
      (initialize!
       (fill-other-fields!
        ('#<procedure #5 ##subtype-set!>
         ('#<procedure #6 ##vector>
          (let ((o Inlinable-Mono-Field-class))
            ('#<procedure #4 ##vector-ref> o 2))
          'v
          #f
          meroon-uninitialized
          #t
          meroon-uninitialized
          1
          1)
         6))))))))

#t

(define gather-cont-k
  (lambda (#:g470)
    (let ((begin-temp.74 (check-class #:g470 gather-cont-class 'k)))
      ('#<procedure #4 ##vector-ref> #:g470 1))))

(define gather-cont-k-set!
  (lambda (#:g471 #:g472)
    (let ((begin-temp.75 (check-class #:g471 gather-cont-class 'k)))
      ('#<procedure #10 ##vector-set!> #:g471 1 #:g472))))

(define gather-cont-v
  (lambda (#:g473)
    (let ((begin-temp.76 (check-class #:g473 gather-cont-class 'v)))
      ('#<procedure #4 ##vector-ref> #:g473 2))))

(define gather-cont-v-set!
  (lambda (#:g474 #:g475)
    (let ((begin-temp.77 (check-class #:g474 gather-cont-class 'v)))
      ('#<procedure #10 ##vector-set!> #:g474 2 #:g475))))

(define gather-cont?
  (lambda (#:g476)
    (and ('#<procedure #3 ##meroon?> #:g476)
         (careless-subclass?
          ('#<procedure #4 ##vector-ref>
           *classes*
           ('#<procedure #4 ##vector-ref> #:g476 0))
          gather-cont-class))))

(define make-gather-cont
  (lambda (k v)
    (initialize!
     ('#<procedure #5 ##subtype-set!>
      ('#<procedure #6 ##vector>
       ('#<procedure #4 ##vector-ref> gather-cont-class 2)
       k
       v)
      6))))

(define allocate-gather-cont (Class-allocator gather-cont-class))

(define ->gather-cont
  (let ((#:g477 ('#<procedure #7 ##box> 'wait)))
    (let ((begin-temp.78
           ('#<procedure #8 ##set-box!>
            #:g477
            (register-Generic-1
             '(1 . 1)
             '->gather-cont
             meroon-uninitialized
             '((o))
             #f))))
      (lambda (o)
        ((careless-determine-method1 ('#<procedure #9 ##unbox> #:g477) o)
         o)))))

(register-method
 '->gather-cont
 '((o gather-cont))
 '(1 . 1)
 (lambda (#:g479 #:g480 #:g481) (lambda (o) o))
 'gather-cont)

'gather-cont

(define bottom-cont-class
  (initialize!
   (fill-other-fields!
    (register-class
     '(1 . 1)
     ((Class-allocator Inlinable-Class-class) 5)
     'bottom-cont
     continuation-class
     ('#<procedure #2 ##list>
      (initialize!
       (fill-other-fields!
        ('#<procedure #5 ##subtype-set!>
         ('#<procedure #6 ##vector>
          (let ((o Inlinable-Mono-Field-class))
            ('#<procedure #4 ##vector-ref> o 2))
          'f
          #f
          meroon-uninitialized
          #t
          meroon-uninitialized
          1
          1)
         6))))))))

#t

(define bottom-cont-k
  (lambda (#:g483)
    (let ((begin-temp.79 (check-class #:g483 bottom-cont-class 'k)))
      ('#<procedure #4 ##vector-ref> #:g483 1))))

(define bottom-cont-k-set!
  (lambda (#:g484 #:g485)
    (let ((begin-temp.80 (check-class #:g484 bottom-cont-class 'k)))
      ('#<procedure #10 ##vector-set!> #:g484 1 #:g485))))

(define bottom-cont-f
  (lambda (#:g486)
    (let ((begin-temp.81 (check-class #:g486 bottom-cont-class 'f)))
      ('#<procedure #4 ##vector-ref> #:g486 2))))

(define bottom-cont-f-set!
  (lambda (#:g487 #:g488)
    (let ((begin-temp.82 (check-class #:g487 bottom-cont-class 'f)))
      ('#<procedure #10 ##vector-set!> #:g487 2 #:g488))))

(define bottom-cont?
  (lambda (#:g489)
    (and ('#<procedure #3 ##meroon?> #:g489)
         (careless-subclass?
          ('#<procedure #4 ##vector-ref>
           *classes*
           ('#<procedure #4 ##vector-ref> #:g489 0))
          bottom-cont-class))))

(define make-bottom-cont
  (lambda (k f)
    (initialize!
     ('#<procedure #5 ##subtype-set!>
      ('#<procedure #6 ##vector>
       ('#<procedure #4 ##vector-ref> bottom-cont-class 2)
       k
       f)
      6))))

(define allocate-bottom-cont (Class-allocator bottom-cont-class))

(define ->bottom-cont
  (let ((#:g490 ('#<procedure #7 ##box> 'wait)))
    (let ((begin-temp.83
           ('#<procedure #8 ##set-box!>
            #:g490
            (register-Generic-1
             '(1 . 1)
             '->bottom-cont
             meroon-uninitialized
             '((o))
             #f))))
      (lambda (o)
        ((careless-determine-method1 ('#<procedure #9 ##unbox> #:g490) o)
         o)))))

(register-method
 '->bottom-cont
 '((o bottom-cont))
 '(1 . 1)
 (lambda (#:g492 #:g493 #:g494) (lambda (o) o))
 'bottom-cont)

'bottom-cont

(define evaluate
  (lambda (e r k)
    (if ('#<procedure #11 ##not> ('#<procedure #12 ##pair?> e))
        (if ('#<procedure #13 ##symbol?> e) (lookup r e k) (resume k e))
        (let ((case-temp.84 ('#<procedure #14 ##car> e)))
          (if ('#<procedure #15 ##eq?> case-temp.84 'quote)
              (let ((v ('#<procedure #53 ##cadr> e))) (resume k v))
              (if ('#<procedure #15 ##eq?> case-temp.84 'if)
                  (let ((ef ('#<procedure #54 ##cadddr> e))
                        (et ('#<procedure #55 ##caddr> e))
                        (ec ('#<procedure #53 ##cadr> e)))
                    (evaluate
                     ec
                     r
                     (initialize!
                      ('#<procedure #5 ##subtype-set!>
                       ('#<procedure #6 ##vector>
                        ('#<procedure #4 ##vector-ref> if-cont-class 2)
                        k
                        et
                        ef
                        r)
                       6))))
                  (if ('#<procedure #15 ##eq?> case-temp.84 'begin)
                      (evaluate-begin ('#<procedure #16 ##cdr> e) r k)
                      (if ('#<procedure #15 ##eq?> case-temp.84 'set!)
                          (let ((e ('#<procedure #55 ##caddr> e))
                                (n ('#<procedure #53 ##cadr> e)))
                            (evaluate
                             e
                             r
                             (initialize!
                              ('#<procedure #5 ##subtype-set!>
                               ('#<procedure #6 ##vector>
                                ('#<procedure #4 ##vector-ref>
                                 set!-cont-class
                                 2)
                                k
                                n
                                r)
                               6))))
                          (if ('#<procedure #15 ##eq?> case-temp.84 'lambda)
                              (let ((e* ('#<procedure #56 ##cddr> e))
                                    (n* ('#<procedure #53 ##cadr> e)))
                                (resume k
                                        (initialize!
                                         ('#<procedure #5 ##subtype-set!>
                                          ('#<procedure #6 ##vector>
                                           ('#<procedure #4 ##vector-ref>
                                            function-class
                                            2)
                                           n*
                                           e*
                                           r)
                                          6))))
                              (let ((e* ('#<procedure #16 ##cdr> e))
                                    (e ('#<procedure #14 ##car> e)))
                                (evaluate
                                 e
                                 r
                                 (initialize!
                                  ('#<procedure #5 ##subtype-set!>
                                   ('#<procedure #6 ##vector>
                                    ('#<procedure #4 ##vector-ref>
                                     evfun-cont-class
                                     2)
                                    k
                                    e*
                                    r)
                                   6)))))))))))))

(define evaluate-variable (lambda (n r k) (lookup r n k)))

(define evaluate-quote (lambda (v r k) (resume k v)))

(define evaluate-if
  (lambda (ec et ef r k)
    (evaluate
     ec
     r
     (initialize!
      ('#<procedure #5 ##subtype-set!>
       ('#<procedure #6 ##vector>
        ('#<procedure #4 ##vector-ref> if-cont-class 2)
        k
        et
        ef
        r)
       6)))))

(define evaluate-set!
  (lambda (n e r k)
    (evaluate
     e
     r
     (initialize!
      ('#<procedure #5 ##subtype-set!>
       ('#<procedure #6 ##vector>
        ('#<procedure #4 ##vector-ref> set!-cont-class 2)
        k
        n
        r)
       6)))))

(define evaluate-lambda
  (lambda (n* e* r k)
    (resume k
            (initialize!
             ('#<procedure #5 ##subtype-set!>
              ('#<procedure #6 ##vector>
               ('#<procedure #4 ##vector-ref> function-class 2)
               n*
               e*
               r)
              6)))))

(define evaluate-begin
  (lambda (e* r k)
    (if ('#<procedure #12 ##pair?> e*)
        (if ('#<procedure #12 ##pair?> ('#<procedure #16 ##cdr> e*))
            (evaluate
             ('#<procedure #14 ##car> e*)
             r
             (initialize!
              ('#<procedure #5 ##subtype-set!>
               ('#<procedure #6 ##vector>
                ('#<procedure #4 ##vector-ref> begin-cont-class 2)
                k
                e*
                r)
               6)))
            (evaluate ('#<procedure #14 ##car> e*) r k))
        (resume k '()))))

(define evaluate-application
  (lambda (e e* r k)
    (evaluate
     e
     r
     (initialize!
      ('#<procedure #5 ##subtype-set!>
       ('#<procedure #6 ##vector>
        ('#<procedure #4 ##vector-ref> evfun-cont-class 2)
        k
        e*
        r)
       6)))))

(define invoke
  (let ((#:g496 ('#<procedure #7 ##box> 'wait)))
    (let ((begin-temp.88
           ('#<procedure #8 ##set-box!>
            #:g496
            (register-Generic-1
             '(1 . 1)
             'invoke
             (lambda (f v* r k)
               (let ((begin-temp.87 ('#<procedure #17 newline>)))
                 (let ((begin-temp.86
                        ('#<procedure #18 display>
                         "error in invoke : cannot apply : ")))
                   (let ((begin-temp.85 (show f)))
                     (wrong "not a function" f r k)))))
             '((f) v* r k)
             #f))))
      (lambda (f v* r k)
        ((careless-determine-method1 ('#<procedure #9 ##unbox> #:g496) f)
         f
         v*
         r
         k)))))

(register-method
 'invoke
 '((f primitive) v* r k)
 '(1 . 1)
 (lambda (#:g498 #:g499 #:g500)
   (lambda (f v* r k) ((primitive-address f) v* r k)))
 'primitive)

(register-method
 'invoke
 '((f function) v* r k)
 '(1 . 1)
 (lambda (#:g502 #:g503 #:g504)
   (lambda (f v* r k)
     (let ((env (extend-env (function-env f) (function-variables f) v*)))
       (evaluate-begin (function-body f) env k))))
 'function)

(register-method
 'invoke
 '((f continuation) v* r k)
 '(1 . 1)
 (lambda (#:g506 #:g507 #:g508)
   (lambda (f v* r k)
     (if ('#<procedure #21 ##fx=> 1 ('#<procedure #19 length> v*))
         (resume f ('#<procedure #14 ##car> v*))
         (wrong "Continuations expect one argument" v* r k))))
 'continuation)

(define evaluate-arguments
  (lambda (e* r k)
    (if ('#<procedure #12 ##pair?> e*)
        (evaluate
         ('#<procedure #14 ##car> e*)
         r
         (initialize!
          ('#<procedure #5 ##subtype-set!>
           ('#<procedure #6 ##vector>
            ('#<procedure #4 ##vector-ref> argument-cont-class 2)
            k
            e*
            r)
           6)))
        (resume k '()))))

(define resume
  (let ((#:g510 ('#<procedure #7 ##box> 'wait)))
    (let ((begin-temp.92
           ('#<procedure #8 ##set-box!>
            #:g510
            (register-Generic-1
             '(1 . 1)
             'resume
             (lambda (k v)
               (let ((begin-temp.91 ('#<procedure #17 newline>)))
                 (let ((begin-temp.90
                        ('#<procedure #18 display>
                         "error in resume : Not a continuation : ")))
                   (let ((begin-temp.89 (show k)))
                     (wrong "Unknown continuation" k)))))
             '((k) v)
             #f))))
      (lambda (k v)
        ((careless-determine-method1 ('#<procedure #9 ##unbox> #:g510) k)
         k
         v)))))

(register-method
 'resume
 '((k if-cont) v)
 '(1 . 1)
 (lambda (#:g512 #:g513 #:g514)
   (lambda (k v)
     (evaluate
      (if v (if-cont-et k) (if-cont-ef k))
      (if-cont-r k)
      (if-cont-k k))))
 'if-cont)

(register-method
 'resume
 '((k begin-cont) v)
 '(1 . 1)
 (lambda (#:g516 #:g517 #:g518)
   (lambda (k v)
     (let ((k (begin-cont-k k))
           (r (begin-cont-r k))
           (e* ('#<procedure #16 ##cdr> (begin-cont-e* k))))
       (if ('#<procedure #12 ##pair?> e*)
           (if ('#<procedure #12 ##pair?> ('#<procedure #16 ##cdr> e*))
               (evaluate
                ('#<procedure #14 ##car> e*)
                r
                (initialize!
                 ('#<procedure #5 ##subtype-set!>
                  ('#<procedure #6 ##vector>
                   ('#<procedure #4 ##vector-ref> begin-cont-class 2)
                   k
                   e*
                   r)
                  6)))
               (evaluate ('#<procedure #14 ##car> e*) r k))
           (resume k '())))))
 'begin-cont)

(register-method
 'resume
 '((k evfun-cont) f)
 '(1 . 1)
 (lambda (#:g520 #:g521 #:g522)
   (lambda (k f)
     (let ((k (make-apply-cont (evfun-cont-k k) f (evfun-cont-r k)))
           (r (let ((begin-temp.57 (check-class k evfun-cont-class 'r)))
                ('#<procedure #4 ##vector-ref> k 3)))
           (e* (let ((begin-temp.55 (check-class k evfun-cont-class 'e*)))
                 ('#<procedure #4 ##vector-ref> k 2))))
       (if ('#<procedure #12 ##pair?> e*)
           (evaluate
            ('#<procedure #14 ##car> e*)
            r
            (initialize!
             ('#<procedure #5 ##subtype-set!>
              ('#<procedure #6 ##vector>
               ('#<procedure #4 ##vector-ref> argument-cont-class 2)
               k
               e*
               r)
              6)))
           (resume k '())))))
 'evfun-cont)

(register-method
 'resume
 '((k apply-cont) v)
 '(1 . 1)
 (lambda (#:g524 #:g525 #:g526)
   (lambda (k v)
     (invoke (apply-cont-f k) v (apply-cont-r k) (apply-cont-k k))))
 'apply-cont)

(register-method
 'resume
 '((k argument-cont) v)
 '(1 . 1)
 (lambda (#:g528 #:g529 #:g530)
   (lambda (k v)
     (let ((k (make-gather-cont (argument-cont-k k) v))
           (r (let ((begin-temp.71 (check-class k argument-cont-class 'r)))
                ('#<procedure #4 ##vector-ref> k 3)))
           (e* ('#<procedure #16 ##cdr>
                (let ((begin-temp.69 (check-class k argument-cont-class 'e*)))
                  ('#<procedure #4 ##vector-ref> k 2)))))
       (if ('#<procedure #12 ##pair?> e*)
           (evaluate
            ('#<procedure #14 ##car> e*)
            r
            (initialize!
             ('#<procedure #5 ##subtype-set!>
              ('#<procedure #6 ##vector>
               ('#<procedure #4 ##vector-ref> argument-cont-class 2)
               k
               e*
               r)
              6)))
           (resume k '())))))
 'argument-cont)

(register-method
 'resume
 '((k gather-cont) v*)
 '(1 . 1)
 (lambda (#:g532 #:g533 #:g534)
   (lambda (k v*)
     (resume (gather-cont-k k)
             ('#<procedure #23 ##cons> (gather-cont-v k) v*))))
 'gather-cont)

(register-method
 'resume
 '((k bottom-cont) v)
 '(1 . 1)
 (lambda (#:g536 #:g537 #:g538) (lambda (k v) ((bottom-cont-f k) v)))
 'bottom-cont)

(define lookup
  (let ((#:g540 ('#<procedure #7 ##box> 'wait)))
    (let ((begin-temp.96
           ('#<procedure #8 ##set-box!>
            #:g540
            (register-Generic-1
             '(1 . 1)
             'lookup
             (lambda (r n k)
               (let ((begin-temp.95 ('#<procedure #17 newline>)))
                 (let ((begin-temp.94
                        ('#<procedure #18 display>
                         "error in lookup : Not an environment : ")))
                   (let ((begin-temp.93 (show r)))
                     (wrong "not an environment" r n k)))))
             '((r) n k)
             #f))))
      (lambda (r n k)
        ((careless-determine-method1 ('#<procedure #9 ##unbox> #:g540) r)
         r
         n
         k)))))

(register-method
 'lookup
 '((r null-env) n k)
 '(1 . 1)
 (lambda (#:g542 #:g543 #:g544)
   (lambda (r n k) (wrong "Unknown variable" n r k)))
 'null-env)

(register-method
 'lookup
 '((r variable-env) n k)
 '(1 . 1)
 (lambda (#:g546 #:g547 #:g548)
   (lambda (r n k)
     (if ('#<procedure #15 ##eq?> n (variable-env-name r))
         (resume k (variable-env-value r))
         (lookup (variable-env-others r) n k))))
 'variable-env)

(register-method
 'resume
 '((k set!-cont) v)
 '(1 . 1)
 (lambda (#:g550 #:g551 #:g552)
   (lambda (k v) (update! (set!-cont-r k) (set!-cont-n k) (set!-cont-k k) v)))
 'set!-cont)

(define update!
  (let ((#:g554 ('#<procedure #7 ##box> 'wait)))
    (let ((begin-temp.100
           ('#<procedure #8 ##set-box!>
            #:g554
            (register-Generic-1
             '(1 . 1)
             'update!
             (lambda (r n k v)
               (let ((begin-temp.99 ('#<procedure #17 newline>)))
                 (let ((begin-temp.98
                        ('#<procedure #18 display>
                         "error in lookup : Not an environment : ")))
                   (let ((begin-temp.97 (show r)))
                     (wrong "not an environment" r n k)))))
             '((r) n k v)
             #f))))
      (lambda (r n k v)
        ((careless-determine-method1 ('#<procedure #9 ##unbox> #:g554) r)
         r
         n
         k
         v)))))

(register-method
 'update!
 '((r null-env) n k v)
 '(1 . 1)
 (lambda (#:g556 #:g557 #:g558)
   (lambda (r n k v) (wrong "Unknown variable" n r k)))
 'null-env)

(register-method
 'update!
 '((r variable-env) n k v)
 '(1 . 1)
 (lambda (#:g560 #:g561 #:g562)
   (lambda (r n k v)
     (if ('#<procedure #24 equal?> n (variable-env-name r))
         (let ((begin-temp.101
                (let ((begin-temp.25
                       (check-class r variable-env-class 'value)))
                  ('#<procedure #10 ##vector-set!> r 3 v))))
           (resume k v))
         (update! (variable-env-others r) n k v))))
 'variable-env)

(define extend-env
  (lambda (env names values)
    (if ('#<procedure #12 ##pair?> names)
        (if ('#<procedure #12 ##pair?> values)
            (let ((value ('#<procedure #14 ##car> values))
                  (name ('#<procedure #14 ##car> names))
                  (others (extend-env
                           env
                           ('#<procedure #16 ##cdr> names)
                           ('#<procedure #16 ##cdr> values))))
              (initialize!
               ('#<procedure #5 ##subtype-set!>
                ('#<procedure #6 ##vector>
                 ('#<procedure #4 ##vector-ref> variable-env-class 2)
                 others
                 name
                 value)
                6)))
            (wrong "too less values" names))
        (if ('#<procedure #25 ##null?> names)
            (if ('#<procedure #12 ##pair?> values)
                (wrong "Too much values" values)
                env)
            (initialize!
             ('#<procedure #5 ##subtype-set!>
              ('#<procedure #6 ##vector>
               ('#<procedure #4 ##vector-ref> variable-env-class 2)
               env
               names
               values)
              6))))))

(define empty-begin-value '())

(define no-more-arguments '())

(define internal-error
  (lambda (msg . culprits) (wrong "Internal error" msg culprits)))

(define r.init (make-null-env))

(set! r.init
      (let ((value (let ((address (lambda (v* r k)
                                    (if ('#<procedure #21 ##fx=>
                                         2
                                         ('#<procedure #19 length> v*))
                                        (resume k
                                                ('#<procedure #26 apply>
                                                 '#<procedure #27 cons>
                                                 v*))
                                        (wrong "Incorrect arity" 'cons v*)))))
                     (initialize!
                      ('#<procedure #5 ##subtype-set!>
                       ('#<procedure #6 ##vector>
                        ('#<procedure #4 ##vector-ref> primitive-class 2)
                        'cons
                        address)
                       6))))
            (others r.init))
        (initialize!
         ('#<procedure #5 ##subtype-set!>
          ('#<procedure #6 ##vector>
           ('#<procedure #4 ##vector-ref> variable-env-class 2)
           others
           'cons
           value)
          6))))

'cons

(set! r.init
      (let ((value (let ((address (lambda (v* r k)
                                    (if ('#<procedure #21 ##fx=>
                                         1
                                         ('#<procedure #19 length> v*))
                                        (resume k
                                                ('#<procedure #26 apply>
                                                 '#<procedure #28 car>
                                                 v*))
                                        (wrong "Incorrect arity" 'car v*)))))
                     (initialize!
                      ('#<procedure #5 ##subtype-set!>
                       ('#<procedure #6 ##vector>
                        ('#<procedure #4 ##vector-ref> primitive-class 2)
                        'car
                        address)
                       6))))
            (others r.init))
        (initialize!
         ('#<procedure #5 ##subtype-set!>
          ('#<procedure #6 ##vector>
           ('#<procedure #4 ##vector-ref> variable-env-class 2)
           others
           'car
           value)
          6))))

'car

(set! r.init
      (let ((value (let ((address (lambda (v* r k)
                                    (if ('#<procedure #21 ##fx=>
                                         1
                                         ('#<procedure #19 length> v*))
                                        (resume k
                                                ('#<procedure #26 apply>
                                                 '#<procedure #29 cdr>
                                                 v*))
                                        (wrong "Incorrect arity" 'cdr v*)))))
                     (initialize!
                      ('#<procedure #5 ##subtype-set!>
                       ('#<procedure #6 ##vector>
                        ('#<procedure #4 ##vector-ref> primitive-class 2)
                        'cdr
                        address)
                       6))))
            (others r.init))
        (initialize!
         ('#<procedure #5 ##subtype-set!>
          ('#<procedure #6 ##vector>
           ('#<procedure #4 ##vector-ref> variable-env-class 2)
           others
           'cdr
           value)
          6))))

'cdr

(set! r.init
      (let ((value (let ((address (lambda (v* r k)
                                    (if ('#<procedure #21 ##fx=>
                                         1
                                         ('#<procedure #19 length> v*))
                                        (resume k
                                                ('#<procedure #26 apply>
                                                 '#<procedure #30 pair?>
                                                 v*))
                                        (wrong "Incorrect arity" 'pair? v*)))))
                     (initialize!
                      ('#<procedure #5 ##subtype-set!>
                       ('#<procedure #6 ##vector>
                        ('#<procedure #4 ##vector-ref> primitive-class 2)
                        'pair?
                        address)
                       6))))
            (others r.init))
        (initialize!
         ('#<procedure #5 ##subtype-set!>
          ('#<procedure #6 ##vector>
           ('#<procedure #4 ##vector-ref> variable-env-class 2)
           others
           'pair?
           value)
          6))))

'pair?

(set! r.init
      (let ((value (let ((address (lambda (v* r k)
                                    (if ('#<procedure #21 ##fx=>
                                         1
                                         ('#<procedure #19 length> v*))
                                        (resume k
                                                ('#<procedure #26 apply>
                                                 '#<procedure #31 symbol?>
                                                 v*))
                                        (wrong "Incorrect arity"
                                               'symbol?
                                               v*)))))
                     (initialize!
                      ('#<procedure #5 ##subtype-set!>
                       ('#<procedure #6 ##vector>
                        ('#<procedure #4 ##vector-ref> primitive-class 2)
                        'symbol?
                        address)
                       6))))
            (others r.init))
        (initialize!
         ('#<procedure #5 ##subtype-set!>
          ('#<procedure #6 ##vector>
           ('#<procedure #4 ##vector-ref> variable-env-class 2)
           others
           'symbol?
           value)
          6))))

'symbol?

(set! r.init
      (let ((value (let ((address (lambda (v* r k)
                                    (if ('#<procedure #21 ##fx=>
                                         2
                                         ('#<procedure #19 length> v*))
                                        (resume k
                                                ('#<procedure #26 apply>
                                                 '#<procedure #32 eq?>
                                                 v*))
                                        (wrong "Incorrect arity" 'eq? v*)))))
                     (initialize!
                      ('#<procedure #5 ##subtype-set!>
                       ('#<procedure #6 ##vector>
                        ('#<procedure #4 ##vector-ref> primitive-class 2)
                        'eq?
                        address)
                       6))))
            (others r.init))
        (initialize!
         ('#<procedure #5 ##subtype-set!>
          ('#<procedure #6 ##vector>
           ('#<procedure #4 ##vector-ref> variable-env-class 2)
           others
           'eq?
           value)
          6))))

'eq?

(set! r.init
      (let ((value (let ((address (lambda (v* r k)
                                    (if ('#<procedure #21 ##fx=>
                                         1
                                         ('#<procedure #19 length> v*))
                                        (resume k
                                                ('#<procedure #26 apply>
                                                 '#<procedure #33 null?>
                                                 v*))
                                        (wrong "Incorrect arity" 'null? v*)))))
                     (initialize!
                      ('#<procedure #5 ##subtype-set!>
                       ('#<procedure #6 ##vector>
                        ('#<procedure #4 ##vector-ref> primitive-class 2)
                        'null?
                        address)
                       6))))
            (others r.init))
        (initialize!
         ('#<procedure #5 ##subtype-set!>
          ('#<procedure #6 ##vector>
           ('#<procedure #4 ##vector-ref> variable-env-class 2)
           others
           'null?
           value)
          6))))

'null?

(set! r.init
      (let ((value (let ((address (lambda (v* r k)
                                    (if ('#<procedure #21 ##fx=>
                                         2
                                         ('#<procedure #19 length> v*))
                                        (resume k
                                                ('#<procedure #26 apply>
                                                 '#<procedure #34 set-car!>
                                                 v*))
                                        (wrong "Incorrect arity"
                                               'set-car!
                                               v*)))))
                     (initialize!
                      ('#<procedure #5 ##subtype-set!>
                       ('#<procedure #6 ##vector>
                        ('#<procedure #4 ##vector-ref> primitive-class 2)
                        'set-car!
                        address)
                       6))))
            (others r.init))
        (initialize!
         ('#<procedure #5 ##subtype-set!>
          ('#<procedure #6 ##vector>
           ('#<procedure #4 ##vector-ref> variable-env-class 2)
           others
           'set-car!
           value)
          6))))

'set-car!

(set! r.init
      (let ((value (let ((address (lambda (v* r k)
                                    (if ('#<procedure #21 ##fx=>
                                         2
                                         ('#<procedure #19 length> v*))
                                        (resume k
                                                ('#<procedure #26 apply>
                                                 '#<procedure #35 set-cdr!>
                                                 v*))
                                        (wrong "Incorrect arity"
                                               'set-cdr!
                                               v*)))))
                     (initialize!
                      ('#<procedure #5 ##subtype-set!>
                       ('#<procedure #6 ##vector>
                        ('#<procedure #4 ##vector-ref> primitive-class 2)
                        'set-cdr!
                        address)
                       6))))
            (others r.init))
        (initialize!
         ('#<procedure #5 ##subtype-set!>
          ('#<procedure #6 ##vector>
           ('#<procedure #4 ##vector-ref> variable-env-class 2)
           others
           'set-cdr!
           value)
          6))))

'set-cdr!

(set! r.init
      (let ((value (let ((address (lambda (v* r k)
                                    (if ('#<procedure #21 ##fx=>
                                         2
                                         ('#<procedure #19 length> v*))
                                        (resume k
                                                ('#<procedure #26 apply>
                                                 '#<procedure #36 +>
                                                 v*))
                                        (wrong "Incorrect arity" '+ v*)))))
                     (initialize!
                      ('#<procedure #5 ##subtype-set!>
                       ('#<procedure #6 ##vector>
                        ('#<procedure #4 ##vector-ref> primitive-class 2)
                        '+
                        address)
                       6))))
            (others r.init))
        (initialize!
         ('#<procedure #5 ##subtype-set!>
          ('#<procedure #6 ##vector>
           ('#<procedure #4 ##vector-ref> variable-env-class 2)
           others
           '+
           value)
          6))))

'+

(set! r.init
      (let ((value (let ((address (lambda (v* r k)
                                    (if ('#<procedure #21 ##fx=>
                                         2
                                         ('#<procedure #19 length> v*))
                                        (resume k
                                                ('#<procedure #26 apply>
                                                 '#<procedure #37 ->
                                                 v*))
                                        (wrong "Incorrect arity" '- v*)))))
                     (initialize!
                      ('#<procedure #5 ##subtype-set!>
                       ('#<procedure #6 ##vector>
                        ('#<procedure #4 ##vector-ref> primitive-class 2)
                        '-
                        address)
                       6))))
            (others r.init))
        (initialize!
         ('#<procedure #5 ##subtype-set!>
          ('#<procedure #6 ##vector>
           ('#<procedure #4 ##vector-ref> variable-env-class 2)
           others
           '-
           value)
          6))))

'-

(set! r.init
      (let ((value (let ((address (lambda (v* r k)
                                    (if ('#<procedure #21 ##fx=>
                                         2
                                         ('#<procedure #19 length> v*))
                                        (resume k
                                                ('#<procedure #26 apply>
                                                 '#<procedure #38 =>
                                                 v*))
                                        (wrong "Incorrect arity" '= v*)))))
                     (initialize!
                      ('#<procedure #5 ##subtype-set!>
                       ('#<procedure #6 ##vector>
                        ('#<procedure #4 ##vector-ref> primitive-class 2)
                        '=
                        address)
                       6))))
            (others r.init))
        (initialize!
         ('#<procedure #5 ##subtype-set!>
          ('#<procedure #6 ##vector>
           ('#<procedure #4 ##vector-ref> variable-env-class 2)
           others
           '=
           value)
          6))))

'=

(set! r.init
      (let ((value (let ((address (lambda (v* r k)
                                    (if ('#<procedure #21 ##fx=>
                                         2
                                         ('#<procedure #19 length> v*))
                                        (resume k
                                                ('#<procedure #26 apply>
                                                 '#<procedure #39 <>
                                                 v*))
                                        (wrong "Incorrect arity" '< v*)))))
                     (initialize!
                      ('#<procedure #5 ##subtype-set!>
                       ('#<procedure #6 ##vector>
                        ('#<procedure #4 ##vector-ref> primitive-class 2)
                        '<
                        address)
                       6))))
            (others r.init))
        (initialize!
         ('#<procedure #5 ##subtype-set!>
          ('#<procedure #6 ##vector>
           ('#<procedure #4 ##vector-ref> variable-env-class 2)
           others
           '<
           value)
          6))))

'<

(set! r.init
      (let ((value (let ((address (lambda (v* r k)
                                    (if ('#<procedure #21 ##fx=>
                                         2
                                         ('#<procedure #19 length> v*))
                                        (resume k
                                                ('#<procedure #26 apply>
                                                 '#<procedure #40 >>
                                                 v*))
                                        (wrong "Incorrect arity" '> v*)))))
                     (initialize!
                      ('#<procedure #5 ##subtype-set!>
                       ('#<procedure #6 ##vector>
                        ('#<procedure #4 ##vector-ref> primitive-class 2)
                        '>
                        address)
                       6))))
            (others r.init))
        (initialize!
         ('#<procedure #5 ##subtype-set!>
          ('#<procedure #6 ##vector>
           ('#<procedure #4 ##vector-ref> variable-env-class 2)
           others
           '>
           value)
          6))))

'>

(set! r.init
      (let ((value (let ((address (lambda (v* r k)
                                    (if ('#<procedure #21 ##fx=>
                                         2
                                         ('#<procedure #19 length> v*))
                                        (resume k
                                                ('#<procedure #26 apply>
                                                 '#<procedure #41 *>
                                                 v*))
                                        (wrong "Incorrect arity" '* v*)))))
                     (initialize!
                      ('#<procedure #5 ##subtype-set!>
                       ('#<procedure #6 ##vector>
                        ('#<procedure #4 ##vector-ref> primitive-class 2)
                        '*
                        address)
                       6))))
            (others r.init))
        (initialize!
         ('#<procedure #5 ##subtype-set!>
          ('#<procedure #6 ##vector>
           ('#<procedure #4 ##vector-ref> variable-env-class 2)
           others
           '*
           value)
          6))))

'*

(set! r.init
      (let ((value (let ((address (lambda (v* r k)
                                    (if ('#<procedure #21 ##fx=>
                                         2
                                         ('#<procedure #19 length> v*))
                                        (resume k
                                                ('#<procedure #26 apply>
                                                 '#<procedure #42 <=>
                                                 v*))
                                        (wrong "Incorrect arity" '<= v*)))))
                     (initialize!
                      ('#<procedure #5 ##subtype-set!>
                       ('#<procedure #6 ##vector>
                        ('#<procedure #4 ##vector-ref> primitive-class 2)
                        '<=
                        address)
                       6))))
            (others r.init))
        (initialize!
         ('#<procedure #5 ##subtype-set!>
          ('#<procedure #6 ##vector>
           ('#<procedure #4 ##vector-ref> variable-env-class 2)
           others
           '<=
           value)
          6))))

'<=

(set! r.init
      (let ((value (let ((address (lambda (v* r k)
                                    (if ('#<procedure #21 ##fx=>
                                         2
                                         ('#<procedure #19 length> v*))
                                        (resume k
                                                ('#<procedure #26 apply>
                                                 '#<procedure #43 >=>
                                                 v*))
                                        (wrong "Incorrect arity" '>= v*)))))
                     (initialize!
                      ('#<procedure #5 ##subtype-set!>
                       ('#<procedure #6 ##vector>
                        ('#<procedure #4 ##vector-ref> primitive-class 2)
                        '>=
                        address)
                       6))))
            (others r.init))
        (initialize!
         ('#<procedure #5 ##subtype-set!>
          ('#<procedure #6 ##vector>
           ('#<procedure #4 ##vector-ref> variable-env-class 2)
           others
           '>=
           value)
          6))))

'>=

(set! r.init
      (let ((value (let ((address (lambda (v* r k)
                                    (if ('#<procedure #21 ##fx=>
                                         2
                                         ('#<procedure #19 length> v*))
                                        (resume k
                                                ('#<procedure #26 apply>
                                                 '#<procedure #44 remainder>
                                                 v*))
                                        (wrong "Incorrect arity"
                                               'remainder
                                               v*)))))
                     (initialize!
                      ('#<procedure #5 ##subtype-set!>
                       ('#<procedure #6 ##vector>
                        ('#<procedure #4 ##vector-ref> primitive-class 2)
                        'remainder
                        address)
                       6))))
            (others r.init))
        (initialize!
         ('#<procedure #5 ##subtype-set!>
          ('#<procedure #6 ##vector>
           ('#<procedure #4 ##vector-ref> variable-env-class 2)
           others
           'remainder
           value)
          6))))

'remainder

(set! r.init
      (let ((value (let ((address (lambda (v* r k)
                                    (if ('#<procedure #21 ##fx=>
                                         1
                                         ('#<procedure #19 length> v*))
                                        (invoke ('#<procedure #14 ##car> v*)
                                                ('#<procedure #2 ##list> k)
                                                r
                                                k)
                                        (wrong "Incorrect arity"
                                               'call/cc
                                               v*)))))
                     (initialize!
                      ('#<procedure #5 ##subtype-set!>
                       ('#<procedure #6 ##vector>
                        ('#<procedure #4 ##vector-ref> primitive-class 2)
                        'call/cc
                        address)
                       6))))
            (others r.init))
        (initialize!
         ('#<procedure #5 ##subtype-set!>
          ('#<procedure #6 ##vector>
           ('#<procedure #4 ##vector-ref> variable-env-class 2)
           others
           'call/cc
           value)
          6))))

'call/cc

(set! r.init
      (let ((value (let ((address (lambda (v* r k)
                                    (if ('#<procedure #45 ##fx>=>
                                         ('#<procedure #19 length> v*)
                                         2)
                                        (if (or (let ((#:g298 ('#<procedure #14 ##car>
                                                               v*)))
                                                  (and ('#<procedure #3 ##meroon?>
                                                        #:g298)
                                                       (careless-subclass?
                                                        ('#<procedure #4 ##vector-ref>
                                                         *classes*
                                                         ('#<procedure #4 ##vector-ref>
                                                          #:g298
                                                          0))
                                                        function-class)))
                                                (or (let ((#:g311 ('#<procedure #14 ##car>
                                                                   v*)))
                                                      (and ('#<procedure #3 ##meroon?>
                                                            #:g311)
                                                           (careless-subclass?
                                                            ('#<procedure #4 ##vector-ref>
                                                             *classes*
                                                             ('#<procedure #4 ##vector-ref>
                                                              #:g311
                                                              0))
                                                            primitive-class)))
                                                    (let ((#:g364 ('#<procedure #14 ##car>
                                                                   v*)))
                                                      (and ('#<procedure #3 ##meroon?>
                                                            #:g364)
                                                           (careless-subclass?
                                                            ('#<procedure #4 ##vector-ref>
                                                             *classes*
                                                             ('#<procedure #4 ##vector-ref>
                                                              #:g364
                                                              0))
                                                            continuation-class)))))
                                            (invoke ('#<procedure #14 ##car>
                                                     v*)
                                                    (letrec ((conc (lambda (args)
                                                                     (if ('#<procedure #12 ##pair?>
                                                                          ('#<procedure #16 ##cdr>
                                                                           args))
                                                                         ('#<procedure #23 ##cons>
                                                                          ('#<procedure #14 ##car>
                                                                           args)
                                                                          (let ((args ('#<procedure #16 ##cdr>
                                                                                       args)))
                                                                            (if ('#<procedure #12 ##pair?>
                                                                                 ('#<procedure #16 ##cdr>
                                                                                  args))
                                                                                ('#<procedure #23 ##cons>
                                                                                 ('#<procedure #14 ##car>
                                                                                  args)
                                                                                 (let ((args ('#<procedure #16 ##cdr>
                                                                                              args)))
                                                                                   (if ('#<procedure #12 ##pair?>
                                                                                        ('#<procedure #16 ##cdr>
                                                                                         args))
                                                                                       ('#<procedure #23 ##cons>
                                                                                        ('#<procedure #14 ##car>
                                                                                         args)
                                                                                        (let ((args ('#<procedure #16 ##cdr>
                                                                                                     args)))
                                                                                          (if ('#<procedure #12 ##pair?>
                                                                                               ('#<procedure #16 ##cdr>
                                                                                                args))
                                                                                              ('#<procedure #23 ##cons>
                                                                                               ('#<procedure #14 ##car>
                                                                                                args)
                                                                                               (let ((args ('#<procedure #16 ##cdr>
                                                                                                            args)))
                                                                                                 (if ('#<procedure #12 ##pair?>
                                                                                                      ('#<procedure #16 ##cdr>
                                                                                                       args))
                                                                                                     ('#<procedure #23 ##cons>
                                                                                                      ('#<procedure #14 ##car>
                                                                                                       args)
                                                                                                      (let ((args ('#<procedure #16 ##cdr>
                                                                                                                   args)))
                                                                                                        (if ('#<procedure #12 ##pair?>
                                                                                                             ('#<procedure #16 ##cdr>
                                                                                                              args))
                                                                                                            ('#<procedure #23 ##cons>
                                                                                                             ('#<procedure #14 ##car>
                                                                                                              args)
                                                                                                             (let ((args ('#<procedure #16 ##cdr>
                                                                                                                          args)))
                                                                                                               (if ('#<procedure #12 ##pair?>
                                                                                                                    ('#<procedure #16 ##cdr>
                                                                                                                     args))
                                                                                                                   ('#<procedure #23 ##cons>
                                                                                                                    ('#<procedure #14 ##car>
                                                                                                                     args)
                                                                                                                    (let ((args ('#<procedure #16 ##cdr>
                                                                                                                                 args)))
                                                                                                                      (if ('#<procedure #12 ##pair?>
                                                                                                                           ('#<procedure #16 ##cdr>
                                                                                                                            args))
                                                                                                                          ('#<procedure #23 ##cons>
                                                                                                                           ('#<procedure #14 ##car>
                                                                                                                            args)
                                                                                                                           (let ((args ('#<procedure #16 ##cdr>
                                                                                                                                        args)))
                                                                                                                             (if ('#<procedure #12 ##pair?>
                                                                                                                                  ('#<procedure #16 ##cdr>
                                                                                                                                   args))
                                                                                                                                 ('#<procedure #23 ##cons>
                                                                                                                                  ('#<procedure #14 ##car>
                                                                                                                                   args)
                                                                                                                                  (let ((args ('#<procedure #16 ##cdr>
                                                                                                                                               args)))
                                                                                                                                    (if ('#<procedure #12 ##pair?>
                                                                                                                                         ('#<procedure #16 ##cdr>
                                                                                                                                          args))
                                                                                                                                        ('#<procedure #23 ##cons>
                                                                                                                                         ('#<procedure #14 ##car>
                                                                                                                                          args)
                                                                                                                                         (let ((args ('#<procedure #16 ##cdr>
                                                                                                                                                      args)))
                                                                                                                                           (if ('#<procedure #12 ##pair?>
                                                                                                                                                ('#<procedure #16 ##cdr>
                                                                                                                                                 args))
                                                                                                                                               ('#<procedure #23 ##cons>
                                                                                                                                                ('#<procedure #14 ##car>
                                                                                                                                                 args)
                                                                                                                                                (conc ('#<procedure #16 ##cdr>
                                                                                                                                                       args)))
                                                                                                                                               ('#<procedure #14 ##car>
                                                                                                                                                args))))
                                                                                                                                        ('#<procedure #14 ##car>
                                                                                                                                         args))))
                                                                                                                                 ('#<procedure #14 ##car>
                                                                                                                                  args))))
                                                                                                                          ('#<procedure #14 ##car>
                                                                                                                           args))))
                                                                                                                   ('#<procedure #14 ##car>
                                                                                                                    args))))
                                                                                                            ('#<procedure #14 ##car>
                                                                                                             args))))
                                                                                                     ('#<procedure #14 ##car>
                                                                                                      args))))
                                                                                              ('#<procedure #14 ##car>
                                                                                               args))))
                                                                                       ('#<procedure #14 ##car>
                                                                                        args))))
                                                                                ('#<procedure #14 ##car>
                                                                                 args))))
                                                                         ('#<procedure #14 ##car>
                                                                          args)))))
                                                      (conc ('#<procedure #16 ##cdr>
                                                             v*)))
                                                    r
                                                    k)
                                            (wrong "Not a function"
                                                   'apply
                                                   ('#<procedure #14 ##car>
                                                    v*)))
                                        (wrong "Incorrect arity" 'apply v*)))))
                     (initialize!
                      ('#<procedure #5 ##subtype-set!>
                       ('#<procedure #6 ##vector>
                        ('#<procedure #4 ##vector-ref> primitive-class 2)
                        'apply
                        address)
                       6))))
            (others r.init))
        (initialize!
         ('#<procedure #5 ##subtype-set!>
          ('#<procedure #6 ##vector>
           ('#<procedure #4 ##vector-ref> variable-env-class 2)
           others
           'apply
           value)
          6))))

'apply

(set! r.init
      (let ((value (let ((address (lambda (v* r k) (resume k v*))))
                     (initialize!
                      ('#<procedure #5 ##subtype-set!>
                       ('#<procedure #6 ##vector>
                        ('#<procedure #4 ##vector-ref> primitive-class 2)
                        'list
                        address)
                       6))))
            (others r.init))
        (initialize!
         ('#<procedure #5 ##subtype-set!>
          ('#<procedure #6 ##vector>
           ('#<procedure #4 ##vector-ref> variable-env-class 2)
           others
           'list
           value)
          6))))

'list

(set! r.init
      (let ((others r.init))
        (initialize!
         ('#<procedure #5 ##subtype-set!>
          ('#<procedure #6 ##vector>
           ('#<procedure #4 ##vector-ref> variable-env-class 2)
           others
           't
           #t)
          6))))

't

(set! r.init
      (let ((others r.init))
        (initialize!
         ('#<procedure #5 ##subtype-set!>
          ('#<procedure #6 ##vector>
           ('#<procedure #4 ##vector-ref> variable-env-class 2)
           others
           'f
           #f)
          6))))

'f

(set! r.init
      (let ((others r.init))
        (initialize!
         ('#<procedure #5 ##subtype-set!>
          ('#<procedure #6 ##vector>
           ('#<procedure #4 ##vector-ref> variable-env-class 2)
           others
           'nil
           '())
          6))))

'nil

(set! r.init
      (let ((others r.init))
        (initialize!
         ('#<procedure #5 ##subtype-set!>
          ('#<procedure #6 ##vector>
           ('#<procedure #4 ##vector-ref> variable-env-class 2)
           others
           'x
           'void)
          6))))

'x

(set! r.init
      (let ((others r.init))
        (initialize!
         ('#<procedure #5 ##subtype-set!>
          ('#<procedure #6 ##vector>
           ('#<procedure #4 ##vector-ref> variable-env-class 2)
           others
           'y
           'void)
          6))))

'y

(set! r.init
      (let ((others r.init))
        (initialize!
         ('#<procedure #5 ##subtype-set!>
          ('#<procedure #6 ##vector>
           ('#<procedure #4 ##vector-ref> variable-env-class 2)
           others
           'z
           'void)
          6))))

'z

(set! r.init
      (let ((others r.init))
        (initialize!
         ('#<procedure #5 ##subtype-set!>
          ('#<procedure #6 ##vector>
           ('#<procedure #4 ##vector-ref> variable-env-class 2)
           others
           'a
           'void)
          6))))

'a

(set! r.init
      (let ((others r.init))
        (initialize!
         ('#<procedure #5 ##subtype-set!>
          ('#<procedure #6 ##vector>
           ('#<procedure #4 ##vector-ref> variable-env-class 2)
           others
           'b
           'void)
          6))))

'b

(set! r.init
      (let ((others r.init))
        (initialize!
         ('#<procedure #5 ##subtype-set!>
          ('#<procedure #6 ##vector>
           ('#<procedure #4 ##vector-ref> variable-env-class 2)
           others
           'c
           'void)
          6))))

'c

(set! r.init
      (let ((others r.init))
        (initialize!
         ('#<procedure #5 ##subtype-set!>
          ('#<procedure #6 ##vector>
           ('#<procedure #4 ##vector-ref> variable-env-class 2)
           others
           'foo
           'void)
          6))))

'foo

(set! r.init
      (let ((others r.init))
        (initialize!
         ('#<procedure #5 ##subtype-set!>
          ('#<procedure #6 ##vector>
           ('#<procedure #4 ##vector-ref> variable-env-class 2)
           others
           'bar
           'void)
          6))))

'bar

(set! r.init
      (let ((others r.init))
        (initialize!
         ('#<procedure #5 ##subtype-set!>
          ('#<procedure #6 ##vector>
           ('#<procedure #4 ##vector-ref> variable-env-class 2)
           others
           'hux
           'void)
          6))))

'hux

(set! r.init
      (let ((others r.init))
        (initialize!
         ('#<procedure #5 ##subtype-set!>
          ('#<procedure #6 ##vector>
           ('#<procedure #4 ##vector-ref> variable-env-class 2)
           others
           'fib
           'void)
          6))))

'fib

(set! r.init
      (let ((others r.init))
        (initialize!
         ('#<procedure #5 ##subtype-set!>
          ('#<procedure #6 ##vector>
           ('#<procedure #4 ##vector-ref> variable-env-class 2)
           others
           'fact
           'void)
          6))))

'fact

(set! r.init
      (let ((others r.init))
        (initialize!
         ('#<procedure #5 ##subtype-set!>
          ('#<procedure #6 ##vector>
           ('#<procedure #4 ##vector-ref> variable-env-class 2)
           others
           'visit
           'void)
          6))))

'visit

(set! r.init
      (let ((others r.init))
        (initialize!
         ('#<procedure #5 ##subtype-set!>
          ('#<procedure #6 ##vector>
           ('#<procedure #4 ##vector-ref> variable-env-class 2)
           others
           'length
           'void)
          6))))

'length

(set! r.init
      (let ((others r.init))
        (initialize!
         ('#<procedure #5 ##subtype-set!>
          ('#<procedure #6 ##vector>
           ('#<procedure #4 ##vector-ref> variable-env-class 2)
           others
           'primes
           'void)
          6))))

'primes

(set! r.init
      (let ((others r.init))
        (initialize!
         ('#<procedure #5 ##subtype-set!>
          ('#<procedure #6 ##vector>
           ('#<procedure #4 ##vector-ref> variable-env-class 2)
           others
           'qsort
           'void)
          6))))

'qsort

(set! r.init
      (let ((others r.init))
        (initialize!
         ('#<procedure #5 ##subtype-set!>
          ('#<procedure #6 ##vector>
           ('#<procedure #4 ##vector-ref> variable-env-class 2)
           others
           'merge
           'void)
          6))))

'merge

(set! r.init
      (let ((others r.init))
        (initialize!
         ('#<procedure #5 ##subtype-set!>
          ('#<procedure #6 ##vector>
           ('#<procedure #4 ##vector-ref> variable-env-class 2)
           others
           'separate
           'void)
          6))))

'separate

(set! r.init
      (let ((others r.init))
        (initialize!
         ('#<procedure #5 ##subtype-set!>
          ('#<procedure #6 ##vector>
           ('#<procedure #4 ##vector-ref> variable-env-class 2)
           others
           'kappend
           'void)
          6))))

'kappend

(set! r.init
      (let ((others r.init))
        (initialize!
         ('#<procedure #5 ##subtype-set!>
          ('#<procedure #6 ##vector>
           ('#<procedure #4 ##vector-ref> variable-env-class 2)
           others
           'wait
           'void)
          6))))

'wait

(set! r.init
      (let ((others r.init))
        (initialize!
         ('#<procedure #5 ##subtype-set!>
          ('#<procedure #6 ##vector>
           ('#<procedure #4 ##vector-ref> variable-env-class 2)
           others
           'queens
           'void)
          6))))

'queens

(set! r.init
      (let ((others r.init))
        (initialize!
         ('#<procedure #5 ##subtype-set!>
          ('#<procedure #6 ##vector>
           ('#<procedure #4 ##vector-ref> variable-env-class 2)
           others
           'foreach
           'void)
          6))))

'foreach

(set! r.init
      (let ((others r.init))
        (initialize!
         ('#<procedure #5 ##subtype-set!>
          ('#<procedure #6 ##vector>
           ('#<procedure #4 ##vector-ref> variable-env-class 2)
           others
           'check
           'void)
          6))))

'check

(set! r.init
      (let ((others r.init))
        (initialize!
         ('#<procedure #5 ##subtype-set!>
          ('#<procedure #6 ##vector>
           ('#<procedure #4 ##vector-ref> variable-env-class 2)
           others
           'check-others
           'void)
          6))))

'check-others

(set! r.init
      (let ((others r.init))
        (initialize!
         ('#<procedure #5 ##subtype-set!>
          ('#<procedure #6 ##vector>
           ('#<procedure #4 ##vector-ref> variable-env-class 2)
           others
           'search-one
           'void)
          6))))

'search-one

(set! r.init
      (let ((others r.init))
        (initialize!
         ('#<procedure #5 ##subtype-set!>
          ('#<procedure #6 ##vector>
           ('#<procedure #4 ##vector-ref> variable-env-class 2)
           others
           'iota
           'void)
          6))))

'iota

(set! r.init
      (let ((others r.init))
        (initialize!
         ('#<procedure #5 ##subtype-set!>
          ('#<procedure #6 ##vector>
           ('#<procedure #4 ##vector-ref> variable-env-class 2)
           others
           'memq
           'void)
          6))))

'memq

(define bench-oo
  (lambda (times e)
    (letrec ((do-temp.102
              (lambda (e i)
                (if ('#<procedure #47 ##fx<=> i 0)
                    #f
                    (let ((begin-temp.104
                           (evaluate
                            e
                            r.init
                            (let ((f (lambda (v)
                                       (let ((begin-temp.103
                                              ('#<procedure #18 display> v)))
                                         ('#<procedure #17 newline>)))))
                              (initialize!
                               ('#<procedure #5 ##subtype-set!>
                                ('#<procedure #6 ##vector>
                                 ('#<procedure #4 ##vector-ref>
                                  bottom-cont-class
                                  2)
                                 'void
                                 f)
                                6))))))
                      (do-temp.102 e ('#<procedure #57 ##fx-> i 1)))))))
      (do-temp.102 e times))))

(define *iterations* 10)

(define *bench*
  '((lambda (fib fact primes)
      (set! fib (lambda (n) (if (<= n 2) 1 (+ (fib (- n 1)) (fib (- n 2))))))
      (set! fact
            (lambda (n)
              ((lambda (factint)
                 (begin
                   (set! factint
                         (lambda (n f) (if (< n 2) 1 (* n (f (- n 1) f)))))
                   (factint n factint)))
               'wait)))
      (set! primes
            (lambda (n f max)
              ((lambda (filter)
                 (begin
                   (set! filter
                         (lambda (p) (lambda (n) (= 0 (remainder n p)))))
                   (if (> n max)
                       '()
                       (if (f n)
                           (primes (+ n 1) f max)
                           (cons n
                                 ((lambda (ff)
                                    (primes (+ n 1)
                                            (lambda (p) (if (f p) t (ff p)))
                                            max))
                                  (filter n)))))))
               'wait)))
      (fib 10)
      (fact 20)
      (primes 2 (lambda (x) f) 40))
    'fib
    'fact
    'primes))

(define start-bench
  (lambda ()
    (let ((begin-temp.108
           ('#<procedure #51 write>
            ('#<procedure #52 ##quasi-list>
             '***
             'Meroon
             'benchmark
             '*iterations*
             '=
             10))))
      (let ((begin-temp.107 ('#<procedure #17 newline>)))
        (let ((begin-temp.106 (show-meroon)))
          (let ((begin-temp.105
                 (bench-oo
                  10
                  '((lambda (fib fact primes)
                      (set! fib
                            (lambda (n)
                              (if (<= n 2) 1 (+ (fib (- n 1)) (fib (- n 2))))))
                      (set! fact
                            (lambda (n)
                              ((lambda (factint)
                                 (begin
                                   (set! factint
                                         (lambda (n f)
                                           (if (< n 2) 1 (* n (f (- n 1) f)))))
                                   (factint n factint)))
                               'wait)))
                      (set! primes
                            (lambda (n f max)
                              ((lambda (filter)
                                 (begin
                                   (set! filter
                                         (lambda (p)
                                           (lambda (n) (= 0 (remainder n p)))))
                                   (if (> n max)
                                       '()
                                       (if (f n)
                                           (primes (+ n 1) f max)
                                           (cons n
                                                 ((lambda (ff)
                                                    (primes (+ n 1)
                                                            (lambda (p)
                                                              (if (f p)
                                                                  t
                                                                  (ff p)))
                                                            max))
                                                  (filter n)))))))
                               'wait)))
                      (fib 10)
                      (fact 20)
                      (primes 2 (lambda (x) f) 40))
                    'fib
                    'fact
                    'primes))))
            'done))))))

