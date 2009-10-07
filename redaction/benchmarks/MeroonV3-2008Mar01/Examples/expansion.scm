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
  (lambda (#:g0)
    (and ('#<procedure #3 ##meroon?> #:g0)
         (careless-subclass?
          ('#<procedure #4 ##vector-ref>
           *classes*
           ('#<procedure #4 ##vector-ref> #:g0 0))
          value-class))))

(define make-value
  (lambda ()
    (initialize!
     ('#<procedure #5 ##subtype-set!>
      ('#<procedure #6 ##vector> ('#<procedure #4 ##vector-ref> value-class 2))
      6))))

(define allocate-value (Class-allocator value-class))

(define ->value
  (let ((#:g1 ('#<procedure #7 ##box> 'wait)))
    (let ((begin-temp.0
           ('#<procedure #8 ##set-box!>
            #:g1
            (register-Generic-1
             '(1 . 1)
             '->value
             meroon-uninitialized
             '((o))
             #f))))
      (lambda (o)
        ((careless-determine-method1 ('#<procedure #9 ##unbox> #:g1) o) o)))))

(register-method
 '->value
 '((o value))
 '(1 . 1)
 (lambda (#:g3 #:g4 #:g5) (lambda (o) o))
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
  (lambda (#:g7)
    (let ((begin-temp.1 (check-class #:g7 function-class 'variables)))
      ('#<procedure #4 ##vector-ref> #:g7 1))))

(define function-variables-set!
  (lambda (#:g8 #:g9)
    (let ((begin-temp.2 (check-class #:g8 function-class 'variables)))
      ('#<procedure #10 ##vector-set!> #:g8 1 #:g9))))

(define function-body
  (lambda (#:g10)
    (let ((begin-temp.3 (check-class #:g10 function-class 'body)))
      ('#<procedure #4 ##vector-ref> #:g10 2))))

(define function-body-set!
  (lambda (#:g11 #:g12)
    (let ((begin-temp.4 (check-class #:g11 function-class 'body)))
      ('#<procedure #10 ##vector-set!> #:g11 2 #:g12))))

(define function-env
  (lambda (#:g13)
    (let ((begin-temp.5 (check-class #:g13 function-class 'env)))
      ('#<procedure #4 ##vector-ref> #:g13 3))))

(define function-env-set!
  (lambda (#:g14 #:g15)
    (let ((begin-temp.6 (check-class #:g14 function-class 'env)))
      ('#<procedure #10 ##vector-set!> #:g14 3 #:g15))))

(define function?
  (lambda (#:g16)
    (and ('#<procedure #3 ##meroon?> #:g16)
         (careless-subclass?
          ('#<procedure #4 ##vector-ref>
           *classes*
           ('#<procedure #4 ##vector-ref> #:g16 0))
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
  (let ((#:g17 ('#<procedure #7 ##box> 'wait)))
    (let ((begin-temp.7
           ('#<procedure #8 ##set-box!>
            #:g17
            (register-Generic-1
             '(1 . 1)
             '->function
             meroon-uninitialized
             '((o))
             #f))))
      (lambda (o)
        ((careless-determine-method1 ('#<procedure #9 ##unbox> #:g17) o) o)))))

(register-method
 '->function
 '((o function))
 '(1 . 1)
 (lambda (#:g19 #:g20 #:g21) (lambda (o) o))
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
  (lambda (#:g23)
    (let ((begin-temp.8 (check-class #:g23 primitive-class 'name)))
      ('#<procedure #4 ##vector-ref> #:g23 1))))

(define primitive-name-set!
  (lambda (#:g24 #:g25)
    (let ((begin-temp.9 (check-class #:g24 primitive-class 'name)))
      ('#<procedure #10 ##vector-set!> #:g24 1 #:g25))))

(define primitive-address
  (lambda (#:g26)
    (let ((begin-temp.10 (check-class #:g26 primitive-class 'address)))
      ('#<procedure #4 ##vector-ref> #:g26 2))))

(define primitive-address-set!
  (lambda (#:g27 #:g28)
    (let ((begin-temp.11 (check-class #:g27 primitive-class 'address)))
      ('#<procedure #10 ##vector-set!> #:g27 2 #:g28))))

(define primitive?
  (lambda (#:g29)
    (and ('#<procedure #3 ##meroon?> #:g29)
         (careless-subclass?
          ('#<procedure #4 ##vector-ref>
           *classes*
           ('#<procedure #4 ##vector-ref> #:g29 0))
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
  (let ((#:g30 ('#<procedure #7 ##box> 'wait)))
    (let ((begin-temp.12
           ('#<procedure #8 ##set-box!>
            #:g30
            (register-Generic-1
             '(1 . 1)
             '->primitive
             meroon-uninitialized
             '((o))
             #f))))
      (lambda (o)
        ((careless-determine-method1 ('#<procedure #9 ##unbox> #:g30) o) o)))))

(register-method
 '->primitive
 '((o primitive))
 '(1 . 1)
 (lambda (#:g32 #:g33 #:g34) (lambda (o) o))
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
  (lambda (#:g36)
    (and ('#<procedure #3 ##meroon?> #:g36)
         (careless-subclass?
          ('#<procedure #4 ##vector-ref>
           *classes*
           ('#<procedure #4 ##vector-ref> #:g36 0))
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
  (let ((#:g37 ('#<procedure #7 ##box> 'wait)))
    (let ((begin-temp.13
           ('#<procedure #8 ##set-box!>
            #:g37
            (register-Generic-1
             '(1 . 1)
             '->environ
             meroon-uninitialized
             '((o))
             #f))))
      (lambda (o)
        ((careless-determine-method1 ('#<procedure #9 ##unbox> #:g37) o) o)))))

(register-method
 '->environ
 '((o environ))
 '(1 . 1)
 (lambda (#:g39 #:g40 #:g41) (lambda (o) o))
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
  (lambda (#:g43)
    (and ('#<procedure #3 ##meroon?> #:g43)
         (careless-subclass?
          ('#<procedure #4 ##vector-ref>
           *classes*
           ('#<procedure #4 ##vector-ref> #:g43 0))
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
  (let ((#:g44 ('#<procedure #7 ##box> 'wait)))
    (let ((begin-temp.14
           ('#<procedure #8 ##set-box!>
            #:g44
            (register-Generic-1
             '(1 . 1)
             '->null-env
             meroon-uninitialized
             '((o))
             #f))))
      (lambda (o)
        ((careless-determine-method1 ('#<procedure #9 ##unbox> #:g44) o) o)))))

(register-method
 '->null-env
 '((o null-env))
 '(1 . 1)
 (lambda (#:g46 #:g47 #:g48) (lambda (o) o))
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
  (lambda (#:g50)
    (let ((begin-temp.15 (check-class #:g50 full-env-class 'others)))
      ('#<procedure #4 ##vector-ref> #:g50 1))))

(define full-env-others-set!
  (lambda (#:g51 #:g52)
    (let ((begin-temp.16 (check-class #:g51 full-env-class 'others)))
      ('#<procedure #10 ##vector-set!> #:g51 1 #:g52))))

(define full-env-name
  (lambda (#:g53)
    (let ((begin-temp.17 (check-class #:g53 full-env-class 'name)))
      ('#<procedure #4 ##vector-ref> #:g53 2))))

(define full-env-name-set!
  (lambda (#:g54 #:g55)
    (let ((begin-temp.18 (check-class #:g54 full-env-class 'name)))
      ('#<procedure #10 ##vector-set!> #:g54 2 #:g55))))

(define full-env?
  (lambda (#:g56)
    (and ('#<procedure #3 ##meroon?> #:g56)
         (careless-subclass?
          ('#<procedure #4 ##vector-ref>
           *classes*
           ('#<procedure #4 ##vector-ref> #:g56 0))
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
  (let ((#:g57 ('#<procedure #7 ##box> 'wait)))
    (let ((begin-temp.19
           ('#<procedure #8 ##set-box!>
            #:g57
            (register-Generic-1
             '(1 . 1)
             '->full-env
             meroon-uninitialized
             '((o))
             #f))))
      (lambda (o)
        ((careless-determine-method1 ('#<procedure #9 ##unbox> #:g57) o) o)))))

(register-method
 '->full-env
 '((o full-env))
 '(1 . 1)
 (lambda (#:g59 #:g60 #:g61) (lambda (o) o))
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
  (lambda (#:g63)
    (let ((begin-temp.20 (check-class #:g63 variable-env-class 'others)))
      ('#<procedure #4 ##vector-ref> #:g63 1))))

(define variable-env-others-set!
  (lambda (#:g64 #:g65)
    (let ((begin-temp.21 (check-class #:g64 variable-env-class 'others)))
      ('#<procedure #10 ##vector-set!> #:g64 1 #:g65))))

(define variable-env-name
  (lambda (#:g66)
    (let ((begin-temp.22 (check-class #:g66 variable-env-class 'name)))
      ('#<procedure #4 ##vector-ref> #:g66 2))))

(define variable-env-name-set!
  (lambda (#:g67 #:g68)
    (let ((begin-temp.23 (check-class #:g67 variable-env-class 'name)))
      ('#<procedure #10 ##vector-set!> #:g67 2 #:g68))))

(define variable-env-value
  (lambda (#:g69)
    (let ((begin-temp.24 (check-class #:g69 variable-env-class 'value)))
      ('#<procedure #4 ##vector-ref> #:g69 3))))

(define variable-env-value-set!
  (lambda (#:g70 #:g71)
    (let ((begin-temp.25 (check-class #:g70 variable-env-class 'value)))
      ('#<procedure #10 ##vector-set!> #:g70 3 #:g71))))

(define variable-env?
  (lambda (#:g72)
    (and ('#<procedure #3 ##meroon?> #:g72)
         (careless-subclass?
          ('#<procedure #4 ##vector-ref>
           *classes*
           ('#<procedure #4 ##vector-ref> #:g72 0))
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
  (let ((#:g73 ('#<procedure #7 ##box> 'wait)))
    (let ((begin-temp.26
           ('#<procedure #8 ##set-box!>
            #:g73
            (register-Generic-1
             '(1 . 1)
             '->variable-env
             meroon-uninitialized
             '((o))
             #f))))
      (lambda (o)
        ((careless-determine-method1 ('#<procedure #9 ##unbox> #:g73) o) o)))))

(register-method
 '->variable-env
 '((o variable-env))
 '(1 . 1)
 (lambda (#:g75 #:g76 #:g77) (lambda (o) o))
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
  (lambda (#:g79)
    (let ((begin-temp.27 (check-class #:g79 continuation-class 'k)))
      ('#<procedure #4 ##vector-ref> #:g79 1))))

(define continuation-k-set!
  (lambda (#:g80 #:g81)
    (let ((begin-temp.28 (check-class #:g80 continuation-class 'k)))
      ('#<procedure #10 ##vector-set!> #:g80 1 #:g81))))

(define continuation?
  (lambda (#:g82)
    (and ('#<procedure #3 ##meroon?> #:g82)
         (careless-subclass?
          ('#<procedure #4 ##vector-ref>
           *classes*
           ('#<procedure #4 ##vector-ref> #:g82 0))
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
  (let ((#:g83 ('#<procedure #7 ##box> 'wait)))
    (let ((begin-temp.29
           ('#<procedure #8 ##set-box!>
            #:g83
            (register-Generic-1
             '(1 . 1)
             '->continuation
             meroon-uninitialized
             '((o))
             #f))))
      (lambda (o)
        ((careless-determine-method1 ('#<procedure #9 ##unbox> #:g83) o) o)))))

(register-method
 '->continuation
 '((o continuation))
 '(1 . 1)
 (lambda (#:g85 #:g86 #:g87) (lambda (o) o))
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
  (lambda (#:g89)
    (let ((begin-temp.30 (check-class #:g89 if-cont-class 'k)))
      ('#<procedure #4 ##vector-ref> #:g89 1))))

(define if-cont-k-set!
  (lambda (#:g90 #:g91)
    (let ((begin-temp.31 (check-class #:g90 if-cont-class 'k)))
      ('#<procedure #10 ##vector-set!> #:g90 1 #:g91))))

(define if-cont-et
  (lambda (#:g92)
    (let ((begin-temp.32 (check-class #:g92 if-cont-class 'et)))
      ('#<procedure #4 ##vector-ref> #:g92 2))))

(define if-cont-et-set!
  (lambda (#:g93 #:g94)
    (let ((begin-temp.33 (check-class #:g93 if-cont-class 'et)))
      ('#<procedure #10 ##vector-set!> #:g93 2 #:g94))))

(define if-cont-ef
  (lambda (#:g95)
    (let ((begin-temp.34 (check-class #:g95 if-cont-class 'ef)))
      ('#<procedure #4 ##vector-ref> #:g95 3))))

(define if-cont-ef-set!
  (lambda (#:g96 #:g97)
    (let ((begin-temp.35 (check-class #:g96 if-cont-class 'ef)))
      ('#<procedure #10 ##vector-set!> #:g96 3 #:g97))))

(define if-cont-r
  (lambda (#:g98)
    (let ((begin-temp.36 (check-class #:g98 if-cont-class 'r)))
      ('#<procedure #4 ##vector-ref> #:g98 4))))

(define if-cont-r-set!
  (lambda (#:g99 #:g100)
    (let ((begin-temp.37 (check-class #:g99 if-cont-class 'r)))
      ('#<procedure #10 ##vector-set!> #:g99 4 #:g100))))

(define if-cont?
  (lambda (#:g101)
    (and ('#<procedure #3 ##meroon?> #:g101)
         (careless-subclass?
          ('#<procedure #4 ##vector-ref>
           *classes*
           ('#<procedure #4 ##vector-ref> #:g101 0))
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
  (let ((#:g102 ('#<procedure #7 ##box> 'wait)))
    (let ((begin-temp.38
           ('#<procedure #8 ##set-box!>
            #:g102
            (register-Generic-1
             '(1 . 1)
             '->if-cont
             meroon-uninitialized
             '((o))
             #f))))
      (lambda (o)
        ((careless-determine-method1 ('#<procedure #9 ##unbox> #:g102) o)
         o)))))

(register-method
 '->if-cont
 '((o if-cont))
 '(1 . 1)
 (lambda (#:g104 #:g105 #:g106) (lambda (o) o))
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
  (lambda (#:g108)
    (let ((begin-temp.39 (check-class #:g108 set!-cont-class 'k)))
      ('#<procedure #4 ##vector-ref> #:g108 1))))

(define set!-cont-k-set!
  (lambda (#:g109 #:g110)
    (let ((begin-temp.40 (check-class #:g109 set!-cont-class 'k)))
      ('#<procedure #10 ##vector-set!> #:g109 1 #:g110))))

(define set!-cont-n
  (lambda (#:g111)
    (let ((begin-temp.41 (check-class #:g111 set!-cont-class 'n)))
      ('#<procedure #4 ##vector-ref> #:g111 2))))

(define set!-cont-n-set!
  (lambda (#:g112 #:g113)
    (let ((begin-temp.42 (check-class #:g112 set!-cont-class 'n)))
      ('#<procedure #10 ##vector-set!> #:g112 2 #:g113))))

(define set!-cont-r
  (lambda (#:g114)
    (let ((begin-temp.43 (check-class #:g114 set!-cont-class 'r)))
      ('#<procedure #4 ##vector-ref> #:g114 3))))

(define set!-cont-r-set!
  (lambda (#:g115 #:g116)
    (let ((begin-temp.44 (check-class #:g115 set!-cont-class 'r)))
      ('#<procedure #10 ##vector-set!> #:g115 3 #:g116))))

(define set!-cont?
  (lambda (#:g117)
    (and ('#<procedure #3 ##meroon?> #:g117)
         (careless-subclass?
          ('#<procedure #4 ##vector-ref>
           *classes*
           ('#<procedure #4 ##vector-ref> #:g117 0))
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
  (let ((#:g118 ('#<procedure #7 ##box> 'wait)))
    (let ((begin-temp.45
           ('#<procedure #8 ##set-box!>
            #:g118
            (register-Generic-1
             '(1 . 1)
             '->set!-cont
             meroon-uninitialized
             '((o))
             #f))))
      (lambda (o)
        ((careless-determine-method1 ('#<procedure #9 ##unbox> #:g118) o)
         o)))))

(register-method
 '->set!-cont
 '((o set!-cont))
 '(1 . 1)
 (lambda (#:g120 #:g121 #:g122) (lambda (o) o))
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
  (lambda (#:g124)
    (let ((begin-temp.46 (check-class #:g124 begin-cont-class 'k)))
      ('#<procedure #4 ##vector-ref> #:g124 1))))

(define begin-cont-k-set!
  (lambda (#:g125 #:g126)
    (let ((begin-temp.47 (check-class #:g125 begin-cont-class 'k)))
      ('#<procedure #10 ##vector-set!> #:g125 1 #:g126))))

(define begin-cont-e*
  (lambda (#:g127)
    (let ((begin-temp.48 (check-class #:g127 begin-cont-class 'e*)))
      ('#<procedure #4 ##vector-ref> #:g127 2))))

(define begin-cont-e*-set!
  (lambda (#:g128 #:g129)
    (let ((begin-temp.49 (check-class #:g128 begin-cont-class 'e*)))
      ('#<procedure #10 ##vector-set!> #:g128 2 #:g129))))

(define begin-cont-r
  (lambda (#:g130)
    (let ((begin-temp.50 (check-class #:g130 begin-cont-class 'r)))
      ('#<procedure #4 ##vector-ref> #:g130 3))))

(define begin-cont-r-set!
  (lambda (#:g131 #:g132)
    (let ((begin-temp.51 (check-class #:g131 begin-cont-class 'r)))
      ('#<procedure #10 ##vector-set!> #:g131 3 #:g132))))

(define begin-cont?
  (lambda (#:g133)
    (and ('#<procedure #3 ##meroon?> #:g133)
         (careless-subclass?
          ('#<procedure #4 ##vector-ref>
           *classes*
           ('#<procedure #4 ##vector-ref> #:g133 0))
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
  (let ((#:g134 ('#<procedure #7 ##box> 'wait)))
    (let ((begin-temp.52
           ('#<procedure #8 ##set-box!>
            #:g134
            (register-Generic-1
             '(1 . 1)
             '->begin-cont
             meroon-uninitialized
             '((o))
             #f))))
      (lambda (o)
        ((careless-determine-method1 ('#<procedure #9 ##unbox> #:g134) o)
         o)))))

(register-method
 '->begin-cont
 '((o begin-cont))
 '(1 . 1)
 (lambda (#:g136 #:g137 #:g138) (lambda (o) o))
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
  (lambda (#:g140)
    (let ((begin-temp.53 (check-class #:g140 evfun-cont-class 'k)))
      ('#<procedure #4 ##vector-ref> #:g140 1))))

(define evfun-cont-k-set!
  (lambda (#:g141 #:g142)
    (let ((begin-temp.54 (check-class #:g141 evfun-cont-class 'k)))
      ('#<procedure #10 ##vector-set!> #:g141 1 #:g142))))

(define evfun-cont-e*
  (lambda (#:g143)
    (let ((begin-temp.55 (check-class #:g143 evfun-cont-class 'e*)))
      ('#<procedure #4 ##vector-ref> #:g143 2))))

(define evfun-cont-e*-set!
  (lambda (#:g144 #:g145)
    (let ((begin-temp.56 (check-class #:g144 evfun-cont-class 'e*)))
      ('#<procedure #10 ##vector-set!> #:g144 2 #:g145))))

(define evfun-cont-r
  (lambda (#:g146)
    (let ((begin-temp.57 (check-class #:g146 evfun-cont-class 'r)))
      ('#<procedure #4 ##vector-ref> #:g146 3))))

(define evfun-cont-r-set!
  (lambda (#:g147 #:g148)
    (let ((begin-temp.58 (check-class #:g147 evfun-cont-class 'r)))
      ('#<procedure #10 ##vector-set!> #:g147 3 #:g148))))

(define evfun-cont?
  (lambda (#:g149)
    (and ('#<procedure #3 ##meroon?> #:g149)
         (careless-subclass?
          ('#<procedure #4 ##vector-ref>
           *classes*
           ('#<procedure #4 ##vector-ref> #:g149 0))
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
  (let ((#:g150 ('#<procedure #7 ##box> 'wait)))
    (let ((begin-temp.59
           ('#<procedure #8 ##set-box!>
            #:g150
            (register-Generic-1
             '(1 . 1)
             '->evfun-cont
             meroon-uninitialized
             '((o))
             #f))))
      (lambda (o)
        ((careless-determine-method1 ('#<procedure #9 ##unbox> #:g150) o)
         o)))))

(register-method
 '->evfun-cont
 '((o evfun-cont))
 '(1 . 1)
 (lambda (#:g152 #:g153 #:g154) (lambda (o) o))
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
  (lambda (#:g156)
    (let ((begin-temp.60 (check-class #:g156 apply-cont-class 'k)))
      ('#<procedure #4 ##vector-ref> #:g156 1))))

(define apply-cont-k-set!
  (lambda (#:g157 #:g158)
    (let ((begin-temp.61 (check-class #:g157 apply-cont-class 'k)))
      ('#<procedure #10 ##vector-set!> #:g157 1 #:g158))))

(define apply-cont-f
  (lambda (#:g159)
    (let ((begin-temp.62 (check-class #:g159 apply-cont-class 'f)))
      ('#<procedure #4 ##vector-ref> #:g159 2))))

(define apply-cont-f-set!
  (lambda (#:g160 #:g161)
    (let ((begin-temp.63 (check-class #:g160 apply-cont-class 'f)))
      ('#<procedure #10 ##vector-set!> #:g160 2 #:g161))))

(define apply-cont-r
  (lambda (#:g162)
    (let ((begin-temp.64 (check-class #:g162 apply-cont-class 'r)))
      ('#<procedure #4 ##vector-ref> #:g162 3))))

(define apply-cont-r-set!
  (lambda (#:g163 #:g164)
    (let ((begin-temp.65 (check-class #:g163 apply-cont-class 'r)))
      ('#<procedure #10 ##vector-set!> #:g163 3 #:g164))))

(define apply-cont?
  (lambda (#:g165)
    (and ('#<procedure #3 ##meroon?> #:g165)
         (careless-subclass?
          ('#<procedure #4 ##vector-ref>
           *classes*
           ('#<procedure #4 ##vector-ref> #:g165 0))
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
  (let ((#:g166 ('#<procedure #7 ##box> 'wait)))
    (let ((begin-temp.66
           ('#<procedure #8 ##set-box!>
            #:g166
            (register-Generic-1
             '(1 . 1)
             '->apply-cont
             meroon-uninitialized
             '((o))
             #f))))
      (lambda (o)
        ((careless-determine-method1 ('#<procedure #9 ##unbox> #:g166) o)
         o)))))

(register-method
 '->apply-cont
 '((o apply-cont))
 '(1 . 1)
 (lambda (#:g168 #:g169 #:g170) (lambda (o) o))
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
  (lambda (#:g172)
    (let ((begin-temp.67 (check-class #:g172 argument-cont-class 'k)))
      ('#<procedure #4 ##vector-ref> #:g172 1))))

(define argument-cont-k-set!
  (lambda (#:g173 #:g174)
    (let ((begin-temp.68 (check-class #:g173 argument-cont-class 'k)))
      ('#<procedure #10 ##vector-set!> #:g173 1 #:g174))))

(define argument-cont-e*
  (lambda (#:g175)
    (let ((begin-temp.69 (check-class #:g175 argument-cont-class 'e*)))
      ('#<procedure #4 ##vector-ref> #:g175 2))))

(define argument-cont-e*-set!
  (lambda (#:g176 #:g177)
    (let ((begin-temp.70 (check-class #:g176 argument-cont-class 'e*)))
      ('#<procedure #10 ##vector-set!> #:g176 2 #:g177))))

(define argument-cont-r
  (lambda (#:g178)
    (let ((begin-temp.71 (check-class #:g178 argument-cont-class 'r)))
      ('#<procedure #4 ##vector-ref> #:g178 3))))

(define argument-cont-r-set!
  (lambda (#:g179 #:g180)
    (let ((begin-temp.72 (check-class #:g179 argument-cont-class 'r)))
      ('#<procedure #10 ##vector-set!> #:g179 3 #:g180))))

(define argument-cont?
  (lambda (#:g181)
    (and ('#<procedure #3 ##meroon?> #:g181)
         (careless-subclass?
          ('#<procedure #4 ##vector-ref>
           *classes*
           ('#<procedure #4 ##vector-ref> #:g181 0))
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
  (let ((#:g182 ('#<procedure #7 ##box> 'wait)))
    (let ((begin-temp.73
           ('#<procedure #8 ##set-box!>
            #:g182
            (register-Generic-1
             '(1 . 1)
             '->argument-cont
             meroon-uninitialized
             '((o))
             #f))))
      (lambda (o)
        ((careless-determine-method1 ('#<procedure #9 ##unbox> #:g182) o)
         o)))))

(register-method
 '->argument-cont
 '((o argument-cont))
 '(1 . 1)
 (lambda (#:g184 #:g185 #:g186) (lambda (o) o))
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
  (lambda (#:g188)
    (let ((begin-temp.74 (check-class #:g188 gather-cont-class 'k)))
      ('#<procedure #4 ##vector-ref> #:g188 1))))

(define gather-cont-k-set!
  (lambda (#:g189 #:g190)
    (let ((begin-temp.75 (check-class #:g189 gather-cont-class 'k)))
      ('#<procedure #10 ##vector-set!> #:g189 1 #:g190))))

(define gather-cont-v
  (lambda (#:g191)
    (let ((begin-temp.76 (check-class #:g191 gather-cont-class 'v)))
      ('#<procedure #4 ##vector-ref> #:g191 2))))

(define gather-cont-v-set!
  (lambda (#:g192 #:g193)
    (let ((begin-temp.77 (check-class #:g192 gather-cont-class 'v)))
      ('#<procedure #10 ##vector-set!> #:g192 2 #:g193))))

(define gather-cont?
  (lambda (#:g194)
    (and ('#<procedure #3 ##meroon?> #:g194)
         (careless-subclass?
          ('#<procedure #4 ##vector-ref>
           *classes*
           ('#<procedure #4 ##vector-ref> #:g194 0))
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
  (let ((#:g195 ('#<procedure #7 ##box> 'wait)))
    (let ((begin-temp.78
           ('#<procedure #8 ##set-box!>
            #:g195
            (register-Generic-1
             '(1 . 1)
             '->gather-cont
             meroon-uninitialized
             '((o))
             #f))))
      (lambda (o)
        ((careless-determine-method1 ('#<procedure #9 ##unbox> #:g195) o)
         o)))))

(register-method
 '->gather-cont
 '((o gather-cont))
 '(1 . 1)
 (lambda (#:g197 #:g198 #:g199) (lambda (o) o))
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
  (lambda (#:g201)
    (let ((begin-temp.79 (check-class #:g201 bottom-cont-class 'k)))
      ('#<procedure #4 ##vector-ref> #:g201 1))))

(define bottom-cont-k-set!
  (lambda (#:g202 #:g203)
    (let ((begin-temp.80 (check-class #:g202 bottom-cont-class 'k)))
      ('#<procedure #10 ##vector-set!> #:g202 1 #:g203))))

(define bottom-cont-f
  (lambda (#:g204)
    (let ((begin-temp.81 (check-class #:g204 bottom-cont-class 'f)))
      ('#<procedure #4 ##vector-ref> #:g204 2))))

(define bottom-cont-f-set!
  (lambda (#:g205 #:g206)
    (let ((begin-temp.82 (check-class #:g205 bottom-cont-class 'f)))
      ('#<procedure #10 ##vector-set!> #:g205 2 #:g206))))

(define bottom-cont?
  (lambda (#:g207)
    (and ('#<procedure #3 ##meroon?> #:g207)
         (careless-subclass?
          ('#<procedure #4 ##vector-ref>
           *classes*
           ('#<procedure #4 ##vector-ref> #:g207 0))
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
  (let ((#:g208 ('#<procedure #7 ##box> 'wait)))
    (let ((begin-temp.83
           ('#<procedure #8 ##set-box!>
            #:g208
            (register-Generic-1
             '(1 . 1)
             '->bottom-cont
             meroon-uninitialized
             '((o))
             #f))))
      (lambda (o)
        ((careless-determine-method1 ('#<procedure #9 ##unbox> #:g208) o)
         o)))))

(register-method
 '->bottom-cont
 '((o bottom-cont))
 '(1 . 1)
 (lambda (#:g210 #:g211 #:g212) (lambda (o) o))
 'bottom-cont)

'bottom-cont

(define evaluate
  (lambda (e r k)
    (if ('#<procedure #11 ##not> ('#<procedure #12 ##pair?> e))
        (if ('#<procedure #13 ##symbol?> e) (lookup r e k) (resume k e))
        (let ((case-temp.84
               (if ('#<procedure #12 ##pair?> e)
                   ('#<procedure #14 ##car> e)
                   (car e))))
          (if ('#<procedure #15 ##eq?> case-temp.84 'quote)
              (let ((v (let ((temp.111
                              (if ('#<procedure #12 ##pair?> e)
                                  ('#<procedure #16 ##cdr> e)
                                  #f)))
                         (if ('#<procedure #12 ##pair?> temp.111)
                             ('#<procedure #14 ##car> temp.111)
                             (cadr e)))))
                (resume k v))
              (if ('#<procedure #15 ##eq?> case-temp.84 'if)
                  (let ((ef (let ((temp.118
                                   (let ((temp.119
                                          (let ((temp.120
                                                 (if ('#<procedure #12 ##pair?>
                                                      e)
                                                     ('#<procedure #16 ##cdr>
                                                      e)
                                                     #f)))
                                            (if ('#<procedure #12 ##pair?>
                                                 temp.120)
                                                ('#<procedure #16 ##cdr>
                                                 temp.120)
                                                #f))))
                                     (if ('#<procedure #12 ##pair?> temp.119)
                                         ('#<procedure #16 ##cdr> temp.119)
                                         #f))))
                              (if ('#<procedure #12 ##pair?> temp.118)
                                  ('#<procedure #14 ##car> temp.118)
                                  (cadddr e))))
                        (et (let ((temp.115
                                   (let ((temp.116
                                          (if ('#<procedure #12 ##pair?> e)
                                              ('#<procedure #16 ##cdr> e)
                                              #f)))
                                     (if ('#<procedure #12 ##pair?> temp.116)
                                         ('#<procedure #16 ##cdr> temp.116)
                                         #f))))
                              (if ('#<procedure #12 ##pair?> temp.115)
                                  ('#<procedure #14 ##car> temp.115)
                                  (caddr e))))
                        (ec (let ((temp.113
                                   (if ('#<procedure #12 ##pair?> e)
                                       ('#<procedure #16 ##cdr> e)
                                       #f)))
                              (if ('#<procedure #12 ##pair?> temp.113)
                                  ('#<procedure #14 ##car> temp.113)
                                  (cadr e)))))
                    (let ((k (make-if-cont k et ef r)))
                      (if ('#<procedure #11 ##not>
                           ('#<procedure #12 ##pair?> ec))
                          (if ('#<procedure #13 ##symbol?> ec)
                              (evaluate-variable ec r k)
                              (evaluate-quote ec r k))
                          (let ((case-temp.84
                                 (if ('#<procedure #12 ##pair?> ec)
                                     ('#<procedure #14 ##car> ec)
                                     (car ec))))
                            (if ('#<procedure #15 ##eq?> case-temp.84 'quote)
                                (evaluate-quote
                                 (let ((temp.111
                                        (if ('#<procedure #12 ##pair?> ec)
                                            ('#<procedure #16 ##cdr> ec)
                                            #f)))
                                   (if ('#<procedure #12 ##pair?> temp.111)
                                       ('#<procedure #14 ##car> temp.111)
                                       (cadr ec)))
                                 r
                                 k)
                                (if ('#<procedure #15 ##eq?> case-temp.84 'if)
                                    (evaluate-if
                                     (let ((temp.113
                                            (if ('#<procedure #12 ##pair?> ec)
                                                ('#<procedure #16 ##cdr> ec)
                                                #f)))
                                       (if ('#<procedure #12 ##pair?> temp.113)
                                           ('#<procedure #14 ##car> temp.113)
                                           (cadr ec)))
                                     (let ((temp.115
                                            (let ((temp.116
                                                   (if ('#<procedure #12 ##pair?>
                                                        ec)
                                                       ('#<procedure #16 ##cdr>
                                                        ec)
                                                       #f)))
                                              (if ('#<procedure #12 ##pair?>
                                                   temp.116)
                                                  ('#<procedure #16 ##cdr>
                                                   temp.116)
                                                  #f))))
                                       (if ('#<procedure #12 ##pair?> temp.115)
                                           ('#<procedure #14 ##car> temp.115)
                                           (caddr ec)))
                                     (let ((temp.118
                                            (let ((temp.119
                                                   (let ((temp.120
                                                          (if ('#<procedure #12 ##pair?>
                                                               ec)
                                                              ('#<procedure #16 ##cdr>
                                                               ec)
                                                              #f)))
                                                     (if ('#<procedure #12 ##pair?>
                                                          temp.120)
                                                         ('#<procedure #16 ##cdr>
                                                          temp.120)
                                                         #f))))
                                              (if ('#<procedure #12 ##pair?>
                                                   temp.119)
                                                  ('#<procedure #16 ##cdr>
                                                   temp.119)
                                                  #f))))
                                       (if ('#<procedure #12 ##pair?> temp.118)
                                           ('#<procedure #14 ##car> temp.118)
                                           (cadddr ec)))
                                     r
                                     k)
                                    (if ('#<procedure #15 ##eq?>
                                         case-temp.84
                                         'begin)
                                        (evaluate-begin
                                         (if ('#<procedure #12 ##pair?> ec)
                                             ('#<procedure #16 ##cdr> ec)
                                             (cdr ec))
                                         r
                                         k)
                                        (if ('#<procedure #15 ##eq?>
                                             case-temp.84
                                             'set!)
                                            (evaluate-set!
                                             (let ((temp.123
                                                    (if ('#<procedure #12 ##pair?>
                                                         ec)
                                                        ('#<procedure #16 ##cdr>
                                                         ec)
                                                        #f)))
                                               (if ('#<procedure #12 ##pair?>
                                                    temp.123)
                                                   ('#<procedure #14 ##car>
                                                    temp.123)
                                                   (cadr ec)))
                                             (let ((temp.125
                                                    (let ((temp.126
                                                           (if ('#<procedure #12 ##pair?>
                                                                ec)
                                                               ('#<procedure #16 ##cdr>
                                                                ec)
                                                               #f)))
                                                      (if ('#<procedure #12 ##pair?>
                                                           temp.126)
                                                          ('#<procedure #16 ##cdr>
                                                           temp.126)
                                                          #f))))
                                               (if ('#<procedure #12 ##pair?>
                                                    temp.125)
                                                   ('#<procedure #14 ##car>
                                                    temp.125)
                                                   (caddr ec)))
                                             r
                                             k)
                                            (if ('#<procedure #15 ##eq?>
                                                 case-temp.84
                                                 'lambda)
                                                (evaluate-lambda
                                                 (let ((temp.128
                                                        (if ('#<procedure #12 ##pair?>
                                                             ec)
                                                            ('#<procedure #16 ##cdr>
                                                             ec)
                                                            #f)))
                                                   (if ('#<procedure #12 ##pair?>
                                                        temp.128)
                                                       ('#<procedure #14 ##car>
                                                        temp.128)
                                                       (cadr ec)))
                                                 (let ((temp.130
                                                        (if ('#<procedure #12 ##pair?>
                                                             ec)
                                                            ('#<procedure #16 ##cdr>
                                                             ec)
                                                            #f)))
                                                   (if ('#<procedure #12 ##pair?>
                                                        temp.130)
                                                       ('#<procedure #16 ##cdr>
                                                        temp.130)
                                                       (cddr ec)))
                                                 r
                                                 k)
                                                (evaluate-application
                                                 (if ('#<procedure #12 ##pair?>
                                                      ec)
                                                     ('#<procedure #14 ##car>
                                                      ec)
                                                     (car ec))
                                                 (if ('#<procedure #12 ##pair?>
                                                      ec)
                                                     ('#<procedure #16 ##cdr>
                                                      ec)
                                                     (cdr ec))
                                                 r
                                                 k))))))))))
                  (if ('#<procedure #15 ##eq?> case-temp.84 'begin)
                      (evaluate-begin
                       (if ('#<procedure #12 ##pair?> e)
                           ('#<procedure #16 ##cdr> e)
                           (cdr e))
                       r
                       k)
                      (if ('#<procedure #15 ##eq?> case-temp.84 'set!)
                          (let ((e (let ((temp.125
                                          (let ((temp.126
                                                 (if ('#<procedure #12 ##pair?>
                                                      e)
                                                     ('#<procedure #16 ##cdr>
                                                      e)
                                                     #f)))
                                            (if ('#<procedure #12 ##pair?>
                                                 temp.126)
                                                ('#<procedure #16 ##cdr>
                                                 temp.126)
                                                #f))))
                                     (if ('#<procedure #12 ##pair?> temp.125)
                                         ('#<procedure #14 ##car> temp.125)
                                         (caddr e))))
                                (n (let ((temp.123
                                          (if ('#<procedure #12 ##pair?> e)
                                              ('#<procedure #16 ##cdr> e)
                                              #f)))
                                     (if ('#<procedure #12 ##pair?> temp.123)
                                         ('#<procedure #14 ##car> temp.123)
                                         (cadr e)))))
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
                              (let ((e* (let ((temp.130
                                               (if ('#<procedure #12 ##pair?>
                                                    e)
                                                   ('#<procedure #16 ##cdr> e)
                                                   #f)))
                                          (if ('#<procedure #12 ##pair?>
                                               temp.130)
                                              ('#<procedure #16 ##cdr>
                                               temp.130)
                                              (cddr e))))
                                    (n* (let ((temp.128
                                               (if ('#<procedure #12 ##pair?>
                                                    e)
                                                   ('#<procedure #16 ##cdr> e)
                                                   #f)))
                                          (if ('#<procedure #12 ##pair?>
                                               temp.128)
                                              ('#<procedure #14 ##car>
                                               temp.128)
                                              (cadr e)))))
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
                              (let ((e* (if ('#<procedure #12 ##pair?> e)
                                            ('#<procedure #16 ##cdr> e)
                                            (cdr e)))
                                    (e (if ('#<procedure #12 ##pair?> e)
                                           ('#<procedure #14 ##car> e)
                                           (car e))))
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
        (if ('#<procedure #12 ##pair?>
             (if ('#<procedure #12 ##pair?> e*)
                 ('#<procedure #16 ##cdr> e*)
                 (cdr e*)))
            (evaluate
             (if ('#<procedure #12 ##pair?> e*)
                 ('#<procedure #14 ##car> e*)
                 (car e*))
             r
             (initialize!
              ('#<procedure #5 ##subtype-set!>
               ('#<procedure #6 ##vector>
                ('#<procedure #4 ##vector-ref> begin-cont-class 2)
                k
                e*
                r)
               6)))
            (evaluate
             (if ('#<procedure #12 ##pair?> e*)
                 ('#<procedure #14 ##car> e*)
                 (car e*))
             r
             k))
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
  (let ((#:g214 ('#<procedure #7 ##box> 'wait)))
    (let ((begin-temp.88
           ('#<procedure #8 ##set-box!>
            #:g214
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
        ((careless-determine-method1 ('#<procedure #9 ##unbox> #:g214) f)
         f
         v*
         r
         k)))))

(register-method
 'invoke
 '((f primitive) v* r k)
 '(1 . 1)
 (lambda (#:g216 #:g217 #:g218)
   (lambda (f v* r k) ((primitive-address f) v* r k)))
 'primitive)

(register-method
 'invoke
 '((f function) v* r k)
 '(1 . 1)
 (lambda (#:g220 #:g221 #:g222)
   (lambda (f v* r k)
     (let ((env (extend-env (function-env f) (function-variables f) v*)))
       (evaluate-begin (function-body f) env k))))
 'function)

(register-method
 'invoke
 '((f continuation) v* r k)
 '(1 . 1)
 (lambda (#:g224 #:g225 #:g226)
   (lambda (f v* r k)
     (if (let ((temp.137 ('#<procedure #19 length> v*)))
           (if ('#<procedure #20 ##fixnum?> temp.137)
               ('#<procedure #21 ##fx=> 1 temp.137)
               ('#<procedure #22 fx=> 1 temp.137)))
         (resume f
                 (if ('#<procedure #12 ##pair?> v*)
                     ('#<procedure #14 ##car> v*)
                     (car v*)))
         (wrong "Continuations expect one argument" v* r k))))
 'continuation)

(define evaluate-arguments
  (lambda (e* r k)
    (if ('#<procedure #12 ##pair?> e*)
        (evaluate
         (if ('#<procedure #12 ##pair?> e*)
             ('#<procedure #14 ##car> e*)
             (car e*))
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
  (let ((#:g228 ('#<procedure #7 ##box> 'wait)))
    (let ((begin-temp.92
           ('#<procedure #8 ##set-box!>
            #:g228
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
        ((careless-determine-method1 ('#<procedure #9 ##unbox> #:g228) k)
         k
         v)))))

(register-method
 'resume
 '((k if-cont) v)
 '(1 . 1)
 (lambda (#:g230 #:g231 #:g232)
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
 (lambda (#:g234 #:g235 #:g236)
   (lambda (k v)
     (let ((k (begin-cont-k k))
           (r (begin-cont-r k))
           (e* (let ((temp.140
                      (let ((begin-temp.48
                             (check-class k begin-cont-class 'e*)))
                        ('#<procedure #4 ##vector-ref> k 2))))
                 (if ('#<procedure #12 ##pair?> temp.140)
                     ('#<procedure #16 ##cdr> temp.140)
                     (cdr temp.140)))))
       (if ('#<procedure #12 ##pair?> e*)
           (if ('#<procedure #12 ##pair?>
                (if ('#<procedure #12 ##pair?> e*)
                    ('#<procedure #16 ##cdr> e*)
                    (cdr e*)))
               (evaluate
                (if ('#<procedure #12 ##pair?> e*)
                    ('#<procedure #14 ##car> e*)
                    (car e*))
                r
                (initialize!
                 ('#<procedure #5 ##subtype-set!>
                  ('#<procedure #6 ##vector>
                   ('#<procedure #4 ##vector-ref> begin-cont-class 2)
                   k
                   e*
                   r)
                  6)))
               (evaluate
                (if ('#<procedure #12 ##pair?> e*)
                    ('#<procedure #14 ##car> e*)
                    (car e*))
                r
                k))
           (resume k '())))))
 'begin-cont)

(register-method
 'resume
 '((k evfun-cont) f)
 '(1 . 1)
 (lambda (#:g238 #:g239 #:g240)
   (lambda (k f)
     (let ((k (make-apply-cont (evfun-cont-k k) f (evfun-cont-r k)))
           (r (let ((begin-temp.57 (check-class k evfun-cont-class 'r)))
                ('#<procedure #4 ##vector-ref> k 3)))
           (e* (let ((begin-temp.55 (check-class k evfun-cont-class 'e*)))
                 ('#<procedure #4 ##vector-ref> k 2))))
       (if ('#<procedure #12 ##pair?> e*)
           (evaluate
            (if ('#<procedure #12 ##pair?> e*)
                ('#<procedure #14 ##car> e*)
                (car e*))
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
 (lambda (#:g242 #:g243 #:g244)
   (lambda (k v)
     (invoke (apply-cont-f k) v (apply-cont-r k) (apply-cont-k k))))
 'apply-cont)

(register-method
 'resume
 '((k argument-cont) v)
 '(1 . 1)
 (lambda (#:g246 #:g247 #:g248)
   (lambda (k v)
     (let ((k (let ((k (let ((begin-temp.67
                              (check-class k argument-cont-class 'k)))
                         ('#<procedure #4 ##vector-ref> k 1))))
                (initialize!
                 ('#<procedure #5 ##subtype-set!>
                  ('#<procedure #6 ##vector>
                   ('#<procedure #4 ##vector-ref> gather-cont-class 2)
                   k
                   v)
                  6))))
           (r (let ((begin-temp.71 (check-class k argument-cont-class 'r)))
                ('#<procedure #4 ##vector-ref> k 3)))
           (e* (let ((temp.141
                      (let ((begin-temp.69
                             (check-class k argument-cont-class 'e*)))
                        ('#<procedure #4 ##vector-ref> k 2))))
                 (if ('#<procedure #12 ##pair?> temp.141)
                     ('#<procedure #16 ##cdr> temp.141)
                     (cdr temp.141)))))
       (if ('#<procedure #12 ##pair?> e*)
           (evaluate
            (if ('#<procedure #12 ##pair?> e*)
                ('#<procedure #14 ##car> e*)
                (car e*))
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
 (lambda (#:g250 #:g251 #:g252)
   (lambda (k v*)
     (resume (gather-cont-k k)
             ('#<procedure #23 ##cons> (gather-cont-v k) v*))))
 'gather-cont)

(register-method
 'resume
 '((k bottom-cont) v)
 '(1 . 1)
 (lambda (#:g254 #:g255 #:g256) (lambda (k v) ((bottom-cont-f k) v)))
 'bottom-cont)

(define lookup
  (let ((#:g258 ('#<procedure #7 ##box> 'wait)))
    (let ((begin-temp.96
           ('#<procedure #8 ##set-box!>
            #:g258
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
        ((careless-determine-method1 ('#<procedure #9 ##unbox> #:g258) r)
         r
         n
         k)))))

(register-method
 'lookup
 '((r null-env) n k)
 '(1 . 1)
 (lambda (#:g260 #:g261 #:g262)
   (lambda (r n k) (wrong "Unknown variable" n r k)))
 'null-env)

(register-method
 'lookup
 '((r variable-env) n k)
 '(1 . 1)
 (lambda (#:g264 #:g265 #:g266)
   (lambda (r n k)
     (if ('#<procedure #15 ##eq?> n (variable-env-name r))
         (resume k (variable-env-value r))
         (lookup (variable-env-others r) n k))))
 'variable-env)

(register-method
 'resume
 '((k set!-cont) v)
 '(1 . 1)
 (lambda (#:g268 #:g269 #:g270)
   (lambda (k v) (update! (set!-cont-r k) (set!-cont-n k) (set!-cont-k k) v)))
 'set!-cont)

(define update!
  (let ((#:g272 ('#<procedure #7 ##box> 'wait)))
    (let ((begin-temp.100
           ('#<procedure #8 ##set-box!>
            #:g272
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
        ((careless-determine-method1 ('#<procedure #9 ##unbox> #:g272) r)
         r
         n
         k
         v)))))

(register-method
 'update!
 '((r null-env) n k v)
 '(1 . 1)
 (lambda (#:g274 #:g275 #:g276)
   (lambda (r n k v) (wrong "Unknown variable" n r k)))
 'null-env)

(register-method
 'update!
 '((r variable-env) n k v)
 '(1 . 1)
 (lambda (#:g278 #:g279 #:g280)
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
            (let ((value (if ('#<procedure #12 ##pair?> values)
                             ('#<procedure #14 ##car> values)
                             (car values)))
                  (name (if ('#<procedure #12 ##pair?> names)
                            ('#<procedure #14 ##car> names)
                            (car names)))
                  (others (let ((values (if ('#<procedure #12 ##pair?> values)
                                            ('#<procedure #16 ##cdr> values)
                                            (cdr values)))
                                (names (if ('#<procedure #12 ##pair?> names)
                                           ('#<procedure #16 ##cdr> names)
                                           (cdr names))))
                            (if ('#<procedure #12 ##pair?> names)
                                (if ('#<procedure #12 ##pair?> values)
                                    (let ((value (if ('#<procedure #12 ##pair?>
                                                      values)
                                                     ('#<procedure #14 ##car>
                                                      values)
                                                     (car values)))
                                          (name (if ('#<procedure #12 ##pair?>
                                                     names)
                                                    ('#<procedure #14 ##car>
                                                     names)
                                                    (car names)))
                                          (others (extend-env
                                                   env
                                                   (if ('#<procedure #12 ##pair?>
                                                        names)
                                                       ('#<procedure #16 ##cdr>
                                                        names)
                                                       (cdr names))
                                                   (if ('#<procedure #12 ##pair?>
                                                        values)
                                                       ('#<procedure #16 ##cdr>
                                                        values)
                                                       (cdr values)))))
                                      (initialize!
                                       ('#<procedure #5 ##subtype-set!>
                                        ('#<procedure #6 ##vector>
                                         ('#<procedure #4 ##vector-ref>
                                          variable-env-class
                                          2)
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
                                       ('#<procedure #4 ##vector-ref>
                                        variable-env-class
                                        2)
                                       env
                                       names
                                       values)
                                      6)))))))
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
                                    (if (let ((temp.147
                                               ('#<procedure #19 length> v*)))
                                          (if ('#<procedure #20 ##fixnum?>
                                               temp.147)
                                              ('#<procedure #21 ##fx=>
                                               2
                                               temp.147)
                                              ('#<procedure #22 fx=>
                                               2
                                               temp.147)))
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
                                    (if (let ((temp.149
                                               ('#<procedure #19 length> v*)))
                                          (if ('#<procedure #20 ##fixnum?>
                                               temp.149)
                                              ('#<procedure #21 ##fx=>
                                               1
                                               temp.149)
                                              ('#<procedure #22 fx=>
                                               1
                                               temp.149)))
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
                                    (if (let ((temp.151
                                               ('#<procedure #19 length> v*)))
                                          (if ('#<procedure #20 ##fixnum?>
                                               temp.151)
                                              ('#<procedure #21 ##fx=>
                                               1
                                               temp.151)
                                              ('#<procedure #22 fx=>
                                               1
                                               temp.151)))
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
                                    (if (let ((temp.153
                                               ('#<procedure #19 length> v*)))
                                          (if ('#<procedure #20 ##fixnum?>
                                               temp.153)
                                              ('#<procedure #21 ##fx=>
                                               1
                                               temp.153)
                                              ('#<procedure #22 fx=>
                                               1
                                               temp.153)))
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
                                    (if (let ((temp.155
                                               ('#<procedure #19 length> v*)))
                                          (if ('#<procedure #20 ##fixnum?>
                                               temp.155)
                                              ('#<procedure #21 ##fx=>
                                               1
                                               temp.155)
                                              ('#<procedure #22 fx=>
                                               1
                                               temp.155)))
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
                                    (if (let ((temp.157
                                               ('#<procedure #19 length> v*)))
                                          (if ('#<procedure #20 ##fixnum?>
                                               temp.157)
                                              ('#<procedure #21 ##fx=>
                                               2
                                               temp.157)
                                              ('#<procedure #22 fx=>
                                               2
                                               temp.157)))
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
                                    (if (let ((temp.159
                                               ('#<procedure #19 length> v*)))
                                          (if ('#<procedure #20 ##fixnum?>
                                               temp.159)
                                              ('#<procedure #21 ##fx=>
                                               1
                                               temp.159)
                                              ('#<procedure #22 fx=>
                                               1
                                               temp.159)))
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
                                    (if (let ((temp.161
                                               ('#<procedure #19 length> v*)))
                                          (if ('#<procedure #20 ##fixnum?>
                                               temp.161)
                                              ('#<procedure #21 ##fx=>
                                               2
                                               temp.161)
                                              ('#<procedure #22 fx=>
                                               2
                                               temp.161)))
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
                                    (if (let ((temp.163
                                               ('#<procedure #19 length> v*)))
                                          (if ('#<procedure #20 ##fixnum?>
                                               temp.163)
                                              ('#<procedure #21 ##fx=>
                                               2
                                               temp.163)
                                              ('#<procedure #22 fx=>
                                               2
                                               temp.163)))
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
                                    (if (let ((temp.165
                                               ('#<procedure #19 length> v*)))
                                          (if ('#<procedure #20 ##fixnum?>
                                               temp.165)
                                              ('#<procedure #21 ##fx=>
                                               2
                                               temp.165)
                                              ('#<procedure #22 fx=>
                                               2
                                               temp.165)))
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
                                    (if (let ((temp.167
                                               ('#<procedure #19 length> v*)))
                                          (if ('#<procedure #20 ##fixnum?>
                                               temp.167)
                                              ('#<procedure #21 ##fx=>
                                               2
                                               temp.167)
                                              ('#<procedure #22 fx=>
                                               2
                                               temp.167)))
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
                                    (if (let ((temp.169
                                               ('#<procedure #19 length> v*)))
                                          (if ('#<procedure #20 ##fixnum?>
                                               temp.169)
                                              ('#<procedure #21 ##fx=>
                                               2
                                               temp.169)
                                              ('#<procedure #22 fx=>
                                               2
                                               temp.169)))
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
                                    (if (let ((temp.171
                                               ('#<procedure #19 length> v*)))
                                          (if ('#<procedure #20 ##fixnum?>
                                               temp.171)
                                              ('#<procedure #21 ##fx=>
                                               2
                                               temp.171)
                                              ('#<procedure #22 fx=>
                                               2
                                               temp.171)))
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
                                    (if (let ((temp.173
                                               ('#<procedure #19 length> v*)))
                                          (if ('#<procedure #20 ##fixnum?>
                                               temp.173)
                                              ('#<procedure #21 ##fx=>
                                               2
                                               temp.173)
                                              ('#<procedure #22 fx=>
                                               2
                                               temp.173)))
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
                                    (if (let ((temp.175
                                               ('#<procedure #19 length> v*)))
                                          (if ('#<procedure #20 ##fixnum?>
                                               temp.175)
                                              ('#<procedure #21 ##fx=>
                                               2
                                               temp.175)
                                              ('#<procedure #22 fx=>
                                               2
                                               temp.175)))
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
                                    (if (let ((temp.177
                                               ('#<procedure #19 length> v*)))
                                          (if ('#<procedure #20 ##fixnum?>
                                               temp.177)
                                              ('#<procedure #21 ##fx=>
                                               2
                                               temp.177)
                                              ('#<procedure #22 fx=>
                                               2
                                               temp.177)))
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
                                    (if (let ((temp.179
                                               ('#<procedure #19 length> v*)))
                                          (if ('#<procedure #20 ##fixnum?>
                                               temp.179)
                                              ('#<procedure #21 ##fx=>
                                               2
                                               temp.179)
                                              ('#<procedure #22 fx=>
                                               2
                                               temp.179)))
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
                                    (if (let ((temp.181
                                               ('#<procedure #19 length> v*)))
                                          (if ('#<procedure #20 ##fixnum?>
                                               temp.181)
                                              ('#<procedure #21 ##fx=>
                                               2
                                               temp.181)
                                              ('#<procedure #22 fx=>
                                               2
                                               temp.181)))
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
                                    (if (let ((temp.183
                                               ('#<procedure #19 length> v*)))
                                          (if ('#<procedure #20 ##fixnum?>
                                               temp.183)
                                              ('#<procedure #21 ##fx=>
                                               1
                                               temp.183)
                                              ('#<procedure #22 fx=>
                                               1
                                               temp.183)))
                                        (invoke (if ('#<procedure #12 ##pair?>
                                                     v*)
                                                    ('#<procedure #14 ##car>
                                                     v*)
                                                    (car v*))
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
                                    (if (let ((temp.185
                                               ('#<procedure #19 length> v*)))
                                          (if ('#<procedure #20 ##fixnum?>
                                               temp.185)
                                              ('#<procedure #45 ##fx>=>
                                               temp.185
                                               2)
                                              ('#<procedure #46 fx>=>
                                               temp.185
                                               2)))
                                        (if (or (let ((#:g16 (if ('#<procedure #12 ##pair?>
                                                                  v*)
                                                                 ('#<procedure #14 ##car>
                                                                  v*)
                                                                 (car v*))))
                                                  (and ('#<procedure #3 ##meroon?>
                                                        #:g16)
                                                       (careless-subclass?
                                                        ('#<procedure #4 ##vector-ref>
                                                         *classes*
                                                         ('#<procedure #4 ##vector-ref>
                                                          #:g16
                                                          0))
                                                        function-class)))
                                                (or (let ((#:g29 (if ('#<procedure #12 ##pair?>
                                                                      v*)
                                                                     ('#<procedure #14 ##car>
                                                                      v*)
                                                                     (car v*))))
                                                      (and ('#<procedure #3 ##meroon?>
                                                            #:g29)
                                                           (careless-subclass?
                                                            ('#<procedure #4 ##vector-ref>
                                                             *classes*
                                                             ('#<procedure #4 ##vector-ref>
                                                              #:g29
                                                              0))
                                                            primitive-class)))
                                                    (let ((#:g82 (if ('#<procedure #12 ##pair?>
                                                                      v*)
                                                                     ('#<procedure #14 ##car>
                                                                      v*)
                                                                     (car v*))))
                                                      (and ('#<procedure #3 ##meroon?>
                                                            #:g82)
                                                           (careless-subclass?
                                                            ('#<procedure #4 ##vector-ref>
                                                             *classes*
                                                             ('#<procedure #4 ##vector-ref>
                                                              #:g82
                                                              0))
                                                            continuation-class)))))
                                            (invoke (if ('#<procedure #12 ##pair?>
                                                         v*)
                                                        ('#<procedure #14 ##car>
                                                         v*)
                                                        (car v*))
                                                    (letrec ((conc (lambda (args)
                                                                     (if ('#<procedure #12 ##pair?>
                                                                          (if ('#<procedure #12 ##pair?>
                                                                               args)
                                                                              ('#<procedure #16 ##cdr>
                                                                               args)
                                                                              (cdr args)))
                                                                         ('#<procedure #23 ##cons>
                                                                          (if ('#<procedure #12 ##pair?>
                                                                               args)
                                                                              ('#<procedure #14 ##car>
                                                                               args)
                                                                              (car args))
                                                                          (let ((args (if ('#<procedure #12 ##pair?>
                                                                                           args)
                                                                                          ('#<procedure #16 ##cdr>
                                                                                           args)
                                                                                          (cdr args))))
                                                                            (if ('#<procedure #12 ##pair?>
                                                                                 (if ('#<procedure #12 ##pair?>
                                                                                      args)
                                                                                     ('#<procedure #16 ##cdr>
                                                                                      args)
                                                                                     (cdr args)))
                                                                                ('#<procedure #23 ##cons>
                                                                                 (if ('#<procedure #12 ##pair?>
                                                                                      args)
                                                                                     ('#<procedure #14 ##car>
                                                                                      args)
                                                                                     (car args))
                                                                                 (let ((args (if ('#<procedure #12 ##pair?>
                                                                                                  args)
                                                                                                 ('#<procedure #16 ##cdr>
                                                                                                  args)
                                                                                                 (cdr args))))
                                                                                   (if ('#<procedure #12 ##pair?>
                                                                                        (if ('#<procedure #12 ##pair?>
                                                                                             args)
                                                                                            ('#<procedure #16 ##cdr>
                                                                                             args)
                                                                                            (cdr args)))
                                                                                       ('#<procedure #23 ##cons>
                                                                                        (if ('#<procedure #12 ##pair?>
                                                                                             args)
                                                                                            ('#<procedure #14 ##car>
                                                                                             args)
                                                                                            (car args))
                                                                                        (let ((args (if ('#<procedure #12 ##pair?>
                                                                                                         args)
                                                                                                        ('#<procedure #16 ##cdr>
                                                                                                         args)
                                                                                                        (cdr args))))
                                                                                          (if ('#<procedure #12 ##pair?>
                                                                                               (if ('#<procedure #12 ##pair?>
                                                                                                    args)
                                                                                                   ('#<procedure #16 ##cdr>
                                                                                                    args)
                                                                                                   (cdr args)))
                                                                                              ('#<procedure #23 ##cons>
                                                                                               (if ('#<procedure #12 ##pair?>
                                                                                                    args)
                                                                                                   ('#<procedure #14 ##car>
                                                                                                    args)
                                                                                                   (car args))
                                                                                               (let ((args (if ('#<procedure #12 ##pair?>
                                                                                                                args)
                                                                                                               ('#<procedure #16 ##cdr>
                                                                                                                args)
                                                                                                               (cdr args))))
                                                                                                 (if ('#<procedure #12 ##pair?>
                                                                                                      (if ('#<procedure #12 ##pair?>
                                                                                                           args)
                                                                                                          ('#<procedure #16 ##cdr>
                                                                                                           args)
                                                                                                          (cdr args)))
                                                                                                     ('#<procedure #23 ##cons>
                                                                                                      (if ('#<procedure #12 ##pair?>
                                                                                                           args)
                                                                                                          ('#<procedure #14 ##car>
                                                                                                           args)
                                                                                                          (car args))
                                                                                                      (let ((args (if ('#<procedure #12 ##pair?>
                                                                                                                       args)
                                                                                                                      ('#<procedure #16 ##cdr>
                                                                                                                       args)
                                                                                                                      (cdr args))))
                                                                                                        (if ('#<procedure #12 ##pair?>
                                                                                                             (if ('#<procedure #12 ##pair?>
                                                                                                                  args)
                                                                                                                 ('#<procedure #16 ##cdr>
                                                                                                                  args)
                                                                                                                 (cdr args)))
                                                                                                            ('#<procedure #23 ##cons>
                                                                                                             (if ('#<procedure #12 ##pair?>
                                                                                                                  args)
                                                                                                                 ('#<procedure #14 ##car>
                                                                                                                  args)
                                                                                                                 (car args))
                                                                                                             (let ((args (if ('#<procedure #12 ##pair?>
                                                                                                                              args)
                                                                                                                             ('#<procedure #16 ##cdr>
                                                                                                                              args)
                                                                                                                             (cdr args))))
                                                                                                               (if ('#<procedure #12 ##pair?>
                                                                                                                    (if ('#<procedure #12 ##pair?>
                                                                                                                         args)
                                                                                                                        ('#<procedure #16 ##cdr>
                                                                                                                         args)
                                                                                                                        (cdr args)))
                                                                                                                   ('#<procedure #23 ##cons>
                                                                                                                    (if ('#<procedure #12 ##pair?>
                                                                                                                         args)
                                                                                                                        ('#<procedure #14 ##car>
                                                                                                                         args)
                                                                                                                        (car args))
                                                                                                                    (let ((args (if ('#<procedure #12 ##pair?>
                                                                                                                                     args)
                                                                                                                                    ('#<procedure #16 ##cdr>
                                                                                                                                     args)
                                                                                                                                    (cdr args))))
                                                                                                                      (if ('#<procedure #12 ##pair?>
                                                                                                                           (if ('#<procedure #12 ##pair?>
                                                                                                                                args)
                                                                                                                               ('#<procedure #16 ##cdr>
                                                                                                                                args)
                                                                                                                               (cdr args)))
                                                                                                                          ('#<procedure #23 ##cons>
                                                                                                                           (if ('#<procedure #12 ##pair?>
                                                                                                                                args)
                                                                                                                               ('#<procedure #14 ##car>
                                                                                                                                args)
                                                                                                                               (car args))
                                                                                                                           (let ((args (if ('#<procedure #12 ##pair?>
                                                                                                                                            args)
                                                                                                                                           ('#<procedure #16 ##cdr>
                                                                                                                                            args)
                                                                                                                                           (cdr args))))
                                                                                                                             (if ('#<procedure #12 ##pair?>
                                                                                                                                  (if ('#<procedure #12 ##pair?>
                                                                                                                                       args)
                                                                                                                                      ('#<procedure #16 ##cdr>
                                                                                                                                       args)
                                                                                                                                      (cdr args)))
                                                                                                                                 ('#<procedure #23 ##cons>
                                                                                                                                  (if ('#<procedure #12 ##pair?>
                                                                                                                                       args)
                                                                                                                                      ('#<procedure #14 ##car>
                                                                                                                                       args)
                                                                                                                                      (car args))
                                                                                                                                  (let ((args (if ('#<procedure #12 ##pair?>
                                                                                                                                                   args)
                                                                                                                                                  ('#<procedure #16 ##cdr>
                                                                                                                                                   args)
                                                                                                                                                  (cdr args))))
                                                                                                                                    (if ('#<procedure #12 ##pair?>
                                                                                                                                         (if ('#<procedure #12 ##pair?>
                                                                                                                                              args)
                                                                                                                                             ('#<procedure #16 ##cdr>
                                                                                                                                              args)
                                                                                                                                             (cdr args)))
                                                                                                                                        ('#<procedure #23 ##cons>
                                                                                                                                         (if ('#<procedure #12 ##pair?>
                                                                                                                                              args)
                                                                                                                                             ('#<procedure #14 ##car>
                                                                                                                                              args)
                                                                                                                                             (car args))
                                                                                                                                         (let ((args (if ('#<procedure #12 ##pair?>
                                                                                                                                                          args)
                                                                                                                                                         ('#<procedure #16 ##cdr>
                                                                                                                                                          args)
                                                                                                                                                         (cdr args))))
                                                                                                                                           (if ('#<procedure #12 ##pair?>
                                                                                                                                                (if ('#<procedure #12 ##pair?>
                                                                                                                                                     args)
                                                                                                                                                    ('#<procedure #16 ##cdr>
                                                                                                                                                     args)
                                                                                                                                                    (cdr args)))
                                                                                                                                               ('#<procedure #23 ##cons>
                                                                                                                                                (if ('#<procedure #12 ##pair?>
                                                                                                                                                     args)
                                                                                                                                                    ('#<procedure #14 ##car>
                                                                                                                                                     args)
                                                                                                                                                    (car args))
                                                                                                                                                (conc (if ('#<procedure #12 ##pair?>
                                                                                                                                                           args)
                                                                                                                                                          ('#<procedure #16 ##cdr>
                                                                                                                                                           args)
                                                                                                                                                          (cdr args))))
                                                                                                                                               (if ('#<procedure #12 ##pair?>
                                                                                                                                                    args)
                                                                                                                                                   ('#<procedure #14 ##car>
                                                                                                                                                    args)
                                                                                                                                                   (car args)))))
                                                                                                                                        (if ('#<procedure #12 ##pair?>
                                                                                                                                             args)
                                                                                                                                            ('#<procedure #14 ##car>
                                                                                                                                             args)
                                                                                                                                            (car args)))))
                                                                                                                                 (if ('#<procedure #12 ##pair?>
                                                                                                                                      args)
                                                                                                                                     ('#<procedure #14 ##car>
                                                                                                                                      args)
                                                                                                                                     (car args)))))
                                                                                                                          (if ('#<procedure #12 ##pair?>
                                                                                                                               args)
                                                                                                                              ('#<procedure #14 ##car>
                                                                                                                               args)
                                                                                                                              (car args)))))
                                                                                                                   (if ('#<procedure #12 ##pair?>
                                                                                                                        args)
                                                                                                                       ('#<procedure #14 ##car>
                                                                                                                        args)
                                                                                                                       (car args)))))
                                                                                                            (if ('#<procedure #12 ##pair?>
                                                                                                                 args)
                                                                                                                ('#<procedure #14 ##car>
                                                                                                                 args)
                                                                                                                (car args)))))
                                                                                                     (if ('#<procedure #12 ##pair?>
                                                                                                          args)
                                                                                                         ('#<procedure #14 ##car>
                                                                                                          args)
                                                                                                         (car args)))))
                                                                                              (if ('#<procedure #12 ##pair?>
                                                                                                   args)
                                                                                                  ('#<procedure #14 ##car>
                                                                                                   args)
                                                                                                  (car args)))))
                                                                                       (if ('#<procedure #12 ##pair?>
                                                                                            args)
                                                                                           ('#<procedure #14 ##car>
                                                                                            args)
                                                                                           (car args)))))
                                                                                (if ('#<procedure #12 ##pair?>
                                                                                     args)
                                                                                    ('#<procedure #14 ##car>
                                                                                     args)
                                                                                    (car args)))))
                                                                         (if ('#<procedure #12 ##pair?>
                                                                              args)
                                                                             ('#<procedure #14 ##car>
                                                                              args)
                                                                             (car args))))))
                                                      (conc (if ('#<procedure #12 ##pair?>
                                                                 v*)
                                                                ('#<procedure #16 ##cdr>
                                                                 v*)
                                                                (cdr v*))))
                                                    r
                                                    k)
                                            (wrong "Not a function"
                                                   'apply
                                                   (if ('#<procedure #12 ##pair?>
                                                        v*)
                                                       ('#<procedure #14 ##car>
                                                        v*)
                                                       (car v*))))
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
                (if (if ('#<procedure #20 ##fixnum?> i)
                        ('#<procedure #47 ##fx<=> i 0)
                        ('#<procedure #48 fx<=> i 0))
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
                      (let ((i (if ('#<procedure #20 ##fixnum?> i)
                                   (let ((temp.201
                                          ('#<procedure #49 ##fx-?> i 1)))
                                     (if temp.201
                                         temp.201
                                         ('#<procedure #50 fx-> i 1)))
                                   ('#<procedure #50 fx-> i 1))))
                        (if (if ('#<procedure #20 ##fixnum?> i)
                                ('#<procedure #47 ##fx<=> i 0)
                                ('#<procedure #48 fx<=> i 0))
                            #f
                            (let ((begin-temp.104
                                   (evaluate
                                    e
                                    r.init
                                    (let ((f (lambda (v)
                                               (let ((begin-temp.103
                                                      ('#<procedure #18 display>
                                                       v)))
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
                              (do-temp.102
                               e
                               (if ('#<procedure #20 ##fixnum?> i)
                                   (let ((temp.201
                                          ('#<procedure #49 ##fx-?> i 1)))
                                     (if temp.201
                                         temp.201
                                         ('#<procedure #50 fx-> i 1)))
                                   ('#<procedure #50 fx-> i 1)))))))))))
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

