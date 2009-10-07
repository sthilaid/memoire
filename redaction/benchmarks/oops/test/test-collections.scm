;;; FILE: "test-collections.scm"
;;; IMPLEMENTS: Unit tests for collections code [for Oops]
;;; LANGUAGE: Gambit Scheme (v4 beta12)
;;; AUTHOR: Ken Dickey
;;;
;;; COPYRIGHT (c) 2005 by Kenneth Alan Dickey
;;;
;;;   Licensed under the Apache License, Version 2.0 (the "License");
;;;   you may not use this file except in compliance with the License.
;;;   You may obtain a copy of the License at
;;;
;;;       http://www.apache.org/licenses/LICENSE-2.0
;;;
;;;   Unless required by applicable law or agreed to in writing, software
;;;   distributed under the License is distributed on an "AS IS" BASIS,
;;;   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;;   See the License for the specific language governing permissions and
;;;   limitations under the License.

;==============================================================;
(include "testing-macros.scm")
;==============================================================;

(add-test-suite 'collections)

;; Setup for collection tests

;;(require 'oops)
(include "../src/oops-macros.scm")

(add-equal-test 'collections
                (vector #\s #\t #\r #\i #\n #\g)
                (as <vector> "string"))

(add-equal-test 'collections
                (vector 1 2 3)
                (as <vector> '(1 2 3)))

(add-equal-test 'collections
                "string"
                (as <string> (vector #\s #\t #\r #\i #\n #\g)))

(add-equal-test 'collections
                "little"
                (as <string> '(#\l #\i #\t #\t #\l #\e)))

(add-equal-test 'collections
                '(#\a #\b #\c)
                (as <pair> "abc"))

(add-equal-test 'collections
                "sss"
                (<string> size: 3 fill: #\s))

(add-equal-test 'collections
                (vector #\s #\s #\s)
                (<vector> size: 3 fill: #\s))

(add-equal-test 'collections
                3
                (elt-ref (vector 0 1 2 3 4) 3))

(add-equal-test 'collections
                #\3
                (elt-ref "01234" 3))

(add-equal-test 'collections
                (vector 0 1 "two" 3 4)
                (let ( (v (vector 0 1 2 3 4)) )
                  (elt-set! v 2 "two")
                  v))

(add-equal-test 'collections
                "01x34"
                (let ( (s  "01234") ) (elt-set! s 2 #\x) s)
                "(elt-set! \"01234\" 2 #\\x)" )

(add-equal-test 'collections
      '(#\a #\space #\l #\i #\t #\t #\l #\e #\space #\t #\e #\s #\t)
      (fold-right cons '() "a little test"))

(add-equal-test 'collections
      '(#\t #\s #\e #\t #\space #\e #\l #\t #\t #\i #\l #\space #\a)
      (fold-left cons '() "a little test"))

(add-equal-test 'collections
      '(1 2 3 4)
      (fold-right cons '() (vector 1 2 3 4)))

(add-equal-test 'collections
      '(4 3 2 1)
      (fold-left cons '() (vector 1 2 3 4)))

(add-equal-test 'collections
      3
      (let ( (total 0) )
        (for-each-key
         (lambda (k) (set! total (+ total k)))
         "123")
        total)
      "for-each-key string")

(add-equal-test 'collections
      3
      (let ( (total 0) )
        (for-each-key
         (lambda (k) (set! total (+ total k)))
         (vector 1 2 3))
        total)
      "for-each-key vector")

(add-equal-test 'collections
      '(3 2 1)
      (let ( (result '()) )
        (for-each-elt
         (lambda (k) (set! result (cons k result)))
         (vector 1 2 3))
        result)
      "for-each-elt vector")

(add-equal-test 'collections
      '(#\3 #\2 #\1)
      (let ( (result '()) )
        (for-each-elt
         (lambda (k) (set! result (cons k result)))
         "123")
        result)
      "for-each-elt string")

(add-equal-test 'collections
      '((2 . #\3) (1 . #\2) (0 . #\1))
      (let ( (result '()) )
        (for-each-key-elt
         (lambda (k e) (set! result (cons (cons k e) result)))
         "123")
        result)
      "for-each-key-elt string")

(define a-dict (<alist>))
(add-key-elt! a-dict 'foo "the big foo")
(add-key-elt! a-dict 'bar "cheers")
(add-key-elt! a-dict 'baz "Get the Buzz")
(add-key-elt! a-dict 'quux "Moby Quux")

(add-equal-test 'collections
      "Get the Buzz"
      (elt-ref a-dict 'baz))

(add-equal-test 'collections
                4
                (num-elts a-dict))

(add-equal-test 'collections
        '((foo . "the big foo")
          (bar . "cheers")
          (baz . "Get the Buzz")
          (quux . "Moby Quux"))
        (let ( (result '()) )
        (for-each-key-elt
         (lambda (k e) (set! result (cons (cons k e) result)))
         a-dict)
        result)
        "for-each-key-elt <alist>")

(add-equal-test 'collections
        '(foo bar baz quux)
        (let ( (result '()) )
        (for-each-key
         (lambda (k) (set! result (cons k result)))
         a-dict)
        result)
        "for-each-key <alist>")

(add-equal-test 'collections
     '("the big foo" "cheers" "Get the Buzz" "Moby Quux")
     (let ( (result '()) )
        (for-each-elt
         (lambda (e) (set! result (cons e result)))
         a-dict)
        result)
      "for-each-elt <alist>")

(add-equal-test 'collections
   '("the big foo" "cheers" "Get the Buzz" "Moby Quux")
   (fold-left cons '() a-dict))

(add-equal-test 'collections
   (vector #\a #\i #\e #\e #\e)
   (collect (lambda (c) (memq c (string->list "aeiou"))) "a little test, yes!"))

(add-equal-test 'collections
   "aieee"
   (collect (lambda (c) (memq c (string->list "aeiou"))) "a little test, yes!" <string>))

(add-equal-test 'collections
   " lttl tst, ys!"
   (reject (lambda (c) (memq c (string->list "aeiou"))) "a little test, yes!" <string>))

(ensure-exception-raised
    'collections
    error-exception?
    (fill "abc")
    "Can't access instance slot of a non-instance.")

(add-equal-test 'collections
    <character>
    (element-type "abc")
    "Can access class slots of a non-instance via `virtual classes`")

(add-equal-test 'collections
                #\a
                (1st "a little test string"))

(add-equal-test 'collections
                0
                (1st (vector 0 1 2 3 4)))

(add-equal-test 'collections
                #\space
                (2nd "a little test string"))

(add-equal-test 'collections
                1
                (2nd (vector 0 1 2 3 4)))

(add-equal-test 'collections
                #\l
                (3rd "a little test string"))

(add-equal-test 'collections
                2
                (3rd (vector 0 1 2 3 4)))

(add-equal-test 'collections
                #\i
                (nth "a little test string" 3))

(add-equal-test 'collections
                3
                (nth (vector 0 1 2 3 4) 3))

(add-equal-test 'collections
                #\g
                (last "a little test string"))

(add-equal-test 'collections
                4
                (last (vector 0 1 2 3 4)))

(add-equal-test 'collections
                (vector #\l #\i #\t #\t #\l #\e)
                (slice "a little test string" 2 8 <vector>))

(add-equal-test 'collections
                "little"
                (slice "a little test string" 2 8 <string>))

(add-equal-test 'collections
                (vector 1 2 3)
                (slice (vector 0 1 2 3 4) 1 4))

(add-equal-test 'collections
                "123"
                (slice (vector #\0 #\1 #\2 #\3 #\4) 1 4 <string>))

(add-equal-test 'collections
                "little"
                (->collection <string> #\l #\i #\t #\t #\l #\e))

(add-equal-test 'collections
                (vector 0 1 2 3)
                (->collection <vector> 0 1 2 3))

(add-equal-test 'collections
                '(1 2 3 4)
                (as <pair> (vector 1 2 3 4)))

(add-equal-test 'collections
                '(1 2 3 4)
                (as <list> (vector 1 2 3 4)))

(add-equal-test 'collections
                '(1 2 3)
                (data (->collection <set> 1 2 3)))

(add-equal-test 'collections
                (vector 1 3)
                (collect odd? (->collection <set> 1 2 3)))

(add-equal-test 'collections
                #t
                (every-elt? char? "test string"))

(add-equal-test 'collections
                #t
                (any-elt? number? '(a b 3 d)))

(add-equal-test 'collections
                #t
                (every-elt? char? (vector #\a #\b #\3)))

(add-equal-test 'collections
                #f
                (every-elt? char? (vector #\a 2 #\3)))

(add-equal-test 'collections
                #f
                (every-elt? number? '(a b 3 d)))


(add-equal-test 'collections
                #f
                (any-elt? number? '(a b c d)))

(add-equal-test 'collections
                #t
                (elts=? (<set> data: '(a b c d e f)) 
                        (let ( (s1 (<set> data: '(a b c d)))
                               (s2 (<set> data: '(f e d c)))
                               )
                          (union s1 s2))))

(add-equal-test 'collections
                #t
                (elts=? (<set> data: '(c d))
                        (let ( (s1 (<set> data: '(a b c d)))
                               (s2 (<set> data: '(f e d c)))
                               )
                          (intersection s1 s2))))

(add-equal-test 'collections
                #t
                (elts=? (->collection <set> 'a 'b)
                        (let ( (s1 (<set> data: '(a b c d)))
                               (s2 (<set> data: '(f e d c)))
                               )
                          (difference s1 s2))))

(add-equal-test 'collections
                (vector 3 2 1)
                (reverse-elts (vector 1 2 3)))

(add-equal-test 'collections
                "cba"
                (reverse-elts "abc"))

(add-equal-test 'collections
                '(3 2 1)
                (reverse-elts '(1 2 3)))

;;==========================================================
(add-test-suite '<list><pair><null>)

(add-equal-test '<list><pair><null>
                <null> ;; special case
                (<singleton> '())
                "Elide common error for (<singleton> '())")

(add-equal-test '<list><pair><null>
                0
                (num-elts '()))

(add-equal-test '<list><pair><null>
                1
                (num-elts '(a)))

(add-equal-test '<list><pair><null>
                3
                (num-elts '(a b c)))

(add-equal-test '<list><pair><null>
                6
                (let ( (result 0) )
                  (for-each-elt
                   (lambda (elt) (set! result (+ result elt)))
                   '(1 2 3))
                  result))

(add-equal-test '<list><pair><null>
                6
                (fold-left + 0 '(1 2 3)))

(add-equal-test '<list><pair><null>
                #t
                (empty? '()))

(add-equal-test '<list><pair><null>
                #f
                (empty? '(a)))

(add-equal-test '<list><pair><null>
                #f
                (empty? '(a b c)))

(add-equal-test '<list><pair><null>
                '(a b c)
                (copy '(a b c)))

(add-equal-test '<list><pair><null>
                #f
                (let ( (list '(a b c)) )
                  (eq? list (copy list))))

(add-equal-test '<list><pair><null>
                '(a b c 1 2 3)
                (concatenate '(a b c) '(1 2 3)))

(add-equal-test '<list><pair><null>
                '(a b c 1 2 3)
                (concatenate '(a b c) (vector 1 2 3)))

(add-equal-test '<list><pair><null>
                '(a b c 1 2 3 #\d #\e #\f)
                (concatenate '(a b c) (vector 1 2 3) "def"))

(add-equal-test '<list><pair><null>
                '(a b c)
                (concatenate '() '(a b c)))

(add-equal-test '<list><pair><null>
                '(a b c 1 2 3)
                (concatenate '() '(a b c) (vector 1 2 3)))

(add-equal-test '<list><pair><null>
                '(#\a #\b #\c 1 2 3)
                (concatenate-as <list> "abc" (vector 1 2 3)))

(add-equal-test '<list><pair><null>
                #t
                (every-elt? odd? '()))

(add-equal-test '<list><pair><null>
                #t
                (every-elt? odd? '(1 3 5 7 9)))

(add-equal-test '<list><pair><null>
                #f
                (every-elt? odd? '(1 2 3 4 5)))

(add-equal-test '<list><pair><null>
                #f
                (any-elt? odd? '()))

(add-equal-test '<list><pair><null>
                #t
                (any-elt? odd? '(1 3 5 7 9)))

(add-equal-test '<list><pair><null>
                #t
                (any-elt? odd? '(1 2 3 4 5)))

(add-equal-test '<list><pair><null>
                #f
                (any-elt? odd? '(2 4 6 8)))

(add-equal-test '<list><pair><null>
                #f
                (contains-elt? '() 3))

(add-equal-test '<list><pair><null>
                #f
                (contains-elt? '(2 4 6 8) 3))

(add-equal-test '<list><pair><null>
                #t
                (contains-elt? '(2 4 6 8) 6))

(add-equal-test '<list><pair><null>
                #t
                (elts=? '() '()))

(add-equal-test '<list><pair><null>
                #f
                (elts=? '(a b c) '()))

(add-equal-test '<list><pair><null>
                #f
                (elts=? '() '(d e f)))

(add-equal-test '<list><pair><null>
                #t
                (elts=? '(a b c d) '(a b c d)))

(add-equal-test '<list><pair><null>
                #f
                (elts=? '(a b (c) d) '(a b (c) d))
                "NB: elts=? for lists uses eqv? ")

(add-equal-test '<list><pair><null>
                #t
                (let ( (c '(c)) )
                  (elts=? (list 1 2 c 'd) (list 1 2 c 'd))))

(add-equal-test '<list><pair><null>
                #t
                (elts=? (list 1 2 3) (->collection <list> 1 2 3)))

(add-equal-test '<list><pair><null>
                #t
                (elts=? (->collection <pair> 1 2 3)
                        (->collection <list> 1 2 3)))

(add-equal-test '<list><pair><null>
                '(a)
                (add-elt '() 'a))

(add-equal-test '<list><pair><null>
                '(a b)
                (add-elt '(b) 'a))

(add-equal-test '<list><pair><null>
                '(a b c)
                (add-elt '(b c) 'a))

(add-equal-test '<list><pair><null>
                '()
                (add-elts '() '()))

(add-equal-test '<list><pair><null>
                '(a b c)
                (add-elts '(a b c) '()))

(add-equal-test '<list><pair><null>
                '(a b c)
                (add-elts '() '(a b c)))

(add-equal-test '<list><pair><null>
                '(1 2 3 a b c)
                (add-elts '(a b c) (vector 1 2 3)))

(add-equal-test '<list><pair><null>
                '(1 2 3 a b c)
                (add-elts '(a b c) '(1 2 3)))

(add-equal-test '<list><pair><null>
                '()
                (remove-elt '() 2))

(add-equal-test '<list><pair><null>
                '(4 6 8)
                (remove-elt '(4 6 8) 2))

(add-equal-test '<list><pair><null>
                '(2 6 8)
                (remove-elt '(4 2 4 6 8 4) 4))

(add-equal-test '<list><pair><null>
                '()
                (remove-elts '() (vector 2 3 4)))

(add-equal-test '<list><pair><null>
                '(1 5 6 7 8)
                (remove-elts '(1 2 3 4 5 6 7 8 4) (vector 2 3 4)))

(add-equal-test '<list><pair><null>
                '(#\a #\b #\c #\g)
                (remove-elts (string->list "abcdefg") "def"))

(add-equal-test '<list><pair><null>
                '(a b c)
                (as <list> '(a b c)))

(add-equal-test '<list><pair><null>
                '(a b c)
                (as <pair> '(a b c)))

(add-equal-test '<list><pair><null>
                (string->list "abc")
                (as <list> "abc"))

(add-equal-test '<list><pair><null>
                '(1 2 3)
                (as <list> (vector 1 2 3)))

(add-equal-test '<list><pair><null>
                (string->list "abc")
                (as <pair> "abc"))

(add-equal-test '<list><pair><null>
                '(1 2 3)
                (as <pair> (vector 1 2 3)))

(add-equal-test '<list><pair><null>
                ""
                (as <string> '()))

(add-equal-test '<list><pair><null>
                'a
                (1st '(a b c)))

(add-equal-test '<list><pair><null>
                'b
                (2nd '(a b c)))

(add-equal-test '<list><pair><null>
                'c
                (3rd '(a b c)))

(add-equal-test '<list><pair><null>
                'c
                (nth '(a b c) 2))

(add-equal-test '<list><pair><null>
                'a
                (nth '(a b c) 0))

(add-equal-test '<list><pair><null>
                '()
                (reverse-elts '()))

(add-equal-test '<list><pair><null>
                '(c b a)
                (reverse-elts '(a b c)))


;;==========================================================
(add-test-suite '<string><vector>)

(add-equal-test '<string><vector>
                3
                (num-elts "abc"))

(add-equal-test '<string><vector>
                4
                (num-elts (vector 0 1 2 3)))

(add-equal-test '<string><vector>
      '(3 2 1)
      (let ( (result '()) )
        (for-each-elt
         (lambda (k) (set! result (cons k result)))
         (vector 1 2 3))
        result)
      "for-each-elt vector")

(add-equal-test '<string><vector>
      '(#\c #\b #\a)
      (let ( (result '()) )
        (for-each-elt
         (lambda (k) (set! result (cons k result)))
         "abc")
        result)
      "for-each-elt string")


(add-equal-test '<string><vector>
                '(#\c #\b #\a)
                (fold-left cons '() "abc"))

(add-equal-test '<string><vector>
                '(#\a #\b #\c)
                (fold-right cons '() "abc"))

(add-equal-test '<string><vector>
                '(3 2 1)
                (fold-left cons '() (vector 1 2 3)))

(add-equal-test '<string><vector>
                '(1 2 3)
                (fold-right cons '() (vector 1 2 3)))

(add-equal-test '<string><vector>
                '"abc"
                (copy "abc"))

(add-equal-test '<string><vector>
                (vector 1 2 3)
                (copy (vector 1 2 3)))

(add-equal-test '<string><vector>
                "abc123def"
                (concatenate "abc"
                             (vector #\1 #\2 #\3)
                             (list #\d #\e #\f)))

(add-equal-test '<string><vector>
                (vector 1 2 3 #\a #\b #\c 4 5 6)
                (concatenate (vector 1 2 3)
                             "abc"
                             (list 4 5 6)))

(add-equal-test '<string><vector>
                "abc123def"
                (concatenate-as <string>
                             "abc"
                             (vector #\1 #\2 #\3)
                             (list #\d #\e #\f)))

(add-equal-test '<string><vector>
                (vector #\a #\b #\c 1 2 3 #\d #\e #\f)
                (concatenate-as <vector>
                             "abc"
                             (vector 1 2 3)
                             (list #\d #\e #\f)))

(add-equal-test '<string><vector>
                #t
                (every-elt? odd? (vector 1 3 5 7)))

(add-equal-test '<string><vector>
                #f
                (every-elt? odd? (vector 1 3 4 7)))

(add-equal-test '<string><vector>
                #t
                (every-elt? char? "abcd"))

(add-equal-test '<string><vector>
                #f
                (any-elt? even? (vector 1 3 5 7)))

(add-equal-test '<string><vector>
                #t
                (any-elt? even? (vector 1 3 4 7)))

(add-equal-test '<string><vector>
                #t
                (any-elt? (lambda (c) (char=? c #\c))
                          "abcdef"))

(add-equal-test '<string><vector>
                #f
                (any-elt? (lambda (c) (char=? c #\c))
                          "abdef"))

(add-equal-test '<string><vector>
                #t
                (contains-elt? "abcdef" #\c))

(add-equal-test '<string><vector>
                #f
                (contains-elt? "abdef" #\c))

(add-equal-test '<string><vector>
                #t
                (contains-elt? (vector 1 2 3 4 5) 3))


(add-equal-test '<string><vector>
                #f
                (contains-elt? (vector 1 2 3 4 5) 9))

(add-equal-test '<string><vector>
                #t
                (elts=? "abc" "abc"))

(add-equal-test '<string><vector>
                #t
                (elts=? (vector 1 2 3) (vector 1 2 3)))

(add-equal-test '<string><vector>
                #f
                (elts=? "abc" "acb"))

(add-equal-test '<string><vector>
                #f
                (elts=? (vector 1 2 3) (vector 2 1 3)))

(add-equal-test '<string><vector>
                #t
                (elts=? "abc" (list #\a #\b #\c)))

(add-equal-test '<string><vector>
                #t
                (elts=? "abc" (vector #\a #\b #\c)))

(add-equal-test '<string><vector>
                #t
                (elts=? (vector #\a #\b #\c)
                        (list #\a #\b #\c)))

(add-equal-test '<string><vector>
                (vector #\a #\b #\c)
                (as <vector> (list #\a #\b #\c)))

(add-equal-test '<string><vector>
                (vector #\a #\b #\c)
                (as <vector> "abc"))

(add-equal-test '<string><vector>
                "abc"
                (as <string> (list #\a #\b #\c)))

(add-equal-test '<string><vector>
                "abc"
                (as <string> (vector #\a #\b #\c)))

(add-equal-test '<string><vector>
                (vector 1 3 5)
                (collect odd? (vector 0 1 2 3 4 5 6)))

(add-equal-test '<string><vector>
                "aei"
                (collect (lambda (c) (memq c '(#\a #\e #\i #\o #\u)))
                         "abcdefghijklmn"
                         <string>))

(add-equal-test '<string><vector>
                (list #\a #\e #\i)
                (collect (lambda (c) (memq c '(#\a #\e #\i #\o #\u)))
                         "abcdefghijklmn"
                         <list>))

(add-equal-test '<string><vector>
                (vector #\a #\e #\i)
                (collect (lambda (c) (memq c '(#\a #\e #\i #\o #\u)))
                         "abcdefghijklmn"
                         <vector>))

(add-equal-test '<string><vector>
                "bcdfghjklmn"
                (reject (lambda (c) (memq c '(#\a #\e #\i #\o #\u)))
                         "abcdefghijklmn"
                         <string>))

(add-equal-test '<string><vector>
                "abcd"
                (add-elt "abc" #\d))

(add-equal-test '<string><vector>
                "abcdef"
                (add-elts "abc" (list #\d #\e #\f)))

(add-equal-test '<string><vector>
                "abcdef"
                (add-elts "abc" "def"))

(add-equal-test '<string><vector>
                "abcdef"
                (add-elts "abc" (vector #\d #\e #\f)))

(add-equal-test '<string><vector>
                (vector 0 1 2 3)
                (add-elt (vector 0 1 2) 3))

(add-equal-test '<string><vector>
                (vector 0 1 2 3)
                (add-elts (vector 0) (list 1 2 3)))

(add-equal-test '<string><vector>
                (vector 0 1 2 3)
                (add-elts (vector 0) (vector 1 2 3)))

(add-equal-test '<string><vector>
                "abcef"
                (remove-elt "abcdef" #\d))

(add-equal-test '<string><vector>
                "abcf"
                (remove-elts "abcdef" "de"))

(add-equal-test '<string><vector>
                "abcf"
                (remove-elts "abcdef" (list #\d #\e)))

(add-equal-test '<string><vector>
                "abcf"
                (remove-elts "abcdef" (vector #\d #\e)))

(add-equal-test '<string><vector>
                #\b
                (elt-ref "abcde" 1))

(add-equal-test '<string><vector>
                1
                (elt-ref (vector 0 1 2) 1))

(add-equal-test '<string><vector>
                #\b
                (elt-ref-or "abcde" 1 #f))

(add-equal-test '<string><vector>
                #f
                (elt-ref-or "abcde" 17 #f))

(add-equal-test '<string><vector>
                2
                (elt-ref-or (vector 0 1 2 3) 2 #f))

(add-equal-test '<string><vector>
                #f
                (elt-ref-or (vector 0 1 2 3) 17 #f))

(add-equal-test '<string><vector>
                (vector 0 "one" 2)
                (elt-set! (vector 0 1 2) 1 "one"))

(add-equal-test '<string><vector>
                "axc"
                (elt-set! "abc" 1 #\x))

(add-equal-test '<string><vector>
                "cba"
                (reverse-elts "abc"))

(add-equal-test '<string><vector>
                (vector 3 2 1)
                (reverse-elts (vector 1 2 3)))

(add-equal-test '<string><vector>
                (vector 2 3 4)
                (slice (vector 0 1 2 3 4 5 6 7 8) 2 5))

(add-equal-test '<string><vector>
                "little"
                (slice "a little test" 2 8))

;;==========================================================
(add-test-suite '<dictionary>)

(define htable(<hash-table>))

;; Populate the table.

(let loop ( (n (char->integer #\a)) )
  (let ( (str (string (integer->char n) (integer->char (+ 1 n)))) )
    (elt-set! htable (string->symbol str) str)
    (if (< n (char->integer #\y)) (loop (+ 1 n)))))

(let loop ( (n (char->integer #\A)) )
  (let ( (str (string (integer->char n) (integer->char (+ 1 n)))) )
    (elt-set! htable (string->symbol str) str)
    (if (< n (char->integer #\Y)) (loop (+ 1 n)))))

(let loop ( (n (char->integer #\b)) )
  (let ( (str (string (integer->char n) (integer->char (- n 1)))) )
    (elt-set! htable (string->symbol str) str)
    (if (< n (char->integer #\z)) (loop (+ 1 n)))))

(let loop ( (n (char->integer #\B)) )
  (let ( (str (string (integer->char n) (integer->char (- n 1)))) )
    (elt-set! htable (string->symbol str) str)
    (if (< n (char->integer #\Z)) (loop (+ 1 n)))))

(add-equal-test '<dictionary>
                100
                (num-elts htable))

(add-equal-test '<dictionary>
                "ab"
                (elt-ref htable 'ab))

(add-equal-test '<dictionary>
                "CeeDee"
                (begin
                  (elt-set! htable 'cd "CeeDee")
                  (elt-ref htable 'cd)))
                
(add-equal-test '<dictionary>
                'not-found
                (begin
                  (remove-key! htable 'cd)
                  (elt-ref-or htable 'cd 'not-found)))

(add-equal-test '<dictionary>
                99
                (num-elts htable))

(add-equal-test '<dictionary>
                "SeeDee"
                (begin
                  (add-key-elt! htable 'cd "SeeDee")
                  (elt-ref-or htable 'cd 'not-found)))

(add-equal-test '<dictionary>
                (num-elts htable)
                (let ( (count 0) )
                  (for-each-elt (lambda (elt) (set! count (+ count 1)))
                                htable)
                  count))

(add-equal-test '<dictionary>
                (num-elts htable)
                (length (fold-left cons '() htable)))

(define small-table
  (->collection <hash-table>
                '(foo . "the big foo")
                '(bar . "cheers")
                '(baz . "Get the Buzz")
                '(quux . "Moby Quux")))

(add-equal-test '<dictionary>
                #t
                (elts=? small-table (fold-left cons '() small-table)))

(add-equal-test '<dictionary>
                #t
                (every-elt? string? small-table))

(add-equal-test '<dictionary>
                #f
                (every-elt? (lambda (elt) (<= (string-length elt) 11))
                            small-table))

(add-equal-test '<dictionary>
                #t
                (every-elt? (lambda (elt) (<= (string-length elt) 12))
                            small-table))

(add-equal-test '<dictionary>
                #t
                (any-elt? (lambda (elt) (= (string-length elt) 12))
                          small-table))

;; Now same tests for <ts-table>s

(define ts-table (<ts-table>))

;; Populate the table.

(let loop ( (n (char->integer #\a)) )
  (let ( (str (string (integer->char n) (integer->char (+ 1 n)))) )
    (elt-set! ts-table (string->symbol str) str)
    (if (< n (char->integer #\y)) (loop (+ 1 n)))))

(let loop ( (n (char->integer #\A)) )
  (let ( (str (string (integer->char n) (integer->char (+ 1 n)))) )
    (elt-set! ts-table (string->symbol str) str)
    (if (< n (char->integer #\Y)) (loop (+ 1 n)))))

(let loop ( (n (char->integer #\b)) )
  (let ( (str (string (integer->char n) (integer->char (- n 1)))) )
    (elt-set! ts-table (string->symbol str) str)
    (if (< n (char->integer #\z)) (loop (+ 1 n)))))

(let loop ( (n (char->integer #\B)) )
  (let ( (str (string (integer->char n) (integer->char (- n 1)))) )
    (elt-set! ts-table (string->symbol str) str)
    (if (< n (char->integer #\Z)) (loop (+ 1 n)))))

(add-equal-test '<dictionary>
                100
                (num-elts ts-table))

(add-equal-test '<dictionary>
                "ab"
                (elt-ref ts-table 'ab))

(add-equal-test '<dictionary>
                "CeeDee"
                (begin
                  (elt-set! ts-table 'cd "CeeDee")
                  (elt-ref ts-table 'cd)))
                
(add-equal-test '<dictionary>
                'not-found
                (begin
                  (remove-key! ts-table 'cd)
                  (elt-ref-or ts-table 'cd 'not-found)))

(add-equal-test '<dictionary>
                99
                (num-elts ts-table))

(add-equal-test '<dictionary>
                "SeeDee"
                (begin
                  (add-key-elt! ts-table 'cd "SeeDee")
                  (elt-ref-or ts-table 'cd 'not-found)))

(add-equal-test '<dictionary>
                (num-elts ts-table)
                (let ( (count 0) )
                  (for-each-elt (lambda (elt) (set! count (+ count 1)))
                                ts-table)
                  count))

(add-equal-test '<dictionary>
                (num-elts ts-table)
                (length (fold-left cons '() ts-table)))

(define small-ts-table
  (->collection <hash-table>
                '(foo . "the big foo")
                '(bar . "cheers")
                '(baz . "Get the Buzz")
                '(quux . "Moby Quux")))

(add-equal-test '<dictionary>
                #t
                (elts=? small-ts-table (fold-left cons '() small-ts-table)))

(add-equal-test '<dictionary>
                #f
                (elts=? small-ts-table small-table))

(add-equal-test '<dictionary>
                #t
                (elts=? small-ts-table small-table string=?))

(add-equal-test '<dictionary>
                #t
                (every-elt? string? small-ts-table))

(add-equal-test '<dictionary>
                #f
                (every-elt? (lambda (elt) (<= (string-length elt) 11))
                            small-ts-table))

(add-equal-test '<dictionary>
                #t
                (every-elt? (lambda (elt) (<= (string-length elt) 12))
                            small-ts-table))

(add-equal-test '<dictionary>
                #t
                (any-elt? (lambda (elt) (= (string-length elt) 12))
                          small-ts-table))

;; @@@

#|
(add-equal-test 'collections
                expected
                actual)

(ensure-exception-raised
    'collections
    error-exception?
    actual
    "Mumble")
|#

;;(run-tests-for 'collections)

'test-collections

;===========================E=O=F==============================;
