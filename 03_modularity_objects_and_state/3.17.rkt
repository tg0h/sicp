#lang sicp

(define counted '())

(define (count-pairs x)
  (define (counted-before? x search-list)
    (cond ((null? search-list) false)
          ((eq? x (car search-list)) true)
          (else (counted-before? x (cdr search-list))))
    )
  (define (count x)
    (cond
      ((not (pair? x)) 0)
      ((counted-before? x counted)
       (+ (count (car x))
          (count (cdr x))))
      (else
       (set! counted (cons x counted))
       (+ (count (car x))
          (count (cdr x))
          1 ))))
  (count x )
  )

(define l3 '( a b c))
(define a '(a ))

(define aa (cons a a))

(count-pairs aa)

(count-pairs (cons (cons a a) (cons a a))) ; displays 3
; the first cons a a is a distinct object different from the second cons a a

(define _a (cons a a))
; _a and _a are the same object, so only has 2
(count-pairs (cons _a _a)) ;displays 2


(define ab '(a b))
(define z (cons ab (cdr ab)))
(count-pairs z) ; returns 4
