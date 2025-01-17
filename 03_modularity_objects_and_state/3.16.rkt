#lang sicp

(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))

(define l3 '( a b c))

l3

(count-pairs l3) ; 3

(count-pairs (cons (list 'a) (list 'b))) ; also 3
