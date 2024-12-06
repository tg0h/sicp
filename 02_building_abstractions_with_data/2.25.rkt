#lang sicp

(define x (list 1 3 (list 5 7 ) 9))

(cdr x)
(cdr(cdr x))
(car(cdr(cdr x)))
(cdr(car(cdr(cdr x))))
(car(cdr(car(cdr(cdr x)))))

(newline)
(define y (list (list 7)))
y
(car(car y))
