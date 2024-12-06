#lang sicp

(list 1 (list 2 (list 3 4)))

(list 1 2 3 4 )

(define x(list 1 (list 2 (list 3 4))))

(car x )
(car (car (cdr x )))
