#lang sicp

;; (eq? 'a 'a)
;; (eq? 1 2)

(define (equal? a b)
  (cond
    ((and (not (pair? a)) (not (pair? b))) (eq? a b))
    ((and (pair? a) (pair? b)) (and (equal? (car a) (car b)) (equal? (cdr a) (cdr b))))
    (else false)
    )
  )

(equal? '(this is a list) '(this is a list))
(equal? '(1 is a list) '(1 is a list))
(equal? '(this is a list) '(this (is a) list))
