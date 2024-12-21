#lang sicp


(list 'a 'b 'c)
;; (list a b c)

(list (list 'george))

(define a 1)
(define b 2)
(car '(a b c))
(car (list 'a 'b))

;; (cdr '((x1 x2) (y1 y2)))
;; (cadr '((x1 x2) (y1 y2)))
;; (pair? (car '(a short list)))

;; (memq 'red '((red shoes) (blue socks)))

(memq 'red '(red shoes blue socks))
