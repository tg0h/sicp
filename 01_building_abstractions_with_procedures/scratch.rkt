#lang sicp

(define x (list 1 2 3))
(define y (list 4 5 6))

;; (list 1 2 3)
;; (1 2 3)
;; (append x y)
(cons nil y)
(car (cons nil y))
(cdr (cons x y))
(cdr (cons 1 nil))
(cdr (cons 1 nil))
(display '())
;; (list x y)

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (permutations s)
  (if (null? s) ;emptyset?
      (list nil) ;sequencecontainingemptyset
      (flatmap (lambda (x)
                 (map (lambda (p) (cons x p)) (permutations (remove x s)))
                 )
               s))
  )

