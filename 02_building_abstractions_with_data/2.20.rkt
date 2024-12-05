#lang sicp

(define (even? n) (= (remainder n 2) 0))

(define (even-list l)
  (cond
    ((null? l) (list nil))
    ((even? (car l)) (cons((car l) (even-list (cdr l)))))
    (else (even-list (cdr l)))
    )
  )

(even-list (list 2))
;; (null? 2)


;; (cons (car (list 2)) (cdr (list 2)))
;; (cons 2 nil)
;; (cdr (list 2))
;; (cdr (list 1 2))

;; (define (same-parity l)
;;   (if (even? (car l) )
;;       )

(define (same-parity first . rest)
  first
  )
