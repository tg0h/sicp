#lang sicp

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        ((< x (car set)) false) ; early return for ordered sets
        (else (element-of-set? x (cdr set)))))


(define set1 (list 1 2 3 7 8 9))
(define set2 (list 4 5 6))

;; (element-of-set? 6 set1)
;; (define set3 (list 3 3 3))

;; (element-of-set? 1 set1 )
;; (element-of-set? 5 set1 )

(define (adjoin-set x set)
  (cond
    ((null? set) (list x))
    ((= x (car set)) set)
    ((< x (car set)) (cons x set))
    (else (cons (car set) (adjoin-set x (cdr set))))
    )
  )

(car (list 1))
;; (adjoin-set 7 (list 1))
;; (adjoin-set 7 (cdr (list 1)))
;; (cdr (list 1))
;; (cons (car (list 1))
(adjoin-set 1 set1)
(adjoin-set 999 set1)

