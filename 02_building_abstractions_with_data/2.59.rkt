#lang sicp

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define set1 (list 1 2 3))
(define set2 (list 4 5 6))
(define set3 (list 3 4 5))

(element-of-set? 1 set1 )
(element-of-set? 5 set1 )

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(adjoin-set 9 set1)
(adjoin-set 1 set1)

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1) (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

(intersection-set set1 set2)
(intersection-set set1 set3)
