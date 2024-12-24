#lang sicp

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define set1 (list 3 2 1))
(define set2 (list 6 4 5))
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

(define (union-set set1 set2)
  (define (iter result _list)
    (cond
      ((null? _list) result)
      ( (element-of-set? (car _list) result)
        (iter result (cdr _list))
        (iter (append (list (car list)) result) (cdr _list))
        )
      )
    )
  (iter '() (append set1 set2))
  )
