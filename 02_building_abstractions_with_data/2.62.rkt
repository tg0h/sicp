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

;; (car (list 1))
;; (adjoin-set 7 (list 1))
;; (adjoin-set 7 (cdr (list 1)))
;; (cdr (list 1))
;; (cons (car (list 1))
;; (adjoin-set 1 set1)
;; (adjoin-set 999 set1)

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1) (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

;; (intersection-set set1 set3)
;; (intersection-set set1 set3)

(define (union-set set1 set2)
  (define (iter result _list)
    (cond
      ((null? _list) result)
      ((element-of-set? (car _list) result) ;already part of result, do not need to add to result
       (iter result (cdr _list)))
      (else
       (iter (append (list (car _list)) result) (cdr _list))
       )
      )
    )
  (iter '() (append set1 set2))
  )

;; (union-set set1 set2)
;; (union-set (list 1 2 3) (list 2 3 4))
;; (append set1 set2)


(define (_union-set set1 set2)
  (cond
    ((null? set2) set1)
    ((null? set1) set2)
    ((= (car set1) (car set2)) (cons (car set1) (_union-set (cdr set1) (cdr set2))))
    ((< (car set1) (car set2)) (cons (car set1) (_union-set (cdr set1) set2)))
    (else
     (cons (car set2) (_union-set set1 (cdr set2)))
     )
    )
  )

(_union-set (list 1 2 3) '())
(_union-set '() (list 4 5 6))
(_union-set (list 1 2 3 4) (list 4 5))
(_union-set (list 7 8 9) (list 4 5))
