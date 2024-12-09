#lang sicp

(define (make-mobile left right) (list left right))
(define (make-branch length structure) (list length structure))

(define (left-branch mobile) (car mobile))
(define (right-branch mobile) (car(cdr mobile)))

(define (branch-length branch) (car branch))
(define (branch-structure branch) (cdr branch))


(define branch-a (make-branch 10 5))
(define branch-b (make-branch 2 1))
(define branch-c (make-branch 3 4))


(make-mobile branch-a branch-b)


;; (define (total-weight mobile)
;;   (cond
;;     ((not (pair? (cdr mobile))) (cdr mobile))
;;     (else (+ (total-weight (left-branch mobile) (total-weight (right-branch mobile)))))
;;     )
;;   )




