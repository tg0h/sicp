#lang sicp

(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))

(define l3 '( a b c))
;; (count-pairs l3) ; 3
;; (count-pairs (cons (list 'a) (list 'b))) ; also 3

(define ab '(a b))
(define z (cons ab (cdr ab)))
(count-pairs z) ; returns 4

(define abab (cons ab ab))
;; abab
;; (count-pairs (cons ab ab))
;; (count-pairs ab)

;; aa
;; (cons aa aa)
(count-pairs (cons ab ab)) ; 5


(define a '(a))
(count-pairs (cons (cons a a) (cons a a))) ; 7
(define _a (cons a a))
(count-pairs (cons _a _a))


;; (define inf '( a b c))
;; (set-cdr! (cddr inf) inf)
;; inf

;; (count-pairs inf)
