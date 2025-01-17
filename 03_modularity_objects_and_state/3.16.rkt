#lang sicp

(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))

(define l3 '( a b c))

;; l3

;; (count-pairs l3) ; 3
;; (count-pairs (cons (list 'a) (list 'b))) ; also 3

(define ab '(a b))

(define z (cons ab (cdr ab)))
;; (set-cdr! z (cdr ab))

(car z)
(cdr z)
(count-pairs z)

;; (count-pairs (cons ab ab)) ; 5

(define my-structure (cons (cons 1 2) (cons (cons 3 4) (cons 5 6))))

(count-pairs my-structure)

