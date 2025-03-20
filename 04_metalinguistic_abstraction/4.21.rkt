#lang sicp

; a

;; ((lambda (n)
;;    (
;;     (lambda (fact) (fact fact n))
;;     (lambda (ft k)
;;       (if (= k 1)
;;           1
;;           (* k (ft ft (- k 1))))
;;       ))
;;    ) 4)

;; ((lambda (n)
;;    ((lambda (fib) (fib fib n))
;;     (lambda (fb n)
;;       (cond
;;         ((= n 0) 0)
;;         ((= n 1) 1)
;;         (else (+ (fb fb (- n 2)) (fb fb (- n 1))))
;;         )))) 10)

(define (fib n)
  (cond
    ((= n 0) 0)
    ((= n 1) 1)
    (else (+
           (fib (- n 2) )
           (fib (- n 1) )
           ))))

;; (fib 10)

; b

(define (f x)
  (define (even? n)
    (if (= n 0) true (odd? (- n 1))))
  (define (odd? n)
    (if (= n 0) false (even? (- n 1))))
  (even? x))

(define (_f x)
  ((lambda (even? odd?) (even? even? odd? x))
   (lambda (ev? od? n)
     (if (= n 0) true (od? ev? od? (- n 1 ))))
   (lambda (ev? od? n)
     (if (= n 0) false (ev? ev? od? (- n 1))))))

;; (_f 10)

; y combinator example

;non-recursive factorial function
(define fact-once
  (lambda (f)
    (lambda (n)
      (if (= n 0)
          1
          (* n (f (- n 1)))))))

;y-combinator
(define Y
  (lambda (f)
    ((lambda (x) (x x))
     (lambda (x) (f (lambda (y) ((x x) y)))))))

(define factorial (Y fact-once))
(factorial 20)  ;=2432902008176640000
