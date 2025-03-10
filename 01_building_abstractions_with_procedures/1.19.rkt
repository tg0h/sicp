#lang sicp

(define (fib n)
  (fib-iter 1 0 0 1 n))
(define (fib-iter a b p q count)
  (display a)
  (display ":" )
  (display b)
  (display ":" )
  (display p)
  (display ":" )
  (display q)
  (display ":" )
  (display count)
  (newline)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter a
                   b
                   (+ (square p) (square q))
                   (+ (* 2 p q) (square q))
                   (/ count 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))))

(define (square x) (* x x))



(fib 128)


