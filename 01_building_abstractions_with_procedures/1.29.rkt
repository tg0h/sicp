#lang sicp

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (integral f a b dx)
  (define (add-dx x)
    (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))


(define (cube x ) (* (* x x) x))

(integral cube 0 1 0.01)

(define (even? n)
  (= (remainder n 2) 0))

(define (y-term f a k h n)
  (cond
    ((= k 1) (f (+ a (* k h))))
    ((= k n) (f (+ a (* k h))))
    ((even? k) (* 2 (f (+ a (* k h)))))
    (else (* 4 (f (+ a (* k h)))))
    )
  )
