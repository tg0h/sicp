#lang sicp

(define (sum term a next b)
  ;; (display "term ")
  ;; (display a)
  ;; (display b)
  ;; (display "=")
  ;; (display (term a))
  ;; (newline)
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


(define (simpsons-rule f a b n)
  (define (h n) (/ (- b a) n))
  (define (next current) (+ current 1))
  (define (y-term k)
    ;; (display "k:") (display k)
    ;; (newline)
    (cond
      ((= k 0) (f a))
      ((= k n) (f (+ a (* k (/ (- b a) n)))))
      ((even? k) (* 2 (f (+ a (* k (/ (- b a) n))))))
      (else (* 4 (f (+ a (* k (/ (- b a) n))))))
      )
    )
  ;; (display (h n))
  (/ (* (sum y-term a next n) (h n)) 3)
  ;; (sum y-term a next n)
  )

;; (simpsons-rule cube 0 1 2)
(simpsons-rule cube 0 1 100)
(simpsons-rule cube 0 1 1000)
;; (y-term cube 0 1 100 1)
