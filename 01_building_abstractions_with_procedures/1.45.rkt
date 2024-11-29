#lang sicp

(define tolerance 0.00001)

(define (abs x)
  (cond
    ((> x 0) x)
    ((= x 0) 0)
    ((< x 0) (- x))
    )
  )

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess)
    (display guess)
    (newline)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))


(define (average x y) (/ (+ x y) 2))
;; average
(define (average-damp f)
  (lambda(x) (average x (f x))))



(define (sqrt-cyclic a) (fixed-point (lambda (x) (/ a x)) 1.0))
;; (sqrt-cyclic 2)


(define (fixed-point-transform f transform)
  (fixed-point (transform f) 1.0)
  )

;; (define (sqrt a)
;;   (fixed-point-transform (lambda (x) (/ a x)) average-damp)
;;   )

(define (sqrt x)
  (fixed-point (average-damp (lambda (y) (/ x y)))
               1.0))

;; (sqrt 2)

(define (cubert x)
  (fixed-point (average-damp (lambda (y) (/ x (* y y)  )))
               1.0))

;; (cubert 2)


(define (root-3 x)
  (fixed-point (average-damp (lambda (y) (/ x (* y y)  )))
               1.0))

(root-3 2)

(define (root-4 x)
  (fixed-point (average-damp (lambda (y) (/ x (* y y y)  )))
               1.0))

(root-4 2)

;; (define (root-4 a) (fixed-point (lambda (x) (/ a (x * x )) 1.0)))




(define (root-n a n)
  (fixed-point (lambda (x) (neg-power a (- n 1))) 1.0)
  )

(define (power a n) (if (= n 1) a (* a (power a (- n 1)))))
(define (neg-power a n) (/ 1 (power a n)))

;; (root-n 2 2)
;; (neg-power 2 (- 2 1))



(define (compose f g) (lambda (x) (f (g x))))
(define (repeated f n)
  (define (iter i)
    (if (= i 1)
        f
        (compose f (iter (- i 1)))
        )
    )
  (iter n)
  )

;; (root-n 2 2) ; square root of 2
;; (root-n 2 3) ; cube root of 2
;; (root-n 2 2) ; 4th root of 2


;; (define (root-n a)
;;   lambda (x) (/ a ()
;;                 )
;;
;;   (sqrt 2)
