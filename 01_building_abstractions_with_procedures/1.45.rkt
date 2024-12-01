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


(define (power x n)
  (if (= n 1)
      x
      (* x (power x (- n 1)))))

(define (root-n a n)
  (fixed-point (lambda (x) (/ a (power x (- n 1 )))) 1.0)
  )

;; (root-n 2 2)

(define (average-damp-repeated n)
  (repeated average-damp n)
  )

(define identity
  (lambda (x) (* 2 x))
  )

;; ((average-damp (lambda(x) x)) 1)

;; (( (average-damp-repeated 1)  identity ) 1)
;; (( (average-damp-repeated 1)  (lambda(x) x) ) 1)

;; (compose
;;  (average-damp-repeated
;;   (lambda (x) x))
;;  )(1)
;; ((repeated average-damp 1)(lambda (x) x))(2)

(define (neg-power a n )
  (lambda (x) (/ a (power x (- n 1 ))))
  )

(define (root-n-average-damp a n repeat)
  (fixed-point
   ((average-damp-repeated repeat)
    (lambda (x) (/ a (power x (- n 1 ))))
    ;; (neg-power a n)
    ) 1.0
      )
  )
(log 16 2)

(define (root-n-correct a n)
  (fixed-point
   ((average-damp-repeated (floor (log n 2)))
    (lambda (x) (/ a (power x (- n 1 ))))
    ;; (neg-power a n)
    ) 1.0
      )
  )

(root-n-correct 2 16)
(newline)
;; (define (nth-root-damped x nth damp)
;;   (fixed-point
;;    ((repeated average-damp damp)
;;     (lambda (y)
;;       (/ x (power y (- nth 1)))))
;;    1.0))

;; (root-n-average-damp 2 2 1)
;; (root-n-average-damp 2 3 1)
;; (root-n-average-damp 2 4 2)
;; (root-n-average-damp 2 5 2)
;; (root-n-average-damp 2 6 2)
;; (root-n-average-damp 2 7 2)
;; (root-n-average-damp 2 8 3)
;; (root-n-average-damp 2 9 3)
;; (root-n-average-damp 2 15 3)
(root-n-average-damp 2 16 4)


;; (define (sqrt-cyclic a) (fixed-point (lambda (x) (/ a x)) 1.0))
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

;; (root-3 2)

(define (average-damp-n f n)
  (repeated average-damp n) f
  )


;; (define (root-4 x 4)
;;   (fixed-point (average-damp-1 (lambda (y) (/ x (power y (- ))  )))
;;                1.0))
;; (root-4 2) ; does not work

;; (define (root-n x n damp)
;;   (fixed-point (average-damp-n (lambda (y) (/ x (power y (- n 1))))
;;                                damp
;;                                ) 1.0)
;;   )
;;
;; (root-n 2 2 1)

