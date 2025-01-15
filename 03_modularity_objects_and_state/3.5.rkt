#lang sicp

(define (rand)
  (random 10000)
  )

(define (estimate-pi trials)
  (sqrt (/ 6 (monte-carlo trials cesaro-test))))

(define (cesaro-test)
  (= (gcd (rand) (rand)) 1))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1)
                 (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1)
                 trials-passed))))
  (iter trials 0))




;; (monte-carlo 10000 cesaro-test)



(define (random1)
  (let (
        (r (random 10000))
        )
    (/ r 10000.0)
    )
  )

(define (square x) (* x x))

;; (define (pred x1 x2 y1 y2)
;;   lambda
;;   (let (
;;         (rand_x (random-in-range x1 x2))
;;         (rand_y (random-in-range y1 y2))
;;         )
;;     (<
;;      (+
;;       (square (- rand_x 1))
;;       (square (- rand_y 1))
;;       )
;;      1
;;      )
;;     )
;;   )


(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))
(define (estimate-integral P x1 x2 y1 y2 trials)
  (monte-carlo trials pred )
  )

;; (pred)


