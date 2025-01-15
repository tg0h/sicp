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
(define (pred)
  (<
   (+
    (square (- (random1) 1))
    (square (- (random1) 1))
    )
   1
   )
  )
;; (define (estimate-integral P x1 x2 y1 y2 trials)
;;
;;   )

(pred)

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

