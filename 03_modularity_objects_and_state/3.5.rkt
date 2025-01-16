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


;; (random-in-range 5.0 10.0)


(experiment)

(define (estimate-integral pred x1 x2 y1 y2 trials)
  (monte-carlo trials pred )
(define (pred x1 x2 y1 y2)
  (define (square x) (* x x))
  (define (random-in-range low high)
    (define range (- high low))
    (+ low (random range))
    )
  (define (experiment)
    (let (
          (rand_x (random-in-range x1 x2))
          (rand_y (random-in-range y1 y2))
          )
      (display rand_x)
      (newline)
      (display rand_y)
      (newline)
      (< (+ (square (- rand_x 1))
            (square (- rand_y 1)))
         1)))
  experiment
  )
  )

;; (pred)


