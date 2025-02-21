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


(define (pred x1 x2 y1 y2)
  (define (square x) (* x x))
  (define (random-in-range low high)
    (define range (- high low))
    (+ low (random range))
    )
  (lambda ()
    (let (
          (rand_x (random-in-range x1 x2))
          (rand_y (random-in-range y1 y2))
          )
      ;; (display rand_x)
      ;; (newline)
      ;; (display rand_y)
      ;; (newline)
      (< (+ (square (- rand_x 1))
            (square (- rand_y 1)))
         1)))
  )

(define (estimate-integral pred x1 x2 y1 y2 trials)
  (* (monte-carlo trials (pred x1 x2 y1 y2)) 4.0)
  )

(estimate-integral pred 0.0 2.0 0.0 2.0 10000)

;; (pred)


(define random-numbers
  (cons-stream
   random-init
   (stream-map rand-update random-numbers)))

(define cesaro-stream
  (map-successive-pairs
   (lambda (r1 r2) (= (gcd r1 r2) 1))
   random-numbers))

(define (map-successive-pairs f s)
  (cons-stream
   (f (stream-car s) (stream-car (stream-cdr s)))
   (map-successive-pairs f (stream-cdr (stream-cdr s)))))

(define
  (monte-carlo experiment-stream passed failed)
  (define (next passed failed)
    (cons-stream
     (/ passed (+ passed failed))
     (monte-carlo
      (stream-cdr experiment-stream) passed failed)))
  (if (stream-car experiment-stream)
      (next (+ passed 1) failed)
      (next passed (+ failed 1))))

(define pi
  (stream-map
   (lambda (p) (sqrt (/ 6 p))) (monte-carlo cesaro-stream 0 0)))
