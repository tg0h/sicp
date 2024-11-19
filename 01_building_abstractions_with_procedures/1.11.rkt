#lang sicp

(define (f n)
  (cond ((< n 3) n)
        (else (+ (f (- n 1))
                 (* 2 (f (- n 2)))
                 (* 3 (f (- n 3)))
                 )
              )
        )
  )

;; (f 1)
;; (f 2)
;; (f 3)
;; (f 4)
;; (f 5)


(define (sum a b c) (+ a (* 2 b) (* 3 c)))
(define (f-iter counter n a b c)
  (cond
    ((= counter n) (sum a b c))
    (else (f-iter (+ counter 1) n (sum a b c) a b))
    )
  )


(f-iter 3 4 2 1 0)
;; (sum 2 1 0)
;; (f-iter 4 4 4 2 1)
;; (sum 2 1 0)
