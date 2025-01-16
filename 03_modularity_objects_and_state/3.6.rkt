#lang sicp

(define (rand-update x)
  (remainder (+ (* 173 x) 33) 101))
;; (define rand
;;   (let ((x random-init))
;;     (lambda ()
;;       (set! x (rand-update x)) x))
;;   )

(define (rand action)
  (define x random-init)
  (cond
    ((eq? action 'generate)
     (lambda ()
       (set! x (rand-update x)) x)
     )
    ((eq? action 'reset)
     (lambda (new-value)
       (set! x (rand-update new-value)) x))
    )
  )

