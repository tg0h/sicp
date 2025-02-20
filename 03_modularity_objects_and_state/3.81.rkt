#lang sicp

(define (rand)
  (random 10000)
  )


(define rand
  (let ((x random-init))
    (lambda ()
      (set! x (rand-update x)) x)))

(define random-numbers
  (cons-stream
   random-init
   (stream-map rand-update random-numbers)))

(define (rand-update x)
  (remainder (+ (* 173 x) 33) 101))
;; (define rand
;;   (let ((x random-init))
;;     (lambda ()
;;       (set! x (rand-update x)) x))
;;   )

(define x 11) ; random seed

;; (define (rand action)
;;   (let ((x 11))
;;     (cond
;;       ((eq? action 'generate)
;;        (begin
;;          ;; (display "x is" )
;;          ;; (display x)
;;          ;; (newline)
;;          (set! x (rand-update x))
;;          x
;;          )
;;        )
;;       ((eq? action 'reset)
;;        (lambda (new-value)
;;          (set! x (rand-update new-value)) x))
;;       )
;;     )
;;   )
(define rand
  (let ((x 11))
    (lambda (m)
      (cond ((eq? m 'generate)
             (set! x (rand-update x)) x)
            ((eq? m 'reset)
             (lambda (x-new) (set! x x-new)))
            (else (error "Unknown request: RAND" m))))))


;; (rand-update 1)
;; (rand-update 4)
(rand 'generate)
(rand 'generate)
(rand 'generate)
((rand 'reset ) 11)
(rand 'generate)
(rand 'generate)
(rand 'generate)

