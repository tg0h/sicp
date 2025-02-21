#lang sicp

;; (define (rand)
;;   (random 10000)
;;   )


;; (define rand
;;   (let ((x random-init))
;;     (lambda ()
;;       (set! x (rand-update x)) x)))

(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))

;; (define
;;   (stream-map proc . argstreams)
;;   (if (null? (car argstreams))
;;       the-empty-stream
;;       (cons-stream
;;        (apply proc (map stream-car argstreams))
;;        (apply stream-map
;;               (cons proc (map stream-cdr argstreams))))))

(define (stream-map proc s)
  (if (stream-null? s) the-empty-stream
      (cons-stream (proc (stream-car s))
                   (stream-map proc (stream-cdr s)))))

(define init 11)

(define random-numbers
  (cons-stream
   init
   (stream-map rand-update random-numbers)))

(define (rand-update x)
  (remainder (+ (* 173 x) 33) 101))
;; (define rand
;;   (let ((x random-init))
;;     (lambda ()
;;       (set! x (rand-update x)) x))
;;   )

(define x 11) ; random seed

(define rand
  (let ((x 11))
    (lambda (m)
      (cond ((eq? m 'generate)
             ;; (set! x (rand-update x))
             random-numbers
             ;; x
             )
            ((eq? m 'reset)

             (lambda (init)
               (define random-numbers
                 (cons-stream
                  init
                  (stream-map rand-update random-numbers)))
               random-numbers
               ;; (set! x x-new)
               )
             )
            (else (error "Unknown request: RAND" m))))))


(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define y random-numbers )



(define z (rand 'generate))
(stream-ref z 0)
(stream-ref z 1)
(stream-ref z 2)
;; (rand 'generate)
((rand 'reset) 12)
;; (stream-cdr ((rand 'reset) 11))
;; (stream-car (stream-cdr ((rand 'reset ) 11)))

;; (rand 'generate)
;; (rand 'generate)
;; (rand 'generate)

