#lang sicp

(define (dummy x)
  (display "I was called with ")
  (display x)
  (newline)
  )

;; (define (make-monitored function)
;;    (define times-called 0)
;;    (define (mf message)
;;      (cond ((eq? message 'how-many-calls?) times-called)
;;            ((eq? message 'reset-count) (set! times-called 0))
;;            (else (set! times-called (+ times-called 1))
;;                  (function message))))
;;    mf)

(define (make-monitored f)
  (let ((counter 0)) ;; don't like nesting, use define instead of let
    (define (proxy x)
      (cond
        ((eq? x 'how-many-calls? ) counter)
        ((eq? x 'reset-count ) (set! counter 0))
        (else
         (set! counter (+ counter 1))
         (f x)
         )))
    proxy))

(define m (make-monitored dummy))
(m 1)
(m 2)
(m 'how-many-calls?)
(m 'reset-count)
(m 'how-many-calls?)
