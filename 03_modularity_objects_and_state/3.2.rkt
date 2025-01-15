#lang sicp

(define (dummy x)
  (display "I was called with ")
  (display x)
  (newline)
  )

(define (make-monitored f)
  (let
      ((counter 0))
    (define (proxy x)
      (cond
        ((x eq? 'how-many-calls? ) counter)
        ((x eq? 'reset-count ) (set! counter 0))
        (else
         (begin
           (set! counter (+ counter 1))
           (f x)
           )
         )
        )
      )
    proxy
    )
  )

(make-monitored dummy)
(dummy 1)
(dummy 2)
(dummy 'how-many-calls?)
