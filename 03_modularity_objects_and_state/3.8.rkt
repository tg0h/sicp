#lang sicp

(define (f x)
  (define called 0)
  (lambda ()
    (set! called (+ called 1))
    (if (and (= called 2) (= x 1)) 0 x)
    ;; y
    ;; (cond
    ;;   ((= x 0)
    ;;    (set! y x)
    ;;    y
    ;;    )
    ;;   ((= x 1)
    ;;    ;; (set! y (+ y 1))
    ;;    0
    ;;    )
    ;;   )
    )
  )


(+ (f 0) (f 1))
