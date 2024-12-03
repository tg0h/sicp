#lang sicp


(define (make-rat n d) 
  (let ((g (gcd n d)))
  (cond 
    ((and (> n 0) (> d 0 )) (cons (/ n g) (/ d g)))
    ((and (> n 0) (< d 0 )) (cons (/ (* -1 n) g) (/ (* -1 d) g)))
    ((and (< n 0) (< d 0 )) (cons (/ (* -1 n) g) (/ (* -1 d) g)))
    ((and (< n 0) (> d 0 )) (cons (/ n g) (/ d g)))
    )
  )
    )

(make-rat -8 -10)
(make-rat 8 10)
(make-rat 8 -10)
(make-rat -8 10)


