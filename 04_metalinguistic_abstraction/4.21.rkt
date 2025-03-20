#lang sicp

((lambda (n)
   (
    (lambda (fact) (fact fact n))
    (lambda (ft k)
      (if (= k 1)
          1
          (* k (ft ft (- k 1))))
      ))
   ) 4)

((lambda (n)
   ((lambda (fib) (fib fib n))
    (lambda (fb n)
      (cond
        ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fb fb (- n 2)) (fb fb (- n 1))))
        )))) 10)

(define (fib n)
  (cond
    ((= n 0) 0)
    ((= n 1) 1)
    (else (+
           (fib (- n 2) )
           (fib (- n 1) )
           ))))


