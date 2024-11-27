#lang sicp

(define (sum term a next b)
  ;; (display "term ")
  ;; (display a)
  ;; (display b)
  ;; (display "=")
  ;; (display (term a))
  ;; (newline)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (integral f a b dx)
  (define (add-dx x)
    (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

(define (cube x ) (* (* x x) x))

(define (next x)
  (+ x 1))

(sum cube 1 next 9)

(define (_sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ (term a) result)))
    )
  (iter a 0)
  )


(_sum cube 1 next 9)
