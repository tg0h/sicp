#lang sicp

(define (fold-right op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (fold-right op initial (cdr sequence)))))

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest) result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))


(define (reverse sequence)
  (fold-right (lambda (x y )
                (append y (list x))
                ) nil sequence)
  )

(reverse (list 1 2 3))

(define (_reverse sequence)
  (fold-left (lambda (x y )
               (append (list y) x)
               ) nil sequence)
  )

;; (append (list 1) nil)
(_reverse (list 1 2 3))

