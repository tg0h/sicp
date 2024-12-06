#lang sicp

;; (reverse (list 1 4 9 16 25))

(define (pop l)
  (if (null? (cdr l))
      nil
      (cons (car l) (pop (cdr l)))
      )
  )
;; (pop (list 1 4 9 16 25))

(define (last-pair l)
  (if (null? (cdr l))
      (car l)
      (last-pair (cdr l))
      )
  )

(define (reverse l)
  (if (null? (cdr l))
      l
      (cons (last-pair l) (reverse (pop  l)))
      )
  )


(define (deep-reverse l)
  ;; (display l)
  ;; (newline)
  (display "l:")(display l)
  (newline)
  (display "cdr l:")(display (cdr l))
  (newline)
  (display "pair? l:")(display (pair? l))
  (newline)
  (display "last-pair l:")(display (last-pair l))
  (newline)
  (display "pop l:")(display (pop l))
  (newline)
  (newline)
  (cond
    ((null? (cdr l) )
     (display ">null   ")(newline)
     nil
               )
    ((not (pair? (cdr l)))
(display ">notpair")(newline)
(cons (deep-reverse(cdr l)) (deep-reverse (pop l)))
                                                                                               )
                                                                                              (else
(display ">else")(newline)
     (cons (deep-reverse (cdr l)) (deep-reverse (pop l)))
)
)
)

;; (reverse (list 1 4 9 16 25))

;; (define x (list (list 1 2) (list 3 4)))
(define x (list 1 (list 2 3)))
x
(newline)
(newline)
(newline)
(deep-reverse x)
;; (last-pair (list 3))
;; (last-pair x)
;; (pop x)
;; (pair?(last-pair x))
;; (last-pair(last-pair x))



;; (cdr (list 1 2 3))
