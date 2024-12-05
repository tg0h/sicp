#lang sicp

(define (even? n) (= (remainder n 2) 0))

;; (define (even-list l)
;;   (cond
;;     ((null? l) (list nil))
;;     ((even? (car l)) (cons((car l) (even-list (cdr l)))))
;;     (else (even-list (cdr l)))
;;     )
;;   )

;; (even-list (list 2))
;; (null? 2)


;; (cons (car (list 2)) (cdr (list 2)))
;; (cons 2 nil)
;; (cdr (list 2))
;; (cdr (list 1 2))

;; (define (same-parity l)
;;   (if (even? (car l) )
;;       )
(define (parity a b)
  (if (even? a)
      (even? b)
      (not (even? b))
      )
  )

;; (parity 2 4)


(define (same-parity first . rest)
  (define (get-parity-list l)
    ;; (display "l is:") (display l) (newline)
    (cond
      ((null? l)
       ;; (display "null>")
       ;; (newline)
       ;; (newline)
       nil)
      ((parity first (car l))
       ;; (display "car l:")
       ;; (display (car l))
       ;; (newline)
       ;; (display "parity>")
       ;; (newline)
       ;; (newline)
       (cons  (car l) (get-parity-list (cdr l)))
       )
      (else
       ;; (display "else>")
       ;; (newline)
       ;; (newline)
       (get-parity-list (cdr l))
       )
      )
    )
  (cons first (get-parity-list rest))
  )

(same-parity 1 2 3 4 5 6 7)
(same-parity 2 3 4 5 6 7)

