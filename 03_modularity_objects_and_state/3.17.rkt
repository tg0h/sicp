#lang sicp

(define counted '())

(define (count-pairs x)
  (define (counted-before? x search-list)
    ;; (display "search: ")
    ;; (display x)
    ;; (display " in: ")
    ;; (display search-list)
    (cond ((null? search-list)
           ;; (display " FALSE")
           ;; (newline)
           false
           )
          ((eq? x (car search-list))
           ;; (display " true")
           ;; (newline)
           true
           )
          (else
           ;; (display " ->> ")
           (counted-before? x (cdr search-list))
           ))
    )
  (define (count x)
    ;; (display "count >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> x is:")
    ;; (display x)
    ;; (display " | counted is:")
    ;; (display counted)
    ;; (newline)
    (cond
      ((not (pair? x))
       ;; (display "return 0")
       ;; (newline)
       0
       )
      ((counted-before? x counted)
       ;; (display "COUNTED BEFORE")
       (+ (count (car x))
          (count (cdr x)))
       )
      (else
       ;; (display "add x:")
       ;; (display x)
       ;; (newline)
       ;; (display " counted: ")
       ;; (display counted)
       ;; (newline)
       (set! counted (cons x counted))
       ;; (display "after add: ")
       ;; (display counted)
       ;; (newline)
       ;; (newline)
       (+ (count (car x))
          (count (cdr x))
          1 )
       )

      )
    )
  (count x )
  )

(define l3 '( a b c))
;; (count-pairs l3)


;; (define ab '(a b))
;; (define x ab)
;; (set-cdr! ab 'c)
;; x
;; ab

;; (eq? x ab)
(define a '(a ))

;; (define abab (cons ab ab))
(define aa (cons a a))
;; (count-pairs abab)

(count-pairs aa)

(count-pairs (cons (cons a a) (cons a a))) ; 7

(define _a (cons a a))
(count-pairs (cons _a _a))

;; (eq? _a _a)

;; (define (search x counted)
;;   (cond ((null? counted) false)
;;         ((eq? x (car counted)) true)
;;         (else (search x (cdr counted))))
;;   )
;; (search l3 counted)

;; (eq? ab ab)
