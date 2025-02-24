#lang sicp

;; (define (eval-if exp env)
;;   (if (true? (eval (if-predicate exp) env))
;;       (eval (if-consequent exp) env)
;;       (eval (if-alternative exp) env)))


(define (and-predicate exp) cadr exp)

(define (eval-and exp env)
  (cond
    ; if and does not have any predicates, return true
    ; eg just a single and
    (null? (cdr exp) true)

    ; if pred is false, return false
    ((not (true? (eval (cadr exp) env))) false)

    ; if true and last expression, return last expression
    ( (and (true? (eval (cadr exp) env))
           (null? (cddr exp)))
      (eval (and-predicate exp)))

    ; if true and not last expression, continue evaluation
    (else (eval-and (cons 'and (cddr exp)) env))

    )
  )

(define (eval-or exp env)
  (cond
    ; if or does not have any predicates, return false
    ; eg just a single and
    (null? (cdr exp) false)

    ; if pred is true, return expression
    ((true? (eval (cadr exp) env)) (eval cadr exp))

    ; if false and last expression, return false
    ( (and (not (true? (eval (cadr exp) env)))
           (null? (cddr exp)))
      false
      )

    ; if false and not last expression, continue evaluation
    (else (eval-and (cons 'or (cddr exp)) env))

    )
  )
