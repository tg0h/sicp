#lang sicp

;; (define (list-of-values exps env)
;;   (if (no-operands? exps)
;;       '()
;;       (cons (eval (first-operand exps) env)
;;             (list-of-values (rest-operands exps) env))))

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (begin
        ; force left to right evaluation
        (define first-exp (eval (first-operand exps) env))
        (define rest-exp (list-of-values (rest-operands exps) env))
        (cons first-exp rest-exp)
        )
      )
  )
