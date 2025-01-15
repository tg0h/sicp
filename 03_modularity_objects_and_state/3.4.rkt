
#lang sicp
(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount)) balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch pw m)
    (cond
      ((not (eq? pw password)) (error "Incorrect Password"))
      ((and (eq? pw password) (eq? m 'withdraw)) withdraw)
      ((and (eq? pw password) (eq? m 'deposit)) deposit)
      (else (error "Unknown request: MAKE-ACCOUNT"
                   m))))
  dispatch)


(define acc (make-account 100 'secret-password))

((acc 'secret-password 'withdraw) 40)
;; 60
((acc 'secret-password 'withdraw) 40)

((acc 'some-other-password 'deposit) 50) "Incorrect password"
