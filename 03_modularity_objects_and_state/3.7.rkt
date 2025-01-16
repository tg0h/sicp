#lang sicp

(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount)) balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define password-list nil)
  (define (add-password password)
    (set! password-list (cons password password-list))
    )
  (define (password-correct? password)
    (memq password password-list)
    )
  (define (dispatch pw m)
    (cond
      ((not (password-correct? password)) (error "Incorrect Password"))
      ((and (password-correct? password) (eq? m 'withdraw)) withdraw)
      ((and (password-correct? password) (eq? m 'deposit)) deposit)
      (else (error "Unknown request: MAKE-ACCOUNT"
                   m))))
  (add-password password)
  dispatch
  )

(define (make-joint account password new-password)
  )


(define acc (make-account 100 'secret-password))

((acc 'secret-password 'withdraw) 40)
;; 60
((acc 'secret-password 'withdraw) 40)

((acc 'some-other-password 'deposit) 50) "Incorrect password"

(define paul-acc
  (make-joint peter-acc 'open-sesame 'rosebud))
