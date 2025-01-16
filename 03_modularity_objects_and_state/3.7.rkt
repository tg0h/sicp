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
    (display "password is: ") (display password)
    (newline)
    (memq password password-list)
    )
  (define (dispatch pw m)
    (cond
      ((not (password-correct? pw)) (error "Incorrect Password"))
      ((and (password-correct? pw) (eq? m 'withdraw)) withdraw)
      ((and (password-correct? pw) (eq? m 'deposit)) deposit)
      ((and (password-correct? pw) (eq? m 'add-password)) add-password)
      (else (error "Unknown request: MAKE-ACCOUNT"
                   m))))
  (add-password password)
  dispatch
  )

(define (make-joint account password new-password)
  ((account password 'add-password) new-password)
  account
  )


(define acc (make-account 100 'secret-password))
(define joint (make-joint acc 'secret-password 'hello))


((acc 'secret-password 'withdraw) 40)
;; 60
((joint 'secret-password 'withdraw) 40)
((joint 'hell 'withdraw) 10)
((joint 'hell 'withdraw) 10)
((joint 'hell 'withdraw) 10)
((joint 'hell 'withdraw) 10)
;; ((joint 'hel 'withdraw) 10)
;; ((acc 'secret-password 'withdraw) 40)
;; (memq 'tim (list 'timothy))

;; ((acc 'some-other-password 'deposit) 50) "Incorrect password"

;; (define paul-acc (make-joint peter-acc 'open-sesame 'rosebud))
