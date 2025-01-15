#lang sicp

(define (make-account balance password)
  (define password-consecutive-wrong-count 0)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount)) balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (call-the-cops)
    (display "calling the cops")
    )
  (define (password-wrong amount)
    (set! password-consecutive-wrong-count (+ password-consecutive-wrong-count 1))
    (display amount)
    (display password-consecutive-wrong-count)
    (if (= password-consecutive-wrong-count 7)
        (call-the-cops)
        (begin
          ;; "tim"
          (display "Incorrect Password ")
          (display password-consecutive-wrong-count)
          (newline)
          ;; (error password-consecutive-wrong-count)
          )
        )
    )
  (define (dispatch pw m)
    (cond
      ((not (eq? pw password)) password-wrong)
      ((and (eq? pw password) (eq? m 'withdraw))
       (set! password-consecutive-wrong-count 0)
       withdraw
       )
      ((and (eq? pw password) (eq? m 'deposit))
       (set! password-consecutive-wrong-count 0)
       deposit
       )
      (else (error "Unknown request: MAKE-ACCOUNT"
                   m))))
  dispatch)


(define acc (make-account 100 'secret-password))

((acc 'secret-password 'withdraw) 40)
;; 60
((acc 'secret-password 'withdraw) 40)

((acc 'some-other-password 'deposit) 5)
((acc 'some-other-password2 'deposit) 4)
;; ((acc 'some-other-password 'deposit) 3)
((acc 'some-other-password 'deposit) 50)
((acc 'secret-password 'withdraw) 40)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
;; ((acc 'some-other-password 'deposit) 50)
