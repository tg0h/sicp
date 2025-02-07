#lang sicp

;; (define (make-account balance)
;;   (define (withdraw amount)
;;     (if (>= balance amount)
;;         (begin (set! balance (- balance amount)) balance)
;;         "Insufficient funds"))
;;   (define (deposit amount)
;;     (set! balance (+ balance amount))
;;     balance)
;;   (let ((protected (make-serializer)))
;;     (define (dispatch m)
;;       (cond ((eq? m 'withdraw) (protected withdraw))
;;             ((eq? m 'deposit) (protected deposit))
;;             ((eq? m 'balance) balance)
;;             (else (error "Unknown request: MAKE-ACCOUNT"
;;                          m))))
;;     dispatch))

(define (rand) (random 10000))

(define (make-serializer)
  (let ((mutex (make-mutex)))
    (lambda (p)
      (define (serialized-p . args)
        (mutex 'acquire)
        (let ((val (apply p args)))
          (mutex 'release)
          val))
      serialized-p)))

(define (make-mutex)
  (let ((cell (list false)))
    (define (the-mutex m)
      (cond ((eq? m 'acquire)
             (if (test-and-set! cell)
                 (the-mutex 'acquire))) ;retry
            ((eq? m 'release) (clear! cell))))
    the-mutex))

(define (clear! cell) (set-car! cell false))

(define (test-and-set! cell)
  (if (car cell)
      true
      (begin
        (set-car! cell true)
        false)))

(define (make-account-and-serializer balance)
  ;; (let ((id (rand)))
  (define id (rand))
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount)) balance)
  (let ((balance-serializer (make-serializer)))
    (define (dispatch m)
      (cond ((eq? m 'withdraw) withdraw) ; do not lock withdraw, export a serializer and let someone else manage the lock
            ((eq? m 'deposit) deposit) ; do not lock deposit, export a serializer and let someone else manage the lock
            ((eq? m 'balance) balance)
            ((eq? m 'serializer) balance-serializer)
            ((eq? m 'id) rand)
            (else (error "Unknown request: MAKE-ACCOUNT" m))))
    dispatch))

(define (serialized-exchange account1 account2)
  (let ((serializer1 (account1 'serializer))
        (serializer2 (account2 'serializer)))
    ((serializer1 (serializer2 exchange)) ; lock both accounts
     account1
     account2)))

(define (exchange account1 account2)
  (let ((difference (- (account1 'balance)
                       (account2 'balance))))
    ((account1 'withdraw) difference) ; this locks itself
    ((account2 'deposit) difference)))
