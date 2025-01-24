#lang sicp

(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let ((new-value
           (logical-and (get-signal a1) (get-signal a2))))
      (after-delay
       and-gate-delay
       (lambda () (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'ok
  )

;; (define (logical-not s)
;;   (cond ((= s 0) 1)
;;         ((= s 1) 0)
;;         (else (error "Invalid signal" s))))

;; (define (logical-or a b)
;;   (cond ((= a 1) 1)
;;         ((= b 1) 1)
;;         ((and (= a 0) (= b 0) ) 0)
;;         (else (error "Invalid signal"))))


(define (inverter input output)
  (define (invert-input)
    (let ((new-value (logical-not (get-signal input))))
      (after-delay inverter-delay
                   (lambda () (set-signal! output new-value)))))
  (add-action! input invert-input) 'ok)

(define (logical-not s)
  (cond ((= s 0) 1) ((= s 1) 0)
        (else (error "Invalid signal" s))))

(define (half-adder a b s c)
  (let ((d (make-wire)) (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok))

(define (or-gate a b output)
  (let ((c (make-wire))
        (d (make-wire))
        (e (make-wire)))
    (inverter a c)
    (inverter b d)
    (and-gate c d e)
    (inverter e output)
    'ok))

(define (full-adder a b c-in sum c-out)
  (let ((s (make-wire)) (c1 (make-wire)) (c2 (make-wire)))
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)
    'ok))

(define (ripple-carry-adder a-n b-n s-n c-cout)
  (define (make-wire-list n)
    (if (= n 0) nil
        (cons (make-wire) make-wire-list (- n 1))
        ))
  (define c-n (cons (c-out (make-wire-list (length a-n) ))))
  (define (connect-full-adders a-n b-n s-n c-n)
    (if (empty? a-n) 'ok
        (begin
          (let ((a (car a-n))
                (b (car b-n))
                (c-in (cadr c-n))
                (s (car s-n))
                (c-out (car c-n)))
            (full-adder a b c-in s c-out)
            (if (null? (cdr a-n)) set-signal! c-in 0)
            (connect-full-adders (cdr a-n) (cdr b-n) (cdr s-n) (cdr c-n))))))
  (connect-full-adders a-n b-n s-n c-n)
  )
