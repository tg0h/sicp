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

