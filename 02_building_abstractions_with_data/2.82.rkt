#lang sicp

(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable
             (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record
                   (assoc key-2 (cdr subtable))))
              (if record (cdr record) false)) false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable
             (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record
                   (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value) (set-cdr! subtable
                                                    (cons (cons key-2 value)
                                                          (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1 (cons key-2 value))
                            (cdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation: TABLE" m))))
    dispatch))

(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))


(define (attach-tag type-tag contents) (cons type-tag contents))
(define (type-tag datum) (if (pair? datum)
                             (car datum)
                             (error "Bad tagged datum: TYPE-TAG" datum)))
(define (contents datum)
  (if (pair? datum) (cdr datum)
      (error "Bad tagged datum: CONTENTS" datum)))

;; (define (apply-generic op . args)
;;   ; use map type-tag because we want to be generic, we want to provide many contents and many types for the contents
;;   ; for now only one arg eg real-part z not real-part z1 z2
;;   (let ((type-tags (map type-tag args))) ; get the type-tag ('rectangular) from the args eg ('rectangular ( 1 . 1 ) )
;;     (let ((proc (get op type-tags)))
;;       (if proc
;;           (apply proc (map contents args)) ; get the contents ( ( 1 . 1 ) ) from the args eg ('rectangular ( 1 . 1 ) )
;;           (error "No method for these types: APPLY-GENERIC"
;;                  (list op type-tags))))))


(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
(define (zero? x ) (apply-generic 'zero? x ))

(define (install-scheme-number-package)
  (define (tag x) (attach-tag 'scheme-number x))
  (put 'zero? '(scheme-number) (lambda (x) (= x 0)))
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  (put 'exp '(scheme-number scheme-number)
       (lambda (x y) (tag (expt x y))))
  ; using primitive expt
  (put 'make 'scheme-number (lambda (x) (tag x)))
  'done)

(define (make-scheme-number n) ((get 'make 'scheme-number) n))

(define (install-rational-package) ;; internal procedures
  ;; these procedures do not need to be aware of the tags
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))
  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))
  (put 'numer '(rational) numer)
  (put 'denom '(rational) denom)
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  'done)

(define (make-rational n d) ((get 'make 'rational) n d))

(define (square x) (* x x))
(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z)
    (sqrt (+ (square (real-part z))
             (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'rectangular x))

  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

(define (install-polar-package)
  ;; internal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z) (* (magnitude z) (cos (angle z))))
  (define (imag-part z) (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y)
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part) ; real-part is a procedure that carries its environment - it is aware of magnitude
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (install-complex-package)
  (install-rectangular-package)
  (install-polar-package)
  ;; imported procedures from rectangular and polar packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))

  ;; internal procedures
  (define (add-complex z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
                         (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2))
                         (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                       (+ (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                       (- (angle z1) (angle z2))))

  ;; interface to rest of the system
  (define (tag z) (attach-tag 'complex z))
  ;; (put 'magnitude '(complex) magnitude)
  (put 'magnitude '(complex)
       (lambda (z1) (magnitude z1))
       ) ; expose magnitude so that generic arithmetic package equ can use this to determine equality for complex numbers
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (make-complex-from-real-imag x y) ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a) ((get 'make-from-mag-ang 'complex) r a))


(install-scheme-number-package)
;; (define s1 (make-scheme-number 1))
;; (define s2 (make-scheme-number 2))
;; (define s3 (make-scheme-number 0))
;; (add s1 s2)

(install-rational-package)
;; ((get 'make 'rational) 1 1)
;; (define r1 (make-rational 0 2))
;; (define r2 (make-rational 3 4))
;; (define r3 (make-rational 2 4))
;; (add r1 r2)
(install-complex-package)
;; (define z1 (make-complex-from-real-imag 1 1))
;; (define z2 (make-complex-from-mag-ang 1.41 0.78))
;; (define z3 (make-complex-from-real-imag 0 0))


(define coerce-table (make-table))
(define get-coercion (coerce-table 'lookup-proc))
(define put-coercion (coerce-table 'insert-proc!))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (let ((t1->t2 (get-coercion type1 type2))
                      (t2->t1 (get-coercion type2 type1)))
                  (cond (
                         (and t1->t2 (not (eq? type1 type2)))
                         (apply-generic op (t1->t2 a1) a2)
                         )
                        (
                         (and t2->t1 (not (eq? type1 type2)))
                         (apply-generic op a1 (t2->t1 a2))
                         )
                        (else (error "No method for these types" (list op type-tags))))))
              (error "No method for these types" (list op type-tags)))
          ))))

(define (loop-op op args cast-type)
  (cond
    (not (pair? args) (error "args is not a pair" args))
    ((< (length args) 2) (error "must provide more than 1 args" args))
    ((null? args) (error "args is nil"))
    (else
     (let ((type-tags (map type-tag args)))
       )
     )
    )
  )

(define (scheme-number->complex n) (make-complex-from-real-imag (contents n) 0))
(put-coercion 'scheme-number
              'complex
              scheme-number->complex)

;; (define (scheme-number->scheme-number n) n)
;; (put-coercion 'scheme-number 'scheme-number scheme-number->scheme-number)

;; (define (complex->complex z) z)
;; (put-coercion 'complex 'complex complex->complex)

(define (exp x y) (apply-generic 'exp x y))

(define s1 (make-scheme-number 1))
(define s2 (make-scheme-number 2))

(define z1 (make-complex-from-real-imag 1 1))
;; (add s1 z1)
;; (add s1 s1 s1)
;; (exp s1 s1)
;; (exp z1 z1)
