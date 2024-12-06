#lang sicp

(define x (list 1 3 (list 5 7 ) 9))

x
(cdr x)
(cdr(cdr x))
(car(cdr(cdr x)))
(cdr(car(cdr(cdr x))))
(car(cdr(car(cdr(cdr x)))))
(newline)

(define y (list (list 7)))
y
(car(car y))
(newline)

(define z (list 1(list 2(list 3(list 4(list 5 (list 6 7)))))))
z
(cdr z)
(car(cdr z))

(car(cdr (car(cdr z))))

(car(cdr (car(cdr (car(cdr z))))))

(car(cdr (car(cdr (car(cdr (car(cdr z))))))))

(car(cdr (car(cdr (car(cdr (car(cdr (car(cdr z))))))))))

(car(cdr
     (car(cdr
          (car(cdr
               (car(cdr
                    (car(cdr
                         (car(cdr z)))
                        )
                    ))
               ))
          ))
     ))
