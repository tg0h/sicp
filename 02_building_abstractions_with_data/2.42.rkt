#lang sicp


(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence)))) (else (filter predicate (cdr sequence)))))

(define (is-point-safe? point-a point-b)
  ;; (display "is-point-safe")(display point-a)(display point-b)(newline)
  (define (horizontal? point-a point-b) (= (cadr point-a) (cadr point-b)))
  (define (diagonal? point-a point-b)
    (define (abs x) (cond ((> x 0) x) ((= x 0) 0) ((< x 0) (- x))))
    (let
        (
         (x-point-a (car point-a))
         (y-point-a (cadr point-a))
         (x-point-b (car point-b))
         (y-point-b (cadr point-b))
         )
      (= (abs (- y-point-b y-point-a)) (abs(- x-point-b x-point-a)))
      )
    )
  (and
   (not (horizontal? point-a point-b))
   (not (diagonal? point-a point-b))
   )
  )

(define (check-point-safe-with-positions test-point positions)
  ;; (display "check-point-safe-with-positions")(display positions)(newline)
  (accumulate
   (lambda (point is-rest-of-points-safe)
     (and (is-point-safe? point test-point) is-rest-of-points-safe)
     )
   #t positions)
  )

(define (safe? k board)
  (display "safe? ")(display k)(display " ")(display board)(newline)
  (let
      (
       (col-k-point (car (filter (lambda (point)
                                   ;; (display "k ->")(display k)(newline)
                                   ;; (display "point->")(display point)(newline)
                                   ;; (display "k point car->")(display (car point))(newline)
                                   (= (car point) k)
                                   ) board
                                     )))
       (test-points (filter (lambda (point)
                              ;; (display "test point->")(display point)(newline)
                              (not (= (car point) k))
                              ) board
                                ))
       )
    ;; (display col-k-point) (display "test against->") (display test-points)
    ;; (newline)
    (check-point-safe-with-positions col-k-point test-points)
    )
  )



(define (adjoin-position row col rest-of-queens)
  ;; (display "adjoin-position ")(display row)(display " ")(display col)(display " ")(display rest-of-queens)(newline)
  (cons (list col row) rest-of-queens)
  )

(define (flatmap proc seq) (accumulate append nil (map proc seq)))
(define (enumerate-interval low high)
  (if (> low high) nil (cons low (enumerate-interval (+ low 1) high)))
  )

(define empty-board nil)

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter (lambda (positions) (safe? k positions))
                (flatmap (lambda (rest-of-queens)
                           (map (lambda (new-row)
                                  (adjoin-position new-row k rest-of-queens))
                                (enumerate-interval 1 board-size)))
                         (queen-cols (- k 1))))))
  (queen-cols board-size)
  )

(queens 4)
