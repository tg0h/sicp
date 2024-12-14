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
  (display "is-point-safe")(display point-a)(display point-b)(newline)
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
  (display "check-point-safe-with-positions")(display positions)(newline)
  (accumulate
   (lambda (point is-rest-of-points-safe)
     (and (is-point-safe? point test-point) is-rest-of-points-safe)
     )
   #t positions)
  )

(define (safe? k board)
  (newline)
  (display "safe? ")(display k)(display " ")(display board)(newline)
  (let
      (
       (col-k-point (car (filter (lambda (point)
                                   (display "k ->")(display k)(newline)
                                   (display "point->")(display point)(newline)
                                   (display "k point car->")(display (car point))(newline)
                                   (= (car point) k)
                                   ) board
                                     )))
       (test-points (filter (lambda (point)
                              (display "test point->")(display point)(newline)
                              (not (= (car point) k))
                              ) board
                                ))
       )
    (display col-k-point) (display "test against->") (display test-points)
    (newline)
    (check-point-safe-with-positions col-k-point test-points)
    )
  ;; (filter (lambda (position)
  ;;           (display "position is->")(display position)(newline)
  ;;           (let
  ;;               (
  ;;                (col-k-point (car (filter (lambda (point)
  ;;                                            (= (car point) k)
  ;;                                            ) position
  ;;                                              )))
  ;;                (test-points (filter (lambda (point)
  ;;                                       (not(= (car point) k))
  ;;                                       ) position
  ;;                                         ))
  ;;                )
  ;;             (display col-k-point) (display "test against->") (display test-points)
  ;;             (newline)
  ;;             (check-point-safe-with-positions col-k-point test-points)
  ;;             )
  ;;           ) positions
  ;;             )
  )


(define positions-3-board
  ;; (list
  (list (list 1 3) (list 2 3))
  ;; (list (list 1 3) )
  ;; (list (list 1 3) ) ; safe
  ;; )
  )
;; positions-3-board
;; (safe? 2 positions-3-board)


(define (adjoin-position row col rest-of-queens)
  ;; (display "adjoin-position ")(display row)(display " ")(display col)(display " ")(display rest-of-queens)(newline)
  (cons (list col row) rest-of-queens)
  )

;; (define aboard (list (list 9 9) ))
(define aboard  nil )
;; aboard
;; (adjoin-position 2 2 aboard)


(define (flatmap proc seq) (accumulate append nil (map proc seq)))
(define (enumerate-interval low high)
  ;; (display "enumerate interval ")(display low)(display " ")(display high)(newline)
  (if (> low high) nil (cons low (enumerate-interval (+ low 1) high)))
  )


;; (map (lambda (new-row)
;;        (adjoin-position new-row 2 aboard))
;;      (enumerate-interval 1 2))

(define empty-board nil)

(define (queens board-size)
  (define (queen-cols k)
    ;; (display "QUEEN-COLS: ")(display k)(newline)
    (if (= k 0)
        (list empty-board)
        (filter (lambda (positions) (safe? k positions))
                (flatmap (lambda (rest-of-queens)
                           (map (lambda (new-row)
                                  (adjoin-position new-row k rest-of-queens))
                                (enumerate-interval 1 board-size)))
                         (queen-cols (- k 1))))))
  ;; (display "qc>>>>>>>>>>>")(display board-size)(display (queen-cols board-size))
  (queen-cols board-size)
  )

(queens 3)

(newline)

;; (flatmap (lambda (rest-of-queens)
;;            (map (lambda (new-row)
;;                   (adjoin-position new-row 1 rest-of-queens))
;;                 (enumerate-interval 1 2 )))
;;          (list nil)
;;          )
;;
;; (newline)
;; (filter (lambda (positions) (safe? 1 positions))
;;         (flatmap (lambda (rest-of-queens)
;;                    (map (lambda (new-row)
;;                           (adjoin-position new-row 1 rest-of-queens))
;;                         (enumerate-interval 1 2 )))
;;                  (list nil)
;;                  )
;;         )
