#lang sicp

;; (define x(list 2 3))

;; (car x)
;; (cdr x)



;; (define (make-branch length structure) (list length structure))
;; (define branch-a (make-branch 10 5))
;; (define branch-b (make-branch 10 branch))
;;
;; (define (branch-length branch) (car branch))
;; (define (branch-structure branch) (cdr branch))
;;
;; (branch-length branch-a)
;; (branch-structure branch-a)

(define (make-mobile left right) (list left right))
(define (make-branch length structure) (list length structure))

(define (left-branch mobile) (car mobile))
(define (right-branch mobile) (car(cdr mobile)))

(define (branch-length branch) (car branch))
(define (branch-structure branch) (car(cdr branch)))


(define branch-a (make-branch 10 5))
(define branch-b (make-branch 5 10))

(define mm1 (make-mobile branch-a branch-b))
(define branch-c (make-branch 10 mm1))
(define mm2 (make-mobile branch-a branch-c))
(define mm3 (make-mobile branch-c branch-c))


;; (define x(make-mobile branch-a branch-b))
;; (define lefty(left-branch x))
;; (right-branch x)
;; (branch-length lefty)
;; (branch-structure lefty)



(define (total-weight mobile)
  (cond
    ((not (pair? (branch-structure mobile))) (branch-structure mobile)) ; if simple branch, return weight immediately
    ( (not (pair? (branch-length mobile)) ) (total-weight (branch-structure mobile))) ; if compound branch, return total weight of compound branch
    (else (+ (total-weight (left-branch mobile)) (total-weight (right-branch mobile))))
    )
  )

(total-weight mm1)
(total-weight mm2)
(total-weight mm3)
;; (right-branch mm1)
;; (right-branch mm2)
;; (left-branch mm1)





(define (torque branch)
  (cond
    ; if simple branch, return torque immediately
    ((not (pair? (branch-structure branch))) (* (branch-length branch) (branch-structure branch)))

    ; if compound branch, return total torque of compound branch
    ( (not (pair? (branch-length branch)) ) (* ( branch-length branch ) (total-weight (branch-structure branch))))

    ;; (else (+ (torque (left-branch branch)) (torque (right-branch branch))))
    )
  )

(torque mm1)

(define (is-balanced? mobile)
  (cond
    ; if simple branch, return true
    ((not (pair? (branch-structure mobile))) #t)

    ; if compound branch
    ( (not (pair? (branch-length mobile)) ) (is-balanced? (branch-structure mobile)))

    (else
     (and (= (torque (left-branch mobile)) (torque (right-branch mobile)))
          (is-balanced? (left-branch mobile))
          (is-balanced? (right-branch mobile))
          )
     )
    )
  )

;; (is-balanced? mm1)

(define m2 (make-mobile
            (make-branch 2 4)
            (make-branch 2
                         (make-mobile
                          (make-branch 1 1)
                          (make-branch 3 3)))))
(is-balanced? m2)
