#lang sicp

;; (define (fringe x)
;;   (define (iter result)
;;     (cond
;;       ((null? x) nil)
;;       ((not(pair? x) nil) x)
;;       (else
;;        (append (fringe (car x)) (fringe (cdr x)))
;;        )
;;       )
;;     )
;;   (iter (list nil))
;;   )

(define x (list (list 1 2) (list 3 4)))


(define (fringe x)
  (cond ((null? x) nil)
        ((pair? x)
         (append (fringe (car x))
                 (fringe (cdr x))))
        (else (list x))))

(fringe x)


;; (define (fringe x)
;;   ;; (display x)
;;   ;; (newline)
;;   (cond
;;     ((null? x) x)
;;     ((not (pair? x)) (list x))
;;     ((pair? x) (append (fringe (car x)) (fringe (cdr x))))
;;     )
;;   )
;;
;; ;; (append (deep-reverse (cdr x))
;; ;;         (list (deep-reverse (car x))))
;;
;;
;; (define x
;;   (list 1
;;         (list 2
;;               (list 3)
;;               )
;;         )
;;   )
;;
