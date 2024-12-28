#lang sicp

(define (make-leaf symbol weight) (list 'leaf symbol weight))
(define (leaf? object) (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree)) (caddr tree)))
(define (weight tree) (if (leaf? tree)
                          (weight-leaf tree)
                          (cadddr tree)))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits) '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit: CHOOSE-BRANCH" bit))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair) ; symbol
                               (cadr pair)) ; frequency
                    (make-leaf-set (cdr pairs))))))

(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree
                    (make-leaf 'D 1)
                    (make-leaf 'C 1)))))
;; sample-tree

;; (
;;  (leaf A 4)
;;  (
;;   (leaf B 2)
;;   (
;;    (leaf D 1)
;;    (leaf C 1)
;;    (D C) 2)
;;   (B D C) 4
;;   ) (A B D C) 8)

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))
(define sample-message2 '(0 0 0 0))

;; (decode sample-message sample-tree)

;; (decode sample-message2 sample-tree)


(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))



; just use memq lol
(define (in-list? symbol list)
  (cond
    ((null? list) false)
    ((eq? symbol (car list)) true)
    (else (in-list? symbol (cdr list)))
    )
  )

;; (in-list? 'F (list 'B 'A 'C))

(define (encode-symbol symbol tree)
  (cond ((leaf? tree) nil)
        ((memq symbol (symbols (left-branch tree))) (cons 0 (encode-symbol symbol (left-branch tree))))
        ((memq symbol (symbols (right-branch tree))) (cons 1 (encode-symbol symbol (right-branch tree))))
        (else (error "bad symbol: " symbol))
        )
  )

;; (encode '(A D A B B C A) sample-tree)
;; (encode '(F F) sample-tree)


;; (make-leaf-set '( (A 8) (B 3)))


(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (successive-merge l)
  (cond
    ; if you return l instead of car l, you add an extra 0 to every symbol
    ; so the song length becomes 84+36 = 120 instead of 84
    ; there are 36 symbols (words) in the song
    ((= (length l) 1) (car l)) ; car l, not l
    (else (successive-merge (adjoin-set (make-code-tree (car l) (cadr l)) (cddr l))))
    )
  )



(define pp '(
             (WAH 1)
             (BOOM 1)
             (GET 2)
             (A 2)
             (JOB 2)
             (SHA 3)
             (YIP 9)
             (NA 16)
             )
  )

(define song-tree (generate-huffman-tree pp))
song-tree

(define song-encoded (encode '(GET
                               A JOB SHA NA NA NA NA NA NA NA NA GET A JOB SHA NA NA NA NA NA NA NA NA WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP SHA BOOM)
                             song-tree
                             ))

;; (
;;  ((leaf NA 16)
;;   ((leaf YIP 9)
;;    ((
;;      (leaf GET 2)
;;      ((leaf BOOM 1)
;;       (leaf WAH 1)
;;       (BOOM WAH) 2)
;;      (GET BOOM WAH) 4)
;;     (
;;      (leaf SHA 3)
;;      (
;;       (leaf JOB 2)
;;       (leaf A 2)
;;       (JOB A) 4)
;;      (SHA JOB A) 7)
;;     (GET BOOM WAH SHA JOB A) 11)
;;    (YIP GET BOOM WAH SHA JOB A) 20)
;;   (NA YIP GET BOOM WAH SHA JOB A) 36)
;;  )

;; song-encoded
(length song-encoded)
;; (encode '(GET) song-tree)
;; (length (encode '(GET) song-tree))
;; (decode song-encoded song-tree) ;LOL
;;

(encode '(GET A) song-tree)