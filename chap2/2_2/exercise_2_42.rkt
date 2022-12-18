#lang racket
(#%require "../../util/utils.rkt")

; n for board-size

(define empty-board nil)

(define (adjoin-position row col rest-of-queens)
  (cons (list col row) rest-of-queens))          

(define (same-row? pos1 pos2)
  (= (cadr pos1) (cadr pos2)))

(define (same-dia? pos1 pos2)
  (let ((row1 (cadr pos1))
        (row2 (cadr pos2))
        (col1 (car pos1))
        (col2 (car pos2)))
    (= (abs (- row1 row2)) (abs (- col1 col2)))))

(define (queen-at-k k pos)
  (car (filter
        (lambda (p) (= k (car p)))
        pos)))

(define (queens-not-at-k k pos)
  (filter
        (lambda (p) (not (= k (car p))))
        pos))
                 

(define (safe? k positions)
  (if (null? positions)
      't
      (accumulate
       (lambda (x y) (and x y))
       't

       ;; don't forget add () to define lambda variable.
       (map (lambda (p) (not (or (same-row? (queen-at-k k positions) p)
                               (same-dia? (queen-at-k k positions) p))))
            (queens-not-at-k k positions)))))

(define (queens n)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position
                    new-row k rest-of-queens))
                 (enumerate-interval 1 n)))
          (queen-cols (- k 1))))))
  (queen-cols n))
(length (queens 8))
