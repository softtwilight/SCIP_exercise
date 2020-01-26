#lang sicp
;; define the accumulate
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

;; exercise_2.36
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      '()
      (cons (accumulate op init
                        (map car seqs))
            (accumulate-n op init
                          (map cdr seqs)))))
(define x (list (list 1 2 3)
                        (list 4 5 6)
                        (list 7 8 9)
                        (list 10 11 12)))
(accumulate-n + 0 x) ;; -> (22 26 30)

;; exercise_2.73  matrix algebra
;; (dot-product v w) returns the sum Σiviwi
(define v1 (list 1 2 3))
(define v2 (list 1 1 2))

(define (dot-product v w)
  (accumulate + 0 (map * v w)))
(dot-product v1 v2) ;; -> 9

;;(matrix-*-vector m v) returns the vector t, where ti = Σjmijvj
(define (matrix-*-vector m v)
  (map (lambda (x) (dot-product x v)) m))
(matrix-*-vector x v2) ;; -> (9 21 33 45)

;; (transpose m) returns the matrix n,where nij = mji
(define (transpose mat)
  (accumulate-n
   cons
   nil
   mat))
(transpose x) ;; -> ((1 4 7 10) (2 5 8 11) (3 6 9 12))

;; (matrix-*-matrix m n) returns the matrix p,where pij = Σkmiknkj
(define (matrix-*matrix m n)
  (let ((cols (transpose n)))
    (map
     (lambda (x) (matrix-*-vector n x))
     m)))
(matrix-*matrix x x)

