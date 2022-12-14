#lang racket
(#%require "../../util/utils.rkt")

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

(define (dot-product v w)
      (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (row)
         (dot-product row v))
         m))

(define matrix (list (list 1 2 3) (list 4 5 6) (list 7 8 9)))
(define vector (list 1 1 1))
;; '(6 15 24)
(matrix-*-vector matrix vector)


(define (transpose mat)
  (accumulate-n cons
                nil
                mat))

;; '((1 4 7) (2 5 8) (3 6 9))
(transpose matrix)

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (x)
           (matrix-*-vector cols x))

     m)))
(define matrix2 (list (list 1 1 1) (list 1 1 1) (list 1 1 1)))
;; '((6 6 6) (15 15 15) (24 24 24))
(matrix-*-matrix matrix matrix2)
