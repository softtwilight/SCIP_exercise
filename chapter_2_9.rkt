#lang sicp
(define (map-tree tree func)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (map-tree sub-tree func)
             (func sub-tree)))
       tree))
(define (scale-tree tree factor)
  (map-tree tree (lambda (x) (* x factor))))

;; print -> (10 (20 (30 40) 50) (60 70))
(scale-tree (list 1 (list 2 (list 3 4) 5) (list 6 7)) 10)

;; exercise_2.30
(define (square-tree tree)
  (map-tree tree (lambda (x) (* x x))))
;; print -> (1 (4 (9 16) 25) (36 49))
(square-tree (list 1 (list 2 (list 3 4) 5) (list 6 7)))

;; exercies_2.32
(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (x) (append (list (car s)) x)) rest)))))
;; print -> (() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3))
(subsets (list 1 2 3))
