#lang racket
(#%require "../../util/utils.rkt")

(define (my_map p sequence)
  (accumulate (lambda (x y) (cons (p x) y))
              nil
              sequence))

(define (my_append seq1 seq2)
  (accumulate cons
              seq2
              seq1))

(define (my_length sequence)
  (accumulate (lambda (x y) (+ 1 y))
              0
              sequence))

(my_length (list 1 2 3))
(my_length (list))

(my_append (list 1 2 3) (list 5 6 ))
              
(my_map square (list 1 2 3 4 5))
(my_map inc (list 1 2 3 4 5))