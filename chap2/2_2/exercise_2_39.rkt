#lang racket
(#%require "../../util/utils.rkt")

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(define (fold-right op initial seq)
  (if (null? seq)
      initial
      (op (car seq)
          (fold-right op initial (cdr seq)))))

(define (reverse sequence)
  (fold-right (lambda (x y)
                (append y (list x)))
              nil sequence))
(reverse (list 1 2 4 6))


(define (reverse_2 sequence)
  (fold-left (lambda (x y)
               (cons y x))
             nil sequence))

(reverse_2 (list 1 2 4 6))