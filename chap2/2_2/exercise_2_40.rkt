#lang racket
(#%require "../../util/utils.rkt")

(define (enumerate-interval start end)
  (if (> start end)
      nil
      (cons start
            (enumerate-interval (+ start 1)
                                 end))))

(define (unique-pairs n)
  (flatmap (lambda (i)
             (map (lambda (j) (list i j))
                  (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))
(unique-pairs 4)

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))
(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))


(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               (unique-pairs n))))
(prime-sum-pairs 5)