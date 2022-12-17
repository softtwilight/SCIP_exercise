#lang racket
(#%require "../../util/utils.rkt")

(define (enumerate-interval start end)
  (if (> start end)
      nil
      (cons start
            (enumerate-interval (+ start 1)
                                 end))))

(define (unique-triples n)
  (flatmap (lambda (i)
             (flatmap (lambda (j)
                        (map (lambda (k)
                               (list k j i))
                             (enumerate-interval 1 (- j 1))))
                  (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))

(define (triple-sum s n)
  (filter (lambda (triple)
            (= s (accumulate + 0 triple)))
          (unique-triples n)))
;; '((2 3 4) (1 3 5))
(triple-sum 9 5)
;; '((2 3 5) (1 4 5) (1 3 6))
(triple-sum 10 6)
;; '()
(triple-sum 18 6)