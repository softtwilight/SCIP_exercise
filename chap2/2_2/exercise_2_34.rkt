#lang racket
(#%require "../../util/utils.rkt")

(define (horner-eval x coefficient-seq)
  (accumulate (lambda (this-coeff higher-terms)
                (+ this-coeff (* x higher-terms)))
              0
              coefficient-seq))

;; x^2 + 3x + 2
(assertEqual (horner-eval 3 (list 2 3 1)) 20)
;; 1 + 3x + 5x^3 + x^5
(assertEqual (horner-eval 2 (list 1 3 0 5 0 1)) 79)