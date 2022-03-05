 #lang sicp
(#%require "../../util/utils.rkt")

(define (make-rat n d)
  (let ((g (gcd n d))
        (sign (if (< d 0) - +)))
        (cons (sign (/ n (abs g))) (abs (/ d g)))))


(assertEqual 1 (car (make-rat -2 -4)))
(assertEqual 1 (car (make-rat 2 4)))
(assertEqual -1 (car (make-rat -2 4)))
(assertEqual -1 (car (make-rat 2 -4)))
