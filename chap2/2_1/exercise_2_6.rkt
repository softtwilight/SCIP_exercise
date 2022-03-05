 #lang sicp
(#%require "../../util/utils.rkt")

(define zero (lambda (f) (lambda (x) x)))

; give a function, apply one more time to the function, when called by x
(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

; give a function, one is apply the function once
(define one
  (lambda (f) (lambda (x) (f x))))

; give a function, one is apply the function twice
(define two
  (lambda (f) (lambda (x) (f (f x)))))

; so a + b means apply the function a + b times
(define (add a b)
  (lambda (f) (lambda (x) ((a f) ((b f) x))))) 

(define three (add-1  two))

(assertEqual 0 ((zero inc) 0))
(assertEqual 1 (((add zero one) inc) 0))
(assertEqual 16 (((add-1 (add-1 zero)) square) 2))
(assertEqual 4 (((add-1 (add-1 two)) inc) 0))
(assertEqual 4294967296 (((add three two) square) 2))