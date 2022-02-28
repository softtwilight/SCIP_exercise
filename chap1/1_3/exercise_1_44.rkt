 #lang sicp
(#%require "../util/utils.rkt")

; 1.44
(define (smooth f)
  (define dx 0.00001)
  (lambda (x)
    (/ (+ (f (- x dx))
          (f x)
          (f (+ x dx)))
       3)))
(define (n-fold-smooth f n)
  ((repeat f n) f))
