#lang sicp

;;exercise 1.46
(define (iter-improve good-enough? improve)
  (lambda (x)
    (define (iter n)
      (if (good-enough? n)
          n
          (iter (improve n))))
    (iter x)))

(define (close-enough? v1 v2) 
   (< (abs (- v1 v2)) 0.0001)) 
(define (fixed-point f guess)
  ((iter-improve
    (lambda (x) (close-enough? x (f x)))
    f)
   guess))

(define (sqrt x) 
   ((iter-improve 
     (lambda (y) 
       (< (abs (- (* y y) x)) 
          0.0001)) 
     (lambda (y) 
       (/ (+ y (/ x y)) 2))) 
    1.0))

(sqrt 4);; -> 2.0000000929222947