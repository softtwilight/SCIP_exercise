#lang sicp
;;lambda

;;exercise 1.34
(define (f g) (g 2))
(f (lambda (z) (* z z)));; -> 4


(define (close-enough? a b)
  (< (abs (- b a)) 0.001))
(define (search f neg pos)
  (let ((midpoint (/ (+ neg pos) 2)))
    (if (close-enough? neg pos)
        midpoint
        (let ((test-value (f midpoint)))
          (cond ((> test-value 0) (search f neg midpoint))
                ((< test-value 0) (search f midpoint pos))
                ((= test-value 0) midpoint))))))
(define (half-interval-method f a b)
  (let ((a-value (f a))
        (b-value (f b)))
    (cond ((and (negative? a-value) (positive? b-value))
           (search f a b))
          ((and (negative? b-value) (positive? a-value))
           (search f b a))
          (else (error "values not opppsite")))))
(half-interval-method sin 2.0 -1.0)
(half-interval-method (lambda (x) (- (* x x x) (* 2 x) 3))
0.0
5.0)


;; fixed-point find
(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))
(fixed-point cos 1.0)
(fixed-point sin 1.0)
(define (sqrt x)
  (fixed-point (lambda (y) (/ (+ y (/ x y)) 2)) 1.0))
(sqrt 9)

;;exercise 1.35
(fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0) ;; -> 1.6180327868852458

;;exercise 1.36
(define (average v1 v2) (/ (+ v1 v2) 2))
;;average damping version
;; step = 10
(fixed-point (lambda (x) (average x (/ (log 1000) (log x)))) 2)
;; common version step = 35
(fixed-point (lambda (x) (/ (log 1000) (log x))) 2)


;;exercise 1.37
(define (cont-frac n d k)
  (define (iter n d k res)
    (if (= k 0)
        res
        (iter n d (- k 1) (/ (n k) (+ (d k) res)))))
  (iter n d k 0))
(define (cont-frac-recur n d k)
  (if (= k 1)
      (/ (n 1) (d 1))
      (/ (n k) (+ (d k) (cont-frac-recur n d (- k 1))))))

(cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           1000) ;; -> 0.6180339887498948
(cont-frac-recur (lambda (i) 1.0)
           (lambda (i) 1.0)
           10) ;; -> 0.6179775280898876

;;exercise 1.38
(define (d-euler n)
  (cont-frac (lambda (i) 1.0)
           (lambda (i)
             (if (= (modulo i 3) 2)
                 (* (/ (+ i 1) 3) 2)
                 1))
           n))
(d-euler 1000) ;; -> 0.7182818284590453

;;exercise 1.39
(define (tan-cf x n)
  (cont-frac (lambda (i)
               (if (= i 1)
                   x
                   (- (* x x))
                   ))
             (lambda (i) (- (+ i i) 1))
             n))
(tan-cf 1.0 10000) ;; -> 1.557407724654902
(tan 1.0) ;; -> 1.5574077246549023
