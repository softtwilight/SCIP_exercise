;1.2
(/ (+  5 4 (- 2 (- 3 (+ 6 (/ 4 5))))) (* 3 (- 6 2) (- 2 7)))

;1.3
(define (sum-of-big-two x y z)
  (+ (max x y) (max (min y x) z)))
(sum-of-big-two 1 2 3)

;1.4
(define (p) (p))
(define (test-order x y)
  (if (= x 0) 0 y))
(test-order 0 (p))

;Newton sqrt method
(define (square x) (* x x))
(define (improve guess x)
  (average guess (/ x guess)))
(define (average x y) (/ (+ x y) 2))
(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))
(define (sqrt-iter guess x)
  (if (good-enough? guess x) guess (sqrt-iter (improve guess x) x)))

(define (sqrtx x)
  (sqrt-iter 1.0 x))
(sqrtx 8)

;1.7
(define (good-enough-diff-version? guess x)
  (< (abs (/ (- guess (improve guess x)) guess)) 0.0001))

;1.8
(define (cube x) (* x x x))
(define (improve guess x)
  (/ (+ (/ x guess guess) guess guess) 3))
(define (good-enough? guess x)
  (< (abs (- (cube guess) x)) 0.01))

(define (sqrt-iter guess x)
  (if (good-enough? guess x) guess (sqrt-iter (improve guess x) x)))

(define (sqrt-cube x)
  (sqrt-iter 1.0 x))
(sqrt-cube 27)

;fib
(define (fib-iter a b count)
  (if (= count 0)
      b
      (fib-iter (+ a b) a (- count 1))))
(define (fib n)
  (fib-iter 1 0 n))
(fib 13)
