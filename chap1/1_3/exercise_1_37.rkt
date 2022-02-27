 #lang sicp

; 1.37
(define (cont-frac n d k)
  (define (help i result)
    (if (= i 0)
        result
        (help (- i 1) (/ (n i) (+ (d i) result)))))
  (help k 0))

(define (cont-frac-recursive n d k)
  (define (recur i)
    (if (> i k)
        0
        (/ (n i) (+ (d i) (recur (inc i))))))
  (recur 1))

(/ 1 (cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 100))
(/ 1 (cont-frac-recursive (lambda (i) 1.0) (lambda (i) 1.0) 10))