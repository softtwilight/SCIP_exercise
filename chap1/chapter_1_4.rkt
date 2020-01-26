#lang sicp

;;exercise 1.29
(define (sum func a next b)
  (if (> a b)
      0
      (+ (func a)
         (sum func (next a) next b))))
(define (indentity x) x)
(define (cube x) (* x x x))
(define (fixed-n x)
  (+ x (remainder x 2)))
(define (simpson f a b n)
  (define h (/ (- b a) (fixed-n n)))
  (define (simpson-term k)
    (define y (f (+ a (* k h))))
    (if (or (= k 0) (= k (fixed-n n)))
        (* 1 y)
        (if (even? k)
            (* 2 y)
            (* 4 ))))
  (* (/ h 3) (sum simpson-term 0 inc (fixed-n n))))

(simpson cube 0 1 1000) ;; -> 2250001/3000000

;;exer 1.30
(define (sum2 term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0.0))

;;exer 1.31
;; compute pi
(define (product term a next b)
  (if (> a b)
      1
      (* (term a) (product term (next a) next b))))
(define (product2 term a next b)
  (define (iter a res)
    (if (> a b) res
        (iter (next a) (* res (term a)))))
  (iter a 1))

(define (func n)
  (if (even? n)
      (/ (+ n 2) (+ n 1))
      (/ (+ n 1) (+ n 2))))
(define (next x) (+ x 1))
(define (fixed-n x)
  (+ x (remainder x 2)))
(define (factorial n)
  (product2 func 1.0 next (fixed-n n)))
(* 4 (factorial 500000)) ;; ->3.141595795171249


;;exer 1.32
;; recursive-version
(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate combiner null-value term (next a) next b))))

;;iterative-version
(define (accumulate-iter combiner null-value term a next b)
  (define (iter a res)
    (if (> a b) res
        (iter (next a) (combiner res (term a)))))
  (iter a null-value))

(define (product term a next b)
  (accumulate * 1 term a next b))
(define (sum term a next b)
  (accumulate + 0 term a next b))

;;exer 1.33
;;accumulate with filter of a
(define (filtered-accumulate combiner null-value filter term a next b)
  (if (> a b)
      null-value
      (combiner (if (filter a) (term a) null-value)
                (filtered-accumulate combiner null-value filter term (next a) next b))))
(define (squares x) (* x x))
(define (indentity x) x)
(define (smallest-div n) 
  (define (divides? a b) 
    (= 0 (remainder b a))) 
  (define (find-div n test) 
    (cond ((> (squares test) n) n) ((divides? test n) test) 
          (else (find-div n (+ test 1))))) 
  (find-div n 2)) 
  
(define (prime? n) 
     (if (= n 1) false (= n (smallest-div n))))

(define (func-a a b)
  (filtered-accumulate + 0 prime? squares a inc b))
(define (func-b a b)
  (filtered-accumulate * 1 prime? indentity a inc b))

(func-a 2 8);; -> 87
(func-b 2 5);; -> 2 * 3 * 5 = 30
