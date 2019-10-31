#lang sicp
(define one-through-four (list 1 2 3 4))
one-through-four
(car one-through-four)
(cdr one-through-four)
(cadr one-through-four)

(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))
(list-ref one-through-four 2)
(length one-through-four)

;; exercise_2.17
(define (last-pair li)
  (if (null? (cdr li))
      (car li)
      (last-pair (cdr li))))
(last-pair one-through-four)

;; exercise_2.18

(define nil '())
(define (reverse li)
  (define (inner-rev nl ol)
    (if (null? ol)
        nl
        (inner-rev (cons (car ol) nl) (cdr ol))))
  (inner-rev nil li))
(reverse (list 1 4 9 16 25))

;; exercise_2.19
(define (no-more? li)
  (null? li))
(define (except-first li)
  (cdr li))
(define (first-coin li)
  (car li))

(define (cc n coin-values)
  (cond ((= n 0) 1)
        ((or (< n 0) (no-more? coin-values)) 0)
        (else
         (+ (cc n (except-first coin-values))
            (cc (- n (first-coin coin-values)) coin-values)))))
(define us-coins (list 50 25 10 5 1))
(define us-coins2 (list 1 25 5 50 10))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))
(cc 100 us-coins2)
(cc 100 uk-coins)


;; exercise_2.20
(define (same-parity first . rest)

  (let ((yes? (if (even? first)
                  even?
                  odd?)))
  (define (same-parity-iter source re)
    (if (null? source)
        re
        (if (yes? (car source))
            (same-parity-iter (cdr source)
                              (append re (list (car source))))
            (same-parity-iter (cdr source) re))))
  (same-parity-iter rest (list first))))

(same-parity 1 2 3 4 5 6 7) ;; -> (1 3 5 7)
(same-parity 2 3 4 5 6 7) ;; -> (2 4 6)


    