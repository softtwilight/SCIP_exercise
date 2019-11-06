#lang sicp

;; define the filter
(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

;; define the accumulate
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))


(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))

(define (enumerate-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))

(define (square x) (* x x))
(define (sum-odd-squares tree)
  (accumulate + 0
              (map square
                   (filter odd?
                           (enumerate-tree tree)))))

;; exercise 2.33
;; the op means combinor of the first value and rest value
(define list1 (list 1 2 3 4 5))
(define list2 (list 6 7 8 9))

(define (s-map p sequence)
  (accumulate (lambda (x y) (cons (p x) y))
              nil sequence))
(s-map square list1) ;; -> (1 4 9 16 25)

(define (s-append seq1 seq2)
  (accumulate cons
              seq2 seq1))
(s-append list1 list2) ;; -> (1 2 3 4 5 6 7 8 9)

(define (s-length sequence)
  (accumulate (lambda (x y) (+ 1 y))
              0 sequence))
(s-length (s-append list1 list2)) ;; -> 9

;; exercise_2.34
(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms)
                (+ this-coeff (* x higher-terms)))
              0
              coefficient-sequence))
(horner-eval 2 (list 1 3 0 5 0 1)) ;; -> 79

;; high order function
(define (hi-horner-eval coefficient-sequence)
  (lambda (x) 
  (accumulate (lambda (this-coeff higher-terms)
                (+ this-coeff (* x higher-terms)))
              0
              coefficient-sequence)))
((hi-horner-eval (list 1 3 0 5 0 1)) 2);; -> 79

;; exercise_2.35
;; recursive version
(define (count-leaves t)
  (accumulate + 0 (map (lambda (x)
                         (if (not (pair? x))
                              1
                             (count-leaves x)))
                       t)))
;; iterative version
(define (count-leaves-iter t)
  (accumulate +
              0
              (map (lambda (x) 1)
                   (enumerate-tree t))))

(define x (cons (list 1 2) (list 3 (list 4 5))))
(count-leaves x) ;; -> 5
(count-leaves-iter x) ;; -> 5


                