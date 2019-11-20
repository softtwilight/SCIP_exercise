#lang sicp
;; exercise_2.58
;; b   s (x + 3 * (x + y + 2))
;; we must to know calculate which part first, so
;; can we add () for multiple operation?


(define (accumulate op initial sequence) 
  (if (null? sequence) 
      initial 
      (op (car sequence) 
          (accumulate op initial (cdr sequence)))))
;; add operation
(define (addend s) (car s))
(define (augend s)    
   (caddr s))
(define (sum? x) (and (pair? x) (pair? (cdr x)) (eq? (cadr x) '+)))
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2))
         (+ a1 a2))
        (else (list a1 '+ a2))))

;; multiple operation
(define (multiplier p) (car p))
(define (multiplicand p)  
  (caddr  p))
(define (product? x) (and (pair? x) (pair? (cdr x)) (eq? (cadr x) '*)))
(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list m1 '* m2))))

;; exponentiation operation
(define (expo? x) (and (pair? x) (pair? (cdr x)) (eq? (cadr x) '**)))
(define (base p) (car p))
(define (exponent p) (caddr p))
(define (make-expo b e)
  (cond ((=number? e 0) 1)
        ((=number? e 1) b)
        (else (list b '** e))))

(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))
(define (=number? exp num) (and (number? exp) (= exp num)))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        ((sum? exp) (make-sum (deriv (addend exp) var)
                              (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (multiplicand exp)
                        (deriv (multiplier exp) var))))
        ((expo? exp)
         (make-product (make-product (exponent exp)
                          (make-expo (base exp)
                                     (make-sum (exponent exp) -1)))
                       (deriv (base exp) var)))
        (else
         (error "unknown expression type: DERIV" exp))))

(deriv '(x + (3 * (x + (y + 2)))) 'x)
(cddr '(x + (3 * (x + (y + 2)))))
(deriv '(x + 3 * (x + y + 2)) 'x)
(cddr '(x + 3 * (x + (y + 2))))
(multiplier (cddr '(x + 3 * (x + y + 2))))
