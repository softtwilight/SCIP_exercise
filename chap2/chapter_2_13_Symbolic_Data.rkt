#lang sicp
(define (memq item x)
  (cond ((null? x) false)
        ((eq? item (car x)) x)
        (else (memq item (cdr x)))))

;; exercise_2.54
(define (equal? x y)
    (cond ((and (symbol? x) (symbol? y))
            (symbol-equal? x y))
          ((and (list? x) (list? y))
            (list-equal? x y))
          (else
            (error "Wrong type input x and y -- EQUAL?" x y))))

(define (symbol-equal? x y)
    (eq? x y))

(define (list-equal? x y)
    (cond ((and (null? x) (null? y))
            #t)
          ((or (null? x) (null? y))     
            #f)
          ((equal? (car x) (car y))    
            (equal? (cdr x) (cdr y)))  
          (else #f)))
(equal? '(this is a list) '(this is a list))

;; exercise_2.55
(car ''abracadabra)
