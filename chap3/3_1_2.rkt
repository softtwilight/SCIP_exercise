#lang sicp


(define (exper trials testor)
  (define (iter remaining passed)
    (cond ((= remaining 0) (/ passed trials 1.0))
          ((testor) (iter (- remaining 1)
                          (+ passed 1)))
          (else (iter (- remaining 1) passed))))
  (iter trials 0))

;; exercise 3.5
  
(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))
(define (square x)
  (* x x))

 (define (estimate-integral P x1 x2 y1 y2 trials) 
   (* (* (- x2 x1) 
         (- y2 y1)) 
      (exper trials P))) 
  
 (define (in-circle) 
   (>= 1 (+ (square (random-in-range -1.0 1.0)) 
            (square (random-in-range -1.0 1.0))))) 
  
  
 (define (estimate-pi) 
   (estimate-integral in-circle -1.0 1.0 -1.0 1.0 1000000))

 (estimate-pi)

;; exercise 3.6
(define (rand-update x) (+ x 1))
(define rand
  (let ((x 0))
    (define (generate)
      (begin (set! x (rand-update x))
             x))
    (define reset
      (lambda (new-value)
        (set! x new-value)))
    (define (dispatch m)
      (cond ((eq? m 'g) (generate)) ;; if message is generate, return the rand result.
            ((eq? m 'r) reset) ;; if message is reset, return the function reset the initial value.
            (else "error")))
    dispatch))
(rand 'g)
(rand 'g)
((rand 'r) 5)
(rand 'g)