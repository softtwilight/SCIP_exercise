;money coin change problem
(define (count-change amount) (cc amount 4))
(define (first-deno coins)
  (cond ((= coins 1) 1)
        ((= coins 2) 5)
        ((= coins 3) 10)
        ((= coins 4) 50)))
(define (cc amount coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= coins 0)) 0)
        (else (+ (cc amount
                     (- coins 1))
                 (cc (- amount
                        (first-deno coins))
                     coins)))))
(count-change 100)

;exercise 1.11 recursive version
;n if n < 3
;f(n − 1) + 2f (n − 2) + 3f (n − 3) if n ≥ 3.
(define (exer-1.11 n)
  (if (< n 3) n
      (+ (exer-1.11 (- n 1))
         (* 2 (exer-1.11 (- n 2)))
         (* 3 (exer-1.11 (- n 3))))))

(exer-1.11 5)

;exercise 1.11 iterative version
(define (sum-iter a b c count)
  (cond ((= count 0) c)
        ((= count 1) b)
        ((= count 2) a)
        (else (sum-iter (+ a b b c c c)
                a
                b
                (- count 1)))))
(define (exer-1.11 n)
  (sum-iter 2 1 0 n))

(exer-1.11 3)


;exercise 1.12
(define (Pascal h n)
  (cond ((= n 1) 1)
        ((= h n) 1)
        (else (+
               (Pascal (- h 1) (- n 1))
               (Pascal (- h 1) n)))))
(Pascal 5 3)
