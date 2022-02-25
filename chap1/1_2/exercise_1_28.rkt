 #lang sicp
(#%require "../util/utils.rkt")
(#%require (lib "27.ss" "srfi"))

; 1.28 miller-rabin-test
; not fool by
; Carmichael number: 561, 1105, 1729, 2465, 2821, 6601...

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (if (and (!= base 1)
                  (!= base (- m 1))
                  (= (% (square base) m) 1))
             0
          (% (square (expmod base (/ exp 2) m)) m)))
        (else (% (* base (expmod base (- exp 1) m))
                 m))))

(define (miller-rabin-test n)
  (define (try-it a)
    (= (expmod a (- n 1) n) 1))
  (try-it (+ 1 (random-integer (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((miller-rabin-test n) (fast-prime? n (- times 1)))
        (else false)))

(define (carm-test n)
  (define (iter n a)
    (cond ((= a n) (print "all equal"))
          ((= (expmod a n n) a) (iter n (+ a 1)))
          (else (print a))))
  (iter n 2))


; test
(prime? 561)
(fast-prime? 561 10)
(prime? 1999)
(fast-prime? 1999 10)
(prime? 85846368679)
(fast-prime? 85846368679 10)
(carm-test 561)
(carm-test 1105)
(carm-test 6601)
