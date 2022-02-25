 #lang sicp
(#%require "../util/utils.rkt")
(#%require (lib "27.ss" "srfi"))

;; 1.24

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp) (% (square (expmod base (/ exp 2) m))
                        m))
        (else (% (* base (expmod base (- exp 1) m))
                 m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random-integer (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))
(define (prime? n) (fast-prime? n 100))


(define (report-prime n elapsed-time)
  (display n)
  (display " *** ")
  (display elapsed-time)
  (newline)
  true)
(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime n (- (runtime) start-time))
      #f
      ))

(define (timed-prime-test n)
  (start-prime-test n (runtime)))

(define (search-for-primes from n)
  (cond ((= n 0)   (display "\n"))
        ((timed-prime-test from)
         (search-for-primes (+ from 1) (- n 1)))
        (else (search-for-primes (+ from 1) n))))

  
(search-for-primes 1000 3)
(search-for-primes 10000 3)
(search-for-primes 100000 3)
(search-for-primes 10000000 3)
(search-for-primes 10000000000000 3)