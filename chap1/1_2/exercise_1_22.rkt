 #lang sicp
(#%require "../util/utils.rkt")

;; 1.22
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