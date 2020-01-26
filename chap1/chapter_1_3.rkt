;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname chapter_1_3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;exercise 1.14
;coin problem time and space analyse
;http://www.ysagade.nl/2015/04/12/sicp-change-growth/



;1.16 iterative exponentiation
(define (fast-expt2 b n a)
  (cond ((= n 0) a)
        ((even? n) (fast-expt2 (* b b) (/ n 2) a))
        (else (fast-expt2 (* b b) (/ (- n 1) 2) (* a b)))))

(define (fast-expt b n)
  (fast-expt2 b n 1))
(fast-expt 2 12)

;1.17 1.18 multiply implenments by + halve and double
(define (mul2 a b n)
  (cond ((= b 0) n)
        ((even? b) (mul2 (+ a a) (/ b 2) n))
        (else (mul2 a (- b 1) (+ a n)))))
(define (mul a b)
  (mul2 a b 0))
(mul 16 5)


;1.19 log(n) fib function
(define (fib-iter a b x y count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter a
                   b
                   (+ (* x x) (* y y))
                   (+ (* 2 x y) (* y y))
                   (/ count 2)))
        (else (fib-iter (+ (* b y) (* a y) (* a x))
                        (+ (* b x) (* a y))
                        x
                        y
                        (- count 1)))))
(define (fib n)
  (fib-iter 1 0 0 1 n))
(fib 400)
