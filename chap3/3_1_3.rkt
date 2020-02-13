#lang sicp
;; sanmeness, 判断相等是一件很难的事。
;; 银行账号钱不一样了，但还是同一个账户。一个自然数加一，不是原来的数了，但还是自然数。
;; 一般需要变化才能判断相等，a和b相等， 变化a，b也会跟着变化
;; 变化本身友蕴含着相等的概念的前提，没有相等，我们不知道什么是变化的。

;; imperative programming use a lot assignment.(set)
;; 必须认真的考虑set get 的相对顺序，复杂（想想高并发）。函数编程是不用考虑这个的。

;; exercise 3.7
(define (make-safe-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (error-p amount)
    "Incorrect password")
  (define (dispatch p m)
    (if (eq? p password)
        (cond ((eq? m 'w) withdraw)
          ((eq? m 'd) deposit)
          (else (error "Unknown : " m)))
        error-p
        ))
  dispatch)
(define (make-joint old-acc old-pass new-pass)
  (define (dispatch p m)
    (if (eq? p new-pass)
        (old-acc old-pass m)
        (lambda (x)
          "error password")))
  dispatch)
(define c1 (make-safe-account 100 'a))
(define c2 (make-joint c1 'a 'b))
((c2 'b 'w) 50) ;; -> 50
((c1 'a 'd) 40) ;; -> 90
((c2 'b 'w) 20) ;; -> 70

;; exercise 3.8

;; define a variable, if called, return arg, otherwise return 0;
;; the key is we must use assignment to change the influence of different call order.
(define f
  (let ((init #f))
    (lambda (x)
      (if init
          0
          (begin (set! init #t)
                 x)))))
(+ (f 0) (f 1)) ;; -> 0  but (+ (f 1) (f 0))  -> 1
  
    