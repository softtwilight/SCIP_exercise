#lang sicp
;; modular, divide naturally into coherent parts.
;; set语法：(set! ⟨name⟩ ⟨new-value⟩)
;; (begin ⟨exp1⟩ ⟨exp2⟩ . . . ⟨expk⟩) begin语法，依次执行ex，最后返回expk
(define balance 100)
(define (withdraw amount)
  (if (>= balance amount)
      (begin (set! balance (- balance amount))
             balance)
      "Insufficient funds"))
(withdraw 25)
(withdraw 25)

(define new-withdraw
  (let ((balance 100))
    (lambda (amount)
       (if (>= balance amount)
      (begin (set! balance (- balance amount))
             balance)
      "Insufficient funds"))))
(new-withdraw 60) ;; -> 40
(new-withdraw 50) ;; -> Insufficient funds

(define (make-withdraw balance)
  (lambda (amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds")))
((make-withdraw 40) 1) ;; -> 39

(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch m)
    (cond ((eq? m 'w) withdraw)
          ((eq? m 'd) deposit)
          (else (error "Unknown : " m))))
  dispatch)

(define m1 (make-account 100))
((m1 'd) 50)
((m1 'w) 70)

;; 3.1
(define (make-accumulator initial)
  (lambda (amount)
    (begin (set! initial (+ initial amount))
           initial)))
(define A (make-accumulator 5))
(A 10) ;; -> 15
(A 9) ;; -> 24

;; 3.2
(define (make-monitored f)
  (let ((times 0))
    (define (call arg)
        (begin (set! times (+ times 1))
               (f arg)))
  (define (dispatch m)
    (cond ((eq? m 'how-many-calls?) times)
          (else (call m))))
  dispatch))
(define s (make-monitored sqrt))
(s 100) ;; -> 10
(s 9) ;; -> 3
(s 'how-many-calls?) ;; -> 2 call 2 times.

;; 3.3
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
(define acc (make-safe-account 100 'secret-password))
((acc 'some-other-password 'd) 50) ;; -> Incorrect password
((acc 'secret-password 'w) 40) ;; -> 60

;; 3.4

(define (make-safest-account balance password)
  (let ((sec-error-times 0))
    (define (withdraw amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds"))
    (define (deposit amount)
      (set! balance (+ balance amount))
      balance)
    (define (error-p amount)
      (begin (set! sec-error-times (+ sec-error-times 1))
             (if (> sec-error-times 7)
                 "more than 7 error times, call the cops"
                 "Incorrect password")))
    (define (dispatch p m)
      (if (eq? p password)
          (begin (set! sec-error-times 0)
                 (cond ((eq? m 'w) withdraw)
                       ((eq? m 'd) deposit)
                       (else (error "Unknown : " m))))
          error-p
          ))
  dispatch))
(define ac (make-safest-account 100 'secret-password))
((ac 'wrongp 'w) 10) ;; -> "Incorrect password"
((ac 'wrongp 'w) 10)
((ac 'wrongp 'w) 10)
((ac 'wrongp 'w) 10)
((ac 'wrongp 'w) 10)
((ac 'wrongp 'w) 10)
((ac 'wrongp 'w) 10) ;; -> "Incorrect password"
((ac 'wrongp 'w) 10) ;; -> "more than 7 error times, call the cops"