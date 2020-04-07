#lang sicp
;; exercise 3.30
(define (full-adder a b c-in sum c-out)
  (let ((s (make-wire))
        (c1 (make-wire))
        (c2 (make-wire)))
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)
    'ok))

(define (ripple-carry carry AK BK SK)
  (define (inter c-in AK BK SK c-out)
    (if (null? AK)
        (display "over")
        (begin
          (full-adder (car AK) (car BK) c-in (car SK) c-out)
          (inter c-out (cdr AK) (cdr BK) (cdr SK) (mk-wire)))))
  (inter carry AK BK SK (mk-wire)))

;; half-add = 'and + max('and + 'inverter, 'or)
;; full-add = 2 * 'half-add + 'or
;; ripple-carry = n * 'full-add