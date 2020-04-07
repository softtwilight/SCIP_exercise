#lang sicp
;;exercise 3.29
(define (or-gate a1 a2 output)
  (let ((s (make-wire))
        (c1 (make-wire))
        (c2 (make-wire)))
  (inverter a1 c1)
  (inverter a2 c2)
    (and-gate c1 c2 s)
    (inverter s output)
    'ok))
;; the delay of the or-gate is 3 * inverter + and-gate