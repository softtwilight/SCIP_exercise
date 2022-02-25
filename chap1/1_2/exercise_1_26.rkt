 #lang sicp
(#%require "../util/utils.rkt")

;; 1.26
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp) (% (*
                         (expmod base (/ exp 2) m)
                         (expmod base (/ exp 2) m))
                        m))
        (else (% (* base (expmod base (- exp 1) m))
                 m))))

;; T(n) = 2 * T(n/2) + c => θ(n)
;; T(n) = T(n/2) + c => θ(log n)