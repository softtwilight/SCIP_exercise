 #lang sicp
(#%require "../../util/utils.rkt")

(define us-coins (list 50 25 10 5 1))
(define us-coins-unorder (list 25 5 10 50 1))

(define jp-coins (list 500 100 50 10 5 1))

(define first-denomination car)
(define except-first-denomination cdr)
(define no-more? null?)
  
(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination coin-values))
            (cc (- amount
                   (first-denomination coin-values))
                coin-values)))))

(assertEqual 292 (cc 100 us-coins))
(assertEqual 292 (cc 100 us-coins-unorder))

;; TODO use the map to optimaze it. DP problem
(cc 500 jp-coins)

