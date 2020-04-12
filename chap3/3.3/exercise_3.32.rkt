#lang sicp
;; 3.32
;; if use list other than queue, the and-gate out put always is 0
;; but if we use queue, the first action 0 > 1 will result the output to 1.
;; then another input change the value 1 -> 0 output return 0;