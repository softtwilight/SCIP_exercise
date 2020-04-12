#lang sicp
;;exercise 3.31
;; if has not run the procedure, the output wire
;; will not change the value at the first. only when the
;; value of wire changes, the output will change, but at the circle
;; connecting moment, the value is default value.
;; image you are build a real eletric logical gate, when you connect the circle,
;; there is no result, but only when you change at least one of input. that's weird.