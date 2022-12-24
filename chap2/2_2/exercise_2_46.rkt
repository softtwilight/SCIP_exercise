#lang racket
(require sicp-pict)
;; api from
;; https://docs.racket-lang.org/sicp-manual/SICP_Picture_Language.html#%28part._.Vectors%29

(define (frame-coord-map frame)
  (lambda (v)
    (vector-add
     (frame-origin frame)
     (vector-add (vector-scale (vector-xcor v) (frame-edge1 frame))
               (vector-scale (vector-ycor v) (frame-edge2 frame))))))
;(define a-frame
;  (make-frame (make-vect 0 0)
;              (make-vect 1 0)
;              (make-vect 0 1)))
;((frame-coord-map a-frame) (make-vect 2 3))
;(frame-origin a-frame)


(define make-vect cons)
(define xcor-vect car)
(define ycor-vect cdr)

(define (add-vect v1 v2)
  (make-vect (+ (xcor-vect v1) (xcor-vect v2))
             (+ (ycor-vect v1) (ycor-vect v2))))

(define (sub-vect v1 v2)
  (make-vect (- (xcor-vect v1) (xcor-vect v2))
             (- (ycor-vect v1) (ycor-vect v2))))

(define (scale-vect s v)
  (make-vect (* s (xcor-vect v))
             (* s (ycor-vect v))))

   
(define v1 (make-vect 1 2))
(define v2 (make-vect 1 1))
(add-vect v1 v2)
(sub-vect v1 v2)
(scale-vect 3 v1)
