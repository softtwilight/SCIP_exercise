#lang racket
(#%require "../../util/utils.rkt")

(define (element-of-set? x set)
  (accumulate (lambda (a b) (or a b))
              #f
              (map (lambda (s) (equal? s x))
                   set)))

(define (make-leaf symbol weight) (list 'leaf symbol weight))
(define (leaf? object) (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))
(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))
(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit: choose-branch()" bit))))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    ;; 
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair) ; symbol
                               (cadr pair)) ; frequency
                    (make-leaf-set (cdr pairs))))))

(define sample-tree (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree
                    (make-leaf 'D 1)
                    (make-leaf 'C 1)))))

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))
;; 2.67
;; '(A D A B B C A)
(decode sample-message sample-tree)

(define symbol-message '(A D A B B C A))

;; 2.68
(define (encode-symbol symbol tree)
  (define (helper x current-branch)
    (if (leaf? current-branch)
        '()
        (if (element-of-set? x (symbols (left-branch current-branch)))
            (cons 0 (helper x (left-branch current-branch)))
            (cons 1 (helper x (right-branch current-branch))))))
  (if (not (element-of-set? symbol (symbols tree)))
      (error "symbol not in the tree" symbol)
      (helper symbol tree)))
                           

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))
;; '(0 1 1 0 0 1 0 1 0 1 1 1 0)
(encode symbol-message sample-tree)


;; 2.69
(define (successive-merge nodes)
  (if (= 1 (length nodes))
      (car nodes)
      (let ((first (car nodes))
            (second (cadr nodes)))
        (successive-merge
         (adjoin-set (make-code-tree first second) (cddr nodes))))))
        
      
(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define pairs (list '(A 4) '(B 2) '(C 1) '(D 1)))
(define new-tree (generate-huffman-tree pairs))
sample-tree
new-tree
(encode symbol-message new-tree)

;; 2.70

(define lyrics (list '(A 2) '(GET 2) '(SHA 3) '(WAH 1)
                     '(BOOM 1) '(JOB 2) '(NA 16) '(YIP 9)))

(define lyrics-tree (generate-huffman-tree lyrics))
;lyrics-tree
(define song 
   '(GET A JOB SHA NA NA NA NA NA NA NA NA GET A JOB SHA NA NA NA NA NA NA NA NA WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP SHA BOOM)) 
;; 84
(length (encode song lyrics-tree))
;; minmun fixed-length code 108
(* 3 (length song))

;; 2.71
;; 1 bit for most frequent symbol, (n - 1) bit for least frequent symbol

;; 2.72
;; for most frequent symbol, is O(1)
;; for least frequent symbol, is O(n)