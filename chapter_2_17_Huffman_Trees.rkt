#lang sicp
(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
(define (leaf? node) (eq? 'leaf (car node)))
(define (symbol-leaf node) (cadr node))
(define (weight-leaf node) (caddr node))

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
        (else (error "bit bit: " bit))))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

;; add a node to a sorted set
(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)
                               (cadr pair))
                    (make-leaf-set (cdr pairs))))))

(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree
                    (make-leaf 'D 1)
                    (make-leaf 'C 1)))))
(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))
(decode sample-message sample-tree) ;; -> (A D A B B C A)

;; exercise_2.68


(define (contain? x set)
  (cond ((null? set) #f)
        ((eq? x (car set)) #t)
        (else (contain? x (cdr set)))))

;; recursive-version
(define (encode-symbol s tree)
  (define (inner result c-branch)
    (cond ((leaf? c-branch) result)
          ((contain? s (symbols (left-branch c-branch)))
           (inner (append result (list '0)) (left-branch c-branch)))
          (else(inner (append result (list '1)) (right-branch c-branch)))))
  (inner '() tree))

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))
(define sample-symbol '(A D A B B C A))
(encode sample-symbol sample-tree)
(equal? (encode sample-symbol sample-tree) sample-message) ;; -> #t




;; exercise_2.69
(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))
(define sample-pairs (list '(A 4) '(B 2) '(C 1) '(D 1)))
(define (successive-merge orderset)
  (if (null? (cdr orderset))
      (car orderset)
      (successive-merge
       (adjoin-set (make-code-tree (car orderset) (cadr orderset))
                   (cddr orderset)))))

(encode sample-symbol (generate-huffman-tree sample-pairs)) ;; -> (A D A B B C A)
  
 ;; exercise_2.70
(define rock-song '(Get a job Sha na na na na na na na na Get a job Sha na na na na na na na na Wah yip yip yip yip yip yip yip yip yip Sha boom))
(define rocktree (generate-huffman-tree '((a 2) (na 16) (boom  1) (sha 3) (get 2) (yip 9) (job 2) (Wah 1)))) 
(length (encode rock-song rocktree)) ;; -> 180

;; exercise_2.71
;; min = 1, max = n - 1;

;; exercise_2.72
;; the balance case, we because the right tree has less to search
;; the best case is n; 1/2 possible, search n; the right regard symethric