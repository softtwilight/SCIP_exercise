#lang sicp
;; 3.3.3 Representing Tables
;; one-dimensional table.
(define (lookup key table)
  (let ((record (assoc key (cdr table))))
    (if record
        (cdr record)
        #f)))
(define (assoc key records)
  (cond ((null? records) #f)
        ((eq? key (caar records)) (car records))
        (else (assoc key (cdr records)))))
(define (insert! key value table)
  (let ((record (assoc key (cdr table))))
    (if record
        (set-cdr! record value)
        (set-cdr! table
                  (cons (cons key value
                        (cdr table)))))
  'ok))
(define (make-table)
  (list '*table*))

;; two-dimensional table. each value is indexed by two keys.
(define (lookup2 key-1 key-2 table)
  (let ((subtable (assoc key-1 (cdr table))))
    (if subtable
        (let ((record (assoc key-2 (cdr subtable))))
          (if record
              (cdr record)
              #f))
        #f)))
(define (insert2! key-1 key-2 value table)
  (let ((subtable (assoc key-1 (cdr table))))
    (if subtable
        (let ((record (assoc key-2 (cdr subtable))))
          (if record
              (set-cdr! record value)
              (set-cdr! subtable
                        (cons (key-2 value)
                              (cdr subtable)))))
        (set-cdr! table
                  (cons (list key-1
                              (cons key-2 value))
                        (cdr table)))))
  'ok)

(define (assoc2 key records same-key?)
  (cond ((null? records) #f)
        ((same-key? key (caar records)) (car records))
        (else (assoc2 key (cdr records) same-key?))))

;; exercise 3.24
(define (make-table2 same-key?)
  (let ((local-table (list '*table*)))
    (define (lookup1 key-1 key-2)
      (let ((subtable (assoc2 key-1 (cdr local-table) same-key?)))
       (if subtable
           (let ((record (assoc2 key-2 (cdr subtable) same-key?)))
             (if record (cdr record) #f))
           #f)))
(define (insert1! key-1 key-2 value)
  (let ((subtable (assoc2 key-1 (cdr local-table) same-key?)))
    (if subtable
        (let ((record (assoc2 key-2 (cdr subtable) same-key?)))
          (if record
              (set-cdr! record value)
              (set-cdr! subtable
                        (cons (cons key-2 value) (cdr subtable)))))
        (set-cdr! local-table
                  (cons (list key-1 (cons key-2 value))
                        (cdr local-table)))))
'ok)
(define (dispatch m)
  (cond ((eq? m 'lookup-proc) lookup1)
        ((eq? m 'insert-proc!) insert1!)
        (else (error "Unknown operation: TABLE" m))))
dispatch))
;;(define operation-table (make-table2 (lambda (x y) #t)))
(define operation-table (make-table2 eq?))
(define get1 (operation-table 'lookup-proc))
(define put1 (operation-table 'insert-proc!))
(put1 'a '1 'd)
(get1 'a '1)
(get1 'a 'a)
(get1 '1 '2)

;; exercise 3.25 Generalizing one and two dimensional tables.

;; the n is unnessary in fact, we can get n from keys length.
;; TODO refactor.
(define (higl-order-make-table)
  (let ((local-table (list '*table*)))
    (define (iter-look table keys)
      (let ((sub (assoc (car keys) (cdr table))))
        (if (null? (cdr keys))
            (if sub
                (cdr sub)
                #f)    
            (if sub
                (iter-look sub (cdr keys))
                #f))))
    (define (inner-look keys)
      (iter-look local-table keys))

    (define (iter-insert table keys value)
      (let ((sub (assoc (car keys) (cdr table))))
        (if (null? (cdr keys))
            (if sub
                (set-cdr! sub value)
                (set-cdr! table
                          (cons (cons (car keys) value)
                                (cdr table))))
            (if sub
                (iter-insert sub (cdr keys) value)
                (begin (set-cdr! table
                                 (cons (list (car keys))
                                             (cdr table)))
                       (iter-insert table keys value))))))
        
    (define (inner-insert keys value)
      (iter-insert local-table keys value))
    (define (dispatch m)
      (cond ((eq? m 'insert) inner-insert)
            ((eq? m 'look-up) inner-look)
            (else (error "error msg" m))))
    dispatch))
(define t2 (higl-order-make-table))
((t2 'insert) (list 'a 'b 'c) 'yxl)
((t2 'insert) (list 'a 'b 'e 'f) 'douge)
((t2 'look-up) (list 'a 'b 'c))
((t2 'look-up) (list 'a 'b 'd))
 ((t2 'look-up) (list 'a 'b 'e 'f))
             

  