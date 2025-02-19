#lang simply-scheme

; Ex 2.20
(define (has-parity? x y)
  (if (= (remainder (- x y) 2) 0)
    #t
    #f))

(define (same-parity x . y)
  (define (sp new rem)
    (if (null? rem)
      new
      (if (has-parity? x (car rem))
	;(sp (cons (car rem) new) (cdr rem)) causes reversed list
	(sp (append new (list (car rem))) (cdr rem))
	(sp new (cdr rem)))))
  (cons x (sp '() y)))

; Ex 2.22
; Pt 1 - The first iterative square-list process results in a reversed
; list because (cons (square (car things) answer)) keeps placing the
; next item at the front of the list instead of the back.
; Pt 2 - Switching the arguments to cons doesn't provide the desired
; result because in (cons answer (square (car things))) we are creating
; a new pair where the first element is the entire list "answer", and the
; second element is the next item.

; Ex 2.23
(define (for-each-2 f l)
  (if (null? (cdr l))
    (f (car l))
    (and (f (car l)) (for-each-2 f (cdr l)))))

; Substitute
(define (get-new-word wd old new)
  (if (equal? wd old) new wd))

(define (substitute seq old new)
  (if (null? seq)
    '()
    (if (word? (car seq))
      (cons (get-new-word (car seq) old new) (substitute (cdr seq) old new))
      (cons (substitute (car seq) old new) (substitute (cdr seq) old new)))))

; Substitute 2
(define (get-word-at index seq)
  (if (= index 0)
    (car seq)
    (get-word-at (- index 1) (cdr seq))))

(define (get-index-of word seq)
  (define (iter index rem)
       (if (null? rem)
	 -1
	 (if (equal? word (car rem))
	   index
	   (iter (+ index 1) (cdr rem)))))
  (iter 0 seq))

(define (get-new-word-2 wd seq-old seq-new)
  (define index (get-index-of wd seq-old))
  (if (> index -1)
    (get-word-at index seq-new)
    wd))


(define (substitute-2 seq old new)
  (if (null? seq)
    '()
    (if (word? (car seq))
      (cons (get-new-word-2 (car seq) old new) (substitute-2 (cdr seq) old new))
      (cons (substitute-2 (car seq) old new) (substitute-2 (cdr seq) old new)))))
