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
