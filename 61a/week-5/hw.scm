#lang simply-scheme

; Ex 2.27
(define (deep-reverse list)
  (define (iter new rem)
    (if (null? rem)
      new
      (if (pair? (car rem))
	(iter (cons (iter '() (car rem)) new) (cdr rem))
	(iter (cons (car rem) new) (cdr rem)))))
  (iter '() list))

; Ex 2.28
(define (fringe l)
  (if (null? l)
    '()
    (if (pair? (car l))
      (append (fringe (car l)) (fringe (cdr l)))
      (cons (car l) (fringe (cdr l))))))

; Ex 2.29
(define (make-mobile l r)
  (list l r))

(define (make-branch leng struct)
  (list leng struct))

(define left-branch car)
(define (right-branch mobile)
  (car (cdr mobile)))
(define branch-length car)
(define (branch-struct branch)
  (car (cdr branch)))

(define (total-weight mobile)
  (if (number? (branch-struct mobile))
    (branch-struct mobile)
    (+ (total-weight (left-branch mobile)) (total-weight (right-branch mobile)))))

(define (total-weight-2 mobile)
  (define (wb branch total)
    (if (number? (branch-struct branch))
      (branch-struct branch)
      (+ (wb (left-branch branch)) (wb (right-branch branch)))))
  (+ (wb (left-branch mobile)) (wb (right-branch mobile))))
    
