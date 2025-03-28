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

; a)

(define left-branch car)

(define (right-branch mobile)
  (car (cdr mobile)))

(define branch-length car)

(define (branch-struct branch)
  (car (cdr branch)))

; b)

(define (total-weight m)
  (define (twb b)
    (if (number? (branch-struct b))
      (branch-struct b)
      (+ 
	(twb (left-branch (branch-struct b)))
	(twb (right-branch (branch-struct b))))))
  (+ (twb (left-branch m)) (twb (right-branch m))))

(define mob1 (make-mobile (make-branch 3 6)
			  (make-branch 2 9)))

(define mob2 (make-mobile (make-branch 6 4)
			  (make-branch 5 mob1)))

(define mob3 (make-mobile (make-branch 15 3)
			  (make-branch 3 mob1)))
; c)
(define (b-torq b)
  (* (branch-length b)
     (if (pair? (branch-struct b))
         (total-weight (branch-struct b))
         (branch-struct b))))

(define (balanced m)
  (define (iter sub-m)
    (if (not (pair? sub-m))
        #t
        (and (=
              (b-torq (left-branch sub-m))
              (b-torq (right-branch sub-m)))
             (and
              (balanced (branch-struct (left-branch sub-m)))
              (balanced (branch-struct (right-branch sub-m)))))))
  (iter m))

(balanced mob1)
(balanced mob2)
(balanced mob3)

; Ex 2.21
(define (sq x) (* x x))

(define (square-list l)
  (if (null? l)
    '()
    (cons (sq (car l)) (square-list (cdr l)))))

(define (square-list-2 l)
  (map sq l))

(define t1 (list 1 (list 2 (list 3 4) 5) (list 6 7)))

; Ex 2.30
(define (sq-tree t)
  (if (null? t)
    '()
    (if (number? (car t))
      (cons (sq (car t)) (sq-tree (cdr t)))
      (cons (sq-tree (car t)) (sq-tree (cdr t))))))
      
; Ex 2.31
(define (tree-map f t)
  (if (null? t)
    '()
    (if (not (pair? (car t)))
      (cons (f (car t)) (tree-map f (cdr t)))
      (cons (tree-map f (car t)) (tree-map f (cdr t))))))

; Ex 2.32
(define (subsets s)
  (if (null? s)
    (list null)
    (let ((rest (subsets (cdr s))))
      (append rest (map (lambda (set) (cons (car s) set)) rest)))))

; Ex 2.36
(define (acc op init seq)
  (if (null? seq)
    init
    (op (car seq)
	(acc op init (cdr seq)))))

(define (acc-n op init seqs)
  (if (null? (car seqs))
    null
    (cons (acc op init (map car seqs))
	  (acc-n op init (map cdr seqs)))))

(define seqs1 (list (list 1 2 3) (list 4 5 6) (list 7 8 9)))

; Ex 2.54
(define (xor x y)
  (if x
    (not y)
    (if y
      (not x)
      #f)))

(define (eq-lists? l1 l2)
  (if (and (null? l1) (null? l2))
    #t
    (if (xor (null? l1) (null? l2))
      #f
      (and
	(eq? (car l1) (car l2))
	(eq-lists? (cdr l1) (cdr l2))))))

(define l1 '(this is a list))
(define l2 '(this (is a) list))

(define (eql? l1 l2)
  (if (and (null? l1) (null? l2))
    #t
    (cond ((and (not (pair? (car l1))) (not (pair? (car l2))))
	    (and (eq? (car l1) (car l2)) (eql? (cdr l1) (cdr l2))))
	   ((and (pair? (car l1)) (pair? (car l2)))
	    (and (eql? (car l1) (car l2)) (eql? (cdr l1) (cdr l2))))
	   (else #f))))
