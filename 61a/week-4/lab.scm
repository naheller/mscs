#lang simply-scheme

; #1
(define x (cons 4 5))
(define y (cons 'hello 'goodbye))
(define z (cons x y))

; #2
; (cdr (car z)) 
; Guess: 5
; Actual: 5

; (car (cons 8 3))
; Guess: 8
; Actual: 8

; (car z)
; Guess: (4 5)
; Actual: '(4 . 5)

; (car 3)
; Guess: Error
; Actual: Error

; #3
(define (make-rational num den)
  (cons num den))

(define (numerator rat)
  (car rat))

(define (denominator rat)
  (cdr rat))

(define (*rat a b)
  (make-rational (* (numerator a) (numerator b))
		 (* (denominator a)(denominator b))))

(define (print-rat rat)
  (word (numerator rat) '/ (denominator rat)))

; #5 +rat
(define (+rat a b)
  (define a2 (make-rational
	       (* (numerator a) (denominator b))
	       (* (denominator a) (denominator b))))
  (define b2 (make-rational
	       (* (numerator b) (denominator a))
	       (* (denominator b) (denominator a))))
  (make-rational
    (+ (numerator a2)(numerator b2))
    (denominator a2)))

; #6
; Ex 2.2

(define (make-point x y) (cons x y))
(define (x-point p) (car p))
(define (y-point p) (cdr p))

(define (make-segment p1 p2) (cons p1 p2))
(define (start-segment s) (car s))
(define (end-segment s) (cdr s))

(define (midpoint-segment s)
  (make-point (/ (+ (car (start-segment s))
		    (car (end-segment s)))
		 2)
	      (/ (+ (cdr (start-segment s))
		    (cdr (end-segment s)))
		 2)))

; Ex 2.3
(define (make-rect p1 p2 p3 p4)
  (define s1 (make-segment p1 p2))
  (define s2 (make-segment p2 p3))
  (define s3 (make-segment p3 p4))
  (define s4 (make-segment p4 p1))

  (cons (cons s1 s2)
	(cons s3 s4)))

(define (square x) (* x x))

(define (length-seg s)
  (define p1 (car s))
  (define p2 (cdr s))
  (sqrt
    (+ (square (- (car p2) (car p1)))
       (square (- (cdr p2) (cdr p1))))))

(define (length-width-rect r)
  (define s1-length (length-seg (car (car r))))
  (define s2-length (length-seg (cdr (car r))))
  (define s3-length (length-seg (car (cdr r))))

  (define length (if (> s1-length s2-length)
		   s1-length
		   (if (> s1-length s3-length)
		     s1-length
		     s3-length)))

  (define width (if (< s1-length s2-length)
		   s1-length
		   (if (< s2-length s3-length)
		     s2-length
		     s3-length)))

  (cons length width))

(define (area r)
  (define lw (length-width-rect r))
  (* (car lw) (cdr lw)))

(define (perimeter r)
  (define lw (length-width-rect r))
  (+ (* (car lw) 2) (* (cdr lw) 2)))
