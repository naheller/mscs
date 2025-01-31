#lang simply-scheme

(define (filtered-accumulate a b null-value pred combiner term next)
  (define (a-or-null)
    (if (pred a)
      a
      null-value))
  (if (> a b) null-value
    (combiner (term (a-or-null))
		(filtered-accumulate 
		  (next a) 
		  b 
		  null-value 
		  pred 
		  combiner 
		  term 
		  next))))

(define (prime? x)
  (define (get-divs y)
    (cond ((eqv? y 1) 1)
	  ((eqv? (modulo x y) 0) (+ 1 (get-divs (- y 1))))
	  (else (get-divs (- y 1)))
	  ))
  (eqv? (get-divs x) 2))

(define (sum-squares-primes a b)
  (filtered-accumulate
	 a
	 b
	 0
	 prime?
	 +
	 (lambda (x) (* x x))
	 (lambda (x) (+ x 1))
	 ))


