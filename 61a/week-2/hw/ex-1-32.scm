#lang simply-scheme

(define (accumulate a b null-value combiner term next)
  (if (> a b) null-value
    ((combiner) (term a)
		(accumulate (next a) b null-value combiner term next)
		)))

(define (sum a b)
  (accumulate 
    a
    b
    0
    (lambda () +)
    (lambda (x) x) 
    (lambda (x) (+ x 1)) 
  ))

(define (product a b)
  (accumulate 
    a
    b
    1
    (lambda () *)
    (lambda (x) x) 
    (lambda (x) (+ x 1)) 
  ))
