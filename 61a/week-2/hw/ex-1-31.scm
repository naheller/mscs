#lang simply-scheme

(define (product1 a b)
  (if (> a b) 1
    (* a (product1 (+ a 1) b))))

(define (product2 a b term next)
  (if (> a b) 1
    (* (term a) 
       (product2 (next a) b term next)
       )))

(product2 2 4 (lambda (x) x) (lambda (x) (+ x 1)))

(define (factorial x)
  (product2 1 x (lambda (x) x) (lambda (x) (+ x 1))))
