#lang simply-scheme

(define (compose f g)
  (lambda (x) (f (g x))))

(define (inc x) (+ x 1))

(define (sq x) (* x x))

(define (repeated f n)
  (define (recompose subfn count)
    (if (< count 2)
      (lambda (x) (subfn x))
      (recompose (compose subfn subfn) (- count 1))))
  (recompose f n))

(define (repeated2 f n)
  (if (< n 2)
    (lambda (x) (f x))
    (compose f (repeated2 f (- n 1)))))
