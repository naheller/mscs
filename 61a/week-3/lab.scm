#lang simply-scheme

; #4 Type Check
(define (type-check f pred value)
  (if (pred value) (f value)
    #f))

; #5 Safe sqrt
(define (make-safe f pred)
  (lambda (value)
    (type-check f pred value)))

(define safe-sqrt (make-safe sqrt number?))
