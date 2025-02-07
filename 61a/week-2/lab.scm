#lang simply-scheme

; #2
(define (substitute sent old-word new-word)
  (define (cond-replace word)
    (if (eqv? word old-word)
      new-word
      word))

  (if (= (count sent) 1)
    (cond-replace (first sent))
    (se (cond-replace (first sent)) (substitute (bf sent) old-word new-word))))

; #3
(define (g)
  (lambda (x) (+ x 2)))

; #4
; f		anything
; (f)		procedure that takes no args
; (f 3)		procedure that takes one arg
; ((f))		proc that returns a proc that takes no args 
; (((f)) 3)	proc that returns a proc that takes one arg	

; #7
(define (make-tester w)
  (lambda (x) (equal? x w)))
