#lang racket/gui
;;(#%require sicp-pict)
(#%require graphics/graphics)
;(#%require teachpacks/racket-turtle)
;; Code for CS61A project 2 -- picture language

(open-graphics)
(define vp (open-viewport "A Picture Language" 500 500))

(define draw (draw-viewport vp))
(define (clear) ((clear-viewport vp)))
(define line (draw-line vp))

;need a wrapper function so that the graphics library works with my code...
(define (vector-to-posn v)
  (make-posn (car v) (car (cdr v))))

(define (below p1 p2)
  (lambda (frame)
    (define paint-top
      (transform-painter 
       p1
       (make-vect 0 0.5)
       (make-vect 1 0.5)
       (make-vect 0 1)))
    (define paint-bottom
      (transform-painter 
       p2
       (make-vect 0 0)
       (make-vect 1 0)
       (make-vect 0 0.5)))
    (paint-top frame)
    (paint-bottom frame)))

(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))

(define (flip-horiz painter)
  (transform-painter
   painter
   (make-vect 1 0)
   (make-vect 0 0)
   (make-vect 1 1)))

(define (make-vect x y)
  (cons x y))

(define (xcor-vect v)
  (car v))

(define (ycor-vect v)
  (cdr v))

(define (add-vect v1 v2)
  (define x (+ (xcor-vect v1) (xcor-vect v2)))
  (define y (+ (ycor-vect v1) (ycor-vect v2)))
  (make-vect x y))

(define (sub-vect v1 v2)
  (define x (- (xcor-vect v1) (xcor-vect v2)))
  (define y (- (ycor-vect v1) (ycor-vect v2)))
  (make-vect x y))

(define (scale-vect s v)
  (make-vect
   (* s (xcor-vect v))
   (* s (ycor-vect v))))

(define (make-frame origin e1 e2)
  (list origin e1 e2))

(define (origin-frame f)
  (car f))

(define (edge1-frame f)
  (cadr f))

(define (edge2-frame f)
  (caddr f))

(define (make-segment start-v end-v)
  (cons start-v end-v))

(define (start-segment seg)
  (car seg))

(define (end-segment seg)
  (cdr seg)) 

(define (flipped-pairs painter)
  (let ((painter2 (beside painter (flip-vert painter))))
    (below painter2 painter2)))

(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
	(beside painter (below smaller smaller)))))

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
	    (right (right-split painter (- n 1))))
	(let ((top-left (beside up up))
	      (bottom-right (below right right))
	      (corner (corner-split painter (- n 1))))
	  (beside (below painter top-left)
		  (below bottom-right corner))))))

(define (square-limit painter n)
  (let ((quarter (corner-split painter n)))
    (let ((half (beside (flip-horiz quarter) quarter)))
      (below (flip-vert half) half))))

(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
	  (bottom (beside (bl painter) (br painter))))
      (below bottom top))))

(define (identity x) x)

;(define (flipped-pairs painter)
;  (let ((combine4 (square-of-four identity flip-vert
;				  identity flip-vert)))
;    (combine4 painter)))

;; or

; (define flipped-pairs
;   (square-of-four identity flip-vert identity flip-vert))

;(define (square-limit painter n)
;  (let ((combine4 (square-of-four flip-horiz identity
;				  rotate180 flip-vert)))
;    (combine4 (corner-split painter n))))

(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (xcor-vect v)
			   (edge1-frame frame))
	       (scale-vect (ycor-vect v)
			   (edge2-frame frame))))))

(define (segments->painter-old1 segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (draw-line
	((frame-coord-map frame) (start-segment segment))
	((frame-coord-map frame) (end-segment segment))))
     segment-list)))

(define (segments->painter-old2 segment-list)   
  (lambda (frame)     
   (for-each     
     (lambda (segment)        
      (line         
        (vector-to-posn ((frame-coord-map frame) (start-segment segment)))         
        (vector-to-posn ((frame-coord-map frame) (end-segment segment)))))      
      segment-list)))

(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (let ((start-coord-map ((frame-coord-map frame) (start-segment segment)))
             (end-coord-map ((frame-coord-map frame) (end-segment segment))))
       (line
        (make-posn (xcor-vect start-coord-map) (ycor-vect start-coord-map))
        (make-posn (xcor-vect end-coord-map) (ycor-vect end-coord-map)))))
     segment-list)))

(define x-segs (list
                (make-segment (make-vect 0 0) (make-vect 1 1))
                (make-segment (make-vect 1 0) (make-vect 0 1))))

(define diamond-segs (list
                (make-segment (make-vect 0 0.5) (make-vect 0.5 0))
                (make-segment (make-vect 0.5 0) (make-vect 1 0.5))
                (make-segment (make-vect 1 0.5) (make-vect 0.5 1))
                (make-segment (make-vect 0.5 1) (make-vect 0 0.5))))

(define x-painter (segments->painter x-segs))
(define diamond-painter (segments->painter diamond-segs))

;(define (draw-line v1 v2)
;  (pen-up)
;  (go-to (- (* (xcor-vect v1) 200) 100)
;	 (- (* (ycor-vect v1) 200) 100))
;  (pen-down)
;  (go-to (- (* (xcor-vect v2) 200) 100)
;	 (- (* (ycor-vect v2) 200) 100)))

(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
	(painter
	 (make-frame new-origin
		     (sub-vect (m corner1) new-origin)
		     (sub-vect (m corner2) new-origin)))))))

(define (flip-vert painter)
  (transform-painter painter
		     (make-vect 0.0 1.0)
		     (make-vect 1.0 1.0)
		     (make-vect 0.0 0.0)))

(define (shrink-to-upper-right painter)
  (transform-painter painter
		    (make-vect 0.5 0.5)
		    (make-vect 1.0 0.5)
		    (make-vect 0.5 1.0)))

(define (rotate90 painter)
  (transform-painter painter
		     (make-vect 1.0 0.0)
		     (make-vect 1.0 1.0)
		     (make-vect 0.0 0.0)))

(define (squash-inwards painter)
  (transform-painter painter
		     (make-vect 0.0 0.0)
		     (make-vect 0.65 0.35)
		     (make-vect 0.35 0.65)))

(define (beside painter1 painter2)
  (let ((split-point (make-vect 0.5 0.0)))
    (let ((paint-left
	   (transform-painter painter1
			      (make-vect 0.0 0.0)
			      split-point
			      (make-vect 0.0 1.0)))
	  (paint-right
	   (transform-painter painter2
			      split-point
			      (make-vect 1.0 0.0)
			      (make-vect 0.5 1.0))))
      (lambda (frame)
	(paint-left frame)
	(paint-right frame)))))

(define full-frame (make-frame (make-vect -0.5 -0.5)
			       (make-vect 2 0)
			       (make-vect 0 2)))

(define unit-frame (make-frame (make-vect 0 500) (make-vect 500 0) (make-vect 0 -500)))
(diamond-painter unit-frame)