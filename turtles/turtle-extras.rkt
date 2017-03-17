#lang typed/racket

(require/typed graphics/turtles
               [draw (-> Real Void)]
               [move (-> Real Void)]
               [turn (-> Real Void)]
               [turtles (-> Boolean Void)])

(: arc-left (-> Real Real Void))
(define (arc-left angle radius)
  (for ([x (in-range angle)])
    (draw (/ (* 2 3.14 radius) 360))
    (turn 1)))

(: arc-right (-> Real Real Void))
(define (arc-right angle radius)
  (for ([x (in-range angle)])
    (draw (/ (* 2 3.14 radius) 360))
    (turn (- 1))))

(: fd (-> Real Void))
(define (fd x)
  (draw x))

(: bk (-> Real Void))
(define (bk x)
  (draw (- x)))

(: right (-> Real Void))
(define (right x)
  (turn (- x)))

(: left (-> Real Void))
(define (left x)
  (turn x))

(: mv (-> Real Void))
(define (mv x)
  (move x))

(: turtles-on (-> Void))
(define (turtles-on)
  (turtles #t)
  (left 90))

(provide (all-defined-out))
