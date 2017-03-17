#lang typed/racket

(require/typed graphics/turtles
               [draw (-> Real Void)]
               [move (-> Real Void)]
               [turn (-> Real Void)]
               [turtles (-> Boolean Void)]
               [save-turtle-bitmap (-> (U String Output-Port)
                               (U 'png 'jpeg 'xbm 'xpm 'bmp)
                               Boolean)])

(: arc-left (->* (Real Real) (#:action (-> Real Void)) Void))
(define (arc-left angle radius #:action [action fd])
  (for ([x (in-range angle)])
    (action (/ (* 2 3.14 radius) 360))
    (turn 1)))

(: arc-right (->* (Real Real) (#:action (-> Real Void)) Void))
(define (arc-right angle radius #:action [action fd])
  (for ([x (in-range angle)])
    (action (/ (* 2 3.14 radius) 360))
    (turn (- 1))))

(: fd (->* (Real) (#:action (-> Real Void)) Void))
(define (fd x #:action [action draw])
  (action x))

(: bk (->* (Real) (#:action (-> Real Void)) Void))
(define (bk x #:action [action draw])
  (action (- x)))

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

(define-syntax pu
  (syntax-rules ()
    ((_ (fd x)) (fd x #:action mv))
    ((_ (bk x)) (bk x #:action mv))
    ((_ (arc-left x y)) (arc-left x y #:action mv))
    ((_ (arc-right x y)) (arc-right x y #:action mv))))
  
(provide (all-defined-out)
         save-turtle-bitmap)

