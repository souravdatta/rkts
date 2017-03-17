#lang typed/racket

(require "turtle-extras.rkt")

(turtles-on)

#|
(fd 100)
(right 90)
(arc-left 180 50)
(right 90)
(fd 100)
(arc-right 180 50)
(arc-right 180 200)
(arc-right 270 40)
(arc-left 270 40)
|#

#|
(: ray (-> Real Void))
(define (ray size)
  (for ([x 2])
    (arc-left 60 size)
    (arc-right 60 size)))


(: sunshine (-> Real Void))
(define (sunshine size)
  (for ([x 20])
    (ray size)
    (right 100)))

(sunshine 40)
|#

#|
(save-turtle-bitmap "/Users/sourav/Pictures/turtle1.png"
                    'png)
|#

#|
(fd 100)
(pu (fd 100))
(fd 100)
(pu (bk 200))
(right 45)
(fd 400)
|#

#|
(arc-left 45 180)
(right 180)
(pu (arc-right 45 180))
(right 180)
(arc-right 45 180)
(right 180)
(pu (arc-left 45 180))
|#




  
