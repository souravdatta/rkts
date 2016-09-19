#lang typed/racket


(define-type Co-routine (-> Co-routine Co-routine))

(: t1 Co-routine)
(define (t1 c)
  (for ([x '(1 2 3 4 5)])
    (println x)
    (set! c (call/cc c)))
  c)


(: t2 Co-routine)
(define (t2 c)
  (for ([x '(11 12 13 14 15)])
    (println x)
    (set! c (call/cc c)))
  c)
