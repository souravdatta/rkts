#lang typed/racket


(define-type Watcher (-> Void))
(define-type (Cell a) (MPairof a (Listof Watcher)))

(: fire-watchers (All (a) (-> (Cell a) Void)))
(define (fire-watchers c)
  (for ([w (mcdr c)])
    (w)))

(: make-cell (All (a) (-> a (Cell a))))
(define (make-cell x) (mcons x '()))

(: get-value (All (a) (-> (Cell a) a)))
(define (get-value c) (mcar c))

(: set-value! (All (a) (-> (Cell a) a Void)))
(define (set-value! c nv)
  (set-mcar! c nv)
  (fire-watchers c))

(: get-watchers (All (a) (-> (Cell a) (Listof Watcher))))
(define (get-watchers c) (mcdr c))

(: add-watcher! (All (a) (-> (Cell a) Watcher Void)))
(define (add-watcher! c w) (set-mcdr! c (cons w (mcdr c))))

(: make-connector (All (r b ...)
                (-> (-> b ... b r)
                    (-> (Cell r)
                        (Cell b) ... b
                        Void))))
(define (make-connector fn)
  (: connect (-> (Cell r) (Cell b) ... b Void))
  (define (connect output . more-inputs)
    (: add-watcher-for (All (x) (-> (Cell x) Void)))
    (define (add-watcher-for cc)
      (add-watcher! cc (lambda () (set-value! output (apply fn (map mcar more-inputs)))))
      (fire-watchers cc)
      (void))
    (map add-watcher-for more-inputs)
    (void))
  connect)

(provide make-cell
         get-value
         set-value!
         get-watchers
         add-watcher!
         make-connector)

#|
(define c1 (make-cell 10))
(define c2 (make-cell 20))
(define c3 (make-cell 0))

(define plusc (make-connector (lambda ([x : Integer] [y : Integer]) (+ x y 10))))
(plusc c3 c1 c2)
|#
