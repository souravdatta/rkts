#lang typed/racket


(define-type Watcher (-> Void))
(define-type (Cell a) (MPairof a (Listof Watcher)))

(: true-value (-> Symbol))
(define (true-value) 'true)

(: false-value (-> Symbol))
(define (false-value) 'false)

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

(: propagator (All (b ...) (-> Watcher
                               (Cell b) ... b
                               Void)))
(define (propagator w . cells)
  (: add-watcher-for (All (x) (-> (Cell x) Void)))
  (define (add-watcher-for c)
    (add-watcher! c w)
    (fire-watchers c)
    (void))
  (map add-watcher-for cells)
  (void))

(: conditional (All (a) (-> (Cell Symbol)
                            (Cell a)
                            (Cell a)
                            (Cell a)
                            Void)))
(define (conditional pred-cell out-cell t-cell f-cell)
  (propagator (lambda ()
                (let ((v (get-value pred-cell)))
                  (cond
                    ((equal? v (true-value)) (set-value! out-cell (get-value t-cell)))
                    ((equal? v (false-value)) (set-value! out-cell (get-value f-cell)))
                    (else (set-value! out-cell (get-value out-cell))))))
              pred-cell t-cell f-cell))             


(provide true-value
         false-value
         make-cell
         get-value
         set-value!
         get-watchers
         add-watcher!
         make-connector
         propagator
         conditional)

#|
(define c1 (make-cell 10))
(define c2 (make-cell 20))
(define c3 (make-cell 0))

(define plusc (make-connector (lambda ([x : Integer] [y : Integer]) (+ x y 10))))
(plusc c3 c1 c2)

(define b1 (make-cell (false-value)))
(define b2 (make-cell 1))
(define b3 (make-cell 11))
(define b4 (make-cell 0))
(conditional b1 b4 b2 b3)
|#

