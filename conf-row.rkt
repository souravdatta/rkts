#lang typed/racket

(require typed/racket/gui)

(define-type Conf-Row% (Class
                        (init (len Nonnegative-Integer))
                        (field (len Nonnegative-Integer) (row (Vectorof Nonnegative-Integer)))
                        (get-conf (-> (Vectorof Nonnegative-Integer)))
                        (render (-> (Instance Pane%) (Instance Pane%)))))

(: conf-row% Conf-Row%)
(define conf-row% (class object%
                    (init-field [len : Nonnegative-Integer])
                    (field [row : (Vectorof Nonnegative-Integer) #[]])
                    (: build-label (-> Nonnegative-Integer String))
                    (define/private (build-label x)
                      (format "~a - ~a" x (vector-ref row x)))
                    (: get-conf (-> (Vectorof Nonnegative-Integer)))
                    (define/public (get-conf)
                      (vector->immutable-vector row))
                    (define/public (render parent)
                      (let ([hpane (new horizontal-pane% [parent parent])])
                        (set! row #{(make-vector (abs len)) :: (Vectorof Nonnegative-Integer)})
                        (for ([x : Nonnegative-Integer (in-range len)])
                          (vector-set! row x 0)
                          (new button%
                               [parent hpane]
                               [label (build-label x)]
                               [min-width 10]
                               [stretchable-width 10]
                               [callback (λ ([b : (Instance Button%)]
                                             [e : Any])
                                           (if (= (vector-ref row x) 0)
                                               (vector-set! row x 1)
                                               (vector-set! row x 0))
                                           (send b set-label (build-label x)))]))
                        hpane))
                    (super-new)))


(provide Conf-Row% conf-row%)
