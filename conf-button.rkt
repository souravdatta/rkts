#lang typed/racket

(require typed/racket/gui)

(define-type Conf-Button%
  (Class
   (init (conf-1 Nonnegative-Integer)
         (conf-2 Nonnegative-Integer)
         (conf-3 Nonnegative-Integer)
         (result Nonnegative-Integer))
   (field (conf-1 Nonnegative-Integer)
          (conf-2 Nonnegative-Integer)
          (conf-3 Nonnegative-Integer)
          (result Nonnegative-Integer))
   (render (-> (Instance Pane%) (Instance Pane%)))
   (render-list
    (-> (Pairof
         (List Nonnegative-Integer Nonnegative-Integer Nonnegative-Integer)
         Nonnegative-Integer)))))

(: conf-button% Conf-Button%)
(define conf-button% (class object%
                       (init-field [conf-1 : Nonnegative-Integer]
                                   [conf-2 : Nonnegative-Integer]
                                   [conf-3 : Nonnegative-Integer]
                                   [result : Nonnegative-Integer])
                       (define/public (render-list)
                         (cons (list conf-1 conf-2 conf-3) result))
                       (: conf->label (-> (U 1 2 3 4) String))
                       (define/private (conf->label i)
                         (cond
                           ((= i 1) (number->string conf-1))
                           ((= i 2) (number->string conf-2))
                           ((= i 3) (number->string conf-3))
                           ((= i 4) (number->string result))
                           (else "")))
                       (define/public (render parent)
                         (let* ([vpane (new vertical-pane% [parent parent])]
                                [hpane (new horizontal-pane% [parent vpane])]
                                [btn1 (new button%
                                           [parent hpane]
                                           [enabled #f]
                                           [label (conf->label 1)])]
                                [btn2 (new button%
                                           [parent hpane]
                                           [enabled #f]
                                           [label (conf->label 2)])]
                                [btn3 (new button%
                                           [parent hpane]
                                           [enabled #f]
                                           [label (conf->label 3)])]
                                [btn4 (new button%
                                           [parent vpane]
                                           [label (conf->label 4)]
                                           [callback (λ ([b : (Instance Button%)]
                                                         [e : Any])
                                                       (set! result (if (= result 0) 1 0))
                                                       (send b set-label (conf->label 4)))])])
                           vpane))
                       (super-new)))


(provide (all-defined-out))
