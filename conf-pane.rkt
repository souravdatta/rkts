#lang typed/racket

(require typed/racket/gui)
(require "conf-button.rkt")

(define-type Conf-Pane% (Class
                         (init (conf-buttons (Listof (Instance Conf-Button%)))
                               (command (-> (Listof Nonnegative-Integer) Any)))
                         (field (command (-> (Listof Nonnegative-Integer) Any))
                                (conf-buttons (Listof (Instance Conf-Button%))))
                         (render (-> (Instance Pane%) (Instance Pane%)))))

(: conf-pane% Conf-Pane%)
(define conf-pane% (class object%
                     (init-field [conf-buttons : (Listof (Instance Conf-Button%))]
                                 [command : (-> (Listof Nonnegative-Integer) Any)])
                     (: gen-config (-> (Listof Nonnegative-Integer)))
                     (define/private (gen-config)
                       (let ([ls (map (λ ([x : (Instance Conf-Button%)])
                                        (send x render-list))
                                      conf-buttons)])
                         (map (λ ([x : (Pairof
                                        (List Nonnegative-Integer Nonnegative-Integer Nonnegative-Integer)
                                        Nonnegative-Integer)])
                                (cdr x))
                              ls)))
                     (define/public (render parent)
                       (let* ([vpane (new vertical-pane% [parent parent])]
                              [genbtn (new button%
                                           [parent vpane]
                                           [label "Generate Configuration"]
                                           [callback (λ ([b : (Instance Button%)]
                                                         [e : Any])
                                                       (command (gen-config)))])])
                         (for ([x conf-buttons])
                           (send x render vpane))
                         vpane))
                     (super-new)))


(provide Conf-Pane% conf-pane%)




