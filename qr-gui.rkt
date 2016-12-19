#lang typed/racket/gui

(require "typed_qrcode.rkt")

(: app-window (Instance Frame%))
(define app-window (new frame%
                        [width 200]
                        [height 300]
                        [label "QR"]))

(: qr-text (Instance Text-Field%))
(define qr-text (new text-field%
                     [parent app-window]
                     [label "QR text"]
                     [init-value ""]))

(: qr-button (Instance Button%))
(define qr-button (new button%
                       [parent app-window]
                       [label "Make"]
                       [callback (lambda (b e)
                                   (let ([lbl (send qr-text get-value)])
                                     (when (string? lbl)
                                       (let ([dc (send qr-canvas get-dc)]
                                             [qbitmap : (Instance Bitmap%) (make-qr-code lbl)])
                                         (send dc clear)
                                         (send dc draw-bitmap qbitmap 40 40)))))]))

(: qr-canvas (Instance Canvas%))
(define qr-canvas (new canvas%
                       [parent app-window]
                       [min-width 200]
                       [min-height 100]))
                       

(send app-window show #t)
