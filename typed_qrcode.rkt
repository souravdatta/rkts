#lang typed/racket


#|

$raco planet install norisys qrcode.plt 1 0

|#

(require typed/racket/draw)

(require/typed (planet norisys/qrcode:1:0/qrcode)
               [make-qr-code (-> String (Instance Bitmap%))])


(provide make-qr-code)
