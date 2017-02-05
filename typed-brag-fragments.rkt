#lang typed/racket

(require/typed brag/support
               [#:opaque Token token-struct?]
               [token (->* ((U String Symbol))
                           (Any
                            #:line Positive-Integer
                            #:column Natural
                            #:position Positive-Integer
                            #:span Natural
                            #:skip? Boolean)
                           Token)])


(define-type Input-Token (U String Symbol Token Void 'EOF))
(define-type Token-Source (U (-> Input-Token)
                             (Listof Input-Token)))
                               
#|                             
(require/typed <grammar_file_name_here>
              [parse (case-> (-> Token-Source (Syntaxof Any))
                             (-> Any Token-Source (Syntaxof Any)))])
|#
