#lang typed/racket

(: memoize (All (r a ...)
                (-> (-> a ... a r)
                    (-> a ... a r))))
(define (memoize fn)
  (let ([store : (HashTable Any r) (make-hash)])
    (define (memfn . [args : a ... a])
      (hash-ref store args
                (lambda ()
                  (let ([result : r (apply fn args)])
                    (hash-set! store args result)
                    result))))
    memfn))

(provide memoize)
