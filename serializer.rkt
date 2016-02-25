#lang racket
(require "mutex.rkt")

(define (make-serializer)
  (let ((mutex (make-mutex)))
    (lambda (f)
      (lambda args
        (mutex-acquire mutex)
        (apply f args)
        (mutex-release mutex)))))

(define (parallel-execute f1 f2)
  (thread f1)
  (thread f2))

(provide make-serializer
         parallel-execute)
