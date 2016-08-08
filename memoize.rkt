#lang racket

(require (for-syntax syntax/parse))

(define (memoize fn)
  (let ([h (make-hash)])
    (lambda args
      (let ([mem (hash-ref h args 'not-found)])
        (if (eq? mem 'not-found)              
            (let ([result (apply fn args)])
              (hash-set! h args result)
              result)
            mem)))))

(define-syntax (define-memoized stx)
  (syntax-parse stx
    [(_ (fn-name:id arg:id ...) body ...+)
     #'(define fn-name (memoize (lambda (arg ...) body ...)))]))
        

#|

Example: - 

(define (fibo n)
  (cond
    ((<= n 1) 1)
    (else (+ (fibo (- n 1))
             (fibo (- n 2))))))

> (time (fibo 35))
cpu time: 829 real time: 829 gc time: 0
14930352

======

(define-memoized (mfibo n)
  (cond
    ((<= n 1) 1)
    (else (+ (mfibo (- n 1))
             (mfibo (- n 2))))))

> (time (mfibo 35))
cpu time: 1 real time: 0 gc time: 0
14930352

|#

(provide memoize define-memoized)
