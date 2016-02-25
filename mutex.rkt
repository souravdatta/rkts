#lang racket

(define (make-mutex)
  (let ((sem (make-semaphore 1)))
    (define (acquire)
      (semaphore-wait sem))
    (define (release)
      (semaphore-post sem))
    (define (dispatch m)
      (cond
        ((eq? m 'acquire) acquire)
        ((eq? m 'release) release)
        (else 'wat?)))
    dispatch))

(define (mutex-acquire m)
  (m 'acquire))

(define (mutex-release m)
  (m 'release))


(provide make-mutex
         mutex-acquire
         mutex-release)
