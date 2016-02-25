#lang racket

(provide define-curry)


(define-syntax def-curry
  (syntax-rules ()
    ((def-curry () body ...)
     (lambda () body ...))
    ((def-curry ((a1 ...) a2 ...) body ...)
     (lambda (a1 ...)
       (def-curry (a2 ...) body ...)))
    ((def-curry (a1 a2 ...) body ...)
     (lambda (a1)
       (def-curry (a2 ...) body ...)))))


(define-syntax define-curry
  (syntax-rules ()
    ((define-curry (name a1 ...) body ...)
     (define name (def-curry (a1 ...) body ...)))))


