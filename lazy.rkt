#lang racket

(define lazy-nil '())

(define (lazy-null? s)
  (equal? s lazy-nil))

(define-syntax delay
  (syntax-rules ()
    ((_ a) (lambda () a))))

(define-syntax lazy-cons
  (syntax-rules ()
    ((_ a b) (cons a (delay b)))))

(define-syntax lazy-list
  (syntax-rules ()
    ((_ x) (lazy-cons x lazy-nil))
    ((_ x y ...) (lazy-cons x (lazy-list y ...)))))

(define (force a)
  (a))

(define lazy-car car)

(define (lazy-cdr s)
  (force (cdr s)))

(define (take n lst)
  (define (lazy-take-aux n lst new-lst)
    (cond
      ((or (lazy-null? lst)
           (<= n 0)) (reverse new-lst))
      (else (lazy-take-aux (- n 1) (lazy-cdr lst) (cons (lazy-car lst) new-lst)))))
  (lazy-take-aux n lst '()))

(define (lazy-map fn . lsts)
  (cond
    ((lazy-null? (car lsts)) lazy-nil)
    (else (lazy-cons
           (apply fn (map lazy-car lsts))
           (apply lazy-map
                  (cons fn (map lazy-cdr lsts)))))))

(define (lazy-filter pred lst)
  (cond
    ((lazy-null? lst) lazy-nil)
    ((pred (lazy-car lst)) (lazy-cons (lazy-car lst)
                                      (lazy-filter pred (lazy-cdr lst))))
    (else (lazy-filter pred (lazy-cdr lst)))))

(define (lazy-range low high . stepl)
  (if (>= low high)
      lazy-nil
      (lazy-cons low (apply lazy-range (append (list (+ (if (not (null? stepl)) (car stepl) 1)
                                                        low)
                                                     high)
                                               stepl)))))


(provide lazy-nil lazy-null? lazy-cons lazy-list lazy-car
         lazy-cdr take lazy-map lazy-filter lazy-range)



