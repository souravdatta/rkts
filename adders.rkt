#lang racket

(require "propagators.rkt")

(define (andf x y)
  (if (= x 1)
      y
      0))

(define (orf x y)
  (if (= x 0)
      y
      1))

(define (notf x)
  (if (= x 0)
      1
      0))

(define and-gate (func->propagator andf))
(define or-gate (func->propagator orf))
(define not-gate (func->propagator notf))

(define (nand-gate x y z)
  (compound-propagator
   (list x y z)
   (lambda ()
     (let ((t1 (make-cell)))
       (and-gate x y t1)
       (not-gate t1 z)))))

         
(define (xor-gate a b c)
  (compound-propagator (list a b c)
                       (lambda ()
                         (let ((ti1 (make-cell))
                               (ti2 (make-cell))
                               (ti3 (make-cell)))
                           (nand-gate a b ti1)
                           (nand-gate a ti1 ti2)
                           (nand-gate b ti1 ti3)
                           (nand-gate ti2 ti3 c)))))


(define (full-adder a b ci s co)
  (compound-propagator
   (list a b ci s co)
   (lambda ()
     (let ((t1 (make-cell))
           (t2 (make-cell))
           (t3 (make-cell)))
       (xor-gate a b t1)
       (xor-gate ci t1 s)
       (and-gate t1 ci t2)
       (and-gate a b t3)
       (or-gate t2 t3 co)))))

(provide full-adder)
