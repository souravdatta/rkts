#lang racket

#|
A mostly complete Object system for Racket.
Features - encapsulation, message passing, inheritance (single), reflecttion capable.
Example below.
|#

#|
(require "objs.rkt")

(define-class Person ((name "Anonymous")))
(define-method (init Person nm) (set-prop! self 'name nm))
(define-method (greetings Person) (format "Hello ~a" (get self name)))

(define-class PersonWithId Person ((id 0)))
(define-method (init PersonWithId nm id)
  (tell super init nm)
  (set-prop! self 'id id))
(define-method (greetings PersonWithId) (format "~a, your ID is ~a" (tell super greetings) (get self id)))

(define ip (new PersonWithId))
(tell ip init "Bruce Wayne" 6565)
(println (tell ip greetings))
(println (tell ip describe))
|#

(define null 'null)

(define no-parent (make-hash
                   (list
                    (cons 'describe
                          (lambda (a b) (format "An object ~a" a)))
                    (cons 'parent
                          null))))

(define this (make-parameter null))

(define this-super (make-parameter null))

(define (rget obj prop)
  (hash-ref obj
            prop
            (lambda () ;; faliure
              (let ([parent (hash-ref obj 'parent)])
                (if (eq? parent null)
                    null
                    (rget parent prop))))))

(define (set-parent! o parent)
  (hash-set! o 'parent parent))

(define (set-prop! o prop val)
  (if (eq? prop 'parent)
      (raise "Cannot set parent property, that is reserved!")
      (hash-set! o prop val)))

(define-syntax (get stx)
  (syntax-case stx ()
    ((_ obj name)
     (let* ([sym (syntax->datum #'name)]
            [sym-quote (list 'quote sym)])
       (with-syntax ([sx (datum->syntax stx sym-quote)]
                     [result (datum->syntax stx 'result)])
         #'(let ([result (rget obj sx)])
             (if (pair? result)
                 (car result)
                 result)))))))

(define-syntax (tell stx)
  (syntax-case stx ()
    ((_ obj name a1 ...)
     (with-syntax ([result (datum->syntax stx 'result)])
       #'(let ([result (get obj name)])
           (parameterize ([this obj]
                          [this-super (get obj parent)])
             (if (procedure? result)
                 (result obj (get obj parent) a1 ...)
                 (raise "I do not understand this"))))))))

(define-syntax (define-class stx)
  (syntax-case stx ()
    ([_ class-name parent props]
     (let* ([o (datum->syntax stx 'o)]
            [prop-list (syntax->datum #'props)]
            [quoted-prop-list (list 'quote prop-list)])
       (with-syntax ([quoted-prop-list-syntax (datum->syntax stx quoted-prop-list)])
         #'(define class-name (make-hash (list (cons 'constructor
                                                     (lambda () (let ([o (make-hash quoted-prop-list-syntax)])
                                                                  (set-parent! o (new parent))
                                                                  o)))
                                               (cons 'prototype
                                                     (make-hash))))))))
    ([_ class-name props]
     (let* ([o (datum->syntax stx 'o)]
            [prop-list (syntax->datum #'props)]
            [quoted-prop-list (list 'quote prop-list)])
       (with-syntax ([quoted-prop-list-syntax (datum->syntax stx quoted-prop-list)])
         #'(define class-name (make-hash (list (cons 'constructor
                                                     (lambda () (let ([o (make-hash quoted-prop-list-syntax)])
                                                                  (set-parent! o no-parent)
                                                                  o)))
                                               (cons 'prototype
                                                     (make-hash))))))))))

(define (new kls . init-param-list)
  (let ([o ((hash-ref kls 'constructor))]
        [methods (hash-ref kls 'prototype)])
    (hash-for-each methods (lambda (k v)
                             (if (not (and (equal? k 'parent)
                                           (equal? k 'type)))
                                 (set-prop! o k v)
                                 (raise "Cannot set parent or type property of an object"))))
    (when (and
           (> (length init-param-list) 0)
           (list? (car init-param-list)))
      (for ([p (car init-param-list)])
        (if (and (pair? p)
                 (symbol? (car p)))
            (set-prop! o (car p) (cdr p))
            null)))                      
    o))

(define-syntax (define-method stx)
  (syntax-case stx ()
    ((_ (meth-name klass a1 ...) b1 ...)
     (let* ([sym (syntax->datum #'meth-name)]
            [sym-quote (list 'quote sym)])
       (with-syntax ([sx (datum->syntax stx sym-quote)]
                     [self-sx (datum->syntax stx 'self)]
                     [super-sx (datum->syntax stx 'super)])
         #'(let ([self-sx (this)]
                 [super-sx (this-super)])
             (hash-set! (hash-ref klass 'prototype)
                        sx
                        (lambda (self-sx super-sx a1 ...) b1 ...))))))))  


(provide get tell define-class define-method set-prop! new no-parent null)
