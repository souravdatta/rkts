#lang racket

(define null 'null)
(define no-parent 'no-parent)

(define this (make-parameter null))


(define (rget obj prop)
  (hash-ref obj
            prop
            (lambda () ;; faliure
              (let ([parent (hash-ref obj 'parent)])
                (if (eq? parent no-parent)
                    null
                    (rget parent prop))))))

(define (tell-symbol obj message . params)
  (apply (rget obj message) (cons obj params)))

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
           (parameterize ([this obj])
             (if (procedure? result)
                 (result obj a1 ...)
                 null)))))))

(define-syntax (define-class stx)
  (syntax-case stx ()
    ([_ class-name parent props]
     (let* ([o (datum->syntax stx 'o)]
            [prop-list (syntax->datum #'props)]
            [quoted-prop-list (list 'quote prop-list)])
       (with-syntax ([quoted-prop-list-syntax (datum->syntax stx quoted-prop-list)])
         #'(define class-name (make-hash (list (cons 'constructor
                                                     (lambda () (let ([o (make-hash quoted-prop-list-syntax)])
                                                                  (set-parent! o (new-object parent))
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

(define (new-object kls . init-param-list)
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
                     [self-sx (datum->syntax stx 'self)])
         #'(let ([self-sx (this)])
             (hash-set! (hash-ref klass 'prototype)
                        sx
                        (lambda (self-sx a1 ...) b1 ...))))))))  


(provide get tell define-class define-method set-prop! new-object no-parent null)
