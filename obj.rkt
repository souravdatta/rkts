#lang racket

(define null 'null)
(define no-parent 'no-parent)


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

(define (make-object)
  (let ([o (make-hash)])
    (hash-set! o 'parent no-parent)
    o))

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
           (if (procedure? result)
               (result obj a1 ...)
               null))))))

(define-syntax (define-object stx)
  (syntax-case stx ()
    ([_ obj-name parent props]
     (let* ([o (datum->syntax stx 'o)]
            [prop-list (syntax->datum #'props)]
            [quoted-prop-list (list 'quote prop-list)])
       (with-syntax ([quoted-prop-list-syntax (datum->syntax stx quoted-prop-list)])
         #'(define obj-name (let ([o (make-hash quoted-prop-list-syntax)])
                              (set-parent! o parent)
                              o)))))))

(define-syntax (define-method stx)
  (syntax-case stx ()
    ((_ (meth-name obj a1 ...) b1 ...)
     (let* ([sym (syntax->datum #'meth-name)]
            [sym-quote (list 'quote sym)])
       (with-syntax ([sx (datum->syntax stx sym-quote)]
                     [self-sx (datum->syntax stx 'self)])
         #'(let ([self-sx obj])
             (set-prop! obj sx (lambda (self a1 ...) b1 ...))))))))


(provide get tell define-object define-method no-parent null)
