#lang typed/racket


;; utils
(: tagged? (-> (Pairof Symbol Any) Symbol Boolean))
(define (tagged? p tag)
  (eqv? (car p) tag))


;; env def
(define-type Primitive (Pairof Symbol Any))
(define-type Env (Pairof (Listof Primitive)
                         (Listof (Pairof Symbol Any))))

(: make-env (-> Env))
(define (make-env)
  (cons empty empty))

(: extend-env (-> Env Symbol Any Env))
(define (extend-env e s v)
  (cons (car e)
        (cons (cons s v) (cdr e))))

(: extend-env-list (-> Env (Listof Symbol) (Listof Any) Env))
(define (extend-env-list e ls lv)
  (cons (car e)
        (append (map (lambda ([a : Symbol]
                        [b : Any])
                 (cons a b)) ls lv)
          (cdr e))))

(: lookup-env (-> Env Symbol Any))
(define (lookup-env e s)
  (let ([r (assoc s (cdr e))])
    (if r
        (cdr r)
        empty)))

(: extend-primitives (-> Env (Listof Primitive) Env))
(define (extend-primitives e lps)
  (cons
   lps
   (cdr e)))

(: lookup-primitive (-> Env Symbol (U False Primitive)))
(define (lookup-primitive e s)
  (let ([r (assoc s (car e))])
    (if r
        r
        #f)))


;; procedue def
(define-type Procedure (List Env
                             (Listof Symbol)
                             (Listof Any)))

(: make-procedure (-> Env (Listof Symbol) (Listof Any) Procedure))
(define (make-procedure e args body)
  (list e args body))


;; lambda
(: lambda? (-> (Pairof Symbol Any) Boolean))
(define (lambda? p)
  (tagged? p 'lambda))

(: lambda-args (-> (Pairof 'lambda Any) (Listof Symbol)))
(define (lambda-args exp)
  (let ([e2 (cdr exp)])
    (if (and (pair? e2) (list? (car e2)) (andmap symbol? (car e2)))
        (car e2)
        empty)))

(: lambda-body (-> (Pairof 'lambda Any) (Listof Any)))
(define (lambda-body exp)
  (let ([e2 (cdr exp)])
    (if (and (pair? e2)
             (list? (car e2))
             (list? (cdr e2)))
        (cdr e2)
        (if (list? e2)
            e2
            (list e2)))))


;; eval's and apply's
(: eval-lambda-def (-> (Pairof 'lambda Any)
                       Env
                       Procedure))
(define (eval-lambda-def exp e)
  (let ([args (lambda-args exp)]
        [body (lambda-body exp)])
    (make-procedure e args body)))

(: eval-seq (-> (Listof Any) Env Any))
(define (eval-seq exps env)
  (for ([e exps])
    (error "not ready yet")))

(: apply-procedure (-> Procedure (Listof Any) Any))
(define (apply-procedure proc act-args)
  (let* ([args (second proc)]
         [body-seq (third proc)]
         [new-env (extend-env-list (first proc) args act-args)])
    (eval-seq body-seq new-env)))
