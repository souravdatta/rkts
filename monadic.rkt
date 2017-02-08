#lang typed/racket

(require (for-syntax syntax/parse))

(provide (struct-out Monad)
         return
         mmap
         bind
         do+
         do<>)

(struct (a) Monad ([content : a]))

(: return (All (a) (-> a (Monad a))))
(define (return x) (Monad x))

(: mmap (All (a b) (-> (Monad a) (-> a b) (Monad b))))
(define (mmap mond f)
  (return (f (Monad-content mond))))

(: bind (All (a b) (-> (Monad a) (-> a (Monad b)) (Monad b))))
(define (bind bx f)
  (f (Monad-content bx)))

(define-syntax (do+ stx)
  (syntax-parse stx
    ([_ ((~literal :=) (var-decl:id : var-type:expr) val-decl:expr) e2 ...]
     #'(bind val-decl (λ ([var-decl : var-type]) (do+ e2 ...))))
    ([_ ((~literal <<) val-decl:expr)]
     #'(return val-decl))))

(define-syntax (do<> stx)
  (syntax-parse stx
    ([_ mond:id ((~literal :=) (var-decl:id : var-type:expr) val-decl:expr) e2 ...]
     (let* ([monad-name (symbol->string (syntax->datum #'mond))]
            [monad-bind-name (format "~a-~a" monad-name "bind")])            ;; convert bind => <Monad>-bind
       (with-syntax ([monad-bind (datum->syntax stx (string->symbol monad-bind-name))])
         #'(monad-bind val-decl (λ ([var-decl : var-type]) (do<> mond e2 ...))))))
    ([_ mond:id ((~literal <<) val-decl:expr)]
     (let* ([monad-name (symbol->string (syntax->datum #'mond))]
            [monad-return-name (format "~a-~a" monad-name "return")])       ;; convert return => <Monad>-return
       (with-syntax ([monad-return (datum->syntax stx (string->symbol monad-return-name))])
         #'(monad-return val-decl))))))


#|

Example of using (Monad a)

(: process1 (-> (Listof Integer) (Monad (Listof Integer))))
(define (process1 li)
  (: filter1 (-> (Listof Integer) (Monad (Listof Integer))))
  (define (filter1 li)
    (return (filter (λ ([x : Integer]) (< x 50)) li)))

  (: add1 (-> (Listof Integer) (Monad (Listof Integer))))
  (define (add1 li)
    (return (map (λ ([x : Integer]) (+ x 1)) li)))

  (do+
   (:= [l1 : (Listof Integer)] (filter1 li))
   (:= [l2 : (Listof Integer)] (add1 l1))
   (<< l2)))


(: process2 (-> (Listof Integer) (Monad (Listof Integer))))
(define (process2 li)
  (: filter1 (-> (Listof Integer) (Monad (Listof Integer))))
  (define (filter1 li)
    (return (filter (λ ([x : Integer]) (< x 25)) li)))

  (: add1 (-> (Listof Integer) (Monad (Listof Integer))))
  (define (add1 li)
    (return (map (λ ([x : Integer]) (+ x 1)) li)))

  (do+
   (:= [l1 : (Listof Integer)] (filter1 li))
   (:= [l2 : (Listof Integer)] (add1 l1))
   (<< l2)))

(define l1 '(1 9 45 89 23 78 3 16 56 54 45 8 89 17 23 28 49 23))

(define r (do+
             (:= [l : (Listof Integer)] (process1 l1))
             (:= [result : (Listof Integer)] (process2 l))
             (<< result)))

> (Monad-content r)
- : (Listof Integer)
'(3 11 25 5 18 10 19 25 25)

|#

#|

Example of using custom Monad which implements <Monad>-bind and <Monad>-return.

(struct Probably ([p : Real]))

(: Probably-return (-> Real Probably))
(define (Probably-return r) (Probably r))

(: Probably-bind (-> Probably (-> Real Probably) Probably))
(define (Probably-bind p f)
  (if (> (Probably-p p) 4.5)
      (f (Probably-p p))
      (Probably-return 0.0)))

(: randomness (-> Real Probably))
(define (randomness r)
  (Probably-return (* r (random 1 100))))

(: p Probably)
(define p (do<> Probably
                (:= [r1 : Real] (randomness 10.2))
                (:= [r2 : Real] (randomness r1))
                (:= [r3 : Real] (randomness r2))
                (:= [r4 : Real] (randomness r3))
                (<< r4)))

|#
