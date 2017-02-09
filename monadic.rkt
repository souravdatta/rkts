#lang typed/racket

(require (for-syntax syntax/parse))

(provide (struct-out Monad)
         Monad-Content
         return
         bind
         mmap
         do+
         do<>)

(define-type (Monad-Content a) (U a False))

(struct (a) Monad ([content : (Monad-Content a)]))

(: return (All (a) (-> (Monad-Content a) (Monad a))))
(define (return x) (Monad x))

(: mmap (All (a b) (-> (Monad a) (-> a b) (Monad b))))
(define (mmap mond f)
  (let ([v (Monad-content mond)])
    (if v
        (return (f v))
        (return #f))))

(: bind (All (a b) (-> (Monad a) (-> a (Monad b)) (Monad b))))
(define (bind bx f)
  (let ([c : (Monad-Content a) (Monad-content bx)])
    (if c
        (f c)
        (return #f))))

(define-syntax (do+ stx)
  (syntax-parse stx
    ([_ ((~literal :=) (var-decl:id : var-type:expr) val-decl:expr) e2 ...]
     #'(bind val-decl (λ ([var-decl : var-type]) (do+ e2 ...))))
    ([_ ((~literal <<) val-decl:expr)]
     #'(return val-decl))))

(define-syntax (do<> stx)
  (syntax-parse stx
    ([_ (mond-binder:id mond-return:id) ((~literal :=) (var-decl:id : var-type:expr) val-decl:expr) e2 ...]
     #'(mond-binder val-decl (λ ([var-decl : var-type]) (do<> (mond-binder mond-return) e2 ...))))
    ([_ (mond-binder:id mond-return:id) ((~literal <<) val-decl:expr)]
     #'(mond-return val-decl))))


#|

Example of using (Monad a)

(: p-a (-> Char (Monad Char)))
(define (p-a c)
  (if (char=? c #\a)
      (return c)
      (return #f)))

(: p-ab (-> Char Char (Monad Char)))
(define (p-ab p c)
  (if (and (char=? c #\b) (char=? p #\a))
      (return c)
      (return #f)))

(: p-abc (-> Char Char Char (Monad Char)))
(define (p-abc p1 p2 c)
  (if (and (char=? c #\c) (char=? p2 #\b) (char=? p1 #\a))
      (return c)
      (return #f)))

(: parse-abc (-> String (Monad Char)))
(define (parse-abc str)
  (do+ 
   [:= (c1 : Char) (p-a (string-ref str 0))]
   [:= (c2 : Char) (p-ab c1 (string-ref str 1))]
   [:= (c3 : Char) (p-abc c1 c2 (string-ref str 2))]
   (<< c3)))

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
(define p (do<> (Probably-bind Probably-return)
                (:= [r1 : Real] (randomness 10.2))
                (:= [r2 : Real] (randomness r1))
                (:= [r3 : Real] (randomness r2))
                (:= [r4 : Real] (randomness r3))
                (<< r4)))

|#
