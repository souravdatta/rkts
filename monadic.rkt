#lang typed/racket

(require (for-syntax syntax/parse))

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
    ([_ (return val-decl:expr)]
     #'(return val-decl))))


#|

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
   (return l2)))


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
   (return l2)))

(define l1 '(1 9 45 89 23 78 3 16 56 54 45 8 89 17 23 28 49 23))

(define r (do+
             (:= [l : (Listof Integer)] (process1 l1))
             (:= [result : (Listof Integer)] (process2 l))
             (return result)))

> (Monad-content r)
- : (Listof Integer)
'(3 11 25 5 18 10 19 25 25)

|#
