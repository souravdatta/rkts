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
(: process2 (-> Integer (Monad Integer)))
(define (process2 x)
  (do+
   [:= {x1 : Integer} (return (+ x 1))]
   [:= [x2 : Integer] (return (+ x1 x1))]
   (return (* x2 x2))))
|#

