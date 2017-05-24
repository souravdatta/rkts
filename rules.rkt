#lang typed/racket

(require math/array)
(require typed/racket/draw)

(define-type Board (Pairof (Mutable-Array Integer) Integer))

(: make-board (-> Integer Board))
(define (make-board dim)
  (cons (array->mutable-array (build-array (vector dim dim)
                                           (λ ([idx : Indexes])
                                             0)))
        dim))

(: board-ref (-> Board Integer Integer Integer))
(define (board-ref br i j)
  (array-ref (car br) (vector i j)))

(: board-set! (-> Board Integer Integer Integer Void))
(define (board-set! br i j val)
  (array-set! (car br) (vector i j) val))

(: board-dimension (-> Board Integer))
(define (board-dimension br)
  (cdr br))

(: board-neighbours (-> Board Integer Integer (List Integer Integer Integer)))
(define (board-neighbours br i j)
  (let ([dim (board-dimension br)])
    (cond
      ((or (< i 0) (< j 0)
           (>= i dim) (>= j dim)
           (= i 0)) '(0 0 0))
      (else
       (list (if (= j 0)
                 (board-ref br
                            (- i 1)
                            (- dim 1))
                 (board-ref br
                            (- i 1)
                            (- j 1)))
             (board-ref br
                        (- i 1)
                        j)
             (if (= j (- dim 1))
                 (board-ref br
                            (- i 1)
                            0)
                 (board-ref br
                            (- i 1)
                            (+ j 1))))))))


(define-type Rule (Listof (Pairof (List Integer Integer Integer) Integer)))

(: match-rule (-> (List Integer Integer Integer) Rule Integer))
(define (match-rule threeple rul)
  (let ([r (assoc threeple rul)])
    (if r
        (cdr r)
        0)))

(: rule-generate (-> (List Integer
                           Integer
                           Integer
                           Integer
                           Integer
                           Integer
                           Integer
                           Integer)
                     Rule))
(define (rule-generate outputs)
  (let ([patterns '((1 1 1)
                    (1 1 0)
                    (1 0 1)
                    (1 0 0)
                    (0 1 1)
                    (0 1 0)
                    (0 0 1)
                    (0 0 0))])
    (map (lambda ([p : (List Integer Integer Integer)]
                  [o : Integer])
           (if (= o 0)
               (cons p 0)
               (cons p 1)))
         patterns outputs)))

(: rule90 Rule)
(define rule90 (rule-generate '(0 1 0 1 1 0 1 0)))

(: rule30 Rule)
(define rule30 (rule-generate '(0 0 0 1 1 1 1 0)))

(: apply-rule! (->* (Board Rule) (#:from-row Integer) Void))
(define (apply-rule! brd rul #:from-row [from-row 1])
  (let ([dim (cdr brd)])
    (for* ([i (in-range from-row dim)]
           [j (in-range dim)])
      (let* ([neighbors (board-neighbours brd i j)]
             [rule-result (match-rule neighbors rul)])
        (board-set! brd i j rule-result)))))

(: make-image (-> Board (Instance Bitmap%)))
(define (make-image brd)
  (let* ([dim (cdr brd)]
         [dim10 (+ (abs dim) 10)]
         [target (make-bitmap dim10 dim10)]
         [dc (new bitmap-dc% [bitmap target])])
    (send dc clear)
    (send dc set-pen "orange" 1 'solid)
    (send dc draw-rectangle 0 0 dim10 dim10)
    (for* ([i (in-range dim)]
           [j (in-range dim)])
      (if (= (board-ref brd i j) 0)
          (send dc set-pen "orange" 1 'solid)
          (send dc set-pen "red" 1 'solid))
      (send dc draw-point (+ j 5) (+ i 5)))
    target))

(: make-typical-image (-> Integer Rule (Instance Bitmap%)))
(define (make-typical-image dim rul)
  (let ([b (make-board dim)])
    (board-set! b 0 (floor (/ dim 2)) 1)
    (apply-rule! b rul)
    (make-image b)))


          

(provide (all-defined-out))

