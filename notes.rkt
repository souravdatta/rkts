#lang typed/racket

(define-type Note (U 'C
                     'C#
                     'Db
                     'D
                     'D#
                     'Eb
                     'E
                     'F
                     'F#
                     'Gb
                     'G
                     'G#
                     'Ab
                     'A
                     'A#
                     'Bb
                     'B))


(define-type Notes-Path (Vectorof Note))

(: notes-path Notes-Path)
(define notes-path #(C
                     C#
                     Db
                     D
                     D#
                     Eb
                     E
                     F
                     F#
                     Gb
                     G
                     G#
                     Ab
                     A
                     A#
                     Bb
                     B))

(: half (-> Notes-Path Note Note))
(define (half path n)
  (let ([n-index (vector-member n path)])
    (if n-index
        (let ([index : Integer n-index]
              [n-name : String (symbol->string n)]
              [steps 1])
          (when (> (string-length n-name) 1)
            (when (string=? (string (string-ref n-name 1)) "#")
              (set! steps 2)))
          (when (= index (- (vector-length path) 1))
            (set! index -1))
          (vector-ref path (+ index steps)))
        'C)))


(: h (-> Note Note))
(define (h n)
  (half notes-path n))

(: whole (-> Notes-Path Note Note))
(define (whole path n)
  (half path (half path n)))

(: w (-> Note Note))
(define (w n)
  (whole notes-path n))

(define-type Notes-Transition (Listof (-> Note Note)))

(: major-scales Notes-Transition)
(define major-scales (list w w h w w w h))

(: majors-for-transition (-> Notes-Transition Note (Listof Note)))
(define (majors-for-transition trans n-start)
  (let ([n-temp : Note n-start]
        [notes : (Listof Note) (list n-start)])
    (map (lambda ([fn : (-> Note Note)])
           (set! n-temp (fn n-temp))
           (set! notes (cons n-temp notes)))
         trans)
    (reverse notes)))

(: majors (-> Note (Listof Note)))
(define (majors n)
  (majors-for-transition major-scales n))

(provide (all-defined-out))
