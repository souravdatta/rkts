#lang typed/racket

(require "sengen.rkt")


(: G1 Grammar)
(define G1 '((S . (NP VP))
             (NP . (Article Noun))
             (VP . (Adverb Verb))
             (Article . ((a the)))
             (Noun . ((girl woman bus ant robot mad-hatter)))
             (Adverb . ((gently fluently quickly slowly readily)))
             (Verb . ((ate went played ran danced spoke)))))


(generate 'S G1)
