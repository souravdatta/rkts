#lang racket


(require "objs.rkt")

(define-class Person ((name "Anonymous")))
(define-method (init Person nm) (set-prop! self 'name nm))
(define-method (greetings Person) (format "Hello ~a" (get self name)))

(define-class PersonWithId Person ((id 0)))
(define-method (init PersonWithId nm id)
  (tell super init nm)
  (set-prop! self 'id id))
(define-method (greetings PersonWithId) (format "~a, your ID is ~a" (tell super greetings) (get self id)))

(define ip (new PersonWithId))
(tell ip init "Antara" 6565)
(println (tell ip greetings))
(println (tell ip describe))
