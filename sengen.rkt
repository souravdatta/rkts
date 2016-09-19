#lang typed/racket


(define-type Grammar (Listof (Pairof Symbol (Listof (U Symbol (Listof Symbol))))))

(provide generate Grammar)

(: generate (-> Symbol Grammar (Listof Symbol)))
(define (generate sym gram)
  
  (: pick-one (-> (Listof Symbol) Symbol))
  (define (pick-one lst)
    (list-ref lst (random (length lst))))
  
  
  (: generate-rhs (-> (Listof (U Symbol (Listof Symbol))) (Listof Symbol)))
  (define (generate-rhs rhs)
    (apply append (map (lambda ([c : (U Symbol (Listof Symbol))])
                         (if (symbol? c)
                             (generate c gram)
                             (list (pick-one c))))
                       rhs)))

  
  (let ([rhs (assoc sym gram)])
    (if rhs
        (generate-rhs (cdr rhs))
        empty)))








