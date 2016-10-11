#lang typed/racket


(define-type Route-Map (Listof (Pairof Symbol (Listof (Pairof Symbol Number)))))

(define-type Frontier-Element (Pairof (Listof Symbol)
                                      Number))

(define-type Frontier (Listof Frontier-Element))

(define-type Arrange-Fn (-> Frontier Frontier Frontier))


(: routes Route-Map)
(define routes '((arad . ((timisoara . 118)
                          (zerind . 75)
                          (sibiu . 140)))
                 (zerind . ((arad . 75)
                            (oradea . 71)))
                 (oradea . ((zerind . 71)
                            (sibiu . 151)))
                 (timisoara . ((arad . 118)
                               (lugoj . 111)))
                 (lugoj . ((timisoara . 111)
                           (mehadia . 70)))
                 (mehadia . ((lugoj . 70)
                             (drobeta . 75)))
                 (drobeta . ((mehadia . 75)
                             (craiova . 120)))
                 (craiova . ((drobeta . 120)
                             (rimnicu-vilcea . 146)
                             (pitesti . 138)))
                 (rimnicu-vilcea . ((craiova . 146)
                                    (pitesti . 97)
                                    (sibiu . 80)))
                 (pitesti . ((rimnicu-vilcea . 97)
                             (craiova . 138)
                             (bucharest . 101)))))


(: make-path (->* ((Pairof Symbol Number) Route-Map) (#:explored (Listof Symbol)) (Listof (Pairof Symbol Number))))
(define (make-path p rts #:explored [explored '()])
  (let ([paths (assoc (car p) rts)])
    (if paths
        (let ([path-list (remf (lambda ([x : (Pairof Symbol Number)]) (member (car x) explored))
                               (cdr paths))])
          (map (lambda ([x : (Pairof Symbol Number)]) (cons (car x)
                                                            (+ (cdr p) (cdr x))))
               path-list))
        empty)))


(: make-frontier (->* (Frontier-Element Route-Map) (#:explored (Listof Symbol)) Frontier))
(define (make-frontier fr rts #:explored [explored '()])
  (let ([head-list (car fr)])
    (if (not (empty? head-list))
        (let ([paths (make-path (cons (car head-list)
                                      (cdr fr)) rts #:explored explored)])
          (map (lambda ([x : (Pairof Symbol Number)])
                 (cons (cons (car x) head-list)
                       (cdr x)))
               paths))
        empty)))


(: reduce-frontier (-> Frontier (Listof Symbol) Route-Map Arrange-Fn (Values Frontier (Listof Symbol))))
(define (reduce-frontier fr explored rts arranger)
  (if (empty? fr)
      (values empty explored)
      (let* ([head (car fr)]
             [tail (cdr fr)]
             [explored1 (cons (first (car head)) explored)])
        (values (arranger tail (make-frontier head rts #:explored explored))
                explored1))))


(: BFS (-> Frontier Frontier Frontier))
(define (BFS l1 l2)
  (append l1 l2))


