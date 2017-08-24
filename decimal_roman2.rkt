#lang typed/racket

(: decimal->roman (-> Integer (Listof Symbol)))
(define (decimal->roman n)

  (: convert (-> (Listof (Pairof Integer (Listof Symbol)))
                 Integer
                 (Listof Symbol)
                 (Listof Symbol)))
  (define (convert clist n accm)
    (cond
      ((empty? clist) (reverse accm))
      (else (let* ([dlist (car clist)]
                   [divider (car dlist)]
                   [index (floor (/ n divider))])
              (convert (cdr clist)
                       (remainder n divider)
                       (if (= index 0)
                           accm
                           (cons (list-ref (cdr dlist) index) accm)))))))

  (when (or (<= n 0)
            (>= n 5000))
    (error "Invalid range for conversion"))
  
  (convert '((1000 . (? M MM MMM MMMM))
             (100 . (? C CC CCC CD D DC DCC DCCC CM))
             (10 . (? X XX XXX XL L LX LXX LXXX XC))
             (1 . (? I II III IV V VI VII VIII IX)))
           n
           '()))


(: decimal->roman-string (-> Integer String))
(define (decimal->roman-string n)
  (apply string-append (map (λ ([x : Symbol]) (symbol->string x)) (decimal->roman n))))
  
