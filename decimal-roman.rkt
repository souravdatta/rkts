#lang typed/racket


(: rdigits (-> Integer (Listof Integer)))
(define (rdigits ln)
  (: digits (-> Integer (Listof Integer)))
  (define (digits n)
    (let looper ([accm : (Listof Integer) '()]
                 [n1 n])
      (if (= n1 0)
          accm
          (looper (cons (remainder n1 10) accm)
                  (floor (/ n1 10))))))
  
  (let ([m 1])
    (map (λ ([x : Integer]) (set! m (* m 10)) (* x (floor (/ m 10)))) (reverse (digits ln)))))

(: make-conversion-rule (-> Integer String 
                            (-> Integer (Pairof String Integer))))
(define (make-conversion-rule n s)
  (λ ([x : Integer]) : (Pairof String Integer)
    (cond
      ((>= x n)
       (cons (for/fold : String
               ([sum ""])
               ([i (in-range (floor (/ x n)))])
               (string-append s sum))
             (remainder x n)))
      (else (cons "" x)))))

(define rule-1000 (make-conversion-rule 1000 "M"))
(define rule-500 (make-conversion-rule 500 "D"))
(define rule-100 (make-conversion-rule 100 "C"))
(define rule-50 (make-conversion-rule 50 "L"))
(define rule-10 (make-conversion-rule 10 "X"))
(define rule-5 (make-conversion-rule 5 "V"))
(define rule-one (make-conversion-rule 1 "I"))

(: chain-rules (-> Integer (Listof (-> Integer (Pairof String Integer)))
                   String))
(define (chain-rules n rl)
  (for/fold : String
    ([sum ""])
    ([r rl])
    (string-append
     sum
     (let* ([p (r n)])
       (set! n (cdr p))
       (car p)))))

(: convert-digit (-> Integer String))
(define (convert-digit n)

  ; 1 <= n <= 9
  (: rule-1 (-> Integer String))
  (define (rule-1 n)
    (cond
      ((= n 4) "IV")
      ((= n 9) "IX")
      (else (chain-rules n (list rule-5 rule-one)))))

  ; 10 <= n <= 99
  (: rule-2 (-> Integer String))
  (define (rule-2 n)
    (cond
      ((= n 40) "XL")
      ((= n 90) "XC")
      (else (chain-rules n (list rule-50 rule-10)))))

  ; 100 <= n <= 999
  (: rule-3 (-> Integer String))
  (define (rule-3 n)
    (cond
      ((= n 400) "CD")
      ((= n 900) "CM")
      (else (chain-rules n (list rule-100 rule-500)))))

  ; n >= 1000
  (: rule-4 (-> Integer String))
  (define (rule-4 n)
    (chain-rules n (list rule-1000)))
  
  (cond
    ((and (> n 0)
          (< n 10)) (rule-1 n))
    ((and (> n 9)
          (< n 100)) (rule-2 n))
    ((and (> n 99)
          (< n 1000)) (rule-3 n))
    ((and (> n 999)
          (< n 5000)) (rule-4 n))
    (else (error "Not in allowed range (1 - 4999)"))))

(: decimal->roman (-> Integer String))
(define (decimal->roman n)
  (apply string-append (reverse (map convert-digit (rdigits n)))))

(provide decimal->roman)
