#lang racket
(define nil '())
(define nothing '*nothing*)
(define (nothing? something) (eq? something nothing))
(define (last-pair lst)
  (car (reverse lst)))
(define (but-last-pair lst)
  (reverse (cdr (reverse lst))))
(define (some? fn lst)
  (cond
   ((null? lst) #f)
   (else (if (fn (car lst))
	     #t
	     (some? fn (cdr lst))))))
(define (all? fn lst)
  (cond
    ((null? lst) #t)
    (else (if (not (fn (car lst)))
              #f
              (all? fn (cdr lst))))))


(define (alert-propagators props)
  (map (lambda (p) (p)) props))

(define (make-cell)
  (let ((content nothing)
	(propagators nil))
    (define (add-propagator! p)
      (if (not (memq p propagators))
	  (begin
	    (set! propagators (cons p propagators))
	    (alert-propagators (list p)))
          'done)
      'ok)
    (define (set-content! v)
      (cond
       ((equal? content v) 'ok)
       (else (begin (set! content v)
		    (alert-propagators propagators)
		    'ok))))
    (define (dispatch m)
      (cond
       ((eq? m 'add-propagator!) add-propagator!)
       ((eq? m 'set-content!) set-content!)
       ((eq? m 'content) content)
       (else 'huh?)))
    dispatch))

(define (add-propagator! cell p)
  ((cell 'add-propagator!) p))

(define (set-content! cell v)
  ((cell 'set-content!) v))

(define (content cell)
  (cell 'content))

(define (propagator cells func-prop)
  (map (lambda (c) (add-propagator! c func-prop)) cells)
  (alert-propagators (list func-prop)))

(define (exclude-nothing func input-cells)
  (lambda args
    (let ((contents (map content input-cells)))
      (if (some? (lambda (c) (nothing? c)) contents)
	  nothing
	  (apply func args)))))

(define (func->propagator func)
  (lambda cells
    (let* ((output-cell (last-pair cells))
	   (input-cells (but-last-pair cells))
	   (mfunc (exclude-nothing func input-cells)))
      (propagator input-cells (lambda ()
				(set-content! output-cell
					      (apply mfunc (map content input-cells))))))))

(define (cond-switch cond-cell ok-cell no-cell output-cell)
  (propagator (list cond-cell ok-cell no-cell output-cell)
	      (lambda ()
		(let ((c (content cond-cell)))
		  (if (nothing? c)
		      'ok
		      (if c
			  (set-content! output-cell (content ok-cell))
			  (set-content! output-cell (content no-cell))))))))

(define (switch cond-cell input-cell output-cell)
  (cond-switch cond-cell input-cell (make-cell) output-cell))
  

(define (compound-propagator cells to-build)
  (let ((done #f))
    (define (to-do)
      (if (and (not done)
               (not (all? nothing? (map content cells))))
          (begin
            (set! done #t)
            (to-build))
          'done))
    (propagator cells to-do)))


(define plus (func->propagator +))
(define minus (func->propagator -))
(define mult (func->propagator *))
(define div (func->propagator /))
(define invert (func->propagator (lambda (x) (not x))))

        
(provide make-cell content set-content! add-propagator! func->propagator cond-switch switch compound-propagator
         plus minus mult div invert)	  
