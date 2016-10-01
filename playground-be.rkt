#lang typed/racket

(require/typed racket/tcp
               [opaque TCP-Listener tcp-listener?]
               [tcp-listen (-> Number TCP-Listener)]
               [tcp-accept (-> TCP-Listener (Values Number Number))])
               

(: base-ns Namespace)
(define base-ns (make-base-namespace))

(define-type Line-info (List Number Number Any))
(define-type Line-info-list (Listof Line-info))
(define-type Exit (-> Line-info Nothing))

(: line-info (-> Number Number Any Line-info))
(define (line-info l1 l2 a)
  (list l1 l2 a))

(: line-eval (-> Line-info Namespace Line-info))
(define (line-eval linfo ns)
  (call/cc (lambda ([exit : Exit])
             (call-with-exception-handler
              (lambda (e)
                (exit (line-info (first linfo)
                                 (second linfo)
                                 (list e))))
              (lambda ()
                (call-with-values (lambda ()
                                    (eval (third linfo) ns))
                                  (lambda args
                                    (line-info (first linfo)
                                               (second linfo)
                                               args))))))))


(: line-eval-list (-> Line-info-list Line-info-list))
(define (line-eval-list infos)
  (map (lambda ([lin : Line-info]) (line-eval lin base-ns)) infos))


(: read-line-info-list (-> Any Line-info-list))
(define (read-line-info-list line)
  (if (list? line)
      (map (lambda (e)
             (if (and
                  (list? e)
                  (= (length e) 3))
                 (let ([n1 (first e)]
                       [n2 (second e)]
                       [n3 (third e)])
                   (if (and (number? n1)
                            (number? n2))
                       (line-info n1 n2 n3)
                       (error "not a valid line-info list - (number number any)")))
                 (error "not a valid line-info list - (number number any)")))
                 
           line)
      (error "should be a list of line-infos")))

(: server (-> Number Void))
(define (server port)
  (let ([listener (tcp-listen port)])
    (define-values (inp outp) (tcp-accept listener))
    (displayln inp)
    (displayln outp)))
