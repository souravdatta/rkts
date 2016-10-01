#lang typed/racket

(require/typed racket/tcp
               [tcp-listen (-> Number TCP-Listener)]
               [tcp-accept (-> TCP-Listener (Values Input-Port Output-Port))])


(: base-ns Namespace)
(define base-ns (make-base-namespace))

(define-type Line-Info (List Number Number Any))
(define-type Line-Info-List (Listof Line-Info))
(define-type Exit (-> Line-Info Nothing))

(: line-info (-> Number Number Any Line-Info))
(define (line-info l1 l2 a)
  (list l1 l2 a))

(: line-eval (-> Line-Info Namespace Line-Info))
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


(: line-eval-list (-> Line-Info-List Line-Info-List))
(define (line-eval-list infos)
  (map (lambda ([lin : Line-Info]) (line-eval lin base-ns)) infos))


(: read-Line-Info-List (-> Any Line-Info-List))
(define (read-Line-Info-List line)
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
                       (error "not a valid Line-Info list - (number number any)")))
                 (error "not a valid Line-Info list - (number number any)")))
           
           line)
      (error "should be a list of Line-Infos")))

(: server (-> Number Void))
(define (server port)

  (: repl (-> Input-Port Output-Port Void))
  (define (repl ip op)
    (let ([read-data (read ip)])
      (writeln (line-eval-list (read-Line-Info-List read-data)) op)
      (flush-output op)
      (repl ip op)))
 
  
  (let ([listener (tcp-listen port)])
    (define-values (inp outp) (tcp-accept listener))
    (repl inp outp)))

