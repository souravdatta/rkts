#lang typed/racket

(require/typed racket/tcp
               [tcp-listen (-> Number TCP-Listener)]
               [tcp-accept (-> TCP-Listener (Values Input-Port Output-Port))]
               [tcp-connect (-> String Number (Values Input-Port Output-Port))])


(: base-ns Namespace)
(define base-ns (make-base-namespace))

(define-type Line-Info (List Number Number Any))
(define-type Line-Info-List (Listof Line-Info))
(define-type Exit (-> Line-Info Nothing))


(: stringify (-> Any String))
(define (stringify sym)
  (format "~a" sym))

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
                                 "Error")))
              (lambda ()
                (call-with-values (lambda ()
                                    (eval (third linfo) ns))
                                  (lambda args
                                    (line-info (first linfo)
                                               (second linfo)
                                               (map stringify args)))))))))


(: line-eval-list (-> Line-Info-List Line-Info-List))
(define (line-eval-list infos)
  (map (lambda ([lin : Line-Info]) (line-eval lin base-ns)) infos))


(: read-Line-Info-List (-> Any Line-Info-List))
(define (read-Line-Info-List line)
  (if (string? line)
      (read-Line-Info-List (read (open-input-string line)))
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
          (error (format "[~a] : should be a list of Line-Infos" line)))))

;; The REPL

(: repl (-> Input-Port Output-Port Void))
(define (repl ip op)
  (let* ([read-data (read ip)]
         [result (line-eval-list (read-Line-Info-List read-data))])
    (writeln read-data)
    (writeln result op)
    (flush-output op)
    (displayln result)
    (repl ip op)))
  

;; Server component

(: server (-> Number Void))
(define (server port)
  (let ([listener (tcp-listen port)])
    (define-values (inp outp) (tcp-accept listener))
    (repl inp outp)))

;; Standard cmd line repl

(: cmdline (-> Void))
(define (cmdline)
  (repl (current-input-port)
        (current-output-port)))

#|

(server 8080)

|#


;; Client components

(: remote-connect (-> String Number (Pairof Input-Port Output-Port)))
(define (remote-connect host port)
  (define-values (i o) (tcp-connect host port))
  (cons i o))


(: remote-disconnect (-> (Pairof Input-Port Output-Port) Void))
(define (remote-disconnect p)
  (close-input-port (car p))
  (close-output-port (cdr p)))


(: remote-eval (-> String (Pairof Input-Port Output-Port) Void))
(define (remote-eval expr p)
  (writeln (format "((0 0 ~a))" expr) (cdr p))
  (flush-output (cdr p))
  (let ([line (read (car p))])
    (displayln line)))

#|

(define p (remote-connect "localhost" 8080))
(remote-eval "(define (foo x) (+ x 1))" p)
(remote-disconnect p)

|#

(provide server remote-connect remote-eval remote-disconnect)
