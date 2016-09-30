#lang typed/racket


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
