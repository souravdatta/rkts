#lang typed/racket

(require furtle)
(require typed/racket/gui)

(define-type Priority (U 1 2 3 4))

(: prop-map (-> Priority (Pairof String Positive-Integer)))
(define (prop-map p)
  (cond
    ((= p 1) (cons "green" 20))
    ((= p 2) (cons "yellow" 22))
    ((= p 3) (cons "orange" 24))
    ((= p 4) (cons "red" 26))
    (else (cons "blue" 1))))

(define-type Task (List Integer Integer Priority String))

(: task-sort (-> (Listof Task)
                 (Listof Task)))
(define (task-sort tasks)
  (sort tasks (λ ([x : Task]
                  [y : Task])
                (< (third x) (third y)))))

(: make-functions (-> (Listof Task) (Listof TurtleF)))
(define (make-functions tasks)
  (let ((tfs : (Listof TurtleF) '())
        (start : Integer 50))
    (for ([t : Task tasks])
      (set! tfs (cons (make-tf t start) tfs))
      (set! start (+ start 35)))
    tfs))

(: make-tf (-> Task Integer TurtleF))
(define (make-tf t rad)
  (let ([props (prop-map (third t))])
    (turtles
     (pen-color (car props))
     (pen-width (cdr props))
     (sarc (* (/ (first t) (second t)) 360)
           rad))))

(: tfs (Listof Task))
(define tfs '((16 20 3 "Task 1")
              (8 20 4 "Task 2asdasdasdsasasdasasasdsdasdsadasdasdsadasdasdsadasdsadsadsadsadsadasdasdsadsadsa")
              (10 20 3 "Task 3")
              (15 20 2 "Task 4Task 4Task 4Task 4Task 4Task 4 ")
              (1 5 1 "Task 5")))

(: show-tasks! (-> (Listof Task) Void))
(define (show-tasks! ltfs)
  (let* ([sorted-tasks (task-sort ltfs)]
         [details-showing : Boolean #f]
         [mywidth 600]
         [myheight 600]
         [master-tf (apply turtles (cons (turtle-hide) (make-functions sorted-tasks)))]
         [frame (new frame%
                     [label "Tasks"]
                     [width mywidth]
                     [height myheight])]
         [vbox (new vertical-pane%
                    [parent frame])]
         [canvas (new canvas%
                      [parent vbox]
                      [min-width (abs (- mywidth 20))]
                      [min-height (abs (- myheight 20))]
                      [paint-callback (lambda ([c : (Instance Canvas%)]
                                               [dc : (Instance DC<%>)])
                                        (let* ([fwidth : Positive-Integer (+ 1 (send frame get-width))]
                                               [fheight : Positive-Integer (+ 1 (send frame get-height))])
                                          (send dc clear)
                                          (turtle-draw (master-tf (make-turtle (/ fwidth 2)
                                                                               (/ fheight 2)))
                                                       fwidth
                                                       fheight
                                                       dc
                                                       1
                                                       "black"
                                                       "black")))])]
         [info-panel (new panel%
                          [parent vbox]
                          [min-width mywidth]
                          [min-height 100]
                          [style '(deleted vscroll auto-hscroll)])]
         [details (new button%
                       [parent vbox]
                       [label "Details"]
                       [callback (λ ([b : (Instance Button%)]
                                     [e : Any])
                                   (if (not details-showing)
                                       (send vbox add-child info-panel)
                                       (send vbox delete-child info-panel))
                                   (set! details-showing (not details-showing)))])]
         [hbox (new vertical-panel%
                    [parent info-panel])])
    (for ([t : Task sorted-tasks])
      (new canvas%
           [parent hbox]
           [min-width mywidth]
           [min-height 40]
           [paint-callback (lambda ([c : (Instance Canvas%)]
                                    [dc : (Instance DC<%>)])
                             (send dc set-pen (car (prop-map (third t))) 1 'solid)
                             (send dc set-brush (car (prop-map (third t))) 'solid)
                             (send dc draw-rectangle 0 0 (send info-panel get-width) 40)
                             (if (<= (third t) 2 )
                                 (send dc set-text-foreground "black")
                                 (send dc set-text-foreground "white"))
                             (send dc set-font (make-object font% 24 'default))
                             (send dc draw-text (fourth t) 0 0))])) 
    (send frame show #t)))



(provide show-tasks!
         Task
         Priority)
