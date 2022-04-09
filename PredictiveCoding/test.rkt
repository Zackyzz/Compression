#lang racket/gui

(define frame (new frame% [label "GUI"] [width 300] [height 200]))

(define tab-panel
  (new tab-panel%
       [parent frame] [choices (list "First" "Second")]
       [callback
        (λ (tp e)
          (case (send tp get-selection)
            [(0) (send tp change-children (λ(x) (list first-panel)))]
            [(1) (send tp change-children (λ(x) (list second-panel)))]))]))

(define first-panel (new panel% [parent tab-panel]))
(define second-panel (new panel% [parent tab-panel]))

(define first-button (new button% [parent first-panel] [label "First"]))
(define second-button (new button% [parent second-panel] [label "Second"]))

(send tab-panel change-children (λ(x) (list first-panel)))
(send frame show #t)