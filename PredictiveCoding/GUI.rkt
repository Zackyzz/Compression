#lang racket/gui
(require plot "PredictiveCoder.rkt")

(define frame
  (new frame%
       [label "BMP"]
       [x 500] [y 250]
       [width 700] [height 400]))

(send frame show #t)

(define main-panel
  (new horizontal-panel%
       [parent frame]))

;--------------------------ENCODE--------------------------

(define input-panel
  (new vertical-panel%
       [parent main-panel]))

(define input-bitmap (make-bitmap SIZE SIZE))
(define input-dc (send input-bitmap make-dc))
(send input-dc set-background (make-color 0 0 0))
(send input-dc clear)

(define input-canvas
  (new canvas%
       [parent input-panel]
       [paint-callback
        (λ (canvas dc)
          (send dc draw-bitmap input-bitmap 20 20))]))

(define input-buffer (make-bytes (* SIZE SIZE 4)))
(define original (get-matrix input-buffer))
(define load-button
  (new button%
       [parent input-panel]
       [label "Load file"]
       [callback
        (λ (button event)
          (define path (get-file))
          (when path
            (set! input-bitmap (read-bitmap path))
            (send input-canvas on-paint)
            (send input-bitmap get-argb-pixels 0 0 SIZE SIZE input-buffer)
            (set! original (get-matrix input-buffer))))]))

(define matrices #f)
(define encode-button
  (new button%
       [parent input-panel]
       [label "Encode"]
       [callback
        (λ (button event)
          (define coder
            (new coder%
                 [original-matrix original]
                 [size SIZE] [range 255] [k 2]
                 [predictor A+B-C]))
          (set! matrices (send coder get-matrices)))]))

;--------------------------HISTOGRAM--------------------------

(define histogram-panel
  (new vertical-panel%
       [parent main-panel]))

(define (plot-histogram pixels dc)
  (plot/dc (discrete-histogram pixels #:y-max 100 #:add-ticks? #f)
           dc 20 20 SIZE SIZE))

(define histogram-canvas
  (new canvas%
       [parent histogram-panel]
       [paint-callback
        (λ(canvas dc)
          (plot-histogram (get-histogram original) dc))]))

(define refresh-button
  (new button%
       [parent histogram-panel]
       [label "Refresh"]
       [callback
        (λ (button event)
          (when matrices
            (plot-histogram (get-histogram
                             (vector-ref matrices (send histogram-choices get-selection))
                             (string->number (send histogram-scale get-value)))
                            (send histogram-canvas get-dc))))]))

(define histogram-choices
  (new choice%
       [label ""]
       [choices (list "Original" "Error Pred" "Error Pred Q" "Decoded")]	 
       [parent histogram-panel]))

(define histogram-scale
  (new text-field%
       [label ""]
       [parent histogram-panel]
       [init-value "0.02"]))
