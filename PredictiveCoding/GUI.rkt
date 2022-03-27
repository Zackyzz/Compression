#lang racket/gui
(require plot "../helpers/bitwr.rkt" "NearLossless.rkt")

(define frame
  (new frame%
       [label "BMP"]
       [x 0] [y 250]
       [width 1800] [height 500]))

(send frame show #t)

(define main-panel
  (new horizontal-panel%
       [parent frame]))

;------------------------------------ENCODE------------------------------------

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
(define original-matrix (get-matrix input-buffer))
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
            (set! original-matrix (get-matrix input-buffer))))]))

(define matrices #f)
(define encode-button
  (new button%
       [parent input-panel]
       [label "Encode"]
       [callback
        (λ (button event)
          (define coder
            (new coder%
                 [original-matrix original-matrix]
                 [size SIZE] [range 255] [k (send k get-value)]
                 [predictor (string->procedure (send predictors get-string-selection))]))
          (set! matrices (send coder get-matrices))
          (set! quantized-values (flatten-matrix (vector-ref matrices 2))))]))

(define writer #f)
(define quantized-values #f)
(define save-button
  (new button%
       [parent input-panel]
       [label "Save"]
       [callback
        (λ (button event)
          (when quantized-values
            (set! writer (new bit-writer% [path "test.nl"]))
            (send writer write-bits (send predictors get-selection) 4)
            (send writer write-bits (send k get-value) 4)
            (send writer write-bits (send save-modes get-selection) 2)
            (for ([i quantized-values]) (send writer write-bits (+ 255 i) 9))
            (send writer close-file)))]))

(define predictors
  (new choice%
       [parent input-panel]
       [label ""]
       [choices (list "->128" "A" "B" "C" "A+B-C" "A+B/2-C/2"
                      "B+A/2-C/2" "A/2+B/2" "jpeg-ls")]))

(define k
  (new slider%
       [parent input-panel]
       [label "k"]
       [min-value 0]
       [max-value 10]
       [init-value 2]))

(define save-modes
  (new choice%
       [parent input-panel]
       [label ""]
       [choices (list "Fixed" "Table" "Arithmetic")]))

;------------------------------------HISTOGRAM------------------------------------

(define histogram-panel
  (new vertical-panel%
       [parent main-panel]))

(define (plot-histogram pixels dc)
  (plot/dc (discrete-histogram pixels #:y-max 100 #:add-ticks? #f #:gap 0)
           dc 50 20 (* 2 SIZE) SIZE #:x-label "0" #:y-label #f))

(define show-plot #f)
(define histogram-canvas%
  (class canvas%
    (super-new)
    (define/override (on-event event)
      (when (and (send event button-down? 'left) show-plot)
        (plot-new-window? #t)
        (plot
         (discrete-histogram
          (get-histogram
           (vector-ref matrices (send histogram-choices get-selection))
           (string->number (send histogram-scale get-value)))
          #:y-max 100 #:gap 0)
         #:x-label #f #:y-label #f)
        (set! show-plot #f)))))

(define histogram-canvas
  (new histogram-canvas%
       [parent histogram-panel]
       [min-width (* 2 SIZE)]
       [min-height SIZE]
       [paint-callback
        (λ(canvas dc)
          (plot-histogram (get-histogram original-matrix) dc))]))

(define refresh-button
  (new button%
       [parent histogram-panel]
       [label "Refresh"]
       [callback
        (λ (button event)
          (when matrices
            (set! show-plot #t)
            (plot-histogram (get-histogram
                             (vector-ref matrices (send histogram-choices get-selection))
                             (string->number (send histogram-scale get-value)))
                            (send histogram-canvas get-dc))))]))

(define histogram-choices
  (new choice%
       [parent histogram-panel]
       [label ""]
       [choices (list "Original" "Error Pred" "Error Pred Q" "Decoded")]))

(define histogram-scale
  (new text-field%
       [parent histogram-panel]
       [label ""]
       [init-value "0.1"]))

;------------------------------------ERROR------------------------------------

(define error-panel
  (new vertical-panel%
       [parent main-panel]))

(define error-bitmap (make-bitmap SIZE SIZE))
(define error-dc (send error-bitmap make-dc))
(send error-dc set-background (make-color 0 0 0))
(send error-dc clear)

(define error-canvas
  (new canvas%
       [parent error-panel]
       [paint-callback
        (λ (canvas dc)
          (send dc draw-bitmap error-bitmap 20 20))]))

(define q-button
  (new button%
       [parent error-panel]
       [label "Q"]
       [callback
        (λ (button event)
          (send error-bitmap set-argb-pixels 0 0 SIZE SIZE (makemake-bytes original-matrix))
          (send error-canvas on-paint))]))

(define (makemake-bytes matrix)
  (list->bytes
   (apply append (map (λ(x) (list 255 x x x))
                      (vector->list (flatten-matrix original-matrix))))))
          
;------------------------------------DECODE------------------------------------

(define decode-panel
  (new vertical-panel%
       [parent main-panel]))

(define decode-bitmap (make-bitmap SIZE SIZE))
(define decode-dc (send decode-bitmap make-dc))
(send decode-dc set-background (make-color 0 0 0))
(send decode-dc clear)

(define decoder-canvas
  (new canvas%
       [parent decode-panel]
       [paint-callback
        (λ (canvas dc)
          (send dc draw-bitmap decode-bitmap 20 20))]))
