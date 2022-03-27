#lang racket/gui
(require plot "../helpers/bitwr.rkt" "NearLossless.rkt"
         "../ArithmeticCoding/AAE.rkt"  "../ArithmeticCoding/AAD.rkt")

(define frame
  (new frame%
       [label "Near-Lossless Predictive Coding"]
       [x 0] [y 250]
       [width 1900] [height 500]))

(send frame show #t)

(define main-panel
  (new horizontal-panel%
       [parent frame]))

;------------------------------------ENCODE PANEL------------------------------------

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

(define writer #f)
(define quantized-values #f)
(define save-fixed
  (new button%
       [parent input-panel]
       [label "Save Fixed"]
       [callback
        (λ (button event)
          (when quantized-values
            (set! writer (new bit-writer% [path "saved/test.bmp.fixed.nl"]))
            (send writer write-bits (send predictors get-selection) 4)
            (send writer write-bits (send k get-value) 4)
            (send writer write-bits 0 2)
            (for ([i quantized-values]) (send writer write-bits (+ 255 i) 9))
            (send writer close-file)))]))

;------------------------------------HISTOGRAM PANEL------------------------------------

(define histogram-panel
  (new vertical-panel%
       [parent main-panel]))

(define (plot-histogram pixels dc)
  (plot/dc (discrete-histogram pixels #:y-max 100 #:add-ticks? #f #:gap 0)
           dc 0 20 (* 2 SIZE) SIZE #:x-label "0" #:y-label #f))

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
       [paint-callback
        (λ(canvas dc)
          (plot-histogram (get-histogram original-matrix) dc))]))

(define refresh-histogram
  (new button%
       [parent histogram-panel]
       [label "Refresh Histogram"]
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

;------------------------------------ERROR PANEL------------------------------------

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

(define refresh-error-image
  (new button%
       [parent error-panel]
       [label "Refresh Error Image"]
       [callback
        (λ (button event)
          (send error-bitmap set-argb-pixels 0 0 SIZE SIZE
                (matrix->bytes (vector-ref matrices (add1 (send error-choices get-selection)))
                               (string->number (send error-scale get-value))))
          (send error-canvas on-paint))]))

(define (error-pixel pixel scale)
  (define (normalize pixel)
    (define x (+ 128 (* pixel scale)))
    (exact-floor (cond [(< x 0) 0] [(> x 255) 255] [else x])))
  (list 255 (normalize pixel) (normalize pixel) (normalize pixel)))

(define (matrix->bytes matrix scale)
  (list->bytes
   (apply append (map (curryr error-pixel scale) (vector->list (flatten-matrix matrix))))))

(define error-choices
  (new radio-box%
       [parent error-panel]
       [label ""]
       [choices (list "Error prediction" "Q Error prediction")]))

(define error-scale
  (new text-field%
       [parent error-panel]
       [label ""]
       [init-value "7.5"]))
          
;------------------------------------DECODE PANEL------------------------------------

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
